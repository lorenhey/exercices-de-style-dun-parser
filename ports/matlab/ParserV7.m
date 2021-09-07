classdef ParserV7 < handle
    % Parser sintáctico generalista incremental (CKY + beam) con features.
    % - Resources neutrales: grammar.json + lexicon.json
    % - Salida: árbol bracketed estilo Penn (S-expr)
    %
    % Diseño:
    %   - Chart CKY: chart{i,j} = array de items (top-K por score, dedup cat+feats)
    %   - Items: cat, score, nodeId, featKeys, featVals, sig
    %   - Arena de nodos para reconstruir árbol

    properties
        beam (1,1) double = 8

        % Symbol table
        sym2id containers.Map
        id2sym cell

        startSym (1,1) double = 0

        % Lexicon: struct array
        lex = struct('word',{},'pos',{},'logw',{},'fk',{},'fv',{},'sig',{})

        % Grammar rules: struct array
        rules = struct('lhs',{},'rhs',{},'logw',{},'prop',{},'cs',{})

        % Indices for fast rule lookup
        unaryIndex containers.Map  % key: rhs1 cat (string) -> vector of rule idx
        binaryIndex containers.Map % key: "rhs1,rhs2" -> vector of rule idx

        % Node arena (struct array)
        arena = struct('label',{},'left',{},'right',{},'isLeaf',{},'leaf',{})
    end

    methods
        function this = ParserV7(grammarPath, lexiconPath, varargin)
            % ParserV7(grammarPath, lexiconPath, 'beam', 8)
            this.sym2id = containers.Map('KeyType','char','ValueType','double');
            this.id2sym = {};
            this.unaryIndex = containers.Map('KeyType','char','ValueType','any');
            this.binaryIndex = containers.Map('KeyType','char','ValueType','any');

            if nargin >= 2 && ~isempty(grammarPath) && ~isempty(lexiconPath)
                for k = 1:2:numel(varargin)
                    if strcmpi(varargin{k}, 'beam')
                        this.beam = varargin{k+1};
                    end
                end
                this.loadResources(grammarPath, lexiconPath);
            end
        end

        function loadResources(this, grammarPath, lexiconPath)
            % Load JSON resources
            g = jsondecode(fileread(grammarPath));
            l = jsondecode(fileread(lexiconPath));

            % Start symbol
            start = '';
            if isfield(g,'start'); start = g.start; end
            if isempty(start) && isfield(g,'root'); start = g.root; end
            if isempty(start) && isfield(g,'start_symbol'); start = g.start_symbol; end
            if isempty(start); start = 'S'; end
            this.startSym = this.intern(start);

            % Lexicon entries
            entries = [];
            if isfield(l,'entries'); entries = l.entries; end
            if isempty(entries) && isfield(l,'lexicon'); entries = l.lexicon; end
            if isempty(entries)
                this.lex = struct('word',{},'pos',{},'logw',{},'fk',{},'fv',{},'sig',{});
            else
                this.lex = this.readLexicon(entries);
            end

            % Grammar rules
            rules = [];
            if isfield(g,'rules'); rules = g.rules; end
            if isempty(rules) && isfield(g,'productions'); rules = g.productions; end
            if isempty(rules)
                this.rules = struct('lhs',{},'rhs',{},'logw',{},'prop',{},'cs',{});
            else
                this.rules = this.readRules(rules);
            end

            % Build indices
            this.buildRuleIndex();
        end

        function tree = parseSentence(this, sentence)
            % Parse a sentence -> bracketed tree string, or "" if no parse
            toks0 = this.tokenize(sentence);
            toks  = this.splitMorphology(toks0);
            n = numel(toks);
            if n == 0
                tree = "";
                return;
            end

            % Reset arena
            this.arena = struct('label',{},'left',{},'right',{},'isLeaf',{},'leaf',{});

            % Chart: n x (n+1)
            chart = cell(n, n+1);
            for i = 1:n
                chart{i,i+1} = this.initCell();
                chart{i,i+1} = this.seedLexical(chart{i,i+1}, toks{i});
                chart{i,i+1} = this.unaryClosure(chart{i,i+1});
            end

            for span = 2:n
                for i = 1:(n-span+1)
                    j = i + span;
                    cellIJ = this.initCell();

                    for k = i+1:j-1
                        left  = chart{i,k};
                        right = chart{k,j};
                        if isempty(left) || isempty(right), continue; end
                        cellIJ = this.combineCells(cellIJ, left, right);
                    end

                    cellIJ = this.unaryClosure(cellIJ);
                    chart{i,j} = cellIJ;
                end
            end

            % Pick best root (startSym)
            rootItems = chart{1,n+1};
            [bestNode, bestScore] = this.pickBestRoot(rootItems);
            if bestNode <= 0
                tree = "";
            else
                tree = this.renderTree(bestNode);
            end
            %#ok<NASGU> bestScore
        end
    end

    %% =======================
    %  Internal: JSON readers
    %% =======================
    methods (Access=private)
        function lex = readLexicon(this, entries)
            % entries can be struct array or cell array
            if iscell(entries), entries = [entries{:}]; end
            m = numel(entries);
            lex(m) = struct('word',"",'pos',0,'logw',0,'fk',[],'fv',[],'sig',"");
            for i = 1:m
                e = entries(i);

                w = "";
                if isfield(e,'word'); w = string(e.word); end
                if strlength(w)==0 && isfield(e,'form'); w = string(e.form); end

                pos = "X";
                if isfield(e,'pos'); pos = string(e.pos); end
                if isfield(e,'tag'); pos = string(e.tag); end

                ww = 1.0;
                if isfield(e,'weight'); ww = double(e.weight); end
                if isfield(e,'prob'); ww = double(e.prob); end
                ww = max(ww, 1e-12);

                [fk, fv] = this.readFeats(e);

                [fk, fv] = this.sortFeats(fk, fv);
                sig = this.featSig(fk, fv);

                lex(i).word = lower(w);
                lex(i).pos  = this.intern(pos);
                lex(i).logw = log(ww);
                lex(i).fk   = fk;
                lex(i).fv   = fv;
                lex(i).sig  = sig;
            end
        end

        function [fk, fv] = readFeats(this, e)
            % Accepts:
            % - feats/features as struct object: {"gen":"m","num":"sg"}
            % - feats/features as array of {k,v} structs: [{"k":"gen","v":"m"}, ...]
            fk = []; fv = [];
            feats = [];
            if isfield(e,'feats'); feats = e.feats; end
            if isempty(feats) && isfield(e,'features'); feats = e.features; end
            if isempty(feats), return; end

            if isstruct(feats) && ~isscalar(feats)
                feats = feats(:);
            end

            if isstruct(feats) && isscalar(feats)
                % object mapping
                names = fieldnames(feats);
                for t = 1:numel(names)
                    k = names{t};
                    v = string(feats.(k));
                    fk(end+1) = this.intern(k); %#ok<AGROW>
                    fv(end+1) = this.intern(v); %#ok<AGROW>
                end
            elseif isstruct(feats)
                % array of {k,v}
                for t = 1:numel(feats)
                    if isfield(feats(t),'k') && isfield(feats(t),'v')
                        fk(end+1) = this.intern(string(feats(t).k)); %#ok<AGROW>
                        fv(end+1) = this.intern(string(feats(t).v)); %#ok<AGROW>
                    end
                end
            elseif iscell(feats)
                feats2 = [feats{:}];
                for t = 1:numel(feats2)
                    if isfield(feats2(t),'k') && isfield(feats2(t),'v')
                        fk(end+1) = this.intern(string(feats2(t).k)); %#ok<AGROW>
                        fv(end+1) = this.intern(string(feats2(t).v)); %#ok<AGROW>
                    end
                end
            end
        end

        function rules = readRules(this, rulesJson)
            if iscell(rulesJson), rulesJson = [rulesJson{:}]; end
            m = numel(rulesJson);
            rules(m) = struct('lhs',0,'rhs',[],'logw',0,'prop',"MERGE",'cs',[]);
            for i = 1:m
                rj = rulesJson(i);

                lhs = "<?>"; 
                if isfield(rj,'lhs'); lhs = string(rj.lhs); end

                rhs = [];
                if isfield(rj,'rhs')
                    rhs = rj.rhs;
                elseif isfield(rj,'rhs_symbols')
                    rhs = rj.rhs_symbols;
                else
                    % tolerate rhs1/rhs2
                    rhs1 = ""; rhs2 = "";
                    if isfield(rj,'rhs1'); rhs1 = string(rj.rhs1); end
                    if isfield(rj,'rhs2'); rhs2 = string(rj.rhs2); end
                    if strlength(rhs2) > 0
                        rhs = [rhs1 rhs2];
                    elseif strlength(rhs1) > 0
                        rhs = rhs1;
                    end
                end
                if ischar(rhs) || isstring(rhs)
                    rhs = string(rhs);
                elseif iscell(rhs)
                    rhs = string(rhs);
                end
                rhs = rhs(:)';

                ww = 1.0;
                if isfield(rj,'weight'); ww = double(rj.weight); end
                if isfield(rj,'prob'); ww = double(rj.prob); end
                ww = max(ww, 1e-12);

                prop = "MERGE";
                if isfield(rj,'propagate'); prop = upper(string(rj.propagate)); end
                if strlength(prop) == 0; prop = "MERGE"; end

                cs = this.readConstraints(rj);

                rules(i).lhs  = this.intern(lhs);
                rules(i).rhs  = arrayfun(@(s)this.intern(s), rhs);
                rules(i).logw = log(ww);
                rules(i).prop = prop;
                rules(i).cs   = cs;
            end
        end

        function cs = readConstraints(this, rj)
            cs = struct('type',"",'target',"LEFT",'target2',"RIGHT",'key',0,'val',0,'key2',0);
            cs = cs([]); % empty
            cArr = [];
            if isfield(rj,'constraints'); cArr = rj.constraints; end
            if isempty(cArr) && isfield(rj,'conds'); cArr = rj.conds; end
            if isempty(cArr), return; end

            if iscell(cArr), cArr = [cArr{:}]; end
            if ~isstruct(cArr), return; end

            cs(numel(cArr)) = struct('type',"",'target',"LEFT",'target2',"RIGHT",'key',0,'val',0,'key2',0);
            for i = 1:numel(cArr)
                c = cArr(i);
                typ = "REQUIRE";
                if isfield(c,'type'); typ = upper(string(c.type)); end
                tgt = "LEFT";
                if isfield(c,'target'); tgt = upper(string(c.target)); end
                tgt2 = "RIGHT";
                if isfield(c,'target2'); tgt2 = upper(string(c.target2)); end
                if isfield(c,'other'); tgt2 = upper(string(c.other)); end

                key = ""; val = ""; key2 = "";
                if isfield(c,'key'); key = string(c.key); end
                if isfield(c,'value'); val = string(c.value); end
                if isfield(c,'key2'); key2 = string(c.key2); end

                cs(i).type   = typ;
                cs(i).target = tgt;
                cs(i).target2= tgt2;
                cs(i).key    = this.intern(key);
                cs(i).val    = this.intern(val);
                cs(i).key2   = this.intern(key2);
            end
        end

        function buildRuleIndex(this)
            this.unaryIndex = containers.Map('KeyType','char','ValueType','any');
            this.binaryIndex = containers.Map('KeyType','char','ValueType','any');

            for i = 1:numel(this.rules)
                rhs = this.rules(i).rhs;
                if numel(rhs) == 1
                    k = sprintf('%d', rhs(1));
                    if ~isKey(this.unaryIndex, k)
                        this.unaryIndex(k) = i;
                    else
                        this.unaryIndex(k) = [this.unaryIndex(k) i];
                    end
                elseif numel(rhs) == 2
                    k = sprintf('%d,%d', rhs(1), rhs(2));
                    if ~isKey(this.binaryIndex, k)
                        this.binaryIndex(k) = i;
                    else
                        this.binaryIndex(k) = [this.binaryIndex(k) i];
                    end
                end
            end
        end
    end

    %% =======================
    %  Internal: tokenization
    %% =======================
    methods (Access=private)
        function toks = tokenize(~, sentence)
            % Unicode-friendly: letters/numbers/underscore as tokens
            toks = regexp(string(sentence), '[\p{L}\p{N}_]+', 'match');
            toks = cellfun(@string, toks, 'UniformOutput', false);
        end

        function toks = splitMorphology(this, toks0)
            % - al -> a el ; del -> de el
            % - enclíticos (heurístico) opcional
            out = {};
            enclitics = ["me","te","se","lo","la","los","las","le","les","nos","os"];
            for i = 1:numel(toks0)
                t = lower(string(toks0{i}));
                if t == "al"
                    out{end+1} = "a"; %#ok<AGROW>
                    out{end+1} = "el"; %#ok<AGROW>
                elseif t == "del"
                    out{end+1} = "de"; %#ok<AGROW>
                    out{end+1} = "el"; %#ok<AGROW>
                else
                    [base, suf] = this.maybeSplitEnclitic(string(toks0{i}), enclitics);
                    if strlength(suf) > 0
                        out{end+1} = lower(base); %#ok<AGROW>
                        out{end+1} = lower(suf);  %#ok<AGROW>
                    else
                        out{end+1} = lower(string(toks0{i})); %#ok<AGROW>
                    end
                end
            end
            toks = out;
        end

        function [base, suf] = maybeSplitEnclitic(~, tok, enclitics)
            % Heurística simple: verbo + pronombre átono concatenado
            t = string(tok);
            tl = lower(t);
            base = t; suf = "";
            for s = enclitics
                if strlength(tl) > strlength(s) + 2 && endsWith(tl, s)
                    cut = strlength(tl) - strlength(s);
                    pre = extractBetween(t, 1, cut);
                    lastch = extractBetween(lower(pre), cut, cut);
                    if any(lastch == ["r","d","n"])
                        base = pre;
                        suf = extractAfter(t, cut);
                        return;
                    end
                end
            end
        end
    end

    %% =======================
    %  Internal: CKY structures
    %% =======================
    methods (Access=private)
        function cellItems = initCell(~)
            cellItems = struct('cat',{},'score',{},'node',{},'fk',{},'fv',{},'sig',{});
        end

        function cellItems = seedLexical(this, cellItems, word)
            w = string(word);
            wl = lower(w);

            any = false;
            for i = 1:numel(this.lex)
                if this.lex(i).word == wl
                    any = true;
                    nodeId = this.addLeafNode(this.lex(i).pos, w);
                    it = this.makeItem(this.lex(i).pos, this.lex(i).logw, nodeId, this.lex(i).fk, this.lex(i).fv);
                    cellItems = this.cellInsert(cellItems, it);
                end
            end

            if ~any
                % OOV fallback
                if strlength(w) > 0 && isstrprop(char(extractBetween(w,1,1)),'upper')
                    cat = this.intern("PROPN");
                else
                    cat = this.intern("NOUN");
                end
                nodeId = this.addLeafNode(cat, w);
                it = this.makeItem(cat, log(1e-6), nodeId, [], []);
                cellItems = this.cellInsert(cellItems, it);
            end
        end

        function cellItems = unaryClosure(this, cellItems)
            changed = true;
            iter = 0;

            while changed && iter < 64
                iter = iter + 1;
                changed = false;

                % Snapshot because we'll append to cellItems
                snap = cellItems;

                for a = 1:numel(snap)
                    src = snap(a);
                    key = sprintf('%d', src.cat);
                    if ~isKey(this.unaryIndex, key), continue; end
                    ruleIdx = this.unaryIndex(key);

                    for rr = ruleIdx
                        rule = this.rules(rr);

                        [ok, outFk, outFv] = this.applyUnaryConstraints(rule, src.fk, src.fv);
                        if ~ok, continue; end

                        nodeId = this.addUnaryNode(rule.lhs, src.node);
                        it = this.makeItem(rule.lhs, src.score + rule.logw, nodeId, outFk, outFv);

                        beforeN = numel(cellItems);
                        cellItems = this.cellInsert(cellItems, it);
                        if numel(cellItems) > beforeN
                            changed = true;
                        end
                    end
                end
            end
        end

        function outCell = combineCells(this, outCell, leftCell, rightCell)
            for a = 1:numel(leftCell)
                L = leftCell(a);
                for b = 1:numel(rightCell)
                    R = rightCell(b);

                    key = sprintf('%d,%d', L.cat, R.cat);
                    if ~isKey(this.binaryIndex, key), continue; end
                    ruleIdx = this.binaryIndex(key);

                    for rr = ruleIdx
                        rule = this.rules(rr);

                        [ok, outFk, outFv] = this.applyBinaryConstraints(rule, L.fk, L.fv, R.fk, R.fv);
                        if ~ok, continue; end

                        nodeId = this.addBinaryNode(rule.lhs, L.node, R.node);
                        it = this.makeItem(rule.lhs, L.score + R.score + rule.logw, nodeId, outFk, outFv);
                        outCell = this.cellInsert(outCell, it);
                    end
                end
            end
        end

        function [bestNode, bestScore] = pickBestRoot(this, cellItems)
            bestNode = 0;
            bestScore = -Inf;
            for i = 1:numel(cellItems)
                if cellItems(i).cat == this.startSym && cellItems(i).score > bestScore
                    bestScore = cellItems(i).score;
                    bestNode = cellItems(i).node;
                end
            end
        end
    end

    %% =======================
    %  Internal: constraints + features
    %% =======================
    methods (Access=private)
        function [ok, outFk, outFv] = applyUnaryConstraints(this, rule, childFk, childFv)
            ok = true;
            outFk = childFk; outFv = childFv;

            cs = rule.cs;
            for i = 1:numel(cs)
                typ = cs(i).type;
                switch typ
                    case "REQUIRE"
                        v = this.featGet(childFk, childFv, cs(i).key);
                        if v ~= cs(i).val
                            ok = false; return;
                        end
                    case "ASSIGN"
                        [outFk, outFv] = this.featPut(outFk, outFv, cs(i).key, cs(i).val);
                    otherwise
                        % ignore others for unary
                end
            end

            [outFk, outFv] = this.sortFeats(outFk, outFv);
        end

        function [ok, outFk, outFv] = applyBinaryConstraints(this, rule, lfK, lfV, rfK, rfV)
            ok = true;

            % propagate base feats
            prop = rule.prop;
            if prop == "LEFT"
                outFk = lfK; outFv = lfV;
            elseif prop == "RIGHT"
                outFk = rfK; outFv = rfV;
            else
                % MERGE: left + missing from right
                [outFk, outFv] = this.mergeFeats(lfK, lfV, rfK, rfV);
            end

            cs = rule.cs;
            for i = 1:numel(cs)
                typ = cs(i).type;
                switch typ
                    case "REQUIRE"
                        if cs(i).target == "RIGHT"
                            v = this.featGet(rfK, rfV, cs(i).key);
                        else
                            v = this.featGet(lfK, lfV, cs(i).key);
                        end
                        if v ~= cs(i).val
                            ok = false; return;
                        end

                    case "UNIFY"
                        k = cs(i).key;
                        v1 = this.featGet(lfK, lfV, k);
                        v2 = this.featGet(rfK, rfV, k);
                        if v1 ~= 0 && v2 ~= 0 && v1 ~= v2
                            ok = false; return;
                        elseif v1 ~= 0
                            [outFk, outFv] = this.featPut(outFk, outFv, k, v1);
                        elseif v2 ~= 0
                            [outFk, outFv] = this.featPut(outFk, outFv, k, v2);
                        end

                    case "AGREE"
                        k = cs(i).key;
                        v1 = this.featGet(lfK, lfV, k);
                        v2 = this.featGet(rfK, rfV, k);
                        if v1 == 0 || v2 == 0 || v1 ~= v2
                            ok = false; return;
                        end
                        [outFk, outFv] = this.featPut(outFk, outFv, k, v1);

                    case "ASSIGN"
                        [outFk, outFv] = this.featPut(outFk, outFv, cs(i).key, cs(i).val);

                    otherwise
                        % ignore unknown
                end
            end

            [outFk, outFv] = this.sortFeats(outFk, outFv);
        end

        function v = featGet(~, fk, fv, key)
            v = 0;
            if isempty(fk), return; end
            idx = find(fk == key, 1, 'first');
            if ~isempty(idx), v = fv(idx); end
        end

        function [fk, fv] = featPut(~, fk, fv, key, val)
            if key == 0 || val == 0, return; end
            if isempty(fk)
                fk = key; fv = val; return;
            end
            idx = find(fk == key, 1, 'first');
            if isempty(idx)
                fk(end+1) = key; %#ok<AGROW>
                fv(end+1) = val; %#ok<AGROW>
            else
                fv(idx) = val;
            end
        end

        function [fk, fv] = sortFeats(~, fk, fv)
            if isempty(fk), return; end
            [~, ord] = sortrows([fk(:) fv(:)], [1 2]);
            fk = fk(ord)';
            fv = fv(ord)';
        end

        function sig = featSig(~, fk, fv)
            if isempty(fk), sig = ""; return; end
            sig = join(string(fk) + ":" + string(fv), ",");
        end

        function [fk, fv] = mergeFeats(this, fk1, fv1, fk2, fv2)
            fk = fk1; fv = fv1;
            for i = 1:numel(fk2)
                if isempty(find(fk == fk2(i), 1))
                    fk(end+1) = fk2(i); %#ok<AGROW>
                    fv(end+1) = fv2(i); %#ok<AGROW>
                end
            end
            [fk, fv] = this.sortFeats(fk, fv);
        end
    end

    %% =======================
    %  Internal: chart cell insert (beam + dedup)
    %% =======================
    methods (Access=private)
        function it = makeItem(this, cat, score, nodeId, fk, fv)
            [fk, fv] = this.sortFeats(fk, fv);
            it = struct( ...
                'cat', double(cat), ...
                'score', double(score), ...
                'node', double(nodeId), ...
                'fk', fk, ...
                'fv', fv, ...
                'sig', this.featSig(fk, fv) ...
            );
        end

        function cellItems = cellInsert(this, cellItems, it)
            % dedup by (cat, sig); keep best score; then keep top beam
            if isempty(cellItems)
                cellItems = it;
                return;
            end

            for i = 1:numel(cellItems)
                if cellItems(i).cat == it.cat && cellItems(i).sig == it.sig
                    if it.score > cellItems(i).score
                        cellItems(i) = it;
                    end
                    cellItems = this.sortCell(cellItems);
                    cellItems = this.trimBeam(cellItems);
                    return;
                end
            end

            cellItems(end+1) = it; %#ok<AGROW>
            cellItems = this.sortCell(cellItems);
            cellItems = this.trimBeam(cellItems);
        end

        function cellItems = sortCell(~, cellItems)
            [~, ord] = sort([cellItems.score], 'descend');
            cellItems = cellItems(ord);
        end

        function cellItems = trimBeam(this, cellItems)
            if numel(cellItems) > this.beam
                cellItems = cellItems(1:this.beam);
            end
        end
    end

    %% =======================
    %  Internal: arena + render
    %% =======================
    methods (Access=private)
        function nodeId = addLeafNode(this, label, leaf)
            nodeId = numel(this.arena) + 1;
            this.arena(nodeId).label  = double(label);
            this.arena(nodeId).left   = 0;
            this.arena(nodeId).right  = 0;
            this.arena(nodeId).isLeaf = true;
            this.arena(nodeId).leaf   = string(leaf);
        end

        function nodeId = addUnaryNode(this, label, child)
            nodeId = numel(this.arena) + 1;
            this.arena(nodeId).label  = double(label);
            this.arena(nodeId).left   = double(child);
            this.arena(nodeId).right  = 0;
            this.arena(nodeId).isLeaf = false;
            this.arena(nodeId).leaf   = "";
        end

        function nodeId = addBinaryNode(this, label, left, right)
            nodeId = numel(this.arena) + 1;
            this.arena(nodeId).label  = double(label);
            this.arena(nodeId).left   = double(left);
            this.arena(nodeId).right  = double(right);
            this.arena(nodeId).isLeaf = false;
            this.arena(nodeId).leaf   = "";
        end

        function s = renderTree(this, nodeId)
            n = this.arena(nodeId);
            lab = this.symStr(n.label);

            if n.isLeaf
                s = "(" + lab + " " + n.leaf + ")";
                return;
            end

            if n.right == 0
                a = this.renderTree(n.left);
                s = "(" + lab + " " + a + ")";
            else
                a = this.renderTree(n.left);
                b = this.renderTree(n.right);
                s = "(" + lab + " " + a + " " + b + ")";
            end
        end
    end

    %% =======================
    %  Internal: symbol table
    %% =======================
    methods (Access=private)
        function id = intern(this, s)
            ss = upper(strtrim(string(s)));
            if ss == ""
                id = 0; return;
            end
            key = char(ss);
            if isKey(this.sym2id, key)
                id = this.sym2id(key);
            else
                id = numel(this.id2sym) + 1;
                this.sym2id(key) = id;
                this.id2sym{id} = key;
            end
        end

        function s = symStr(this, id)
            if id <= 0 || id > numel(this.id2sym)
                s = "<?>"; return;
            end
            s = string(this.id2sym{id});
        end
    end
end
