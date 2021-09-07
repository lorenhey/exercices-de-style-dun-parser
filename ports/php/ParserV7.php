<?php
declare(strict_types=1);

final class ParserV7
{
    public int $beam;

    /** @var array<string,int> */
    private array $sym2id = [];
    /** @var array<int,string|null> 1-based */
    private array $id2sym = [null];

    private int $startSym = 0;

    /** @var array<string,array<int,array>> word -> entries */
    private array $lexByWord = [];

    /** @var array<int,array> */
    private array $rules = [];

    /** @var array<string,array<int,int>> rhs1 -> [ruleIdx...] */
    private array $unaryIndex = [];
    /** @var array<string,array<int,int>> "rhs1,rhs2" -> [ruleIdx...] */
    private array $binaryIndex = [];

    /** @var array<int,array> 1-based nodes */
    private array $arena = [null];

    public function __construct(string $grammarPath, string $lexiconPath, int $beam = 8)
    {
        $this->beam = max(1, $beam);
        $this->loadResources($grammarPath, $lexiconPath);
    }

    public function loadResources(string $grammarPath, string $lexiconPath): void
    {
        $g = $this->readJson($grammarPath);
        $l = $this->readJson($lexiconPath);

        $start = $g['start'] ?? $g['root'] ?? $g['start_symbol'] ?? 'S';
        $this->startSym = $this->intern((string)$start);

        $this->loadLexicon($l);
        $this->loadGrammar($g);
        $this->buildRuleIndex();
    }

    public function parseSentence(string $sentence): string
    {
        $toks0 = $this->tokenize($sentence);
        $toks  = $this->splitMorphology($toks0);
        $n = count($toks);
        if ($n === 0) return "";

        $this->arena = [null];

        // chart[i][j] spans [i,j) with i in [0..n-1], j in [1..n]
        $chart = array_fill(0, $n, array_fill(0, $n + 1, null));

        for ($i = 0; $i < $n; $i++) {
            $cell = $this->initCell();
            $this->seedLexical($cell, $toks[$i]);
            $this->unaryClosure($cell);
            $chart[$i][$i + 1] = $cell;
        }

        for ($span = 2; $span <= $n; $span++) {
            for ($i = 0; $i <= $n - $span; $i++) {
                $j = $i + $span;
                $cell = $this->initCell();

                for ($k = $i + 1; $k <= $j - 1; $k++) {
                    $left = $chart[$i][$k];
                    $right = $chart[$k][$j];
                    if ($left === null || $right === null) continue;
                    $this->combineCells($cell, $left, $right);
                }

                $this->unaryClosure($cell);
                $chart[$i][$j] = $cell;
            }
        }

        $rootCell = $chart[0][$n];
        $best = $this->pickBestRoot($rootCell);
        if ($best === null) return "";
        return $this->renderTree($best['node']);
    }

    // -------------------------
    // JSON / load
    // -------------------------
    private function readJson(string $path): array
    {
        $txt = file_get_contents($path);
        if ($txt === false) {
            throw new RuntimeException("No se pudo leer: $path");
        }
        $data = json_decode($txt, true, 512, JSON_THROW_ON_ERROR);
        if (!is_array($data)) return [];
        return $data;
    }

    private function loadLexicon(array $l): void
    {
        $this->lexByWord = [];
        $entries = $l['entries'] ?? $l['lexicon'] ?? [];
        if (!is_array($entries)) $entries = [];

        foreach ($entries as $e) {
            if (!is_array($e)) continue;

            $word = strtolower((string)($e['word'] ?? $e['form'] ?? ''));
            if ($word === '') continue;

            $pos = $this->intern((string)($e['pos'] ?? $e['tag'] ?? 'X'));

            $w = $e['weight'] ?? $e['prob'] ?? 1.0;
            $w = (is_numeric($w) && (float)$w > 0.0) ? (float)$w : 1e-12;

            [$feats, $sig] = $this->readFeats($e);

            $entry = [
                'word' => $word,
                'pos'  => $pos,
                'logw' => log($w),
                'feats'=> $feats, // keyId => valId
                'sig'  => $sig
            ];

            $this->lexByWord[$word] ??= [];
            $this->lexByWord[$word][] = $entry;
        }
    }

    private function readFeats(array $e): array
    {
        $raw = $e['feats'] ?? $e['features'] ?? null;
        $map = [];

        if (is_array($raw)) {
            // if associative: mapping; if list: list of {k,v}
            $isAssoc = array_keys($raw) !== range(0, count($raw) - 1);

            if ($isAssoc) {
                foreach ($raw as $k => $v) {
                    $kid = $this->intern((string)$k);
                    $vid = $this->intern((string)$v);
                    if ($kid && $vid) $map[$kid] = $vid;
                }
            } else {
                foreach ($raw as $p) {
                    if (!is_array($p)) continue;
                    if (!array_key_exists('k', $p) || !array_key_exists('v', $p)) continue;
                    $kid = $this->intern((string)$p['k']);
                    $vid = $this->intern((string)$p['v']);
                    if ($kid && $vid) $map[$kid] = $vid;
                }
            }
        }

        ksort($map, SORT_NUMERIC);
        $sigParts = [];
        foreach ($map as $k => $v) $sigParts[] = $k . ":" . $v;
        $sig = implode(",", $sigParts);

        return [$map, $sig];
    }

    private function loadGrammar(array $g): void
    {
        $this->rules = [];
        $rules = $g['rules'] ?? $g['productions'] ?? [];
        if (!is_array($rules)) $rules = [];

        foreach ($rules as $r) {
            if (!is_array($r)) continue;

            $lhs = $this->intern((string)($r['lhs'] ?? '<?>'));

            $rhs = $r['rhs'] ?? $r['rhs_symbols'] ?? null;
            $rhsArr = [];

            if (is_array($rhs)) {
                foreach ($rhs as $s) $rhsArr[] = (string)$s;
            } else {
                $r1 = $r['rhs1'] ?? null;
                $r2 = $r['rhs2'] ?? null;
                if ($r1 !== null && trim((string)$r1) !== '') $rhsArr[] = (string)$r1;
                if ($r2 !== null && trim((string)$r2) !== '') $rhsArr[] = (string)$r2;
            }

            if (count($rhsArr) < 1 || count($rhsArr) > 2) continue;

            $rhsIds = array_map(fn($s) => $this->intern((string)$s), $rhsArr);

            $w = $r['weight'] ?? $r['prob'] ?? 1.0;
            $w = (is_numeric($w) && (float)$w > 0.0) ? (float)$w : 1e-12;

            $prop = strtoupper(trim((string)($r['propagate'] ?? 'MERGE')));
            if ($prop === '') $prop = 'MERGE';

            $csRaw = $r['constraints'] ?? $r['conds'] ?? [];
            $cs = [];
            if (is_array($csRaw)) {
                foreach ($csRaw as $c) {
                    if (!is_array($c)) continue;
                    $cs[] = [
                        'type'    => strtoupper((string)($c['type'] ?? 'REQUIRE')),
                        'target'  => strtoupper((string)($c['target'] ?? 'LEFT')),
                        'target2' => strtoupper((string)($c['target2'] ?? ($c['other'] ?? 'RIGHT'))),
                        'key'     => $this->intern((string)($c['key'] ?? '')),
                        'val'     => $this->intern((string)($c['value'] ?? '')),
                        'key2'    => $this->intern((string)($c['key2'] ?? '')),
                    ];
                }
            }

            $this->rules[] = [
                'lhs'    => $lhs,
                'rhsLen' => count($rhsIds),
                'rhs1'   => $rhsIds[0],
                'rhs2'   => $rhsIds[1] ?? 0,
                'logw'   => log($w),
                'prop'   => $prop,
                'cs'     => $cs
            ];
        }
    }

    private function buildRuleIndex(): void
    {
        $this->unaryIndex = [];
        $this->binaryIndex = [];

        foreach ($this->rules as $i => $r) {
            if ($r['rhsLen'] === 1) {
                $k = (string)$r['rhs1'];
                $this->unaryIndex[$k] ??= [];
                $this->unaryIndex[$k][] = $i;
            } else {
                $k = $r['rhs1'] . "," . $r['rhs2'];
                $this->binaryIndex[$k] ??= [];
                $this->binaryIndex[$k][] = $i;
            }
        }
    }

    // -------------------------
    // Tokenization / morphology
    // -------------------------
    private function tokenize(string $s): array
    {
        preg_match_all('/[\p{L}\p{N}_]+/u', $s, $m);
        $t = $m[0] ?? [];
        return array_map(fn($x) => mb_strtolower((string)$x, 'UTF-8'), $t);
    }

    private function splitMorphology(array $toks): array
    {
        $encl = ['me'=>1,'te'=>1,'se'=>1,'lo'=>1,'la'=>1,'los'=>1,'las'=>1,'le'=>1,'les'=>1,'nos'=>1,'os'=>1];
        $out = [];

        foreach ($toks as $t0) {
            $t = mb_strtolower((string)$t0, 'UTF-8');

            if ($t === 'al') { $out[] = 'a'; $out[] = 'el'; continue; }
            if ($t === 'del'){ $out[] = 'de'; $out[] = 'el'; continue; }

            [$base, $suf] = $this->maybeSplitEnclitic($t, $encl);
            if ($suf !== '') { $out[] = $base; $out[] = $suf; }
            else $out[] = $t;
        }
        return $out;
    }

    private function maybeSplitEnclitic(string $tok, array $encl): array
    {
        foreach ($encl as $suf => $_) {
            if (mb_strlen($tok,'UTF-8') <= mb_strlen($suf,'UTF-8') + 2) continue;
            if (!str_ends_with($tok, $suf)) continue;

            $cut = mb_strlen($tok,'UTF-8') - mb_strlen($suf,'UTF-8');
            $base = mb_substr($tok, 0, $cut, 'UTF-8');
            $last = mb_substr($base, -1, 1, 'UTF-8');

            if ($last === 'r' || $last === 'd' || $last === 'n') {
                return [$base, $suf];
            }
        }
        return [$tok, ''];
    }

    // -------------------------
    // Symbol table
    // -------------------------
    private function intern(string $s): int
    {
        $key = strtoupper(trim($s));
        if ($key === '') return 0;
        if (isset($this->sym2id[$key])) return $this->sym2id[$key];
        $id = count($this->id2sym); // 1-based
        $this->sym2id[$key] = $id;
        $this->id2sym[$id] = $key;
        return $id;
    }

    private function symStr(int $id): string
    {
        return $this->id2sym[$id] ?? '<?>';

    }

    // -------------------------
    // Arena
    // -------------------------
    private function addLeafNode(int $label, string $leaf): int
    {
        $this->arena[] = ['label'=>$label,'left'=>0,'right'=>0,'isLeaf'=>true,'leaf'=>$leaf];
        return count($this->arena) - 1;
    }

    private function addUnaryNode(int $label, int $child): int
    {
        $this->arena[] = ['label'=>$label,'left'=>$child,'right'=>0,'isLeaf'=>false,'leaf'=>""];
        return count($this->arena) - 1;
    }

    private function addBinaryNode(int $label, int $left, int $right): int
    {
        $this->arena[] = ['label'=>$label,'left'=>$left,'right'=>$right,'isLeaf'=>false,'leaf'=>""];
        return count($this->arena) - 1;
    }

    // -------------------------
    // Cell / items
    // -------------------------
    private function initCell(): array
    {
        return ['items'=>[], 'byKey'=>[]]; // byKey: "cat|sig" -> idx
    }

    private function makeItem(int $cat, float $score, int $node, array $feats): array
    {
        ksort($feats, SORT_NUMERIC);
        $sigParts = [];
        foreach ($feats as $k => $v) $sigParts[] = $k . ":" . $v;
        $sig = implode(",", $sigParts);
        return ['cat'=>$cat,'score'=>$score,'node'=>$node,'feats'=>$feats,'sig'=>$sig];
    }

    private function cellInsert(array &$cell, array $it): void
    {
        $k = $it['cat'] . "|" . $it['sig'];

        if (isset($cell['byKey'][$k])) {
            $idx = $cell['byKey'][$k];
            if ($it['score'] > $cell['items'][$idx]['score']) {
                $cell['items'][$idx] = $it;
            }
            $this->sortTrim($cell);
            return;
        }

        $cell['items'][] = $it;
        $this->sortTrim($cell);
    }

    private function sortTrim(array &$cell): void
    {
        usort($cell['items'], fn($a,$b) => $b['score'] <=> $a['score']);
        if (count($cell['items']) > $this->beam) {
            $cell['items'] = array_slice($cell['items'], 0, $this->beam);
        }
        $cell['byKey'] = [];
        foreach ($cell['items'] as $i => $it) {
            $cell['byKey'][$it['cat']."|".$it['sig']] = $i;
        }
    }

    // -------------------------
    // Lexical seeding
    // -------------------------
    private function seedLexical(array &$cell, string $word): void
    {
        $w = mb_strtolower($word, 'UTF-8');
        $entries = $this->lexByWord[$w] ?? null;

        if (is_array($entries) && count($entries) > 0) {
            foreach ($entries as $e) {
                $node = $this->addLeafNode($e['pos'], $word);
                $it = $this->makeItem($e['pos'], (float)$e['logw'], $node, $e['feats']);
                $this->cellInsert($cell, $it);
            }
            return;
        }

        // OOV fallback
        $first = mb_substr($word, 0, 1, 'UTF-8');
        $isUpper = preg_match('/^\p{Lu}$/u', $first) === 1;
        $cat = $this->intern($isUpper ? 'PROPN' : 'NOUN');
        $node = $this->addLeafNode($cat, $word);
        $it = $this->makeItem($cat, log(1e-6), $node, []);
        $this->cellInsert($cell, $it);
    }

    // -------------------------
    // Unary closure
    // -------------------------
    private function unaryClosure(array &$cell): void
    {
        $changed = true;
        $iter = 0;

        while ($changed && $iter < 64) {
            $iter++;
            $changed = false;
            $snapshot = $cell['items'];

            foreach ($snapshot as $src) {
                $key = (string)$src['cat'];
                $idxs = $this->unaryIndex[$key] ?? null;
                if (!is_array($idxs)) continue;

                foreach ($idxs as $ri) {
                    $rule = $this->rules[$ri];

                    [$ok, $outFeats] = $this->applyUnaryConstraints($rule, $src['feats']);
                    if (!$ok) continue;

                    $node = $this->addUnaryNode($rule['lhs'], $src['node']);
                    $it = $this->makeItem($rule['lhs'], $src['score'] + $rule['logw'], $node, $outFeats);

                    $before = count($cell['items']);
                    $this->cellInsert($cell, $it);
                    if (count($cell['items']) > $before) $changed = true;
                }
            }
        }
    }

    private function applyUnaryConstraints(array $rule, array $childFeats): array
    {
        $out = $childFeats;

        foreach ($rule['cs'] as $c) {
            $typ = $c['type'];

            if ($typ === 'REQUIRE') {
                $v = $out[$c['key']] ?? 0;
                if ($v !== $c['val']) return [false, $out];
            } elseif ($typ === 'ASSIGN') {
                if ($c['key'] && $c['val']) $out[$c['key']] = $c['val'];
            }
        }

        ksort($out, SORT_NUMERIC);
        return [true, $out];
    }

    // -------------------------
    // Combine (binary)
    // -------------------------
    private function combineCells(array &$outCell, array $left, array $right): void
    {
        foreach ($left['items'] as $L) {
            foreach ($right['items'] as $R) {
                $key = $L['cat'] . "," . $R['cat'];
                $idxs = $this->binaryIndex[$key] ?? null;
                if (!is_array($idxs)) continue;

                foreach ($idxs as $ri) {
                    $rule = $this->rules[$ri];

                    [$ok, $outFeats] = $this->applyBinaryConstraints($rule, $L['feats'], $R['feats']);
                    if (!$ok) continue;

                    $node = $this->addBinaryNode($rule['lhs'], $L['node'], $R['node']);
                    $it = $this->makeItem($rule['lhs'], $L['score'] + $R['score'] + $rule['logw'], $node, $outFeats);
                    $this->cellInsert($outCell, $it);
                }
            }
        }
    }

    private function applyBinaryConstraints(array $rule, array $lf, array $rf): array
    {
        // propagate base
        $out = [];
        if ($rule['prop'] === 'LEFT') {
            $out = $lf;
        } elseif ($rule['prop'] === 'RIGHT') {
            $out = $rf;
        } else {
            $out = $lf;
            foreach ($rf as $k => $v) {
                if (!array_key_exists($k, $out)) $out[$k] = $v;
            }
        }

        foreach ($rule['cs'] as $c) {
            $typ = $c['type'];

            if ($typ === 'REQUIRE') {
                $src = ($c['target'] === 'RIGHT') ? $rf : $lf;
                $v = $src[$c['key']] ?? 0;
                if ($v !== $c['val']) return [false, $out];

            } elseif ($typ === 'UNIFY') {
                $k = $c['key'];
                $v1 = $lf[$k] ?? 0;
                $v2 = $rf[$k] ?? 0;
                if ($v1 && $v2 && $v1 !== $v2) return [false, $out];
                if ($v1) $out[$k] = $v1;
                elseif ($v2) $out[$k] = $v2;

            } elseif ($typ === 'AGREE') {
                $k = $c['key'];
                $v1 = $lf[$k] ?? 0;
                $v2 = $rf[$k] ?? 0;
                if (!$v1 || !$v2 || $v1 !== $v2) return [false, $out];
                $out[$k] = $v1;

            } elseif ($typ === 'ASSIGN') {
                if ($c['key'] && $c['val']) $out[$c['key']] = $c['val'];
            }
        }

        ksort($out, SORT_NUMERIC);
        return [true, $out];
    }

    // -------------------------
    // Root / render
    // -------------------------
    private function pickBestRoot(?array $cell): ?array
    {
        if ($cell === null) return null;
        $best = null;
        foreach ($cell['items'] as $it) {
            if ($it['cat'] === $this->startSym) {
                if ($best === null || $it['score'] > $best['score']) $best = $it;
            }
        }
        return $best;
    }

    private function renderTree(int $nodeId): string
    {
        $n = $this->arena[$nodeId] ?? null;
        if ($n === null) return '';

        $lab = $this->symStr((int)$n['label']);

        if ($n['isLeaf']) {
            return '(' . $lab . ' ' . $n['leaf'] . ')';
        }

        if ((int)$n['right'] === 0) {
            $a = $this->renderTree((int)$n['left']);
            return '(' . $lab . ' ' . $a . ')';
        }

        $a = $this->renderTree((int)$n['left']);
        $b = $this->renderTree((int)$n['right']);
        return '(' . $lab . ' ' . $a . ' ' . $b . ')';
    }
}
