#!/usr/bin/env ruby
# Parser V7 - Ruby
# CKY + beam + unary-closure + tokenización (al/del + enclíticos 1)
# Consume lexicon.json + grammar.json (mismos archivos neutrales del experimento).
#
# Uso:
#   ruby parser_v7.rb --file corpus.txt --print --json out.json
#   ruby parser_v7.rb --text "..." --trees --print

require 'json'
require 'optparse'

# =========================
# Symtab
# =========================
class Symtab
  def initialize
    @m = {}        # Text -> id
    @id2s = []     # id -> Text
  end

  def intern(s)
    id = @m[s]
    return id if id
    id = @id2s.length
    @id2s << s
    @m[s] = id
    id
  end

  def str(id)
    @id2s[id] || "?"
  end

  def var?(id)
    s = str(id)
    s.start_with?("?")
  end
end

# =========================
# Feats helpers
# =========================
module Feats
  FNV_OFF = 1469598103934665603
  FNV_PR  = 1099511628211
  MASK64  = 0xFFFFFFFFFFFFFFFF

  module_function

  def norm(fs)
    fs.sort_by { |k,v| [k,v] }
  end

  def find(fs, key)
    fs.each { |k,v| return v if k == key }
    nil
  end

  def hash64(fs)
    h = FNV_OFF
    fs.each do |k,v|
      h = ((h ^ k) * FNV_PR) & MASK64
      h = ((h ^ v) * FNV_PR) & MASK64
    end
    h
  end

  # Unificación simple con variables "?x"
  def unify(symtab, a, b)
    mp = {}
    a.each { |k,v| mp[k] = v }

    b.each do |k,vb|
      va = mp[k]
      if va.nil?
        mp[k] = vb
        next
      end
      next if va == vb

      va_var = symtab.var?(va)
      vb_var = symtab.var?(vb)

      if va_var && !vb_var
        mp[k] = vb
      elsif !va_var && vb_var
        # keep va
      elsif va_var && vb_var
        # keep
      else
        return nil
      end
    end

    norm(mp.map { |k,v| [k,v] })
  end

  def require(symtab, fs, k, v)
    unify(symtab, fs, norm([[k,v]]))
  end

  def replace_value(fs, from_v, to_v)
    norm(fs.map { |k,v| [k, (v == from_v ? to_v : v)] })
  end
end

# =========================
# Grammar / Rules
# =========================
Rule = Struct.new(
  :lhs, :rhs_len, :rhs1, :rhs2, :weight,
  :op, :arg_key, :arg_val, :arg_type,
  :prop_idx_to_right
)

# =========================
# Token / Node / Arena / Item
# =========================
Token = Struct.new(:raw, :text, :index)

Node = Struct.new(
  :label, :is_leaf, :leaf_raw, :feats, :score,
  :left, :right, :child
)

Item = Struct.new(:cat, :feats, :feats_h, :score, :node)

class Arena
  def initialize
    @nodes = []
  end

  def add(node)
    id = @nodes.length
    @nodes << node
    id
  end

  def get(id)
    @nodes[id]
  end

  def clone_replace(root_id, from_v, to_v)
    n0 = get(root_id)

    feats2 = Feats.replace_value(n0.feats || [], from_v, to_v)

    child2 = n0.child.nil? ? nil : clone_replace(n0.child, from_v, to_v)
    left2  = n0.left.nil?  ? nil : clone_replace(n0.left, from_v, to_v)
    right2 = n0.right.nil? ? nil : clone_replace(n0.right, from_v, to_v)

    n2 = Node.new(n0.label, n0.is_leaf, n0.leaf_raw, feats2, n0.score, left2, right2, child2)
    add(n2)
  end

  def pretty(symtab, node_id)
    rec = lambda do |nid, ind|
      n = get(nid)
      pad = " " * (ind * 2)
      if n.is_leaf
        "#{pad}#{n.leaf_raw}\n"
      else
        label = symtab.str(n.label)
        feat_str = ""
        if n.feats && !n.feats.empty?
          inside = n.feats.map { |k,v| "#{symtab.str(k)}=#{symtab.str(v)}" }.join(", ")
          feat_str = " [#{inside}]"
        end
        head = "#{pad}#{label}#{feat_str}  (score=#{format('%.3f', n.score)})\n"
        if n.child
          head + rec.call(n.child, ind+1)
        else
          out = head
          out += rec.call(n.left, ind+1)  if n.left
          out += rec.call(n.right, ind+1) if n.right
          out
        end
      end
    end
    rec.call(node_id, 0)
  end
end

# =========================
# Buckets / Cells with beam + dedupe
# =========================
class Bucket
  attr_reader :items, :hashes
  def initialize
    @items = []  # desc score
    @hashes = {} # feats_h -> true
  end

  def has_hash?(h) = @hashes.key?(h)

  def insert(item, beam)
    @hashes[item.feats_h] = true
    # insert desc
    i = 0
    while i < @items.length && @items[i].score >= item.score
      i += 1
    end
    @items.insert(i, item)
    pruned = 0
    if @items.length > beam
      pruned = @items.length - beam
      @items = @items.take(beam)
      # NOTE: hashes no se achica; ok (solo dedupe conservador).
    end
    pruned
  end
end

# =========================
# Tokenización
# =========================
def split_sentences(text)
  text.split(/[.\n\r]+/).map(&:strip).reject(&:empty?)
end

def tokenize(sent)
  re = /[\p{L}]+(?:[-'][\p{L}]+)*/u
  out = []
  idx = 0

  sent.scan(re) do |raw|
    low = raw.downcase

    if low == "al"
      out << Token.new("a", "a", idx); idx += 1
      out << Token.new("el", "el", idx); idx += 1
      next
    elsif low == "del"
      out << Token.new("de", "de", idx); idx += 1
      out << Token.new("el", "el", idx); idx += 1
      next
    end

    split = enclitic_split(raw, low, idx)
    if split
      out << split[0]; out << split[1]
      idx += 2
    else
      out << Token.new(raw, low, idx)
      idx += 1
    end
  end

  out
end

def enclitic_split(raw, low, idx)
  clitics = %w[me te se lo la los las le les nos os]
  best = clitics.select { |c| low.end_with?(c) }.max_by(&:length)
  return nil unless best

  low_chars = low.chars
  base_len = low_chars.length - best.length
  return nil if base_len <= 2

  base = low_chars[0...base_len].join
  looks_verb =
    base.end_with?("ar") || base.end_with?("er") || base.end_with?("ir") ||
    base.end_with?("ando") || base.end_with?("iendo")
  return nil unless looks_verb

  raw_chars = raw.chars
  raw_base = raw_chars[0...base_len].join
  raw_cl   = raw_chars[base_len..-1].join
  t1 = Token.new(raw_base, raw_base.downcase, idx)
  t2 = Token.new(raw_cl, raw_cl.downcase, idx + 1)
  [t1, t2]
end

# =========================
# OOV guess
# =========================
def det_word?(w) = %w[el la los las].include?(w)

def guess_lex(symtab, c, tok)
  raw = tok.raw
  low = tok.text
  entries = []

  if !raw.empty? && raw[0] =~ /[A-Z]/ && !det_word?(low)
    entries << { pos: c[:PropN], weight: 0.03, feats: Feats.norm([[c[:num], c[:sg]]]) }
  end

  if low.end_with?("mente")
    entries << { pos: c[:Adv], weight: -0.03, feats: [] }
  end

  base_v_feats = Feats.norm([[c[:fin], c[:no]], [c[:obl], c[:no]]])
  if low.end_with?("ar") || low.end_with?("er") || low.end_with?("ir")
    entries << { pos: c[:Vi], weight: -0.12, feats: base_v_feats }
    entries << { pos: c[:Vt], weight: -0.14, feats: base_v_feats }
  elsif low.end_with?("ando") || low.end_with?("iendo")
    entries << { pos: c[:Vi], weight: -0.14, feats: base_v_feats }
    entries << { pos: c[:Vt], weight: -0.16, feats: base_v_feats }
  end

  if entries.empty?
    vg = symtab.intern("?g")
    vn = symtab.intern("?n")
    entries << { pos: c[:N], weight: -0.35, feats: Feats.norm([[c[:gen], vg],[c[:num], vn]]) }
  end

  entries
end

# =========================
# Ops DSL
# =========================
def apply_op(symtab, c, rule, lf, rf)
  case rule.op
  when "EMPTY"
    []
  when "LEFT"
    lf
  when "RIGHT"
    rf
  when "UNIFY"
    Feats.unify(symtab, lf, rf)
  when "REQUIRE_LEFT"
    return nil unless rule.arg_key && rule.arg_val
    Feats.require(symtab, lf, rule.arg_key, rule.arg_val)
  when "REQUIRE_RIGHT"
    return nil unless rule.arg_key && rule.arg_val
    Feats.require(symtab, rf, rule.arg_key, rule.arg_val)
  when "MAKE_GAP"
    return nil unless rule.arg_type
    Feats.norm([[c[:idx], c[:qi]], [c[:gap], rule.arg_type]])
  when "RELCLAUSE_OBL"
    ok = Feats.require(symtab, rf, c[:obl], c[:yes])
    return nil unless ok
    Feats.unify(symtab, lf, Feats.norm([[c[:gap], c[:obl]]]))
  else
    nil
  end
end

# =========================
# Sanity checks
# =========================
def has_desc_label?(arena, node_id, label)
  n = arena.get(node_id)
  return true if !n.is_leaf && n.label == label
  return true if n.child && has_desc_label?(arena, n.child, label)
  return true if n.left  && has_desc_label?(arena, n.left, label)
  return true if n.right && has_desc_label?(arena, n.right, label)
  false
end

def sanity_s_has_vpfin?(arena, c, tree_id)
  s = c[:S]
  vpfin = c[:VP_FIN]
  rec = lambda do |nid|
    n = arena.get(nid)
    found_here = (!n.is_leaf && n.label == s) && (
      (n.child && arena.get(n.child).label == vpfin) ||
      (n.left  && arena.get(n.left).label  == vpfin) ||
      (n.right && arena.get(n.right).label == vpfin)
    )
    return true if found_here
    return true if n.child && rec.call(n.child)
    return true if n.left  && rec.call(n.left)
    return true if n.right && rec.call(n.right)
    false
  end
  rec.call(tree_id)
end

def sanity_sin_takes_vpnf?(arena, c, tree_id)
  pinf = c[:Pinf]
  vpnf = c[:VP_NF]
  rec = lambda do |nid|
    n = arena.get(nid)
    ok_here = true
    if !n.is_leaf && n.label == pinf
      ok_here = has_desc_label?(arena, nid, vpnf)
    end
    return false unless ok_here
    return false if n.child && !rec.call(n.child)
    return false if n.left  && !rec.call(n.left)
    return false if n.right && !rec.call(n.right)
    true
  end
  rec.call(tree_id)
end

def sanity_enclitic_only_nf?(arena, c, tree_id)
  vp = c[:VP]
  cl = c[:Cl]
  vt = c[:Vt]
  vi = c[:Vi]
  rec = lambda do |nid|
    n = arena.get(nid)
    bad_here = false
    if !n.is_leaf && n.label == vp && n.left && n.right
      ln = arena.get(n.left)
      rn = arena.get(n.right)
      if !rn.is_leaf && rn.label == cl && !ln.is_leaf && (ln.label == vt || ln.label == vi)
        bad_here = true
      end
    end
    return false if bad_here
    return false if n.child && !rec.call(n.child)
    return false if n.left  && !rec.call(n.left)
    return false if n.right && !rec.call(n.right)
    true
  end
  rec.call(tree_id)
end

# =========================
# Load lexicon / grammar
# =========================
def load_lexicon(symtab, path)
  root = JSON.parse(File.read(path))
  ent = root["entries"] or raise "lexicon.json: falta entries{}"
  lex = {}
  ent.each do |word, arr|
    lex[word] = arr.map do |obj|
      pos_id = symtab.intern(obj["pos"])
      w = obj["weight"].to_f
      feats_obj = obj["feats"] || {}
      fs = feats_obj.map do |k,v|
        [symtab.intern(k), symtab.intern(v)]
      end
      { pos: pos_id, weight: w, feats: Feats.norm(fs) }
    end
  end
  lex
end

def load_grammar(symtab, path)
  root = JSON.parse(File.read(path))
  rules0 = root["rules"] or raise "grammar.json: falta rules[]"

  rules = []
  rules0.each do |r|
    lhs = symtab.intern(r["lhs"])
    rhs = r["rhs"]
    raise "grammar.json: rhs len debe ser 1 o 2" unless rhs.is_a?(Array) && rhs.length.between?(1,2)
    rhs1 = symtab.intern(rhs[0])
    rhs2 = rhs.length == 2 ? symtab.intern(rhs[1]) : nil
    weight = r["weight"].to_f
    op = (r["op"] || "EMPTY")
    args = r["args"] || {}
    arg_key  = args["key"]   ? symtab.intern(args["key"])   : nil
    arg_val  = args["value"] ? symtab.intern(args["value"]) : nil
    arg_type = args["type"]  ? symtab.intern(args["type"])  : nil
    post = r["post"] || []
    prop = post.include?("PROPAGATE_IDX_TO_RIGHT")
    rules << Rule.new(lhs, rhs.length, rhs1, rhs2, weight, op, arg_key, arg_val, arg_type, prop)
  end

  unary = Hash.new { |h,k| h[k] = [] }
  binary = Hash.new { |h,k| h[k] = [] }

  rules.each do |ru|
    if ru.rhs_len == 1
      unary[ru.rhs1] << ru
    else
      binary[[ru.rhs1, ru.rhs2]] << ru
    end
  end

  { rules: rules, unary: unary, binary: binary }
end

# =========================
# Constants
# =========================
def ensure_constants(symtab)
  need = %w[
    idx ?i gap obl yes fin no gen num sg
    TOK S VP_FIN Pinf VP_NF VP Cl
    N PropN Pron Adv Vi Vt
  ]
  c = {}
  need.each { |s| c[s.to_sym] = symtab.intern(s) }
  c
end

# =========================
# Chart helpers
# =========================
def cidx(i,j,n) = i*(n+1) + j

def chart_new(n)
  Array.new((n+1)*(n+1)) { {} } # each is Cell: cat -> Bucket
end

def chart_get(chart, i, j, n)
  chart[cidx(i,j,n)]
end

def chart_set(chart, i, j, n, cell)
  chart[cidx(i,j,n)] = cell
end

def cell_add_item(cell, item, beam)
  bk = (cell[item.cat] ||= Bucket.new)
  return [0, false] if bk.has_hash?(item.feats_h)
  pruned = bk.insert(item, beam)
  [pruned, true]
end

# =========================
# Unary closure
# =========================
def unary_closure(cell, symtab, c, grammar, arena, beam)
  pruned = 0
  unary_apps = 0

  loop do
    changed_any = false
    snapshot = cell.to_a # [cat, bucket]
    snapshot.each do |rhs_cat, bk|
      rules = grammar[:unary][rhs_cat]
      next if rules.nil? || rules.empty?
      items_snap = bk.items.dup
      rules.each do |ru|
        items_snap.each do |child_it|
          pf = apply_op(symtab, c, ru, child_it.feats, [])
          next unless pf
          unary_apps += 1
          score = child_it.score + ru.weight
          node = Node.new(ru.lhs, false, nil, pf, score, nil, nil, child_it.node)
          nid = arena.add(node)
          it = Item.new(ru.lhs, pf, Feats.hash64(pf), score, nid)
          pr, changed = cell_add_item(cell, it, beam)
          pruned += pr
          changed_any ||= changed
        end
      end
    end
    break unless changed_any
  end

  [cell, arena, pruned, unary_apps]
end

# =========================
# Emit lexical items
# =========================
def emit_entries(entries, tok, symtab, c, arena, cell, beam, tids)
  pruned = 0
  entries.each do |e|
    pos = e[:pos]
    w   = e[:weight]
    fs0 = e[:feats] || []

    fs1 = fs0
    if pos == c[:N] || pos == c[:PropN] || pos == c[:Pron]
      if Feats.find(fs0, c[:idx]).nil?
        tid = tids[tok.index]
        uni = Feats.unify(symtab, fs0, Feats.norm([[c[:idx], tid]]))
        fs1 = uni if uni
      end
    end

    leaf = Node.new(c[:TOK], true, tok.raw, [], w, nil, nil, nil)
    leaf_id = arena.add(leaf)
    pre = Node.new(pos, false, nil, fs1, w, nil, nil, leaf_id)
    pre_id = arena.add(pre)

    it = Item.new(pos, fs1, Feats.hash64(fs1), w, pre_id)
    pr, _ = cell_add_item(cell, it, beam)
    pruned += pr
  end
  [cell, arena, pruned]
end

# =========================
# CKY
# =========================
def parse_sentence(sent, symtab, c, lexicon, grammar, topk, beam, want_trees, want_print)
  t0 = Process.clock_gettime(Process::CLOCK_MONOTONIC, :millisecond)

  toks = tokenize(sent)
  n = toks.length
  if n == 0
    return [{ sentence: sent, tokens: 0, oovTokens: 0, parsed: false, nParsesReturned: 0,
              bestScore: nil, timeMs: 0.0, chartItemsTotal: 0, chartItemsMaxCell: 0,
              prunedByBeam: 0, unaryApplications: 0, ambiguousCells: 0,
              sanitySHasVpFin: false, sanitySinTakesVpNf: false, sanityEncliticOnlyNf: false,
              notes: ["empty"], bestTree: nil }, 0, 0, 0, 0.0]
  end

  # intern tids per sentence
  tids = {}
  (0...n).each { |i| tids[i] = symtab.intern("t#{i}") }

  chart = chart_new(n)
  arena = Arena.new

  oov = 0
  pruned_total = 0
  unary_total = 0

  # lexical init
  (0...n).each do |i|
    tok = toks[i]
    cell = chart_get(chart, i, i+1, n)

    entries = lexicon[tok.text]
    if entries.nil?
      oov += 1
      entries = guess_lex(symtab, c, tok)
    end

    cell, arena, pr = emit_entries(entries, tok, symtab, c, arena, cell, beam, tids)
    pruned_total += pr

    cell, arena, pr2, u2 = unary_closure(cell, symtab, c, grammar, arena, beam)
    pruned_total += pr2
    unary_total += u2

    chart_set(chart, i, i+1, n, cell)
  end

  # CKY spans
  (2..n).each do |span|
    (0..(n-span)).each do |i|
      j = i + span
      cell = chart_get(chart, i, j, n)

      ((i+1)...j).each do |k|
        lcell = chart_get(chart, i, k, n)
        rcell = chart_get(chart, k, j, n)
        next if lcell.empty? || rcell.empty?

        lcell.each do |cat_l, bk_l|
          rcell.each do |cat_r, bk_r|
            rules = grammar[:binary][[cat_l, cat_r]]
            next if rules.nil? || rules.empty?

            rules.each do |ru|
              bk_l.items.each do |il|
                bk_r.items.each do |ir|
                  pf = apply_op(symtab, c, ru, il.feats, ir.feats)
                  next unless pf
                  score = il.score + ir.score + ru.weight

                  right_node_id = ir.node
                  if ru.prop_idx_to_right
                    idxv = Feats.find(il.feats, c[:idx])
                    if idxv
                      right_node_id = arena.clone_replace(ir.node, c[:qi], idxv)
                    end
                  end

                  node = Node.new(ru.lhs, false, nil, pf, score, il.node, right_node_id, nil)
                  nid = arena.add(node)
                  it = Item.new(ru.lhs, pf, Feats.hash64(pf), score, nid)
                  pr, _ = cell_add_item(cell, it, beam)
                  pruned_total += pr
                end
              end
            end
          end
        end
      end

      cell, arena, pr2, u2 = unary_closure(cell, symtab, c, grammar, arena, beam)
      pruned_total += pr2
      unary_total += u2

      chart_set(chart, i, j, n, cell)
    end
  end

  # metrics
  tot_items = 0
  max_cell = 0
  amb_cells = 0
  chart.each do |cell|
    count = cell.values.sum { |bk| bk.items.length }
    tot_items += count
    max_cell = [max_cell, count].max
    amb_cells += 1 if cell.size >= 2
  end

  # best S
  cell_sn = chart_get(chart, 0, n, n)
  s_sym = c[:S]
  best_it = nil
  if (bk = cell_sn[s_sym]) && !bk.items.empty?
    best_it = bk.items.first
  end

  parsed = !best_it.nil?
  best_score = parsed ? best_it.score : nil
  n_ret = 0
  notes = []
  best_tree = nil
  s1 = s2 = s3 = false

  if parsed
    items_s = cell_sn[s_sym].items
    n_ret = [topk, items_s.length].min
    tree_id = best_it.node
    s1 = sanity_s_has_vpfin?(arena, c, tree_id)
    s2 = sanity_sin_takes_vpnf?(arena, c, tree_id)
    s3 = sanity_enclitic_only_nf?(arena, c, tree_id)
    notes << "WARN: S sin VP_FIN visible" unless s1
    notes << "WARN: 'sin' sin VP_NF bajo Pinf" unless s2
    notes << "WARN: enclítico con verbo finito" unless s3
    best_tree = want_trees ? arena.pretty(symtab, tree_id) : nil
  else
    notes << "NO_PARSE"
  end

  t1 = Process.clock_gettime(Process::CLOCK_MONOTONIC, :millisecond)
  time_ms = (t1 - t0).to_f

  row = {
    sentence: sent,
    tokens: n,
    oovTokens: oov,
    parsed: parsed,
    nParsesReturned: n_ret,
    bestScore: best_score,
    timeMs: time_ms,
    chartItemsTotal: tot_items,
    chartItemsMaxCell: max_cell,
    prunedByBeam: pruned_total,
    unaryApplications: unary_total,
    ambiguousCells: amb_cells,
    sanitySHasVpFin: s1,
    sanitySinTakesVpNf: s2,
    sanityEncliticOnlyNf: s3,
    notes: notes,
    bestTree: best_tree
  }

  if want_print
    puts "=============================================================================="
    puts sent
    puts "tokens=#{n}  oov=#{oov}  parsed=#{parsed ? 1 : 0}  parses=#{n_ret}  bestScore=#{best_score.nil? ? "null" : format('%.6f', best_score)}  time_ms=#{format('%.1f', time_ms)}"
    puts "chart_items=#{tot_items}  max_cell=#{max_cell}  pruned=#{pruned_total}  unary_apps=#{unary_total}  amb_cells=#{amb_cells}"
    puts "notes: #{notes.join('; ')}" unless notes.empty?
    puts best_tree if want_trees && best_tree
  end

  [row, parsed ? 1 : 0, n, oov, time_ms]
end

# =========================
# CLI
# =========================
opts = {
  lex: "lexicon.json",
  grammar: "grammar.json",
  file: "corpus.txt",
  text: nil,
  json: nil,
  beam: 16,
  topk: 1,
  trees: false,
  print: false
}

OptionParser.new do |o|
  o.banner = "Uso: ruby parser_v7.rb [--lex lexicon.json] [--grammar grammar.json] [--file corpus.txt | --text \"...\"] [--beam 16] [--topk 1] [--trees] [--print] [--json out.json]"
  o.on("--lex PATH")     { |v| opts[:lex] = v }
  o.on("--grammar PATH") { |v| opts[:grammar] = v }
  o.on("--file PATH")    { |v| opts[:file] = v }
  o.on("--text TEXT")    { |v| opts[:text] = v }
  o.on("--json PATH")    { |v| opts[:json] = v }
  o.on("--beam N", Integer) { |v| opts[:beam] = v }
  o.on("--topk N", Integer) { |v| opts[:topk] = v }
  o.on("--trees")        { opts[:trees] = true }
  o.on("--print")        { opts[:print] = true }
  o.on("--help")         { puts o; exit 0 }
end.parse!

symtab = Symtab.new
lexicon = load_lexicon(symtab, opts[:lex])
grammar = load_grammar(symtab, opts[:grammar])
c = ensure_constants(symtab)

corpus =
  if opts[:text]
    opts[:text]
  else
    File.read(opts[:file], encoding: "UTF-8")
  end

sentences = split_sentences(corpus)

rows = []
parsed_count = 0
tot_tok = 0
tot_oov = 0
tot_time = 0.0

sentences.each do |s|
  row, parsed01, tok_n, oov_n, t_ms =
    parse_sentence(s, symtab, c, lexicon, grammar, opts[:topk], opts[:beam], opts[:trees], opts[:print])
  rows << row
  parsed_count += parsed01
  tot_tok += tok_n
  tot_oov += oov_n
  tot_time += t_ms
end

sent_n = sentences.length
coverage = sent_n == 0 ? 0.0 : parsed_count.to_f / sent_n
avg_tok  = sent_n == 0 ? 0.0 : tot_tok.to_f / sent_n
avg_oov  = sent_n == 0 ? 0.0 : tot_oov.to_f / sent_n
avg_time = sent_n == 0 ? 0.0 : tot_time / sent_n

if opts[:print]
  puts "=============================================================================="
  puts "SUMMARY"
  puts "sentences=#{sent_n}  coverage=#{format('%.3f', coverage)}  avg_tokens=#{format('%.2f', avg_tok)}  avg_oov=#{format('%.2f', avg_oov)}  avg_time_ms=#{format('%.1f', avg_time)}  beam=#{opts[:beam]}  top_k=#{opts[:topk]}"
else
  puts "SUMMARY: sentences=#{sent_n} coverage=#{format('%.3f', coverage)} avg_time_ms=#{format('%.1f', avg_time)} beam=#{opts[:beam]} top_k=#{opts[:topk]}"
end

if opts[:json]
  summary = {
    sentences: sent_n,
    coverage: coverage,
    avgTokens: avg_tok,
    avgOov: avg_oov,
    totalTimeMs: tot_time,
    avgTimeMs: avg_time,
    beam: opts[:beam],
    topK: opts[:topk],
    rows: rows
  }
  File.write(opts[:json], JSON.pretty_generate(summary))
  puts "Wrote JSON: #{opts[:json]}"
end
