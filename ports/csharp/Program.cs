// Program.cs - Parser V7 (C# / .NET 8)
// CKY + beam + rasgos mínimos + carga grammar.json/lexicon.json + tokenización (al/del + enclíticos)
// Export JSON y pretty tree. Sin dependencias externas (usa System.Text.Json).

using System.Text;
using System.Text.Json;
using System.Text.Json.Serialization;

static class Die
{
    public static void Now(string msg)
    {
        Console.Error.WriteLine(msg);
        Environment.Exit(1);
    }
}

static class Util
{
    public static string ReadFile(string path)
    {
        if (!File.Exists(path)) Die.Now($"No puedo abrir: {path}");
        return File.ReadAllText(path, Encoding.UTF8);
    }

    public static bool EndsWith(string s, string suf) => s.EndsWith(suf, StringComparison.Ordinal);

    public static bool IsVar(string sym) => sym.Length > 0 && sym[0] == '?';

    public static string JsonEscape(string s)
    {
        // Minimal escaper
        var sb = new StringBuilder();
        sb.Append('\"');
        foreach (var c in s)
        {
            switch (c)
            {
                case '\"': sb.Append("\\\""); break;
                case '\\': sb.Append("\\\\"); break;
                case '\n': sb.Append("\\n"); break;
                case '\r': sb.Append("\\r"); break;
                case '\t': sb.Append("\\t"); break;
                default: sb.Append(c); break;
            }
        }
        sb.Append('\"');
        return sb.ToString();
    }
}

sealed class Symtab
{
    private readonly Dictionary<string, int> _m = new(StringComparer.Ordinal);
    private readonly List<string> _id2s = new();

    public int Intern(string s)
    {
        if (_m.TryGetValue(s, out var id)) return id;
        id = _id2s.Count;
        _id2s.Add(s);
        _m[s] = id;
        return id;
    }

    public string Str(int id) => (id >= 0 && id < _id2s.Count) ? _id2s[id] : "?";
}

readonly struct Feat
{
    public readonly int Key;
    public readonly int Val;
    public Feat(int k, int v) { Key = k; Val = v; }
}

sealed class Feats
{
    public List<Feat> A = new();

    public Feats() { }
    public Feats(IEnumerable<Feat> feats) { A = feats.ToList(); Sort(); }

    public int Count => A.Count;

    public void Sort()
    {
        A.Sort((x, y) =>
        {
            int c = x.Key.CompareTo(y.Key);
            return c != 0 ? c : x.Val.CompareTo(y.Val);
        });
    }

    public int FindKey(int key)
    {
        for (int i = 0; i < A.Count; i++)
            if (A[i].Key == key) return i;
        return -1;
    }

    public ulong Hash()
    {
        // FNV-1a 64
        ulong h = 1469598103934665603UL;
        foreach (var f in A)
        {
            h ^= (ulong)f.Key; h *= 1099511628211UL;
            h ^= (ulong)f.Val; h *= 1099511628211UL;
        }
        return h;
    }

    public Feats Copy() => new Feats(A);

    public static bool Unify(Symtab st, Feats a, Feats b, out Feats merged)
    {
        // Copia de a + inserción/merge de b con unificación simple (valores variables tipo "?x")
        var tmp = new List<Feat>(a.A);

        foreach (var fb in b.A)
        {
            int k = fb.Key;
            int vb = fb.Val;

            int idx = -1;
            for (int i = 0; i < tmp.Count; i++)
                if (tmp[i].Key == k) { idx = i; break; }

            if (idx < 0) { tmp.Add(fb); continue; }

            int va = tmp[idx].Val;
            if (va == vb) continue;

            var sa = st.Str(va);
            var sb = st.Str(vb);
            bool vaVar = Util.IsVar(sa);
            bool vbVar = Util.IsVar(sb);

            if (vaVar && !vbVar) { tmp[idx] = new Feat(k, vb); continue; }
            if (!vaVar && vbVar) { continue; }
            if (vaVar && vbVar) { continue; }
            merged = new Feats();
            return false;
        }

        merged = new Feats(tmp);
        return true;
    }

    public static bool Require(Symtab st, Feats f, int key, int val, out Feats merged)
    {
        var req = new Feats(new[] { new Feat(key, val) });
        return Unify(st, f, req, out merged);
    }
}

sealed class LexEntry
{
    public int Pos;
    public double Weight;
    public Feats Feats = new();
}

sealed class Lexicon
{
    public readonly Dictionary<string, List<LexEntry>> Entries = new(StringComparer.Ordinal);
}

enum Op
{
    EMPTY = 0,
    LEFT,
    RIGHT,
    UNIFY,
    REQUIRE_LEFT,
    REQUIRE_RIGHT,
    MAKE_GAP,
    RELCLAUSE_OBL
}

[Flags]
enum PostFlag
{
    NONE = 0,
    PROPAGATE_IDX_TO_RIGHT = 1
}

sealed class Rule
{
    public int Lhs;
    public int RhsLen;
    public int Rhs1;
    public int Rhs2;
    public double Weight;
    public Op Op;
    public int ArgKey = -1;
    public int ArgVal = -1;
    public int ArgType = -1;
    public PostFlag Post = PostFlag.NONE;
}

sealed class Grammar
{
    public readonly List<Rule> Rules = new();
}

static class ModelLoader
{
    public static void LoadLexicon(Symtab st, Lexicon lx, string path)
    {
        using var doc = JsonDocument.Parse(Util.ReadFile(path));
        var root = doc.RootElement;

        if (!root.TryGetProperty("entries", out var entries) || entries.ValueKind != JsonValueKind.Object)
            Die.Now("lexicon.json: falta entries{}");

        foreach (var wprop in entries.EnumerateObject())
        {
            string word = wprop.Name;
            var arr = wprop.Value;
            if (arr.ValueKind != JsonValueKind.Array) Die.Now($"lexicon.json: entries.{word} no es array");

            var list = new List<LexEntry>();
            foreach (var item in arr.EnumerateArray())
            {
                if (item.ValueKind != JsonValueKind.Object) Die.Now("lexicon.json: entry no es objeto");
                if (!item.TryGetProperty("pos", out var posEl)) Die.Now("lexicon.json: falta pos");
                if (!item.TryGetProperty("weight", out var wEl)) Die.Now("lexicon.json: falta weight");
                if (!item.TryGetProperty("feats", out var fEl)) Die.Now("lexicon.json: falta feats");

                var le = new LexEntry
                {
                    Pos = st.Intern(posEl.GetString() ?? "?"),
                    Weight = wEl.GetDouble(),
                    Feats = new Feats()
                };

                if (fEl.ValueKind == JsonValueKind.Object)
                {
                    foreach (var fp in fEl.EnumerateObject())
                    {
                        int k = st.Intern(fp.Name);
                        int v = st.Intern(fp.Value.GetString() ?? "?");
                        le.Feats.A.Add(new Feat(k, v));
                    }
                    le.Feats.Sort();
                }
                list.Add(le);
            }
            lx.Entries[word] = list;
        }
    }

    public static void LoadGrammar(Symtab st, Grammar gr, string path)
    {
        using var doc = JsonDocument.Parse(Util.ReadFile(path));
        var root = doc.RootElement;

        if (!root.TryGetProperty("rules", out var rules) || rules.ValueKind != JsonValueKind.Array)
            Die.Now("grammar.json: falta rules[]");

        foreach (var rEl in rules.EnumerateArray())
        {
            if (rEl.ValueKind != JsonValueKind.Object) Die.Now("grammar.json: regla no es objeto");

            string lhs = rEl.GetProperty("lhs").GetString() ?? "?";
            var rhsEl = rEl.GetProperty("rhs");
            double w = rEl.GetProperty("weight").GetDouble();
            string opStr = rEl.GetProperty("op").GetString() ?? "EMPTY";

            if (rhsEl.ValueKind != JsonValueKind.Array) Die.Now("grammar.json: rhs no es array");
            int rhsLen = rhsEl.GetArrayLength();
            if (rhsLen < 1 || rhsLen > 2) Die.Now("grammar.json: rhs len debe ser 1 o 2");

            var rule = new Rule
            {
                Lhs = st.Intern(lhs),
                RhsLen = rhsLen,
                Rhs1 = st.Intern(rhsEl[0].GetString() ?? "?"),
                Weight = w,
                Op = Enum.TryParse<Op>(opStr, out var op) ? op : DieOp(opStr)
            };
            if (rhsLen == 2) rule.Rhs2 = st.Intern(rhsEl[1].GetString() ?? "?");

            if (rEl.TryGetProperty("args", out var args) && args.ValueKind == JsonValueKind.Object)
            {
                if (args.TryGetProperty("key", out var kEl)) rule.ArgKey = st.Intern(kEl.GetString() ?? "?");
                if (args.TryGetProperty("value", out var vEl)) rule.ArgVal = st.Intern(vEl.GetString() ?? "?");
                if (args.TryGetProperty("type", out var tEl)) rule.ArgType = st.Intern(tEl.GetString() ?? "?");
            }

            if (rEl.TryGetProperty("post", out var post) && post.ValueKind == JsonValueKind.Array)
            {
                foreach (var p in post.EnumerateArray())
                {
                    var s = p.GetString() ?? "";
                    if (s == "PROPAGATE_IDX_TO_RIGHT") rule.Post |= PostFlag.PROPAGATE_IDX_TO_RIGHT;
                }
            }

            gr.Rules.Add(rule);
        }

        static Op DieOp(string s) { Die.Now("op desconocido: " + s); return Op.EMPTY; }
    }
}

/* =========================
 * Tokenización
 * ========================= */

sealed class Token
{
    public string Raw = "";
    public string Text = "";
    public int Index;
}

static class Tokenizer
{
    private static readonly string[] Clitics = { "me","te","se","lo","la","los","las","le","les","nos","os" };

    public static List<Token> Tokenize(string s)
    {
        var outt = new List<Token>(32);
        int idx = 0;

        int i = 0;
        while (i < s.Length)
        {
            if (!char.IsLetter(s[i])) { i++; continue; }
            int start = i;
            i++;
            while (i < s.Length)
            {
                char c = s[i];
                if (char.IsLetter(c) || c == '-' || c == '\'') i++;
                else break;
            }
            string raw = s.Substring(start, i - start);
            string low = raw.ToLowerInvariant();

            if (low == "al")
            {
                outt.Add(new Token { Raw = "a", Text = "a", Index = idx++ });
                outt.Add(new Token { Raw = "el", Text = "el", Index = idx++ });
                continue;
            }
            if (low == "del")
            {
                outt.Add(new Token { Raw = "de", Text = "de", Index = idx++ });
                outt.Add(new Token { Raw = "el", Text = "el", Index = idx++ });
                continue;
            }

            // enclítico (1): elegir el más largo
            int best = -1;
            for (int k = 0; k < Clitics.Length; k++)
            {
                if (low.EndsWith(Clitics[k], StringComparison.Ordinal))
                {
                    if (best < 0 || Clitics[k].Length > Clitics[best].Length) best = k;
                }
            }

            if (best >= 0)
            {
                int cllen = Clitics[best].Length;
                int baselen = low.Length - cllen;
                if (baselen > 2)
                {
                    string basePart = low.Substring(0, baselen);
                    bool looksVerb =
                        basePart.EndsWith("ar", StringComparison.Ordinal) ||
                        basePart.EndsWith("er", StringComparison.Ordinal) ||
                        basePart.EndsWith("ir", StringComparison.Ordinal) ||
                        basePart.EndsWith("ando", StringComparison.Ordinal) ||
                        basePart.EndsWith("iendo", StringComparison.Ordinal);

                    if (looksVerb)
                    {
                        string rawBase = raw.Substring(0, raw.Length - cllen);
                        string rawCl = raw.Substring(raw.Length - cllen);
                        outt.Add(new Token { Raw = rawBase, Text = rawBase.ToLowerInvariant(), Index = idx++ });
                        outt.Add(new Token { Raw = rawCl, Text = rawCl.ToLowerInvariant(), Index = idx++ });
                        continue;
                    }
                }
            }

            outt.Add(new Token { Raw = raw, Text = low, Index = idx++ });
        }

        return outt;
    }

    public static List<string> SplitSentences(string corpus)
    {
        var outt = new List<string>();
        int start = 0;
        for (int i = 0; i <= corpus.Length; i++)
        {
            char c = (i == corpus.Length) ? '\0' : corpus[i];
            if (c == '.' || c == '\n' || c == '\0')
            {
                int end = i;
                while (start < end && char.IsWhiteSpace(corpus[start])) start++;
                while (end > start && char.IsWhiteSpace(corpus[end - 1])) end--;
                if (end > start) outt.Add(corpus.Substring(start, end - start));
                start = i + 1;
            }
        }
        return outt;
    }
}

/* =========================
 * Árbol
 * ========================= */

sealed class Node
{
    public int Label;
    public bool IsLeafToken;
    public string? LeafRaw;

    public Feats Feats = new();
    public double Score;

    public Node? Left;
    public Node? Right;
    public Node? Child;
}

static class TreeUtil
{
    public static void ReplaceFeatValue(Node? n, int fromVal, int toVal)
    {
        if (n is null) return;
        for (int i = 0; i < n.Feats.A.Count; i++)
        {
            var f = n.Feats.A[i];
            if (f.Val == fromVal) n.Feats.A[i] = new Feat(f.Key, toVal);
        }
        ReplaceFeatValue(n.Child, fromVal, toVal);
        ReplaceFeatValue(n.Left, fromVal, toVal);
        ReplaceFeatValue(n.Right, fromVal, toVal);
    }

    public static void Pretty(Symtab st, Node? n, int indent, StringBuilder sb)
    {
        if (n is null) return;

        sb.Append(' ', indent * 2);

        if (n.IsLeafToken)
        {
            sb.AppendLine(n.LeafRaw ?? "");
            return;
        }

        sb.Append(st.Str(n.Label));
        if (n.Feats.A.Count > 0)
        {
            sb.Append(" [");
            for (int i = 0; i < n.Feats.A.Count; i++)
            {
                if (i > 0) sb.Append(", ");
                var f = n.Feats.A[i];
                sb.Append(st.Str(f.Key)).Append('=').Append(st.Str(f.Val));
            }
            sb.Append(']');
        }
        sb.Append("  (score=").Append(n.Score.ToString("0.000")).AppendLine(")");

        if (n.Child != null) Pretty(st, n.Child, indent + 1, sb);
        else
        {
            Pretty(st, n.Left, indent + 1, sb);
            Pretty(st, n.Right, indent + 1, sb);
        }
    }
}

/* =========================
 * Chart + beam
 * ========================= */

sealed class Item
{
    public int Cat;
    public Feats Feats = new();
    public ulong FeatsH;
    public double Score;
    public Node Node = new();
}

sealed class Bucket
{
    public int Cat = -1;
    public List<Item> Items = new();     // desc por score
    public List<ulong> Hashes = new();   // dedupe por feats hash
}

sealed class Cell
{
    public Dictionary<int, Bucket> B = new();
}

static class ChartUtil
{
    public static void AddItem(Cell cell, Item it, int beam, ref int pruned)
    {
        if (!cell.B.TryGetValue(it.Cat, out var bk))
        {
            bk = new Bucket { Cat = it.Cat };
            cell.B[it.Cat] = bk;
        }

        // dedupe
        for (int i = 0; i < bk.Hashes.Count; i++)
            if (bk.Hashes[i] == it.FeatsH) return;

        // insert sorted desc
        int pos = 0;
        while (pos < bk.Items.Count && bk.Items[pos].Score > it.Score) pos++;

        bk.Items.Insert(pos, it);
        bk.Hashes.Insert(pos, it.FeatsH);

        if (bk.Items.Count > beam)
        {
            int removed = bk.Items.Count - beam;
            bk.Items.RemoveRange(beam, removed);
            bk.Hashes.RemoveRange(beam, removed);
            pruned += removed;
        }
    }
}

/* =========================
 * Aplicación de reglas (op DSL)
 * ========================= */

static class RuleOps
{
    public static bool Apply(Symtab st, Rule r, Feats L, Feats R, out Feats Out)
    {
        Out = new Feats();

        switch (r.Op)
        {
            case Op.EMPTY:
                Out = new Feats();
                return true;

            case Op.LEFT:
                Out = L.Copy();
                return true;

            case Op.RIGHT:
                Out = R.Copy();
                return true;

            case Op.UNIFY:
                return Feats.Unify(st, L, R, out Out);

            case Op.REQUIRE_LEFT:
                if (r.ArgKey < 0 || r.ArgVal < 0) return false;
                return Feats.Require(st, L, r.ArgKey, r.ArgVal, out Out);

            case Op.REQUIRE_RIGHT:
                if (r.ArgKey < 0 || r.ArgVal < 0) return false;
                return Feats.Require(st, R, r.ArgKey, r.ArgVal, out Out);

            case Op.MAKE_GAP:
                {
                    int kIdx = st.Intern("idx");
                    int vQi = st.Intern("?i");
                    int kGap = st.Intern("gap");
                    int vType = r.ArgType;
                    if (vType < 0) return false;
                    Out = new Feats(new[]
                    {
                        new Feat(kIdx, vQi),
                        new Feat(kGap, vType)
                    });
                    return true;
                }

            case Op.RELCLAUSE_OBL:
                {
                    int kObl = st.Intern("obl");
                    int vYes = st.Intern("yes");
                    if (!Feats.Require(st, R, kObl, vYes, out var tmp)) return false;

                    var baseF = L.Copy();
                    int kGap = st.Intern("gap");
                    int vObl = st.Intern("obl");
                    var req = new Feats(new[] { new Feat(kGap, vObl) });
                    return Feats.Unify(st, baseF, req, out Out);
                }
        }

        return false;
    }
}

/* =========================
 * Heurísticas OOV
 * ========================= */

static class Guess
{
    static bool IsDet(string w) => w is "el" or "la" or "los" or "las";

    public static List<LexEntry> GuessLex(Symtab st, Token tk)
    {
        var outt = new List<LexEntry>();

        // PropN por mayúscula ASCII inicial
        if (tk.Raw.Length > 0 && tk.Raw[0] >= 'A' && tk.Raw[0] <= 'Z' && !IsDet(tk.Text))
        {
            outt.Add(new LexEntry
            {
                Pos = st.Intern("PropN"),
                Weight = 0.03,
                Feats = new Feats(new[] { new Feat(st.Intern("num"), st.Intern("sg")) })
            });
        }

        if (Util.EndsWith(tk.Text, "mente"))
        {
            outt.Add(new LexEntry
            {
                Pos = st.Intern("Adv"),
                Weight = -0.03,
                Feats = new Feats()
            });
        }

        void AddV(double wVi, double wVt)
        {
            var baseFeats = new Feats(new[]
            {
                new Feat(st.Intern("fin"), st.Intern("no")),
                new Feat(st.Intern("obl"), st.Intern("no"))
            });

            outt.Add(new LexEntry { Pos = st.Intern("Vi"), Weight = wVi, Feats = baseFeats.Copy() });
            outt.Add(new LexEntry { Pos = st.Intern("Vt"), Weight = wVt, Feats = baseFeats.Copy() });
        }

        if (Util.EndsWith(tk.Text, "ar") || Util.EndsWith(tk.Text, "er") || Util.EndsWith(tk.Text, "ir"))
            AddV(-0.12, -0.14);

        if (Util.EndsWith(tk.Text, "ando") || Util.EndsWith(tk.Text, "iendo"))
            AddV(-0.14, -0.16);

        if (outt.Count == 0)
        {
            outt.Add(new LexEntry
            {
                Pos = st.Intern("N"),
                Weight = -0.35,
                Feats = new Feats(new[]
                {
                    new Feat(st.Intern("gen"), st.Intern("?g")),
                    new Feat(st.Intern("num"), st.Intern("?n"))
                })
            });
        }

        return outt;
    }
}

/* =========================
 * Sanity checks
 * ========================= */

static class Sanity
{
    static bool HasDescLabel(Node? n, int label)
    {
        if (n is null) return true;
        if (!n.IsLeafToken && n.Label == label) return true;
        if (HasDescLabel(n.Child, label)) return true;
        if (HasDescLabel(n.Left, label)) return true;
        if (HasDescLabel(n.Right, label)) return true;
        return false;
    }

    static void WalkS(Node? n, int symS, int symVPFIN, ref bool found)
    {
        if (n is null) return;
        if (!n.IsLeafToken && n.Label == symS)
        {
            if (n.Child != null && n.Child.Label == symVPFIN) found = true;
            if (n.Left != null && n.Left.Label == symVPFIN) found = true;
            if (n.Right != null && n.Right.Label == symVPFIN) found = true;
        }
        WalkS(n.Child, symS, symVPFIN, ref found);
        WalkS(n.Left, symS, symVPFIN, ref found);
        WalkS(n.Right, symS, symVPFIN, ref found);
    }

    public static bool SHasVpFin(Symtab st, Node? tree)
    {
        int symS = st.Intern("S");
        int symVPFIN = st.Intern("VP_FIN");
        bool found = false;
        WalkS(tree, symS, symVPFIN, ref found);
        return found;
    }

    public static bool SinTakesVpNf(Symtab st, Node? tree)
    {
        int symPinf = st.Intern("Pinf");
        int symVPNF = st.Intern("VP_NF");
        if (tree is null) return true;

        if (!tree.IsLeafToken && tree.Label == symPinf)
            if (!HasDescLabel(tree, symVPNF)) return false;

        if (!SinTakesVpNf(st, tree.Child)) return false;
        if (!SinTakesVpNf(st, tree.Left)) return false;
        if (!SinTakesVpNf(st, tree.Right)) return false;
        return true;
    }

    public static bool EncliticOnlyNf(Symtab st, Node? tree)
    {
        int symVP = st.Intern("VP");
        int symCl = st.Intern("Cl");
        int symVt = st.Intern("Vt");
        int symVi = st.Intern("Vi");

        if (tree is null) return true;

        if (!tree.IsLeafToken && tree.Label == symVP && tree.Left != null && tree.Right != null)
        {
            if (!tree.Right.IsLeafToken && tree.Right.Label == symCl)
            {
                if (!tree.Left.IsLeafToken && (tree.Left.Label == symVt || tree.Left.Label == symVi))
                    return false;
            }
        }

        if (!EncliticOnlyNf(st, tree.Child)) return false;
        if (!EncliticOnlyNf(st, tree.Left)) return false;
        if (!EncliticOnlyNf(st, tree.Right)) return false;
        return true;
    }
}

/* =========================
 * Parse
 * ========================= */

sealed class ParseStats
{
    public string Sentence = "";
    public int Tokens;
    public int Oov;
    public bool Parsed;
    public int NParses;
    public double? BestScore;

    public int ChartItemsTotal;
    public int ChartItemsMaxCell;
    public int Pruned;
    public int UnaryApps;
    public int AmbiguousCells;

    public bool Sanity1;
    public bool Sanity2;
    public bool Sanity3;

    public List<string> Notes = new();
    public string? BestTree;
    public double TimeMs;
}

static class Parser
{
    public static void UnaryClosure(Symtab st, Grammar gr, Cell cell, int beam, ref int pruned, ref int unaryApps)
    {
        bool changed = true;
        while (changed)
        {
            changed = false;

            foreach (var r in gr.Rules)
            {
                if (r.RhsLen != 1) continue;
                if (!cell.B.TryGetValue(r.Rhs1, out var bk)) continue;

                int before = cell.B.TryGetValue(r.Lhs, out var lhsBk) ? lhsBk.Items.Count : 0;

                foreach (var ch in bk.Items)
                {
                    if (!RuleOps.Apply(st, r, ch.Feats, new Feats(), out var pf)) continue;
                    unaryApps++;

                    var it = new Item
                    {
                        Cat = r.Lhs,
                        Feats = pf,
                        FeatsH = pf.Hash(),
                        Score = ch.Score + r.Weight,
                    };

                    var nn = new Node
                    {
                        Label = r.Lhs,
                        Child = ch.Node,
                        Feats = pf,
                        Score = it.Score
                    };
                    it.Node = nn;

                    ChartUtil.AddItem(cell, it, beam, ref pruned);
                }

                int after = cell.B.TryGetValue(r.Lhs, out var lhsBk2) ? lhsBk2.Items.Count : 0;
                if (after != before) changed = true;
            }
        }
    }

    public static ParseStats ParseSentence(Symtab st, Lexicon lx, Grammar gr, string sentence, int topk, int beam, bool includeTree)
    {
        var stats = new ParseStats { Sentence = sentence };
        var sw = System.Diagnostics.Stopwatch.StartNew();

        var toks = Tokenizer.Tokenize(sentence);
        int n = toks.Count;
        stats.Tokens = n;
        if (n == 0)
        {
            stats.Notes.Add("empty");
            stats.TimeMs = 0;
            return stats;
        }

        var chart = new Cell[n, n + 1];
        for (int i = 0; i < n; i++)
            for (int j = 0; j <= n; j++)
                chart[i, j] = new Cell();

        int pruned = 0, unaryApps = 0;

        int symTOK = st.Intern("TOK");
        int symN = st.Intern("N");
        int symPropN = st.Intern("PropN");
        int symPron = st.Intern("Pron");
        int kIdx = st.Intern("idx");

        // Lexical init
        for (int i = 0; i < n; i++)
        {
            var cell = chart[i, i + 1];

            bool inLex = lx.Entries.TryGetValue(toks[i].Text, out var entries);
            if (!inLex) stats.Oov++;

            void Emit(LexEntry le)
            {
                var lf = le.Feats.Copy();

                // default idx para N/PropN/Pron
                if (le.Pos == symN || le.Pos == symPropN || le.Pos == symPron)
                {
                    if (lf.FindKey(kIdx) < 0)
                    {
                        int v = st.Intern("t" + toks[i].Index);
                        var req = new Feats(new[] { new Feat(kIdx, v) });
                        if (Feats.Unify(st, lf, req, out var merged)) lf = merged;
                    }
                }

                var leaf = new Node
                {
                    Label = symTOK,
                    IsLeafToken = true,
                    LeafRaw = toks[i].Raw,
                    Score = le.Weight
                };

                var pre = new Node
                {
                    Label = le.Pos,
                    Child = leaf,
                    Feats = lf,
                    Score = le.Weight
                };

                var it = new Item
                {
                    Cat = le.Pos,
                    Feats = lf,
                    FeatsH = lf.Hash(),
                    Score = le.Weight,
                    Node = pre
                };

                ChartUtil.AddItem(cell, it, beam, ref pruned);
            }

            if (inLex)
                foreach (var le in entries!)
                    Emit(le);

            foreach (var ge in Guess.GuessLex(st, toks[i]))
                Emit(ge);

            UnaryClosure(st, gr, cell, beam, ref pruned, ref unaryApps);
        }

        // CKY
        for (int span = 2; span <= n; span++)
        {
            for (int i = 0; i + span <= n; i++)
            {
                int j = i + span;
                var cell = chart[i, j];

                for (int k = i + 1; k < j; k++)
                {
                    var L = chart[i, k];
                    var R = chart[k, j];
                    if (L.B.Count == 0 || R.B.Count == 0) continue;

                    foreach (var rule in gr.Rules)
                    {
                        if (rule.RhsLen != 2) continue;
                        if (!L.B.TryGetValue(rule.Rhs1, out var lb)) continue;
                        if (!R.B.TryGetValue(rule.Rhs2, out var rb)) continue;

                        foreach (var ib in lb.Items)
                        {
                            foreach (var ic in rb.Items)
                            {
                                if (!RuleOps.Apply(st, rule, ib.Feats, ic.Feats, out var pf)) continue;

                                double score = ib.Score + ic.Score + rule.Weight;

                                var rightNode = ic.Node;
                                if ((rule.Post & PostFlag.PROPAGATE_IDX_TO_RIGHT) != 0)
                                {
                                    int idxPos = ib.Feats.FindKey(kIdx);
                                    if (idxPos >= 0)
                                    {
                                        int idxVal = ib.Feats.A[idxPos].Val;
                                        int from = st.Intern("?i");
                                        TreeUtil.ReplaceFeatValue(rightNode, from, idxVal);
                                    }
                                }

                                var nn = new Node
                                {
                                    Label = rule.Lhs,
                                    Left = ib.Node,
                                    Right = rightNode,
                                    Feats = pf,
                                    Score = score
                                };

                                var it = new Item
                                {
                                    Cat = rule.Lhs,
                                    Feats = pf,
                                    FeatsH = pf.Hash(),
                                    Score = score,
                                    Node = nn
                                };

                                ChartUtil.AddItem(cell, it, beam, ref pruned);
                            }
                        }
                    }
                }

                UnaryClosure(st, gr, cell, beam, ref pruned, ref unaryApps);
            }
        }

        // chart metrics
        int totalItems = 0, maxCell = 0, ambCells = 0;
        for (int i = 0; i < n; i++)
        {
            for (int j = i + 1; j <= n; j++)
            {
                int cellItems = 0;
                foreach (var kv in chart[i, j].B) cellItems += kv.Value.Items.Count;
                totalItems += cellItems;
                if (cellItems > maxCell) maxCell = cellItems;
                if (chart[i, j].B.Count >= 2) ambCells++;
            }
        }
        stats.ChartItemsTotal = totalItems;
        stats.ChartItemsMaxCell = maxCell;
        stats.AmbiguousCells = ambCells;
        stats.Pruned = pruned;
        stats.UnaryApps = unaryApps;

        // best S
        int symS = st.Intern("S");
        if (!chart[0, n].B.TryGetValue(symS, out var sBucket) || sBucket.Items.Count == 0)
        {
            stats.Parsed = false;
            stats.Notes.Add("NO_PARSE");
        }
        else
        {
            stats.Parsed = true;
            stats.NParses = Math.Min(topk, sBucket.Items.Count);
            stats.BestScore = sBucket.Items[0].Score;

            var best = sBucket.Items[0].Node;
            stats.Sanity1 = Sanity.SHasVpFin(st, best);
            stats.Sanity2 = Sanity.SinTakesVpNf(st, best);
            stats.Sanity3 = Sanity.EncliticOnlyNf(st, best);

            if (!stats.Sanity1) stats.Notes.Add("WARN: S sin VP_FIN visible");
            if (!stats.Sanity2) stats.Notes.Add("WARN: 'sin' sin VP_NF bajo Pinf");
            if (!stats.Sanity3) stats.Notes.Add("WARN: enclítico con verbo finito");

            if (includeTree)
            {
                var sb = new StringBuilder();
                TreeUtil.Pretty(st, best, 0, sb);
                stats.BestTree = sb.ToString();
            }
        }

        sw.Stop();
        stats.TimeMs = sw.Elapsed.TotalMilliseconds;
        return stats;
    }
}

/* =========================
 * JSON export
 * ========================= */

static class Export
{
    public static void WriteSummaryJson(string path, List<ParseStats> rows, int beam, int topk)
    {
        int sent = rows.Count;
        int parsed = rows.Count(r => r.Parsed);
        double coverage = sent == 0 ? 0 : (double)parsed / sent;

        double avgTokens = sent == 0 ? 0 : rows.Average(r => r.Tokens);
        double avgOov = sent == 0 ? 0 : rows.Average(r => r.Oov);
        double totalTime = rows.Sum(r => r.TimeMs);
        double avgTime = sent == 0 ? 0 : rows.Average(r => r.TimeMs);

        using var w = new StreamWriter(path, false, Encoding.UTF8);
        w.WriteLine("{");
        w.WriteLine($"  \"sentences\": {sent},");
        w.WriteLine($"  \"coverage\": {coverage:0.000000},");
        w.WriteLine($"  \"avgTokens\": {avgTokens:0.000000},");
        w.WriteLine($"  \"avgOov\": {avgOov:0.000000},");
        w.WriteLine($"  \"totalTimeMs\": {totalTime:0.000000},");
        w.WriteLine($"  \"avgTimeMs\": {avgTime:0.000000},");
        w.WriteLine($"  \"beam\": {beam},");
        w.WriteLine($"  \"topK\": {topk},");
        w.WriteLine("  \"rows\": [");

        for (int i = 0; i < rows.Count; i++)
        {
            var r = rows[i];
            w.WriteLine("    {");
            w.WriteLine($"      \"sentence\": {Util.JsonEscape(r.Sentence)},");
            w.WriteLine($"      \"tokens\": {r.Tokens},");
            w.WriteLine($"      \"oovTokens\": {r.Oov},");
            w.WriteLine($"      \"parsed\": {(r.Parsed ? "true" : "false")},");
            w.WriteLine($"      \"nParsesReturned\": {r.NParses},");
            w.WriteLine($"      \"bestScore\": {(r.BestScore.HasValue ? r.BestScore.Value.ToString("0.000000") : "null")},");
            w.WriteLine($"      \"timeMs\": {r.TimeMs:0.000000},");
            w.WriteLine($"      \"chartItemsTotal\": {r.ChartItemsTotal},");
            w.WriteLine($"      \"chartItemsMaxCell\": {r.ChartItemsMaxCell},");
            w.WriteLine($"      \"prunedByBeam\": {r.Pruned},");
            w.WriteLine($"      \"unaryApplications\": {r.UnaryApps},");
            w.WriteLine($"      \"ambiguousCells\": {r.AmbiguousCells},");
            w.WriteLine($"      \"sanitySHasVpFin\": {(r.Sanity1 ? "true" : "false")},");
            w.WriteLine($"      \"sanitySinTakesVpNf\": {(r.Sanity2 ? "true" : "false")},");
            w.WriteLine($"      \"sanityEncliticOnlyNf\": {(r.Sanity3 ? "true" : "false")},");
            w.Write("      \"notes\": [");
            for (int k = 0; k < r.Notes.Count; k++)
            {
                if (k > 0) w.Write(", ");
                w.Write(Util.JsonEscape(r.Notes[k]));
            }
            w.WriteLine("],");
            w.WriteLine($"      \"bestTree\": {(r.BestTree is null ? "null" : Util.JsonEscape(r.BestTree))}");
            w.WriteLine("    }" + (i + 1 == rows.Count ? "" : ","));
        }

        w.WriteLine("  ]");
        w.WriteLine("}");
    }
}

/* =========================
 * CLI
 * ========================= */

static void Usage()
{
    Console.WriteLine(
@"Uso:
  dotnet run -c Release -- [--lex lexicon.json] [--grammar grammar.json]
                         [--file corpus.txt | --text ""...""]
                         [--beam 16] [--topk 1] [--trees] [--print] [--json out.json]

Ejemplos:
  dotnet run -c Release -- --file corpus.txt --print --json out.json
  dotnet run -c Release -- --text ""Los científicos lo estudiaron durante décadas sin comprenderlo."" --trees --print
");
}

string lexPath = "lexicon.json";
string grammarPath = "grammar.json";
string filePath = "corpus.txt";
string text = "";
string jsonOut = "";
int beam = 16;
int topk = 1;
bool trees = false;
bool print = false;

for (int i = 0; i < args.Length; i++)
{
    string a = args[i];
    string Need()
    {
        if (i + 1 >= args.Length) Die.Now("Falta valor para " + a);
        return args[++i];
    }

    if (a is "--help" or "-h") { Usage(); return; }
    else if (a == "--lex") lexPath = Need();
    else if (a == "--grammar") grammarPath = Need();
    else if (a == "--file") filePath = Need();
    else if (a == "--text") text = Need();
    else if (a == "--json") jsonOut = Need();
    else if (a == "--beam") beam = int.Parse(Need());
    else if (a == "--topk") topk = int.Parse(Need());
    else if (a == "--trees") trees = true;
    else if (a == "--print") print = true;
    else Die.Now("Arg desconocido: " + a);
}

var st = new Symtab();
var lx = new Lexicon();
var gr = new Grammar();

ModelLoader.LoadLexicon(st, lx, lexPath);
ModelLoader.LoadGrammar(st, gr, grammarPath);

string corpus = string.IsNullOrWhiteSpace(text) ? Util.ReadFile(filePath) : text;
var sents = Tokenizer.SplitSentences(corpus);

var rows = new List<ParseStats>(sents.Count);

int parsedCount = 0;
for (int si = 0; si < sents.Count; si++)
{
    var row = Parser.ParseSentence(st, lx, gr, sents[si], topk, beam, trees);
    rows.Add(row);
    if (row.Parsed) parsedCount++;

    if (print)
    {
        Console.WriteLine("==============================================================================");
        Console.WriteLine(row.Sentence);
        Console.WriteLine($"tokens={row.Tokens}  oov={row.Oov}  parsed={(row.Parsed ? 1 : 0)}  parses={row.NParses}  bestScore={(row.BestScore?.ToString("0.000000") ?? "null")}  time_ms={row.TimeMs:0.0}");
        Console.WriteLine($"chart_items={row.ChartItemsTotal}  max_cell={row.ChartItemsMaxCell}  pruned={row.Pruned}  unary_apps={row.UnaryApps}  amb_cells={row.AmbiguousCells}");
        if (row.Notes.Count > 0)
            Console.WriteLine("notes: " + string.Join("; ", row.Notes));
        if (trees && row.BestTree is not null)
            Console.Write(row.BestTree);
    }
}

double coverage = sents.Count == 0 ? 0 : (double)parsedCount / sents.Count;
double avgTime = rows.Count == 0 ? 0 : rows.Average(r => r.TimeMs);

if (!print)
{
    Console.WriteLine($"SUMMARY: sentences={sents.Count} coverage={coverage:0.000} avg_time_ms={avgTime:0.0} beam={beam} top_k={topk}");
}
else
{
    Console.WriteLine("==============================================================================");
    Console.WriteLine($"SUMMARY\nsentences={sents.Count}  coverage={coverage:0.000}  avg_tokens={(rows.Count==0?0:rows.Average(r=>r.Tokens)):0.00}  avg_oov={(rows.Count==0?0:rows.Average(r=>r.Oov)):0.00}  avg_time_ms={avgTime:0.0}  beam={beam}  top_k={topk}");
}

if (!string.IsNullOrWhiteSpace(jsonOut))
{
    Export.WriteSummaryJson(jsonOut, rows, beam, topk);
    Console.WriteLine("Wrote JSON: " + jsonOut);
}
