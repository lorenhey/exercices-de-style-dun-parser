import java.util.*;
import java.util.regex.*;
import java.time.*;

/**
 * ParserV7 - CKY + beam pruning + rasgos mínimos + evaluación.
 * Port "conceptual" del parser incremental en Python (Etapa 7).
 *
 * Compilar: javac ParserV7.java
 * Ejecutar: java ParserV7
 */
public class ParserV7 {

    /* ============================================================
     * Modelos de datos
     * ============================================================ */

    static final class Token {
        final String raw;
        final String text; // lower
        final int index;
        Token(String raw, String text, int index) { this.raw = raw; this.text = text; this.index = index; }
        @Override public String toString(){ return raw; }
    }

    static final class Node {
        final String label;
        final List<Node> children;
        final Map<String,String> feats;
        final double score;
        Node(String label, List<Node> children, Map<String,String> feats, double score) {
            this.label = label;
            this.children = children == null ? List.of() : children;
            this.feats = feats == null ? Map.of() : feats;
            this.score = score;
        }
        String pretty() { return pretty(0); }
        String pretty(int indent) {
            String pad = "  ".repeat(indent);
            String featsStr = "";
            if (!feats.isEmpty()) {
                List<String> parts = new ArrayList<>();
                for (var e : feats.entrySet()) parts.add(e.getKey()+"="+e.getValue());
                Collections.sort(parts);
                featsStr = " [" + String.join(", ", parts) + "]";
            }
            if (children.isEmpty()) return pad + label + featsStr;
            StringBuilder sb = new StringBuilder();
            sb.append(pad).append(label).append(featsStr)
              .append("  (score=").append(String.format(Locale.US,"%.3f", score)).append(")\n");
            for (int i=0;i<children.size();i++){
                sb.append(children.get(i).pretty(indent+1));
                if (i<children.size()-1) sb.append("\n");
            }
            return sb.toString();
        }
    }

    static final class LexEntry {
        final String pos;
        final Map<String,String> feats;
        final double weight;
        LexEntry(String pos, Map<String,String> feats, double weight) {
            this.pos = pos; this.feats = feats; this.weight = weight;
        }
    }

    static final class Item {
        final String cat;
        final Map<String,String> feats; // immutable-ish (treat as immutable)
        final Node node;
        final double score;
        Item(String cat, Map<String,String> feats, Node node, double score) {
            this.cat = cat; this.feats = feats; this.node = node; this.score = score;
        }
    }

    interface Composer {
        Map<String,String> compose(Map<String,String> a, Map<String,String> b);
    }

    static final class Rule {
        final String lhs;
        final String[] rhs; // length 1 or 2
        final Composer composer;
        final double weight;
        Rule(String lhs, String[] rhs, Composer composer, double weight) {
            this.lhs = lhs; this.rhs = rhs; this.composer = composer; this.weight = weight;
        }
        boolean isUnary(){ return rhs.length == 1; }
        boolean isBinary(){ return rhs.length == 2; }
    }

    static final class ParseStats {
        String sentence;
        int tokens;
        int oovTokens;
        boolean parsed;
        int nParsesReturned;
        Double bestScore;
        double timeMs;

        int chartItemsTotal;
        int chartItemsMaxCell;
        int prunedByBeam;
        int unaryApplications;
        int ambiguousCells;
        List<String> finalCellCategories;

        boolean sanitySHasVpFin;
        boolean sanitySinTakesVpNf;
        boolean sanityEncliticOnlyNf;
        List<String> notes = new ArrayList<>();
    }

    /* ============================================================
     * Utilidades de rasgos: unify + helpers
     * ============================================================ */

    static boolean isVar(String v){ return v != null && v.startsWith("?"); }

    static Map<String,String> unify(Map<String,String> a, Map<String,String> b) {
        // Unificación simple, variable "?x" unifica con cualquier constante.
        Map<String,String> out = new HashMap<>(a);
        for (var e : b.entrySet()) {
            String k = e.getKey();
            String vb = e.getValue();
            if (!out.containsKey(k)) {
                out.put(k, vb);
                continue;
            }
            String va = out.get(k);
            if (Objects.equals(va, vb)) continue;

            if (isVar(va) && !isVar(vb)) { out.put(k, vb); continue; }
            if (isVar(vb) && !isVar(va)) { continue; }
            if (isVar(va) && isVar(vb)) { continue; }

            return null; // conflicto
        }
        return out;
    }

    static Map<String,String> requireFeat(Map<String,String> f, String key, String val) {
        return unify(f, Map.of(key, val));
    }

    static Node replaceFeatValues(Node node, Map<String,String> mapping) {
        Map<String,String> feats = new HashMap<>(node.feats);
        boolean changed = false;
        for (var e : mapping.entrySet()) {
            for (var k : new ArrayList<>(feats.keySet())) {
                if (Objects.equals(feats.get(k), e.getKey())) {
                    feats.put(k, e.getValue());
                    changed = true;
                }
            }
        }
        List<Node> newChildren = new ArrayList<>();
        boolean childChanged = false;
        for (Node ch : node.children) {
            Node nch = replaceFeatValues(ch, mapping);
            newChildren.add(nch);
            if (nch != ch) childChanged = true;
        }
        if (changed || childChanged) return new Node(node.label, newChildren, feats, node.score);
        return node;
    }

    /* ============================================================
     * Tokenización (contracciones + enclíticos)
     * ============================================================ */

    static final String[] CLITICS = {"me","te","se","lo","la","los","las","le","les","nos","os"};
    static final Pattern WORD_RE = Pattern.compile("[A-Za-zÁÉÍÓÚÜÑáéíóúüñ]+(?:[-'][A-Za-zÁÉÍÓÚÜÑáéíóúüñ]+)*");

    static List<String> splitContraction(String raw) {
        String w = raw.toLowerCase(Locale.ROOT);
        if (w.equals("al")) return List.of("a","el");
        if (w.equals("del")) return List.of("de","el");
        return null;
    }

    static List<String> splitEnclitic(String raw) {
        String w = raw.toLowerCase(Locale.ROOT);
        // 1 clítico (suficiente para tu corpus)
        // preferimos el más largo
        List<String> clList = new ArrayList<>(Arrays.asList(CLITICS));
        clList.sort((a,b)->Integer.compare(b.length(), a.length()));
        for (String cl : clList) {
            if (w.endsWith(cl) && w.length() > cl.length() + 2) {
                String base = w.substring(0, w.length() - cl.length());
                if (base.endsWith("ar") || base.endsWith("er") || base.endsWith("ir") ||
                    base.endsWith("ando") || base.endsWith("iendo")) {
                    String baseRaw = raw.substring(0, raw.length() - cl.length());
                    String clRaw   = raw.substring(raw.length() - cl.length());
                    return List.of(baseRaw, clRaw);
                }
            }
        }
        return null;
    }

    static List<Token> tokenize(String text) {
        List<Token> out = new ArrayList<>();
        Matcher m = WORD_RE.matcher(text);
        int idx = 0;
        while (m.find()) {
            String raw = m.group(0);
            List<String> parts = splitContraction(raw);
            if (parts == null) parts = splitEnclitic(raw);
            if (parts == null) parts = List.of(raw);

            for (String p : parts) {
                out.add(new Token(p, p.toLowerCase(Locale.ROOT), idx));
                idx++;
            }
        }
        return out;
    }

    static List<String> splitSentences(String text) {
        String[] parts = text.split("[.\\n]+");
        List<String> out = new ArrayList<>();
        for (String p : parts) {
            String s = p.trim();
            if (!s.isEmpty()) out.add(s);
        }
        return out;
    }

    /* ============================================================
     * Léxico + heurísticas OOV
     * ============================================================ */

    static Map<String,List<LexEntry>> LEXICON = buildLexicon();

    static Map<String,List<LexEntry>> buildLexicon() {
        Map<String,List<LexEntry>> lex = new HashMap<>();
        // helper
        var add = (Adder)(word, entry) -> lex.computeIfAbsent(word, k->new ArrayList<>()).add(entry);

        // determinantes
        add.add("el",  new LexEntry("Det", Map.of("gen","m","num","sg"), 0.25));
        add.add("la",  new LexEntry("Det", Map.of("gen","f","num","sg"), 0.25));
        add.add("la",  new LexEntry("Cl",  Map.of(), 0.18));
        add.add("los", new LexEntry("Det", Map.of("gen","m","num","pl"), 0.25));
        add.add("los", new LexEntry("Cl",  Map.of(), 0.12));
        add.add("las", new LexEntry("Det", Map.of("gen","f","num","pl"), 0.25));
        add.add("las", new LexEntry("Cl",  Map.of(), 0.12));
        add.add("un",  new LexEntry("Det", Map.of("gen","m","num","sg"), 0.18));
        add.add("una", new LexEntry("Det", Map.of("gen","f","num","sg"), 0.18));

        // que bifurcado
        add.add("que", new LexEntry("Comp",  Map.of("idx","?i"), 0.30));
        add.add("que", new LexEntry("RelPro",Map.of("idx","?i"), 0.18));

        // preps
        add.add("a", new LexEntry("P", Map.of(), 0.18));
        add.add("de", new LexEntry("P", Map.of(), 0.20));
        add.add("en", new LexEntry("P", Map.of(), 0.20));
        add.add("durante", new LexEntry("P", Map.of(), 0.16));
        add.add("sin", new LexEntry("Pinf", Map.of(), 0.14));
        add.add("sin", new LexEntry("P", Map.of(), -0.10)); // alternativa penalizada

        // clíticos
        add.add("lo", new LexEntry("Cl", Map.of(), 0.22));
        add.add("le", new LexEntry("Cl", Map.of(), 0.20));
        add.add("se", new LexEntry("Cl", Map.of(), 0.20));

        // adverbios
        add.add("nunca", new LexEntry("Adv", Map.of(), 0.12));
        add.add("repentinamente", new LexEntry("Adv", Map.of(), 0.06));
        add.add("espontáneamente", new LexEntry("Adv", Map.of(), 0.06));

        // corpus-oriented: N
        add.add("filósofo", new LexEntry("N", Map.of("gen","m","num","sg"), 0.12));
        add.add("tratado", new LexEntry("N", Map.of("gen","m","num","sg"), 0.10));
        add.add("exilio", new LexEntry("N", Map.of("gen","m","num","sg"), 0.10));
        add.add("teoría", new LexEntry("N", Map.of("gen","f","num","sg"), 0.12));
        add.add("epistemología", new LexEntry("N", Map.of("gen","f","num","sg"), 0.10));
        add.add("científicos", new LexEntry("N", Map.of("gen","m","num","pl"), 0.10));
        add.add("décadas", new LexEntry("N", Map.of("gen","f","num","pl"), 0.10));
        add.add("paradigma", new LexEntry("N", Map.of("gen","m","num","sg"), 0.10));
        add.add("física", new LexEntry("N", Map.of("gen","f","num","sg"), 0.10));
        add.add("evidencia", new LexEntry("N", Map.of("gen","f","num","sg"), 0.10));
        add.add("investigador", new LexEntry("N", Map.of("gen","m","num","sg"), 0.10));
        add.add("hipótesis", new LexEntry("N", Map.of("gen","f","num","sg"), 0.10));
        add.add("manuscrito", new LexEntry("N", Map.of("gen","m","num","sg"), 0.10));
        add.add("anotaciones", new LexEntry("N", Map.of("gen","f","num","pl"), 0.10));
        add.add("revolución", new LexEntry("N", Map.of("gen","f","num","sg"), 0.10));
        add.add("estructuras", new LexEntry("N", Map.of("gen","f","num","pl"), 0.10));
        add.add("argumento", new LexEntry("N", Map.of("gen","m","num","sg"), 0.10));
        add.add("autor", new LexEntry("N", Map.of("gen","m","num","sg"), 0.10));
        add.add("capítulos", new LexEntry("N", Map.of("gen","m","num","pl"), 0.10));
        add.add("consecuencias", new LexEntry("N", Map.of("gen","f","num","pl"), 0.10));
        add.add("economistas", new LexEntry("N", Map.of("gen","m","num","pl"), 0.10));
        add.add("fenómeno", new LexEntry("N", Map.of("gen","m","num","sg"), 0.10));
        add.add("sistemas", new LexEntry("N", Map.of("gen","m","num","pl"), 0.10));
        add.add("crítica", new LexEntry("N", Map.of("gen","f","num","sg"), 0.10));
        add.add("empiristas", new LexEntry("N", Map.of("gen","m","num","pl"), 0.10));
        add.add("concepto", new LexEntry("N", Map.of("gen","m","num","sg"), 0.10));
        add.add("categorías", new LexEntry("N", Map.of("gen","f","num","pl"), 0.10));

        // Adj (con variables simples)
        add.add("industrial", new LexEntry("Adj", Map.of("gen","?g","num","sg"), 0.05));
        add.add("contemporánea", new LexEntry("Adj", Map.of("gen","f","num","sg"), 0.08));
        add.add("alternativa", new LexEntry("Adj", Map.of("gen","f","num","sg"), 0.08));
        add.add("marginales", new LexEntry("Adj", Map.of("gen","?g","num","pl"), 0.05));
        add.add("extensas", new LexEntry("Adj", Map.of("gen","f","num","pl"), 0.06));
        add.add("sociales", new LexEntry("Adj", Map.of("gen","?g","num","pl"), 0.05));
        add.add("posteriores", new LexEntry("Adj", Map.of("gen","?g","num","pl"), 0.05));
        add.add("complejos", new LexEntry("Adj", Map.of("gen","m","num","pl"), 0.05));
        add.add("insuficiente", new LexEntry("Adj", Map.of("gen","?g","num","sg"), 0.05));
        add.add("tradicionales", new LexEntry("Adj", Map.of("gen","?g","num","pl"), 0.05));

        // Verbs: feats fin/obl
        // Vi finitos
        add.add("murió", new LexEntry("Vi", Map.of("fin","yes","obl","yes"), 0.12));
        add.add("colapsó", new LexEntry("Vi", Map.of("fin","yes","obl","yes"), 0.12));
        add.add("prevalecían", new LexEntry("Vi", Map.of("fin","yes","obl","no"), 0.12));
        add.add("emerge", new LexEntry("Vi", Map.of("fin","yes","obl","yes"), 0.12));
        add.add("materializaron", new LexEntry("Vi", Map.of("fin","yes","obl","no"), 0.08));

        // Vt finitos
        add.add("escribió", new LexEntry("Vt", Map.of("fin","yes","obl","no"), 0.12));
        add.add("propuso", new LexEntry("Vt", Map.of("fin","yes","obl","no"), 0.12));
        add.add("transformó", new LexEntry("Vt", Map.of("fin","yes","obl","no"), 0.12));
        add.add("estudiaron", new LexEntry("Vt", Map.of("fin","yes","obl","yes"), 0.12));
        add.add("dominaba", new LexEntry("Vt", Map.of("fin","yes","obl","no"), 0.12));
        add.add("descubrieron", new LexEntry("Vt", Map.of("fin","yes","obl","no"), 0.12));
        add.add("contiene", new LexEntry("Vt", Map.of("fin","yes","obl","no"), 0.12));
        add.add("desarrolla", new LexEntry("Vt", Map.of("fin","yes","obl","no"), 0.12));
        add.add("refuta", new LexEntry("Vt", Map.of("fin","yes","obl","yes"), 0.12));
        add.add("previeron", new LexEntry("Vt", Map.of("fin","yes","obl","no"), 0.12));
        add.add("formularon", new LexEntry("Vt", Map.of("fin","yes","obl","no"), 0.12));
        add.add("introduce", new LexEntry("Vt", Map.of("fin","yes","obl","no"), 0.12));
        add.add("desestabiliza", new LexEntry("Vt", Map.of("fin","yes","obl","no"), 0.12));

        // Vdt (muy mínimo)
        add.add("sugiere", new LexEntry("Vdt", Map.of("fin","yes","obl","yes"), 0.12));

        return lex;
    }

    interface Adder { void add(String word, LexEntry entry); }

    static List<LexEntry> guessLex(Token tk) {
        String w = tk.text;
        List<LexEntry> out = new ArrayList<>();

        // PropN por capitalización
        if (!tk.raw.isEmpty() && Character.isUpperCase(tk.raw.charAt(0)) &&
                !Set.of("el","la","los","las").contains(w)) {
            out.add(new LexEntry("PropN", Map.of("num","sg"), 0.03));
        }
        if (w.endsWith("mente")) out.add(new LexEntry("Adv", Map.of(), -0.03));

        // no finitos
        if (w.endsWith("ar") || w.endsWith("er") || w.endsWith("ir")) {
            out.add(new LexEntry("Vi", Map.of("fin","no","obl","no"), -0.12));
            out.add(new LexEntry("Vt", Map.of("fin","no","obl","no"), -0.14));
        }
        if (w.endsWith("ando") || w.endsWith("iendo")) {
            out.add(new LexEntry("Vi", Map.of("fin","no","obl","no"), -0.14));
            out.add(new LexEntry("Vt", Map.of("fin","no","obl","no"), -0.16));
        }

        if (out.isEmpty()) out.add(new LexEntry("N", Map.of("gen","?g","num","?n"), -0.35));
        return out;
    }

    /* ============================================================
     * Gramática (mismo espíritu v7)
     * ============================================================ */

    static final List<Rule> RULES = buildRules();
    static final List<Rule> UNARY = RULES.stream().filter(Rule::isUnary).toList();
    static final List<Rule> BINARY = RULES.stream().filter(Rule::isBinary).toList();

    static Map<String,String> makeGap(String type) {
        return Map.of("idx","?i", "gap", type);
    }

    static List<Rule> buildRules() {
        List<Rule> r = new ArrayList<>();

        // helpers
        Composer UNARY_PASS = (a,b)->new HashMap<>(a);
        Composer EMPTY = (a,b)->new HashMap<>();

        // S solo acepta VP_FIN
        r.add(new Rule("S", new String[]{"NP","VP_FIN"}, (a,b)-> new HashMap<>(), 0.28));
        r.add(new Rule("S", new String[]{"VP_FIN"}, (a,b)-> new HashMap<>(a), -0.08));

        // VP_FIN / VP_NF
        r.add(new Rule("VP_FIN", new String[]{"VP"}, (a,b)-> requireFeat(a,"fin","yes"), 0.04));
        r.add(new Rule("VP_NF",  new String[]{"VP"}, (a,b)-> requireFeat(a,"fin","no"),  0.04));

        // VP base
        r.add(new Rule("VP", new String[]{"Vi"}, (a,b)-> new HashMap<>(a), 0.10));
        r.add(new Rule("VP", new String[]{"Vt","NP"}, (a,b)-> new HashMap<>(a), 0.22));

        r.add(new Rule("VP", new String[]{"Vdt","PP"}, (a,b)-> new HashMap<>(a), 0.06));
        r.add(new Rule("VP", new String[]{"VP","NP"}, (a,b)-> new HashMap<>(a), 0.08));

        // Adjuntos
        r.add(new Rule("VP", new String[]{"VP","PP"}, (a,b)-> new HashMap<>(a), 0.06));
        r.add(new Rule("VP", new String[]{"VP","Adv"}, (a,b)-> new HashMap<>(a), 0.06));
        r.add(new Rule("VP", new String[]{"Adv","VP"}, (a,b)-> new HashMap<>(b), 0.02));

        // Clíticos proclíticos
        r.add(new Rule("VP", new String[]{"Cl","VP"}, (a,b)-> new HashMap<>(b), 0.06));
        r.add(new Rule("VP", new String[]{"Cl","Vt"}, (a,b)-> new HashMap<>(b), 0.18));
        r.add(new Rule("VP", new String[]{"Cl","Vi"}, (a,b)-> new HashMap<>(b), 0.10));

        // Enclíticos SOLO con no finitos
        r.add(new Rule("Vt_NF", new String[]{"Vt"}, (a,b)-> requireFeat(a,"fin","no"), 0.02));
        r.add(new Rule("Vi_NF", new String[]{"Vi"}, (a,b)-> requireFeat(a,"fin","no"), 0.02));
        r.add(new Rule("VP", new String[]{"Vt_NF","Cl"}, (a,b)-> new HashMap<>(a), 0.10));
        r.add(new Rule("VP", new String[]{"Vi_NF","Cl"}, (a,b)-> new HashMap<>(a), 0.06));

        // PP
        r.add(new Rule("PP", new String[]{"P","NP"}, (a,b)-> new HashMap<>(), 0.16));
        r.add(new Rule("PP", new String[]{"Pinf","VP_NF"}, (a,b)-> new HashMap<>(), 0.30));

        // NP / NBar
        r.add(new Rule("NP", new String[]{"Det","NBar"}, (a,b)-> unify(a,b), 0.38));
        r.add(new Rule("NP", new String[]{"NBar"}, (a,b)-> new HashMap<>(a), 0.08));
        r.add(new Rule("NP", new String[]{"Pron"}, (a,b)-> new HashMap<>(a), 0.10));
        r.add(new Rule("NP", new String[]{"PropN"}, (a,b)-> new HashMap<>(a), 0.10));

        r.add(new Rule("NBar", new String[]{"N"}, (a,b)-> new HashMap<>(a), 0.14));
        r.add(new Rule("NBar", new String[]{"Adj","NBar"}, (a,b)-> unify(a,b), 0.06));
        r.add(new Rule("NBar", new String[]{"NBar","Adj"}, (a,b)-> unify(a,b), 0.08));
        r.add(new Rule("NP", new String[]{"NBar","PP"}, (a,b)-> new HashMap<>(a), 0.04));

        // Relativas desnudas
        r.add(new Rule("RelClause", new String[]{"Comp","S_SUBJ_GAP"}, (a,b)-> unify(a,b), 0.30));
        r.add(new Rule("RelClause", new String[]{"Comp","S_OBJ_GAP"},  (a,b)-> unify(a,b), 0.30));

        // Relativas oblicuas (pied-piping)
        r.add(new Rule("S_OBL", new String[]{"S"}, (a,b)-> requireFeat(a,"obl","yes"), 0.02));
        r.add(new Rule("S_OBL", new String[]{"VP_FIN"}, (a,b)-> requireFeat(a,"obl","yes"), 0.02));
        r.add(new Rule("NP_REL", new String[]{"RelPro"}, (a,b)-> new HashMap<>(a), 0.15));
        r.add(new Rule("NP_REL", new String[]{"Det","RelPro"}, (a,b)-> unify(a,b), 0.20));
        r.add(new Rule("PP_REL", new String[]{"P","NP_REL"}, (a,b)-> unify(a,b), 0.22));
        r.add(new Rule("RelClause", new String[]{"PP_REL","S_OBL"}, (a,b)-> {
            if (requireFeat(b,"obl","yes")==null) return null;
            Map<String,String> out = new HashMap<>(a);
            out.put("gap","obl");
            return out;
        }, 0.18));

        // Adjunción de relativa al NBar
        r.add(new Rule("NBar", new String[]{"NBar","RelClause"}, (a,b)-> unify(a,b), 0.14));

        // Hueco sujeto
        r.add(new Rule("S_SUBJ_GAP", new String[]{"VP_FIN"}, (a,b)-> makeGap("subj"), 0.22));

        // Hueco objeto
        r.add(new Rule("S_OBJ_GAP", new String[]{"NP","VP_OBJ_GAP"}, (a,b)-> makeGap("obj"), 0.22));
        r.add(new Rule("S_OBJ_GAP", new String[]{"VP_OBJ_GAP"}, (a,b)-> makeGap("obj"), 0.12));
        r.add(new Rule("VP_OBJ_GAP", new String[]{"Vt"}, (a,b)-> makeGap("obj"), 0.18));
        r.add(new Rule("VP_OBJ_GAP", new String[]{"Vdt","PP"}, (a,b)-> makeGap("obj"), 0.10));
        r.add(new Rule("VP_OBJ_GAP", new String[]{"VP_OBJ_GAP","PP"}, (a,b)-> new HashMap<>(a), 0.04));
        r.add(new Rule("VP_OBJ_GAP", new String[]{"VP_OBJ_GAP","Adv"}, (a,b)-> new HashMap<>(a), 0.04));

        return r;
    }

    /* ============================================================
     * CKY + beam + stats
     * ============================================================ */

    static List<Item> keepTopK(List<Item> items, int k) {
        items.sort((x,y)->Double.compare(y.score, x.score));
        if (items.size() <= k) return items;
        return new ArrayList<>(items.subList(0, k));
    }

    static final class Cell {
        // cat -> items
        final Map<String, List<Item>> map = new HashMap<>();
        // dedupe key: cat + featsStr + nodeIdentity
        final Map<String, Set<String>> seen = new HashMap<>();
    }

    static void add(Cell cell, Item item, int beam, Counter pruned) {
        String featsKey = featsKey(item.feats);
        String nodeKey = System.identityHashCode(item.node) + "";
        String key = item.cat + "|" + featsKey + "|" + nodeKey;

        Set<String> s = cell.seen.computeIfAbsent(item.cat, k->new HashSet<>());
        if (s.contains(key)) return;
        s.add(key);

        List<Item> lst = cell.map.computeIfAbsent(item.cat, k->new ArrayList<>());
        int before = lst.size();
        lst.add(item);
        lst = keepTopK(lst, beam);
        int after = lst.size();
        if (after < before + 1) pruned.value += (before + 1 - after);
        cell.map.put(item.cat, lst);
    }

    static String featsKey(Map<String,String> feats) {
        if (feats == null || feats.isEmpty()) return "";
        List<String> parts = new ArrayList<>();
        for (var e : feats.entrySet()) parts.add(e.getKey()+"="+e.getValue());
        Collections.sort(parts);
        return String.join("&", parts);
    }

    static void unaryClosure(Cell cell, int beam, Counter pruned, Counter unaryApps) {
        boolean changed = true;
        while (changed) {
            changed = false;
            for (Rule rule : UNARY) {
                String B = rule.rhs[0];
                List<Item> children = cell.map.get(B);
                if (children == null) continue;

                // snapshot
                List<Item> snap = new ArrayList<>(children);
                for (Item ch : snap) {
                    Map<String,String> pf = rule.composer.compose(ch.feats, Map.of());
                    if (pf == null) continue;
                    unaryApps.value++;

                    double score = ch.score + rule.weight;
                    Node node = new Node(rule.lhs, List.of(ch.node), pf, score);

                    int before = cell.map.getOrDefault(rule.lhs, List.of()).size();
                    add(cell, new Item(rule.lhs, pf, node, score), beam, pruned);
                    int after = cell.map.getOrDefault(rule.lhs, List.of()).size();
                    if (after != before) changed = true;
                }
            }
        }
    }

    static final class Counter { int value=0; }

    static class ParseResult {
        final List<Node> trees;
        final ParseStats stats;
        ParseResult(List<Node> trees, ParseStats stats) { this.trees = trees; this.stats = stats; }
    }

    static ParseResult parseSentence(String sentence, int topK, int beam) {
        long t0 = System.nanoTime();

        List<Token> toks = tokenize(sentence);
        int n = toks.size();

        ParseStats st = new ParseStats();
        st.sentence = sentence;
        st.tokens = n;

        if (n == 0) {
            st.parsed = false;
            st.notes.add("empty");
            st.timeMs = 0.0;
            return new ParseResult(List.of(), st);
        }

        Cell[][] chart = new Cell[n][n+1];
        for (int i=0;i<n;i++) for (int j=0;j<n+1;j++) chart[i][j] = new Cell();

        Counter pruned = new Counter();
        Counter unaryApps = new Counter();

        // Lex init
        int oov = 0;
        for (int i=0;i<n;i++) {
            Token tk = toks.get(i);
            List<LexEntry> lex = LEXICON.get(tk.text);
            if (lex == null) { oov++; lex = new ArrayList<>(); }
            List<LexEntry> guessed = guessLex(tk);
            List<LexEntry> all = new ArrayList<>(lex);
            all.addAll(guessed);

            Cell cell = chart[i][i+1];

            for (LexEntry le : all) {
                Map<String,String> feats = new HashMap<>(le.feats);
                if (Set.of("N","PropN","Pron").contains(le.pos)) {
                    feats.putIfAbsent("idx", "t" + tk.index);
                }
                Node leaf = new Node(tk.raw, List.of(), Map.of(), le.weight);
                Node pre  = new Node(le.pos, List.of(leaf), feats, le.weight);
                add(cell, new Item(le.pos, feats, pre, le.weight), beam, pruned);
            }
            unaryClosure(cell, beam, pruned, unaryApps);
        }
        st.oovTokens = oov;

        // CKY spans
        for (int span=2; span<=n; span++) {
            for (int i=0; i<=n-span; i++) {
                int j = i + span;
                Cell cell = chart[i][j];

                for (int k=i+1; k<j; k++) {
                    Cell left = chart[i][k];
                    Cell right = chart[k][j];

                    if (left.map.isEmpty() || right.map.isEmpty()) continue;

                    for (Rule rule : BINARY) {
                        String B = rule.rhs[0];
                        String C = rule.rhs[1];
                        List<Item> lb = left.map.get(B);
                        List<Item> rc = right.map.get(C);
                        if (lb == null || rc == null) continue;

                        for (Item ib : lb) {
                            for (Item ic : rc) {
                                Map<String,String> pf = rule.composer.compose(ib.feats, ic.feats);
                                if (pf == null) continue;
                                double score = ib.score + ic.score + rule.weight;

                                Node L = ib.node;
                                Node R = ic.node;

                                // propagación idx al adjuntar relativa
                                if (rule.lhs.equals("NBar") && B.equals("NBar") && C.equals("RelClause")) {
                                    String idx = ib.feats.get("idx");
                                    if (idx != null) R = replaceFeatValues(R, Map.of("?i", idx));
                                }

                                Node node = new Node(rule.lhs, List.of(L, R), pf, score);
                                add(cell, new Item(rule.lhs, pf, node, score), beam, pruned);
                            }
                        }
                    }
                }

                unaryClosure(cell, beam, pruned, unaryApps);
            }
        }

        // results: S in cell (0,n)
        Cell finalCell = chart[0][n];
        List<Item> finals = finalCell.map.getOrDefault("S", List.of());
        finals.sort((x,y)->Double.compare(y.score, x.score));
        if (finals.size() > topK) finals = finals.subList(0, topK);

        List<Node> trees = new ArrayList<>();
        for (Item it : finals) trees.add(it.node);

        long t1 = System.nanoTime();
        st.timeMs = (t1 - t0) / 1_000_000.0;

        st.parsed = !trees.isEmpty();
        st.nParsesReturned = trees.size();
        st.bestScore = st.parsed ? trees.get(0).score : null;

        // chart metrics
        int totalItems = 0;
        int maxCell = 0;
        int ambCells = 0;
        for (int a=0; a<n; a++) {
            for (int b=a+1; b<=n; b++) {
                Cell c = chart[a][b];
                int cats = c.map.size();
                if (cats >= 2) ambCells++;
                int cellCount = 0;
                for (List<Item> l : c.map.values()) cellCount += l.size();
                totalItems += cellCount;
                if (cellCount > maxCell) maxCell = cellCount;
            }
        }

        st.chartItemsTotal = totalItems;
        st.chartItemsMaxCell = maxCell;
        st.prunedByBeam = pruned.value;
        st.unaryApplications = unaryApps.value;
        st.ambiguousCells = ambCells;
        st.finalCellCategories = new ArrayList<>(finalCell.map.keySet());
        Collections.sort(st.finalCellCategories);

        // sanity checks
        if (st.parsed) {
            boolean sOk = sanitySHasVpFin(trees.get(0));
            boolean sinOk = sanitySinTakesVpNf(trees.get(0));
            boolean enclOk = sanityEncliticOnlyNf(trees.get(0));
            st.sanitySHasVpFin = sOk;
            st.sanitySinTakesVpNf = sinOk;
            st.sanityEncliticOnlyNf = enclOk;

            if (!sOk) st.notes.add("WARN: S sin VP_FIN visible");
            if (!sinOk) st.notes.add("WARN: 'sin' sin VP_NF bajo Pinf");
            if (!enclOk) st.notes.add("WARN: enclítico con verbo finito");
        } else {
            st.sanitySHasVpFin = true;
            st.sanitySinTakesVpNf = true;
            st.sanityEncliticOnlyNf = true;
            st.notes.add("NO_PARSE");
            if (!st.finalCellCategories.isEmpty()) st.notes.add("final_cell=" + String.join(",", st.finalCellCategories));
        }

        return new ParseResult(trees, st);
    }

    /* ============================================================
     * Sanity checks (aproximados)
     * ============================================================ */

    static Iterable<Node> walk(Node root) {
        List<Node> out = new ArrayList<>();
        Deque<Node> dq = new ArrayDeque<>();
        dq.add(root);
        while (!dq.isEmpty()) {
            Node n = dq.removeFirst();
            out.add(n);
            for (Node ch : n.children) dq.addLast(ch);
        }
        return out;
    }

    static boolean sanitySHasVpFin(Node tree) {
        for (Node n : walk(tree)) {
            if (n.label.equals("S")) {
                for (Node ch : n.children) if (ch.label.equals("VP_FIN")) return true;
            }
        }
        return false;
    }

    static boolean sanitySinTakesVpNf(Node tree) {
        // si hay Pinf, debe tener VP_NF bajo él
        for (Node n : walk(tree)) {
            if (n.label.equals("Pinf")) {
                boolean found = false;
                for (Node d : walk(n)) if (d.label.equals("VP_NF")) { found = true; break; }
                if (!found) return false;
            }
        }
        return true;
    }

    static boolean sanityEncliticOnlyNf(Node tree) {
        // chequeo rudimentario: VP -> (Vt|Vi) + Cl debería no ocurrir
        for (Node n : walk(tree)) {
            if (n.label.equals("VP") && n.children.size() == 2) {
                Node a = n.children.get(0);
                Node b = n.children.get(1);
                if (b.label.equals("Cl") && (a.label.equals("Vt") || a.label.equals("Vi"))) return false;
            }
        }
        return true;
    }

    /* ============================================================
     * Evaluación de corpus
     * ============================================================ */

    static void evaluateCorpus(String corpus, int topK, int beam, boolean showTrees) {
        List<String> sents = splitSentences(corpus);

        int parsedCount = 0;
        double totalTime = 0.0;
        int totalTokens = 0;
        int totalOov = 0;

        for (String s : sents) {
            ParseResult res = parseSentence(s, topK, beam);
            ParseStats st = res.stats;

            System.out.println("=".repeat(78));
            System.out.println(s);
            System.out.println("tokens=" + st.tokens + "  oov=" + st.oovTokens + "  parsed=" + st.parsed +
                    "  parses=" + st.nParsesReturned + "  score=" + st.bestScore + "  time_ms=" +
                    String.format(Locale.US,"%.1f", st.timeMs));
            System.out.println("chart_items=" + st.chartItemsTotal + "  max_cell=" + st.chartItemsMaxCell +
                    "  pruned=" + st.prunedByBeam + "  unary_apps=" + st.unaryApplications +
                    "  amb_cells=" + st.ambiguousCells);
            if (!st.notes.isEmpty()) System.out.println("notes: " + String.join("; ", st.notes));
            if (showTrees && !res.trees.isEmpty()) {
                System.out.println(res.trees.get(0).pretty());
            }

            parsedCount += st.parsed ? 1 : 0;
            totalTime += st.timeMs;
            totalTokens += st.tokens;
            totalOov += st.oovTokens;
        }

        double coverage = sents.isEmpty() ? 0.0 : (parsedCount * 1.0 / sents.size());
        System.out.println("\n" + "#".repeat(78));
        System.out.println("SUMMARY");
        System.out.println("sentences=" + sents.size() +
                "  coverage=" + String.format(Locale.US,"%.3f", coverage) +
                "  avg_tokens=" + String.format(Locale.US,"%.2f", (sents.isEmpty()?0:(totalTokens*1.0/sents.size()))) +
                "  avg_oov=" + String.format(Locale.US,"%.2f", (sents.isEmpty()?0:(totalOov*1.0/sents.size()))) +
                "  avg_time_ms=" + String.format(Locale.US,"%.1f", (sents.isEmpty()?0:(totalTime/sents.size()))) +
                "  beam=" + beam + "  top_k=" + topK);
    }

    /* ============================================================
     * MAIN
     * ============================================================ */

    public static void main(String[] args) {
        String corpus = """
        El filósofo que escribió el tratado murió en el exilio.
        La teoría que Kuhn propuso transformó la epistemología contemporánea.
        Los científicos lo estudiaron durante décadas sin comprenderlo.
        El paradigma que dominaba la física colapsó repentinamente.
        La evidencia le sugiere al investigador una hipótesis alternativa.
        El manuscrito que descubrieron contiene anotaciones marginales extensas.
        La revolución industrial transformó las estructuras sociales que prevalecían.
        El argumento que desarrolla el autor lo refuta en capítulos posteriores.
        Las consecuencias que previeron los economistas nunca se materializaron.
        El fenómeno emerge espontáneamente en sistemas complejos.
        La crítica que formularon los empiristas le pareció insuficiente a Kant.
        El concepto que introduce Foucault desestabiliza las categorías tradicionales.
        """;

        int topK = 1;
        int beam = 16;
        boolean showTrees = false; // ponelo en true si querés imprimir árboles

        evaluateCorpus(corpus, topK, beam, showTrees);
    }
}
