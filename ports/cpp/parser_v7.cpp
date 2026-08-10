// parser_v7.cpp - C++17
// CKY + beam + rasgos mínimos + carga grammar.json/lexicon.json + tokenización (al/del + enclíticos)
// JSON parser embebido (tokenizador estilo jsmn) + export JSON sin deps.

#include <bits/stdc++.h>
using namespace std;

/* ============================================================
 * Utilidades
 * ============================================================ */

[[noreturn]] static void die(const string& msg) {
  cerr << msg << "\n";
  exit(1);
}

static string read_file(const string& path) {
  ifstream in(path, ios::binary);
  if (!in) die("No puedo abrir: " + path);
  string s((istreambuf_iterator<char>(in)), istreambuf_iterator<char>());
  return s;
}

static inline bool ends_with(const string& s, const string& suf) {
  return s.size() >= suf.size() && memcmp(s.data() + (s.size()-suf.size()), suf.data(), suf.size()) == 0;
}

static inline void ascii_lower_inplace(string& s) {
  for (char& c : s) {
    unsigned char uc = (unsigned char)c;
    if (uc >= 'A' && uc <= 'Z') c = (char)(uc - 'A' + 'a');
  }
}

static inline bool is_word_byte(unsigned char c) {
  if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')) return true;
  if (c >= 0xC0) return true; // UTF-8 bytes
  return false;
}

/* ============================================================
 * Arena simple (para nodos/items/feats)
 * ============================================================ */

struct Arena {
  vector<unsigned char> buf;
  size_t len = 0;

  explicit Arena(size_t cap = (1u<<20)) { buf.resize(cap); }

  void* alloc(size_t n, size_t align = alignof(max_align_t)) {
    size_t p = (len + (align-1)) & ~(align-1);
    if (p + n > buf.size()) {
      size_t newcap = buf.size() ? buf.size()*2 : (1u<<20);
      while (newcap < p + n) newcap *= 2;
      buf.resize(newcap);
    }
    void* out = buf.data() + p;
    len = p + n;
    memset(out, 0, n);
    return out;
  }

  template <class T, class... Args>
  T* make(Args&&... args) {
    void* p = alloc(sizeof(T), alignof(T));
    return new(p) T(std::forward<Args>(args)...);
  }

  char* dup_cstr(const string& s) {
    char* p = (char*)alloc(s.size() + 1, 1);
    memcpy(p, s.data(), s.size());
    p[s.size()] = '\0';
    return p;
  }
};

/* ============================================================
 * Symtab: string <-> int
 * ============================================================ */

struct Symtab {
  unordered_map<string,int> m;
  vector<string> id2s;

  int intern(const string& s) {
    auto it = m.find(s);
    if (it != m.end()) return it->second;
    int id = (int)id2s.size();
    id2s.push_back(s);
    m.emplace(id2s.back(), id);
    return id;
  }

  const string& str(int id) const {
    static string q = "?";
    if (id < 0 || id >= (int)id2s.size()) return q;
    return id2s[id];
  }

  static bool is_var(const string& s) { return !s.empty() && s[0] == '?'; }
};

/* ============================================================
 * JSON tokenizer (minimalista, estilo jsmn)
 * ============================================================ */

enum JType { J_UNDEF=0, J_OBJ=1, J_ARR=2, J_STR=3, J_PRIM=4 };

struct JTok {
  JType type = J_UNDEF;
  int start = -1;
  int end = -1;
  int size = 0;
};

struct JParser {
  unsigned pos = 0;
  unsigned toknext = 0;
  int toksuper = -1;
};

static void jinit(JParser& p){ p.pos=0; p.toknext=0; p.toksuper=-1; }

static JTok* jalloc(JParser& p, vector<JTok>& toks) {
  if (p.toknext >= toks.size()) return nullptr;
  JTok* t = &toks[p.toknext++];
  *t = JTok{};
  return t;
}

static void jfill(JTok& t, JType type, int start, int end) {
  t.type=type; t.start=start; t.end=end; t.size=0;
}

static int jparse_primitive(JParser& p, const string& js, vector<JTok>& toks) {
  int start = (int)p.pos;
  while (p.pos < js.size()) {
    char c = js[p.pos];
    if (c=='\t'||c=='\r'||c=='\n'||c==' '||c==','||c==']'||c=='}') break;
    p.pos++;
  }
  JTok* t = jalloc(p, toks);
  if (!t) return -1;
  jfill(*t, J_PRIM, start, (int)p.pos);
  p.pos--;
  return 0;
}

static int jparse_string(JParser& p, const string& js, vector<JTok>& toks) {
  int start = (int)p.pos;
  p.pos++; // skip "
  for (; p.pos < js.size(); p.pos++) {
    char c = js[p.pos];
    if (c == '"') {
      JTok* t = jalloc(p, toks);
      if (!t) return -1;
      jfill(*t, J_STR, start+1, (int)p.pos);
      return 0;
    }
    if (c == '\\') p.pos++; // skip escaped
  }
  return -2;
}

static int jparse(JParser& p, const string& js, vector<JTok>& toks) {
  for (; p.pos < js.size(); p.pos++) {
    char c = js[p.pos];
    switch(c) {
      case '{': case '[': {
        JTok* t = jalloc(p, toks);
        if (!t) return -1;
        t->type = (c=='{') ? J_OBJ : J_ARR;
        t->start = (int)p.pos;
        p.toksuper = (int)p.toknext - 1;
      } break;
      case '}': case ']': {
        JType type = (c=='}') ? J_OBJ : J_ARR;
        int i = (int)p.toknext - 1;
        for (; i>=0; i--) {
          if (toks[i].start != -1 && toks[i].end == -1) {
            if (toks[i].type != type) return -3;
            toks[i].end = (int)p.pos + 1;
            p.toksuper = -1;
            for (int j=i-1;j>=0;j--) {
              if (toks[j].start != -1 && toks[j].end == -1) { p.toksuper = j; break; }
            }
            break;
          }
        }
      } break;
      case '"':
        if (jparse_string(p, js, toks) < 0) return -2;
        if (p.toksuper != -1) toks[p.toksuper].size++;
        break;
      case '\t': case '\r': case '\n': case ' ': case ':': case ',':
        break;
      default:
        if (jparse_primitive(p, js, toks) < 0) return -1;
        if (p.toksuper != -1) toks[p.toksuper].size++;
        break;
    }
  }
  for (int i=(int)p.toknext-1;i>=0;i--) if (toks[i].start!=-1 && toks[i].end==-1) toks[i].end = (int)js.size();
  return (int)p.toknext;
}

static bool jsoneq(const string& json, const JTok& tok, const string& s) {
  int n = tok.end - tok.start;
  return tok.type==J_STR && (int)s.size()==n && memcmp(json.data()+tok.start, s.data(), (size_t)n)==0;
}

static string tok_str(const string& json, const JTok& tok) {
  return json.substr((size_t)tok.start, (size_t)(tok.end - tok.start));
}

static double tok_double(const string& json, const JTok& tok) {
  string s = tok_str(json, tok);
  return atof(s.c_str());
}

static int tok_skip(const vector<JTok>& toks, int i) {
  int j = i + 1;
  if (toks[i].type == J_OBJ) {
    for (int k=0;k<toks[i].size;k++) { j = tok_skip(toks, j); j = tok_skip(toks, j); }
  } else if (toks[i].type == J_ARR) {
    for (int k=0;k<toks[i].size;k++) j = tok_skip(toks, j);
  }
  return j;
}

static int obj_find_key(const string& json, const vector<JTok>& toks, int obj_i, const string& key) {
  if (toks[obj_i].type != J_OBJ) return -1;
  int j = obj_i + 1;
  for (int k=0;k<toks[obj_i].size;k++) {
    int key_i = j;
    int val_i = j + 1;
    if (jsoneq(json, toks[key_i], key)) return val_i;
    j = tok_skip(toks, val_i);
  }
  return -1;
}

/* ============================================================
 * Feats + unificación
 * ============================================================ */

struct Feat { int key= -1; int val= -1; };

struct Feats {
  Feat* a = nullptr;
  int n = 0;
};

static inline void feats_sort(Feats& f) {
  sort(f.a, f.a + f.n, [](const Feat& x, const Feat& y){
    if (x.key != y.key) return x.key < y.key;
    return x.val < y.val;
  });
}

static inline int feat_find(const Feats& f, int key) {
  for (int i=0;i<f.n;i++) if (f.a[i].key == key) return i;
  return -1;
}

static unsigned long feats_hash(const Feats& f) {
  unsigned long h = 1469598103934665603UL;
  for (int i=0;i<f.n;i++) {
    h ^= (unsigned long)f.a[i].key; h *= 1099511628211UL;
    h ^= (unsigned long)f.a[i].val; h *= 1099511628211UL;
  }
  return h;
}

static Feats feats_copy(Arena& A, const Feats& src) {
  Feats out;
  out.n = src.n;
  out.a = (Feat*)A.alloc(sizeof(Feat) * (size_t)out.n, alignof(Feat));
  memcpy(out.a, src.a, sizeof(Feat) * (size_t)out.n);
  return out;
}

static bool feats_unify(Arena& A, Symtab& st, const Feats& A1, const Feats& B1, Feats& out) {
  // Copia A1 a tmp y agrega B1 con unificación simple (variables "?x")
  Feat* tmp = (Feat*)A.alloc(sizeof(Feat) * (size_t)(A1.n + B1.n), alignof(Feat));
  int tn = A1.n;
  memcpy(tmp, A1.a, sizeof(Feat) * (size_t)A1.n);

  for (int i=0;i<B1.n;i++) {
    int k = B1.a[i].key;
    int vb= B1.a[i].val;
    int j = -1;
    for (int t=0;t<tn;t++) if (tmp[t].key==k){ j=t; break; }

    if (j<0) { tmp[tn++] = {k,vb}; continue; }

    int va = tmp[j].val;
    if (va == vb) continue;

    const string& sa = st.str(va);
    const string& sb = st.str(vb);
    bool va_var = Symtab::is_var(sa);
    bool vb_var = Symtab::is_var(sb);

    if (va_var && !vb_var) { tmp[j].val = vb; continue; }
    if (!va_var && vb_var) { continue; }
    if (va_var && vb_var) { continue; }
    return false; // conflicto
  }

  out.n = tn;
  out.a = (Feat*)A.alloc(sizeof(Feat) * (size_t)out.n, alignof(Feat));
  memcpy(out.a, tmp, sizeof(Feat) * (size_t)out.n);
  feats_sort(out);
  return true;
}

static bool feats_require(Arena& A, Symtab& st, const Feats& f, int key, int val, Feats& out) {
  Feats req;
  req.n = 1;
  req.a = (Feat*)A.alloc(sizeof(Feat), alignof(Feat));
  req.a[0] = {key,val};
  feats_sort(req);
  return feats_unify(A, st, f, req, out);
}

/* ============================================================
 * Lexicon / Grammar
 * ============================================================ */

struct LexEntry {
  int pos = -1;
  double weight = 0.0;
  vector<Feat> feats_heap; // se copia a arena por oración
};

struct Lexicon {
  unordered_map<string, vector<LexEntry>> entries;
};

enum Op {
  OP_EMPTY=0, OP_LEFT, OP_RIGHT, OP_UNIFY,
  OP_REQUIRE_LEFT, OP_REQUIRE_RIGHT,
  OP_MAKE_GAP, OP_RELCLAUSE_OBL
};

enum PostFlag {
  POST_NONE=0,
  POST_PROPAGATE_IDX_TO_RIGHT=1
};

struct Rule {
  int lhs=-1;
  int rhs_len=0;
  int rhs1=-1;
  int rhs2=-1;
  double weight=0.0;
  Op op=OP_EMPTY;
  int arg_key=-1;
  int arg_val=-1;
  int arg_type=-1; // for MAKE_GAP
  int post_flags=POST_NONE;
};

struct Grammar {
  vector<Rule> rules;
};

static Op op_from(const string& s) {
  if (s=="EMPTY") return OP_EMPTY;
  if (s=="LEFT") return OP_LEFT;
  if (s=="RIGHT") return OP_RIGHT;
  if (s=="UNIFY") return OP_UNIFY;
  if (s=="REQUIRE_LEFT") return OP_REQUIRE_LEFT;
  if (s=="REQUIRE_RIGHT") return OP_REQUIRE_RIGHT;
  if (s=="MAKE_GAP") return OP_MAKE_GAP;
  if (s=="RELCLAUSE_OBL") return OP_RELCLAUSE_OBL;
  die("op desconocido: " + s);
  return OP_EMPTY;
}

static void load_lexicon_json(Symtab& st, Lexicon& lx, const string& path) {
  string json = read_file(path);
  vector<JTok> toks(json.size()/6 + 512);
  JParser p; jinit(p);
  int nt = jparse(p, json, toks);
  if (nt < 0) die("JSON parse lexicon falló: " + to_string(nt));
  toks.resize((size_t)nt);

  int root=0;
  int entries_i = obj_find_key(json, toks, root, "entries");
  if (entries_i < 0 || toks[entries_i].type != J_OBJ) die("lexicon.json: falta entries{}");

  int j = entries_i + 1;
  for (int k=0;k<toks[entries_i].size;k++) {
    int word_i = j;
    int arr_i  = j + 1;
    string word = tok_str(json, toks[word_i]);
    if (toks[arr_i].type != J_ARR) die("lexicon.json: entries."+word+" no es array");

    auto& vec = lx.entries[word];

    int a = arr_i + 1;
    for (int e=0;e<toks[arr_i].size;e++) {
      int obj = a;
      if (toks[obj].type != J_OBJ) die("lexicon.json: entry no es objeto");

      int pos_i = obj_find_key(json, toks, obj, "pos");
      int w_i   = obj_find_key(json, toks, obj, "weight");
      int f_i   = obj_find_key(json, toks, obj, "feats");
      if (pos_i<0 || w_i<0 || f_i<0) die("lexicon.json: entry incompleta (pos/weight/feats)");

      LexEntry le;
      le.pos = st.intern(tok_str(json, toks[pos_i]));
      le.weight = tok_double(json, toks[w_i]);

      if (toks[f_i].type != J_OBJ) die("lexicon.json: feats no es objeto");
      if (toks[f_i].size > 0) {
        int fj = f_i + 1;
        le.feats_heap.reserve((size_t)toks[f_i].size);
        for (int z=0; z<toks[f_i].size; z++) {
          int kk = fj;
          int vv = fj + 1;
          string kstr = tok_str(json, toks[kk]);
          string vstr = tok_str(json, toks[vv]);
          le.feats_heap.push_back({st.intern(kstr), st.intern(vstr)});
          fj = tok_skip(toks, vv);
        }
        sort(le.feats_heap.begin(), le.feats_heap.end(), [](const Feat& a, const Feat& b){
          if (a.key != b.key) return a.key < b.key;
          return a.val < b.val;
        });
      }

      vec.push_back(std::move(le));
      a = tok_skip(toks, obj);
    }

    j = tok_skip(toks, arr_i);
  }
}

static void load_grammar_json(Symtab& st, Grammar& gr, const string& path) {
  string json = read_file(path);
  vector<JTok> toks(json.size()/6 + 512);
  JParser p; jinit(p);
  int nt = jparse(p, json, toks);
  if (nt < 0) die("JSON parse grammar falló: " + to_string(nt));
  toks.resize((size_t)nt);

  int root=0;
  int rules_i = obj_find_key(json, toks, root, "rules");
  if (rules_i < 0 || toks[rules_i].type != J_ARR) die("grammar.json: falta rules[]");

  int a = rules_i + 1;
  for (int i=0;i<toks[rules_i].size;i++) {
    int obj = a;
    if (toks[obj].type != J_OBJ) die("grammar.json: regla no es objeto");

    int lhs_i = obj_find_key(json, toks, obj, "lhs");
    int rhs_i = obj_find_key(json, toks, obj, "rhs");
    int w_i   = obj_find_key(json, toks, obj, "weight");
    int op_i  = obj_find_key(json, toks, obj, "op");
    int args_i= obj_find_key(json, toks, obj, "args");
    int post_i= obj_find_key(json, toks, obj, "post");
    if (lhs_i<0 || rhs_i<0 || w_i<0 || op_i<0) die("grammar.json: regla incompleta");

    Rule r;
    r.lhs = st.intern(tok_str(json, toks[lhs_i]));
    r.weight = tok_double(json, toks[w_i]);
    r.op = op_from(tok_str(json, toks[op_i]));

    if (toks[rhs_i].type != J_ARR) die("grammar.json: rhs no es array");
    if (toks[rhs_i].size < 1 || toks[rhs_i].size > 2) die("grammar.json: rhs len debe ser 1 o 2");
    r.rhs_len = toks[rhs_i].size;
    int rr = rhs_i + 1;
    r.rhs1 = st.intern(tok_str(json, toks[rr]));
    if (r.rhs_len == 2) {
      rr = tok_skip(toks, rr);
      r.rhs2 = st.intern(tok_str(json, toks[rr]));
    }

    // args
    if (args_i >= 0 && toks[args_i].type == J_OBJ) {
      int key_i = obj_find_key(json, toks, args_i, "key");
      int val_i = obj_find_key(json, toks, args_i, "value");
      int type_i= obj_find_key(json, toks, args_i, "type");
      if (key_i >= 0) r.arg_key = st.intern(tok_str(json, toks[key_i]));
      if (val_i >= 0) r.arg_val = st.intern(tok_str(json, toks[val_i]));
      if (type_i>= 0) r.arg_type= st.intern(tok_str(json, toks[type_i]));
    }

    // post
    if (post_i >= 0 && toks[post_i].type == J_ARR) {
      int pj = post_i + 1;
      for (int k=0;k<toks[post_i].size;k++) {
        string ps = tok_str(json, toks[pj]);
        if (ps == "PROPAGATE_IDX_TO_RIGHT") r.post_flags |= POST_PROPAGATE_IDX_TO_RIGHT;
        pj = tok_skip(toks, pj);
      }
    }

    gr.rules.push_back(r);
    a = tok_skip(toks, obj);
  }
}

/* ============================================================
 * Tokenización + split oraciones
 * ============================================================ */

struct Token {
  string raw;
  string text; // lower ascii
  int index=0;
};

static const vector<string> CLITICS = {"me","te","se","lo","la","los","las","le","les","nos","os"};

static vector<Token> tokenize(const string& s) {
  vector<Token> out;
  out.reserve(32);
  int idx=0;
  int n=(int)s.size();
  int i=0;
  while (i<n) {
    unsigned char c = (unsigned char)s[i];
    if (!is_word_byte(c)) { i++; continue; }
    int start=i;
    i++;
    while (i<n) {
      unsigned char d = (unsigned char)s[i];
      if (is_word_byte(d) || d=='-' || d=='\'') i++;
      else break;
    }
    int end=i;
    string raw = s.substr((size_t)start, (size_t)(end-start));
    string low = raw;
    ascii_lower_inplace(low);

    if (low == "al") {
      out.push_back({"a","a",idx++});
      out.push_back({"el","el",idx++});
      continue;
    }
    if (low == "del") {
      out.push_back({"de","de",idx++});
      out.push_back({"el","el",idx++});
      continue;
    }

    // enclítico (1): preferir el más largo
    int best=-1;
    for (int k=0;k<(int)CLITICS.size();k++) {
      if (ends_with(low, CLITICS[k])) {
        if (best<0 || CLITICS[k].size() > CLITICS[best].size()) best=k;
      }
    }
    if (best>=0) {
      int cllen=(int)CLITICS[best].size();
      int baselen=(int)low.size()-cllen;
      if (baselen>2) {
        string base = low.substr(0,(size_t)baselen);
        if (ends_with(base,"ar")||ends_with(base,"er")||ends_with(base,"ir")||
            ends_with(base,"ando")||ends_with(base,"iendo")) {
          string raw_base = raw.substr(0, raw.size()-cllen);
          string raw_cl   = raw.substr(raw.size()-cllen);
          string txt_base = raw_base; ascii_lower_inplace(txt_base);
          string txt_cl   = raw_cl;   ascii_lower_inplace(txt_cl);
          out.push_back({raw_base, txt_base, idx++});
          out.push_back({raw_cl, txt_cl, idx++});
          continue;
        }
      }
    }

    out.push_back({raw, low, idx++});
  }
  return out;
}

static vector<string> split_sentences(const string& corpus) {
  vector<string> out;
  int n=(int)corpus.size();
  int start=0;
  for (int i=0;i<=n;i++) {
    char c = (i==n)? '\0' : corpus[i];
    if (c=='.' || c=='\n' || c=='\0') {
      int end=i;
      while (start<end && isspace((unsigned char)corpus[start])) start++;
      while (end>start && isspace((unsigned char)corpus[end-1])) end--;
      if (end>start) out.push_back(corpus.substr((size_t)start, (size_t)(end-start)));
      start=i+1;
    }
  }
  return out;
}

/* ============================================================
 * Árbol
 * ============================================================ */

struct Node {
  int label=-1;              // símbolo/categoría
  bool is_leaf_token=false;
  const char* leaf_raw=nullptr;

  Feats feats;
  double score=0.0;

  Node* left=nullptr;
  Node* right=nullptr;
  Node* child=nullptr;
};

static void node_replace_feat_value(Node* n, int from_val, int to_val) {
  if (!n) return;
  for (int i=0;i<n->feats.n;i++) if (n->feats.a[i].val == from_val) n->feats.a[i].val = to_val;
  if (n->child) node_replace_feat_value(n->child, from_val, to_val);
  if (n->left)  node_replace_feat_value(n->left, from_val, to_val);
  if (n->right) node_replace_feat_value(n->right, from_val, to_val);
}

static void node_pretty_rec(const Symtab& st, const Node* n, int indent, ostream& os) {
  if (!n) return;
  for (int i=0;i<indent;i++) os << "  ";

  if (n->is_leaf_token) {
    os << (n->leaf_raw ? n->leaf_raw : "") << "\n";
    return;
  }

  os << st.str(n->label);
  if (n->feats.n > 0) {
    os << " [";
    for (int i=0;i<n->feats.n;i++) {
      if (i) os << ", ";
      os << st.str(n->feats.a[i].key) << "=" << st.str(n->feats.a[i].val);
    }
    os << "]";
  }
  os << "  (score=" << fixed << setprecision(3) << n->score << ")\n";

  if (n->child) node_pretty_rec(st, n->child, indent+1, os);
  else { node_pretty_rec(st, n->left, indent+1, os); node_pretty_rec(st, n->right, indent+1, os); }
}

/* ============================================================
 * Chart + beam
 * ============================================================ */

struct Item {
  int cat=-1;
  Feats feats;
  unsigned long feats_h=0;
  double score=0.0;
  Node* node=nullptr;
};

struct Bucket {
  int cat=-1;
  vector<Item*> items;          // orden desc por score
  vector<unsigned long> hashes; // dedupe por feats hash
};

struct Cell {
  unordered_map<int, Bucket> b;
};

static bool bucket_has_hash(const Bucket& bk, unsigned long h) {
  for (auto x : bk.hashes) if (x == h) return true;
  return false;
}

static void bucket_insert_sorted(Bucket& bk, Item* it, int beam, int& pruned) {
  // insert keeping score descending
  auto pos = lower_bound(bk.items.begin(), bk.items.end(), it,
    [](const Item* a, const Item* b){ return a->score > b->score; });
  size_t idx = (size_t)(pos - bk.items.begin());
  bk.items.insert(pos, it);
  bk.hashes.insert(bk.hashes.begin() + (ptrdiff_t)idx, it->feats_h);

  if ((int)bk.items.size() > beam) {
    int removed = (int)bk.items.size() - beam;
    bk.items.resize((size_t)beam);
    bk.hashes.resize((size_t)beam);
    pruned += removed;
  }
}

static void cell_add_item(Cell& cell, Item* it, int beam, int& pruned) {
  auto& bk = cell.b[it->cat];
  if (bk.cat == -1) bk.cat = it->cat;
  if (bucket_has_hash(bk, it->feats_h)) return;
  bucket_insert_sorted(bk, it, beam, pruned);
}

/* ============================================================
 * Operaciones de reglas (op DSL)
 * ============================================================ */

static bool apply_op(Arena& A, Symtab& st, const Rule& r, const Feats& L, const Feats& R, Feats& out) {
  switch (r.op) {
    case OP_EMPTY: out = Feats{}; return true;
    case OP_LEFT:  out = feats_copy(A, L); return true;
    case OP_RIGHT: out = feats_copy(A, R); return true;
    case OP_UNIFY: return feats_unify(A, st, L, R, out);

    case OP_REQUIRE_LEFT:
      if (r.arg_key < 0 || r.arg_val < 0) return false;
      return feats_require(A, st, L, r.arg_key, r.arg_val, out);

    case OP_REQUIRE_RIGHT:
      if (r.arg_key < 0 || r.arg_val < 0) return false;
      return feats_require(A, st, R, r.arg_key, r.arg_val, out);

    case OP_MAKE_GAP: {
      int k_idx = st.intern("idx");
      int v_qi  = st.intern("?i");
      int k_gap = st.intern("gap");
      int v_type= r.arg_type;
      if (v_type < 0) return false;

      out.n = 2;
      out.a = (Feat*)A.alloc(sizeof(Feat)*2, alignof(Feat));
      out.a[0] = {k_idx, v_qi};
      out.a[1] = {k_gap, v_type};
      feats_sort(out);
      return true;
    }

    case OP_RELCLAUSE_OBL: {
      int k_obl = st.intern("obl");
      int v_yes = st.intern("yes");

      Feats tmp;
      if (!feats_require(A, st, R, k_obl, v_yes, tmp)) return false;

      Feats base = feats_copy(A, L);

      int k_gap = st.intern("gap");
      int v_obl = st.intern("obl");
      Feats req;
      req.n = 1;
      req.a = (Feat*)A.alloc(sizeof(Feat), alignof(Feat));
      req.a[0] = {k_gap, v_obl};
      feats_sort(req);

      return feats_unify(A, st, base, req, out);
    }
  }
  return false;
}

/* ============================================================
 * Guess OOV
 * ============================================================ */

static bool is_det_word(const string& w) {
  return (w=="el"||w=="la"||w=="los"||w=="las");
}

static vector<LexEntry> guess_lex(Symtab& st, const Token& tk) {
  vector<LexEntry> out;

  // PropN por mayúscula ASCII
  if (!tk.raw.empty() && tk.raw[0] >= 'A' && tk.raw[0] <= 'Z' && !is_det_word(tk.text)) {
    LexEntry e;
    e.pos = st.intern("PropN");
    e.weight = 0.03;
    e.feats_heap = { {st.intern("num"), st.intern("sg")} };
    out.push_back(std::move(e));
  }

  if (ends_with(tk.text, "mente")) {
    LexEntry e;
    e.pos = st.intern("Adv");
    e.weight = -0.03;
    out.push_back(std::move(e));
  }

  auto add_nf_vi_vt = [&](double w_vi, double w_vt){
    {
      LexEntry vi;
      vi.pos = st.intern("Vi");
      vi.weight = w_vi;
      vi.feats_heap = {{st.intern("fin"), st.intern("no")},{st.intern("obl"), st.intern("no")}};
      sort(vi.feats_heap.begin(), vi.feats_heap.end(), [](const Feat&a,const Feat&b){ return a.key<b.key || (a.key==b.key && a.val<b.val); });
      out.push_back(std::move(vi));
    }
    {
      LexEntry vt;
      vt.pos = st.intern("Vt");
      vt.weight = w_vt;
      vt.feats_heap = {{st.intern("fin"), st.intern("no")},{st.intern("obl"), st.intern("no")}};
      sort(vt.feats_heap.begin(), vt.feats_heap.end(), [](const Feat&a,const Feat&b){ return a.key<b.key || (a.key==b.key && a.val<b.val); });
      out.push_back(std::move(vt));
    }
  };

  if (ends_with(tk.text,"ar")||ends_with(tk.text,"er")||ends_with(tk.text,"ir")) add_nf_vi_vt(-0.12, -0.14);
  if (ends_with(tk.text,"ando")||ends_with(tk.text,"iendo")) add_nf_vi_vt(-0.14, -0.16);

  if (out.empty()) {
    LexEntry n;
    n.pos = st.intern("N");
    n.weight = -0.35;
    n.feats_heap = {{st.intern("gen"), st.intern("?g")},{st.intern("num"), st.intern("?n")}};
    sort(n.feats_heap.begin(), n.feats_heap.end(), [](const Feat&a,const Feat&b){ return a.key<b.key || (a.key==b.key && a.val<b.val); });
    out.push_back(std::move(n));
  }
  return out;
}

/* ============================================================
 * Unary closure
 * ============================================================ */

static void unary_closure(Arena& A, Symtab& st, const Grammar& gr, Cell& cell, int beam, int& pruned, int& unary_apps) {
  bool changed = true;
  while (changed) {
    changed = false;
    for (const Rule& r : gr.rules) {
      if (r.rhs_len != 1) continue;
      auto itb = cell.b.find(r.rhs1);
      if (itb == cell.b.end()) continue;

      int before = (cell.b.count(r.lhs) ? (int)cell.b[r.lhs].items.size() : 0);

      Bucket& bk = itb->second;
      for (Item* ch : bk.items) {
        Feats pf;
        Feats empty{};
        if (!apply_op(A, st, r, ch->feats, empty, pf)) continue;
        unary_apps++;

        Item* it = A.make<Item>();
        it->cat = r.lhs;
        it->feats = pf;
        it->feats_h = feats_hash(it->feats);
        it->score = ch->score + r.weight;

        Node* n = A.make<Node>();
        n->label = r.lhs;
        n->child = ch->node;
        n->feats = it->feats;
        n->score = it->score;
        it->node = n;

        cell_add_item(cell, it, beam, pruned);
      }

      int after = (cell.b.count(r.lhs) ? (int)cell.b[r.lhs].items.size() : 0);
      if (after != before) changed = true;
    }
  }
}

/* ============================================================
 * Sanity checks
 * ============================================================ */

static bool has_desc_label(const Node* n, int label) {
  if (!n) return true;
  if (!n->is_leaf_token && n->label == label) return true;
  if (n->child && has_desc_label(n->child, label)) return true;
  if (n->left && has_desc_label(n->left, label)) return true;
  if (n->right && has_desc_label(n->right, label)) return true;
  return false;
}

static void walk_sanity(const Node* n, int symS, int symVPFIN, bool& found) {
  if (!n) return;
  if (!n->is_leaf_token && n->label == symS) {
    if (n->child) { if (n->child->label == symVPFIN) found = true; }
    else {
      if (n->left && n->left->label == symVPFIN) found = true;
      if (n->right && n->right->label == symVPFIN) found = true;
    }
  }
  if (n->child) walk_sanity(n->child, symS, symVPFIN, found);
  if (n->left)  walk_sanity(n->left, symS, symVPFIN, found);
  if (n->right) walk_sanity(n->right, symS, symVPFIN, found);
}

static bool sanity_s_has_vpfin(Symtab& st, const Node* tree) {
  int symS = st.intern("S");
  int symVPFIN = st.intern("VP_FIN");
  bool found=false;
  walk_sanity(tree, symS, symVPFIN, found);
  return found;
}

static bool sanity_sin_takes_vpnf(Symtab& st, const Node* tree) {
  int symPinf = st.intern("Pinf");
  int symVPNF = st.intern("VP_NF");
  if (!tree) return true;
  if (!tree->is_leaf_token && tree->label == symPinf) {
    if (!has_desc_label(tree, symVPNF)) return false;
  }
  if (tree->child && !sanity_sin_takes_vpnf(st, tree->child)) return false;
  if (tree->left  && !sanity_sin_takes_vpnf(st, tree->left)) return false;
  if (tree->right && !sanity_sin_takes_vpnf(st, tree->right)) return false;
  return true;
}

static bool sanity_enclitic_only_nf(Symtab& st, const Node* tree) {
  int symVP = st.intern("VP");
  int symCl = st.intern("Cl");
  int symVt = st.intern("Vt");
  int symVi = st.intern("Vi");
  if (!tree) return true;

  if (!tree->is_leaf_token && tree->label == symVP && tree->left && tree->right) {
    if (!tree->right->is_leaf_token && tree->right->label == symCl) {
      if (!tree->left->is_leaf_token && (tree->left->label==symVt || tree->left->label==symVi)) return false;
    }
  }
  if (tree->child && !sanity_enclitic_only_nf(st, tree->child)) return false;
  if (tree->left  && !sanity_enclitic_only_nf(st, tree->left)) return false;
  if (tree->right && !sanity_enclitic_only_nf(st, tree->right)) return false;
  return true;
}

/* ============================================================
 * Parse sentence
 * ============================================================ */

struct ParseStats {
  string sentence;
  int tokens=0;
  int oov=0;
  bool parsed=false;
  int n_parses=0;
  double best_score=0.0;

  int chart_items_total=0;
  int chart_items_max_cell=0;
  int pruned=0;
  int unary_apps=0;
  int ambiguous_cells=0;

  bool sanity1=false, sanity2=false, sanity3=false;

  vector<string> notes;
  string best_tree; // pretty
  double time_ms=0.0;
};

static void parse_sentence(Symtab& st, const Lexicon& lx, const Grammar& gr,
                           const string& sentence, int topk, int beam, bool include_tree,
                           ParseStats& out) {
  out = ParseStats{};
  out.sentence = sentence;

  auto t0 = chrono::steady_clock::now();

  Arena A(1u<<22);
  auto toks = tokenize(sentence);
  int n = (int)toks.size();
  out.tokens = n;
  if (n==0) {
    out.notes.push_back("empty");
    out.time_ms = 0.0;
    return;
  }

  vector<vector<Cell>> chart(n, vector<Cell>(n+1));

  int pruned=0, unary_apps=0;

  int symTOK = st.intern("TOK");
  int symN = st.intern("N");
  int symPropN = st.intern("PropN");
  int symPron = st.intern("Pron");
  int k_idx = st.intern("idx");

  // lexical init
  for (int i=0;i<n;i++) {
    Cell& cell = chart[i][i+1];

    auto itlex = lx.entries.find(toks[i].text);
    bool inLex = (itlex != lx.entries.end());
    if (!inLex) out.oov++;

    auto emit_entry = [&](const LexEntry& le){
      // feats -> arena
      Feats lf{};
      if (!le.feats_heap.empty()) {
        lf.n = (int)le.feats_heap.size();
        lf.a = (Feat*)A.alloc(sizeof(Feat)*(size_t)lf.n, alignof(Feat));
        memcpy(lf.a, le.feats_heap.data(), sizeof(Feat)*(size_t)lf.n);
        feats_sort(lf);
      }

      // default idx for N/PropN/Pron
      if (le.pos==symN || le.pos==symPropN || le.pos==symPron) {
        if (feat_find(lf, k_idx) < 0) {
          string v = "t" + to_string(toks[i].index);
          Feats req{};
          req.n=1;
          req.a=(Feat*)A.alloc(sizeof(Feat), alignof(Feat));
          req.a[0] = {k_idx, st.intern(v)};
          feats_sort(req);
          Feats merged{};
          if (feats_unify(A, st, lf, req, merged)) lf = merged;
        }
      }

      // leaf token node
      Node* leaf = A.make<Node>();
      leaf->label = symTOK;
      leaf->is_leaf_token = true;
      leaf->leaf_raw = A.dup_cstr(toks[i].raw);
      leaf->score = le.weight;

      // preterminal node
      Node* pre = A.make<Node>();
      pre->label = le.pos;
      pre->child = leaf;
      pre->feats = lf;
      pre->score = le.weight;

      Item* it = A.make<Item>();
      it->cat = le.pos;
      it->feats = lf;
      it->feats_h = feats_hash(it->feats);
      it->score = le.weight;
      it->node = pre;

      cell_add_item(cell, it, beam, pruned);
    };

    if (inLex) for (const LexEntry& le : itlex->second) emit_entry(le);

    // guessed
    auto guessed = guess_lex(st, toks[i]);
    for (const LexEntry& le : guessed) emit_entry(le);

    unary_closure(A, st, gr, cell, beam, pruned, unary_apps);
  }

  // CKY spans
  for (int span=2; span<=n; span++) {
    for (int i=0;i+span<=n;i++) {
      int j = i + span;
      Cell& cell = chart[i][j];

      for (int k=i+1;k<j;k++) {
        Cell& L = chart[i][k];
        Cell& R = chart[k][j];
        if (L.b.empty() || R.b.empty()) continue;

        for (const Rule& rule : gr.rules) {
          if (rule.rhs_len != 2) continue;
          auto itL = L.b.find(rule.rhs1);
          if (itL == L.b.end()) continue;
          auto itR = R.b.find(rule.rhs2);
          if (itR == R.b.end()) continue;

          const Bucket& lb = itL->second;
          const Bucket& rb = itR->second;

          for (Item* ib : lb.items) {
            for (Item* ic : rb.items) {
              Feats pf{};
              if (!apply_op(A, st, rule, ib->feats, ic->feats, pf)) continue;

              double score = ib->score + ic->score + rule.weight;

              Node* right_node = ic->node;
              if (rule.post_flags & POST_PROPAGATE_IDX_TO_RIGHT) {
                int idx_pos = feat_find(ib->feats, k_idx);
                if (idx_pos >= 0) {
                  int idx_val = ib->feats.a[idx_pos].val;
                  int from = st.intern("?i");
                  node_replace_feat_value(right_node, from, idx_val);
                }
              }

              Item* it = A.make<Item>();
              it->cat = rule.lhs;
              it->feats = pf;
              it->feats_h = feats_hash(it->feats);
              it->score = score;

              Node* nn = A.make<Node>();
              nn->label = rule.lhs;
              nn->left = ib->node;
              nn->right= right_node;
              nn->feats = it->feats;
              nn->score = it->score;
              it->node = nn;

              cell_add_item(cell, it, beam, pruned);
            }
          }
        }
      }

      unary_closure(A, st, gr, cell, beam, pruned, unary_apps);
    }
  }

  // chart metrics
  int total_items=0, max_cell=0, amb_cells=0;
  for (int i=0;i<n;i++) {
    for (int j=i+1;j<=n;j++) {
      int cell_items=0;
      for (auto& kv : chart[i][j].b) cell_items += (int)kv.second.items.size();
      total_items += cell_items;
      max_cell = max(max_cell, cell_items);
      if (chart[i][j].b.size() >= 2) amb_cells++;
    }
  }
  out.chart_items_total = total_items;
  out.chart_items_max_cell = max_cell;
  out.ambiguous_cells = amb_cells;
  out.pruned = pruned;
  out.unary_apps = unary_apps;

  int symS = st.intern("S");
  auto itS = chart[0][n].b.find(symS);
  if (itS == chart[0][n].b.end() || itS->second.items.empty()) {
    out.parsed = false;
    out.notes.push_back("NO_PARSE");
  } else {
    out.parsed = true;
    out.n_parses = min((int)itS->second.items.size(), topk);
    out.best_score = itS->second.items[0]->score;

    const Node* best = itS->second.items[0]->node;
    out.sanity1 = sanity_s_has_vpfin(st, best);
    out.sanity2 = sanity_sin_takes_vpnf(st, best);
    out.sanity3 = sanity_enclitic_only_nf(st, best);

    if (!out.sanity1) out.notes.push_back("WARN: S sin VP_FIN visible");
    if (!out.sanity2) out.notes.push_back("WARN: 'sin' sin VP_NF bajo Pinf");
    if (!out.sanity3) out.notes.push_back("WARN: enclítico con verbo finito");

    if (include_tree) {
      ostringstream oss;
      node_pretty_rec(st, best, 0, oss);
      out.best_tree = oss.str();
    }
  }

  auto t1 = chrono::steady_clock::now();
  out.time_ms = chrono::duration<double, std::milli>(t1-t0).count();
}

/* ============================================================
 * Export JSON
 * ============================================================ */

static void json_escape(ostream& os, const string& s) {
  os << '"';
  for (unsigned char c : s) {
    if (c=='"' || c=='\\') { os << '\\' << (char)c; }
    else if (c=='\n') os << "\\n";
    else if (c=='\r') os << "\\r";
    else if (c=='\t') os << "\\t";
    else os << (char)c;
  }
  os << '"';
}

struct EvalSummary {
  vector<ParseStats> rows;
  int beam=16;
  int topk=1;
  double coverage=0.0;
  double avg_tokens=0.0;
  double avg_oov=0.0;
  double total_time_ms=0.0;
  double avg_time_ms=0.0;
};

static void write_summary_json(const string& path, const EvalSummary& sum) {
  ofstream f(path, ios::binary);
  if (!f) die("No puedo escribir " + path);

  f << "{\n";
  f << "  \"sentences\": " << sum.rows.size() << ",\n";
  f << "  \"coverage\": " << fixed << setprecision(6) << sum.coverage << ",\n";
  f << "  \"avgTokens\": " << sum.avg_tokens << ",\n";
  f << "  \"avgOov\": " << sum.avg_oov << ",\n";
  f << "  \"totalTimeMs\": " << sum.total_time_ms << ",\n";
  f << "  \"avgTimeMs\": " << sum.avg_time_ms << ",\n";
  f << "  \"beam\": " << sum.beam << ",\n";
  f << "  \"topK\": " << sum.topk << ",\n";
  f << "  \"rows\": [\n";

  for (size_t i=0;i<sum.rows.size();i++) {
    const auto& st = sum.rows[i];
    f << "    {\n";
    f << "      \"sentence\": "; json_escape(f, st.sentence); f << ",\n";
    f << "      \"tokens\": " << st.tokens << ",\n";
    f << "      \"oovTokens\": " << st.oov << ",\n";
    f << "      \"parsed\": " << (st.parsed ? "true":"false") << ",\n";
    f << "      \"nParsesReturned\": " << st.n_parses << ",\n";
    f << "      \"bestScore\": " << (st.parsed ? to_string(st.best_score) : string("null")) << ",\n";
    f << "      \"timeMs\": " << st.time_ms << ",\n";
    f << "      \"chartItemsTotal\": " << st.chart_items_total << ",\n";
    f << "      \"chartItemsMaxCell\": " << st.chart_items_max_cell << ",\n";
    f << "      \"prunedByBeam\": " << st.pruned << ",\n";
    f << "      \"unaryApplications\": " << st.unary_apps << ",\n";
    f << "      \"ambiguousCells\": " << st.ambiguous_cells << ",\n";
    f << "      \"sanitySHasVpFin\": " << (st.sanity1 ? "true":"false") << ",\n";
    f << "      \"sanitySinTakesVpNf\": " << (st.sanity2 ? "true":"false") << ",\n";
    f << "      \"sanityEncliticOnlyNf\": " << (st.sanity3 ? "true":"false") << ",\n";

    f << "      \"notes\": [";
    for (size_t k=0;k<st.notes.size();k++) {
      if (k) f << ", ";
      json_escape(f, st.notes[k]);
    }
    f << "],\n";

    f << "      \"bestTree\": ";
    if (!st.best_tree.empty()) json_escape(f, st.best_tree);
    else f << "null";
    f << "\n";

    f << "    }" << (i+1==sum.rows.size() ? "" : ",") << "\n";
  }

  f << "  ]\n";
  f << "}\n";
}

/* ============================================================
 * CLI
 * ============================================================ */

static void usage() {
  cout <<
    "Uso:\n"
    "  ./parser_v7 [--lex lexicon.json] [--grammar grammar.json]\n"
    "             [--file corpus.txt | --text \"...\"]\n"
    "             [--beam 16] [--topk 1] [--trees] [--print] [--json out.json]\n\n"
    "Ejemplos:\n"
    "  ./parser_v7 --file corpus.txt --print --json out.json\n"
    "  ./parser_v7 --text \"Los científicos lo estudiaron durante décadas sin comprenderlo.\" --trees --print\n";
}

int main(int argc, char** argv) {
  string lex_path = "lexicon.json";
  string grammar_path = "grammar.json";
  string file_path = "corpus.txt";
  string text;
  string json_out;
  int beam = 16;
  int topk = 1;
  bool trees = false;
  bool print = false;

  auto need = [&](int& i)->string{
    if (i+1 >= argc) die(string("Falta valor para ") + argv[i]);
    return string(argv[++i]);
  };

  for (int i=1;i<argc;i++) {
    string a = argv[i];
    if (a=="--help" || a=="-h") { usage(); return 0; }
    else if (a=="--lex") lex_path = need(i);
    else if (a=="--grammar") grammar_path = need(i);
    else if (a=="--file") file_path = need(i);
    else if (a=="--text") text = need(i);
    else if (a=="--json") json_out = need(i);
    else if (a=="--beam") beam = stoi(need(i));
    else if (a=="--topk") topk = stoi(need(i));
    else if (a=="--trees") trees = true;
    else if (a=="--print") print = true;
    else die("Arg desconocido: " + a);
  }

  Symtab st;
  Lexicon lx;
  Grammar gr;

  load_lexicon_json(st, lx, lex_path);
  load_grammar_json(st, gr, grammar_path);

  string corpus = text.empty() ? read_file(file_path) : text;
  auto sents = split_sentences(corpus);

  EvalSummary sum;
  sum.beam = beam;
  sum.topk = topk;

  int parsed=0, total_tokens=0, total_oov=0;
  double total_time=0.0;

  for (const auto& s : sents) {
    ParseStats row;
    parse_sentence(st, lx, gr, s, topk, beam, trees, row);

    if (print) {
      cout << "==============================================================================\n";
      cout << row.sentence << "\n";
      cout << "tokens=" << row.tokens
           << "  oov=" << row.oov
           << "  parsed=" << (row.parsed?1:0)
           << "  parses=" << row.n_parses
           << "  bestScore=" << (row.parsed ? to_string(row.best_score) : string("null"))
           << "  time_ms=" << fixed << setprecision(1) << row.time_ms << "\n";
      cout << "chart_items=" << row.chart_items_total
           << "  max_cell=" << row.chart_items_max_cell
           << "  pruned=" << row.pruned
           << "  unary_apps=" << row.unary_apps
           << "  amb_cells=" << row.ambiguous_cells << "\n";
      if (!row.notes.empty()) {
        cout << "notes: ";
        for (size_t k=0;k<row.notes.size();k++) {
          if (k) cout << "; ";
          cout << row.notes[k];
        }
        cout << "\n";
      }
      if (trees && !row.best_tree.empty()) cout << row.best_tree;
    }

    parsed += row.parsed ? 1 : 0;
    total_tokens += row.tokens;
    total_oov += row.oov;
    total_time += row.time_ms;

    sum.rows.push_back(std::move(row));
  }

  sum.coverage = sents.empty() ? 0.0 : (double)parsed / (double)sents.size();
  sum.avg_tokens = sents.empty() ? 0.0 : (double)total_tokens / (double)sents.size();
  sum.avg_oov = sents.empty() ? 0.0 : (double)total_oov / (double)sents.size();
  sum.total_time_ms = total_time;
  sum.avg_time_ms = sents.empty() ? 0.0 : total_time / (double)sents.size();

  if (!print) {
    cout << "SUMMARY: sentences=" << sents.size()
         << " coverage=" << fixed << setprecision(3) << sum.coverage
         << " avg_time_ms=" << fixed << setprecision(1) << sum.avg_time_ms
         << " beam=" << beam << " top_k=" << topk << "\n";
  } else {
    cout << "==============================================================================\n";
    cout << "SUMMARY\nsentences=" << sents.size()
         << "  coverage=" << fixed << setprecision(3) << sum.coverage
         << "  avg_tokens=" << fixed << setprecision(2) << sum.avg_tokens
         << "  avg_oov=" << fixed << setprecision(2) << sum.avg_oov
         << "  avg_time_ms=" << fixed << setprecision(1) << sum.avg_time_ms
         << "  beam=" << beam << "  top_k=" << topk << "\n";
  }

  if (!json_out.empty()) {
    write_summary_json(json_out, sum);
    cout << "Wrote JSON: " << json_out << "\n";
  }

  return 0;
}
