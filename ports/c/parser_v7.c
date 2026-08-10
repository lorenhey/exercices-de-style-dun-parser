// parser_v7.c - C99
// CKY + beam + rasgos mínimos + carga grammar.json/lexicon.json + tokenización (al/del + enclíticos)
// JSON parser embebido (jsmn-like) + export JSON sin deps.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>
#include <time.h>

/* ============================================================
 * Utilidades generales
 * ============================================================ */

static void die(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
  va_end(ap);
  exit(1);
}

static double now_ms(void) {
  // portable-ish: CLOCK_MONOTONIC si existe; fallback clock()
#if defined(_POSIX_C_SOURCE) && _POSIX_C_SOURCE >= 199309L
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return (double)ts.tv_sec * 1000.0 + (double)ts.tv_nsec / 1e6;
#else
  return (double)clock() * 1000.0 / (double)CLOCKS_PER_SEC;
#endif
}

static char *read_file(const char *path, size_t *out_len) {
  FILE *f = fopen(path, "rb");
  if (!f) die("No puedo abrir: %s", path);
  fseek(f, 0, SEEK_END);
  long n = ftell(f);
  fseek(f, 0, SEEK_SET);
  if (n < 0) die("ftell falló: %s", path);
  char *buf = (char*)malloc((size_t)n + 1);
  if (!buf) die("OOM leyendo %s", path);
  size_t r = fread(buf, 1, (size_t)n, f);
  fclose(f);
  if (r != (size_t)n) die("Lectura incompleta de %s", path);
  buf[n] = '\0';
  if (out_len) *out_len = (size_t)n;
  return buf;
}

static int starts_with(const char *s, const char *p) {
  return strncmp(s, p, strlen(p)) == 0;
}

static int ends_with(const char *s, const char *suffix) {
  size_t n = strlen(s), m = strlen(suffix);
  if (m > n) return 0;
  return memcmp(s + (n - m), suffix, m) == 0;
}

static void ascii_lower_inplace(char *s) {
  for (; *s; s++) {
    unsigned char c = (unsigned char)*s;
    if (c >= 'A' && c <= 'Z') *s = (char)(c - 'A' + 'a');
  }
}

static char *xstrdup(const char *s) {
  size_t n = strlen(s);
  char *p = (char*)malloc(n + 1);
  if (!p) die("OOM strdup");
  memcpy(p, s, n + 1);
  return p;
}

/* ============================================================
 * Arena simple
 * ============================================================ */

typedef struct {
  unsigned char *buf;
  size_t cap;
  size_t len;
} Arena;

static void arena_init(Arena *a, size_t cap) {
  a->buf = (unsigned char*)malloc(cap);
  if (!a->buf) die("OOM arena");
  a->cap = cap;
  a->len = 0;
}

static void *arena_alloc(Arena *a, size_t n) {
  if (a->len + n > a->cap) {
    size_t newcap = a->cap * 2;
    while (newcap < a->len + n) newcap *= 2;
    unsigned char *nb = (unsigned char*)realloc(a->buf, newcap);
    if (!nb) die("OOM arena realloc");
    a->buf = nb;
    a->cap = newcap;
  }
  void *p = a->buf + a->len;
  a->len += n;
  memset(p, 0, n);
  return p;
}

static char *arena_strdup(Arena *a, const char *s) {
  size_t n = strlen(s);
  char *p = (char*)arena_alloc(a, n + 1);
  memcpy(p, s, n + 1);
  return p;
}

/* ============================================================
 * Interning + symbol table (string -> id)
 * ============================================================ */

typedef struct {
  char **keys;
  int  *vals;
  size_t cap;
  size_t len;
} StrIntMap;

static unsigned long hash_str(const char *s) {
  // FNV-1a 64-ish in unsigned long
  unsigned long h = 1469598103934665603UL;
  while (*s) {
    h ^= (unsigned char)*s++;
    h *= 1099511628211UL;
  }
  return h;
}

static void simap_init(StrIntMap *m, size_t cap) {
  m->cap = cap;
  m->len = 0;
  m->keys = (char**)calloc(cap, sizeof(char*));
  m->vals = (int*)calloc(cap, sizeof(int));
  if (!m->keys || !m->vals) die("OOM simap");
}

static void simap_rehash(StrIntMap *m) {
  size_t oldcap = m->cap;
  char **oldk = m->keys;
  int  *oldv = m->vals;

  simap_init(m, oldcap * 2);
  for (size_t i=0;i<oldcap;i++) {
    if (!oldk[i]) continue;
    // insert
    unsigned long h = hash_str(oldk[i]);
    size_t idx = (size_t)(h % m->cap);
    while (m->keys[idx]) idx = (idx + 1) % m->cap;
    m->keys[idx] = oldk[i];
    m->vals[idx] = oldv[i];
    m->len++;
  }
  free(oldk);
  free(oldv);
}

static int simap_get(StrIntMap *m, const char *key, int *out) {
  unsigned long h = hash_str(key);
  size_t idx = (size_t)(h % m->cap);
  for (size_t step=0; step<m->cap; step++) {
    size_t j = (idx + step) % m->cap;
    if (!m->keys[j]) return 0;
    if (strcmp(m->keys[j], key) == 0) {
      if (out) *out = m->vals[j];
      return 1;
    }
  }
  return 0;
}

static void simap_put(StrIntMap *m, char *key_owned, int val) {
  if ((m->len + 1) * 10 >= m->cap * 7) simap_rehash(m); // load >0.7
  unsigned long h = hash_str(key_owned);
  size_t idx = (size_t)(h % m->cap);
  while (m->keys[idx]) {
    if (strcmp(m->keys[idx], key_owned) == 0) { m->vals[idx] = val; free(key_owned); return; }
    idx = (idx + 1) % m->cap;
  }
  m->keys[idx] = key_owned;
  m->vals[idx] = val;
  m->len++;
}

typedef struct {
  StrIntMap map;
  char **id2str;
  int cap;
  int len;
} Symtab;

static void symtab_init(Symtab *st) {
  simap_init(&st->map, 1024);
  st->cap = 1024;
  st->len = 0;
  st->id2str = (char**)malloc(sizeof(char*) * st->cap);
  if (!st->id2str) die("OOM symtab");
}

static int sym_intern(Symtab *st, const char *s) {
  int id;
  if (simap_get(&st->map, s, &id)) return id;
  if (st->len >= st->cap) {
    st->cap *= 2;
    st->id2str = (char**)realloc(st->id2str, sizeof(char*) * st->cap);
    if (!st->id2str) die("OOM symtab realloc");
  }
  id = st->len++;
  char *owned = xstrdup(s);
  st->id2str[id] = owned;
  simap_put(&st->map, xstrdup(s), id);
  return id;
}

static const char *sym_str(Symtab *st, int id) {
  if (id < 0 || id >= st->len) return "?";
  return st->id2str[id];
}

static int is_var_str(const char *s) { return s && s[0] == '?'; }

/* ============================================================
 * JSON minimal parser (tokenizer) - jsmn-like
 * ============================================================ */

typedef enum { JSMN_UNDEFINED=0, JSMN_OBJECT=1, JSMN_ARRAY=2, JSMN_STRING=3, JSMN_PRIMITIVE=4 } jsmntype_t;

typedef struct {
  jsmntype_t type;
  int start;
  int end;
  int size; // number of children
} jsmntok_t;

typedef struct {
  unsigned int pos;
  unsigned int toknext;
  int toksuper;
} jsmn_parser;

static void jsmn_init(jsmn_parser *p) {
  p->pos = 0;
  p->toknext = 0;
  p->toksuper = -1;
}

static jsmntok_t *jsmn_alloc_token(jsmn_parser *p, jsmntok_t *toks, size_t ntoks) {
  if (p->toknext >= ntoks) return NULL;
  jsmntok_t *t = &toks[p->toknext++];
  t->start = t->end = -1;
  t->size = 0;
  t->type = JSMN_UNDEFINED;
  return t;
}

static void jsmn_fill_token(jsmntok_t *t, jsmntype_t type, int start, int end) {
  t->type = type;
  t->start = start;
  t->end = end;
  t->size = 0;
}

static int jsmn_parse_primitive(jsmn_parser *p, const char *js, size_t len, jsmntok_t *toks, size_t ntoks) {
  int start = (int)p->pos;
  while (p->pos < len) {
    char c = js[p->pos];
    if (c=='\t' || c=='\r' || c=='\n' || c==' ' || c==',' || c==']' || c=='}') break;
    p->pos++;
  }
  jsmntok_t *t = jsmn_alloc_token(p, toks, ntoks);
  if (!t) return -1;
  jsmn_fill_token(t, JSMN_PRIMITIVE, start, (int)p->pos);
  p->pos--;
  return 0;
}

static int jsmn_parse_string(jsmn_parser *p, const char *js, size_t len, jsmntok_t *toks, size_t ntoks) {
  int start = (int)p->pos;
  p->pos++; // skip "
  for (; p->pos < len; p->pos++) {
    char c = js[p->pos];
    if (c == '\"') {
      jsmntok_t *t = jsmn_alloc_token(p, toks, ntoks);
      if (!t) return -1;
      jsmn_fill_token(t, JSMN_STRING, start+1, (int)p->pos);
      return 0;
    }
    if (c == '\\') p->pos++; // skip escaped
  }
  return -2;
}

static int jsmn_parse(jsmn_parser *p, const char *js, size_t len, jsmntok_t *toks, size_t ntoks) {
  for (; p->pos < len; p->pos++) {
    char c = js[p->pos];
    jsmntok_t *t;
    switch (c) {
      case '{': case '[':
        t = jsmn_alloc_token(p, toks, ntoks);
        if (!t) return -1;
        t->type = (c=='{') ? JSMN_OBJECT : JSMN_ARRAY;
        t->start = (int)p->pos;
        p->toksuper = (int)(p->toknext - 1);
        break;

      case '}': case ']': {
        jsmntype_t type = (c=='}') ? JSMN_OBJECT : JSMN_ARRAY;
        int i = (int)p->toknext - 1;
        for (; i>=0; i--) {
          if (toks[i].start != -1 && toks[i].end == -1) {
            if (toks[i].type != type) return -3;
            toks[i].end = (int)p->pos + 1;
            p->toksuper = -1;
            // find parent
            for (int j=i-1; j>=0; j--) {
              if (toks[j].start != -1 && toks[j].end == -1) { p->toksuper = j; break; }
            }
            break;
          }
        }
        break;
      }

      case '\"':
        if (jsmn_parse_string(p, js, len, toks, ntoks) < 0) return -2;
        if (p->toksuper != -1) toks[p->toksuper].size++;
        break;

      case '\t': case '\r': case '\n': case ' ': case ':': case ',':
        break;

      default:
        if (jsmn_parse_primitive(p, js, len, toks, ntoks) < 0) return -1;
        if (p->toksuper != -1) toks[p->toksuper].size++;
        break;
    }
  }
  // set end for root if missing
  for (int i=(int)p->toknext-1; i>=0; i--) {
    if (toks[i].start != -1 && toks[i].end == -1) toks[i].end = (int)len;
  }
  return (int)p->toknext;
}

static int jsoneq(const char *json, jsmntok_t *tok, const char *s) {
  int n = tok->end - tok->start;
  return (tok->type == JSMN_STRING && (int)strlen(s) == n && strncmp(json + tok->start, s, (size_t)n) == 0);
}

static char *json_tok_strdup(Arena *a, const char *json, jsmntok_t *tok) {
  int n = tok->end - tok->start;
  char *s = (char*)arena_alloc(a, (size_t)n + 1);
  memcpy(s, json + tok->start, (size_t)n);
  s[n] = '\0';
  return s;
}

static double json_tok_todouble(const char *json, jsmntok_t *tok) {
  int n = tok->end - tok->start;
  char tmp[64];
  if (n <= 0 || n >= (int)sizeof(tmp)) die("Número JSON demasiado largo");
  memcpy(tmp, json + tok->start, (size_t)n);
  tmp[n] = '\0';
  return atof(tmp);
}

/* ============================================================
 * Modelo: feats, lexicon, grammar
 * ============================================================ */

typedef struct { int key; int val; } Feat;

typedef struct {
  Feat *a;
  int n;
} Feats;

static int feat_find(const Feats *f, int key) {
  for (int i=0;i<f->n;i++) if (f->a[i].key == key) return i;
  return -1;
}

static unsigned long feats_hash(const Feats *f) {
  // sort-insensitive hash: we assume pairs already sorted by key
  unsigned long h = 1469598103934665603UL;
  for (int i=0;i<f->n;i++) {
    h ^= (unsigned long)f->a[i].key; h *= 1099511628211UL;
    h ^= (unsigned long)f->a[i].val; h *= 1099511628211UL;
  }
  return h;
}

static void feats_sort(Feats *f) {
  // insertion sort small n
  for (int i=1;i<f->n;i++) {
    Feat x = f->a[i];
    int j=i-1;
    while (j>=0 && (f->a[j].key > x.key || (f->a[j].key==x.key && f->a[j].val > x.val))) {
      f->a[j+1]=f->a[j]; j--;
    }
    f->a[j+1]=x;
  }
}

static Feats feats_copy(Arena *a, const Feats *src) {
  Feats out;
  out.n = src->n;
  out.a = (Feat*)arena_alloc(a, sizeof(Feat) * (size_t)out.n);
  memcpy(out.a, src->a, sizeof(Feat) * (size_t)out.n);
  return out;
}

static Feats feats_empty(void) {
  Feats f; f.a = NULL; f.n = 0; return f;
}

static int feats_unify(Arena *a, Symtab *st, const Feats *A, const Feats *B, Feats *out) {
  // Unificación simple con variables tipo "?x"
  // out se aloja en arena
  // Estrategia: copiar A y luego incorporar B.
  Feats tmp;
  tmp.n = A->n;
  tmp.a = (Feat*)arena_alloc(a, sizeof(Feat) * (size_t)(A->n + B->n));
  memcpy(tmp.a, A->a, sizeof(Feat) * (size_t)A->n);

  for (int i=0;i<B->n;i++) {
    int k = B->a[i].key;
    int vb = B->a[i].val;
    int j = feat_find(&tmp, k);
    if (j < 0) {
      tmp.a[tmp.n++] = (Feat){k, vb};
      continue;
    }
    int va = tmp.a[j].val;
    if (va == vb) continue;

    const char *sa = sym_str(st, va);
    const char *sb = sym_str(st, vb);

    int va_var = is_var_str(sa);
    int vb_var = is_var_str(sb);

    if (va_var && !vb_var) { tmp.a[j].val = vb; continue; }
    if (!va_var && vb_var) { continue; }
    if (va_var && vb_var) { continue; }

    return 0; // conflicto
  }

  // normalizar orden
  Feats res;
  res.n = tmp.n;
  res.a = (Feat*)arena_alloc(a, sizeof(Feat) * (size_t)res.n);
  memcpy(res.a, tmp.a, sizeof(Feat) * (size_t)res.n);
  feats_sort(&res);
  *out = res;
  return 1;
}

static int feats_require(Arena *a, Symtab *st, const Feats *f, int key, int val, Feats *out) {
  Feats req;
  req.n = 1;
  req.a = (Feat*)arena_alloc(a, sizeof(Feat));
  req.a[0] = (Feat){key, val};
  feats_sort(&req);
  return feats_unify(a, st, f, &req, out);
}

typedef struct {
  int pos;      // symbol id
  double weight;
  Feats feats;
} LexEntry;

typedef struct {
  char *word;        // owned in map
  LexEntry *entries; // malloc
  int n;
  int cap;
} LexBucket;

typedef struct {
  LexBucket *b;
  int n;
  int cap;
} Lexicon;

// simple linear lexicon (suficiente para prototipo); se puede cambiar por hash map si crece.
static void lexicon_init(Lexicon *lx) {
  lx->n = 0;
  lx->cap = 128;
  lx->b = (LexBucket*)calloc((size_t)lx->cap, sizeof(LexBucket));
  if (!lx->b) die("OOM lexicon");
}

static LexBucket *lexicon_get_bucket(Lexicon *lx, const char *word) {
  for (int i=0;i<lx->n;i++) if (strcmp(lx->b[i].word, word) == 0) return &lx->b[i];
  return NULL;
}

static LexBucket *lexicon_ensure_bucket(Lexicon *lx, const char *word) {
  LexBucket *bk = lexicon_get_bucket(lx, word);
  if (bk) return bk;
  if (lx->n >= lx->cap) {
    lx->cap *= 2;
    lx->b = (LexBucket*)realloc(lx->b, sizeof(LexBucket) * (size_t)lx->cap);
    if (!lx->b) die("OOM lexicon realloc");
    memset(lx->b + (lx->cap/2), 0, sizeof(LexBucket) * (size_t)(lx->cap/2));
  }
  bk = &lx->b[lx->n++];
  bk->word = xstrdup(word);
  bk->n = 0;
  bk->cap = 4;
  bk->entries = (LexEntry*)malloc(sizeof(LexEntry) * (size_t)bk->cap);
  if (!bk->entries) die("OOM lex bucket");
  return bk;
}

static void lexbucket_add(LexBucket *bk, LexEntry e) {
  if (bk->n >= bk->cap) {
    bk->cap *= 2;
    bk->entries = (LexEntry*)realloc(bk->entries, sizeof(LexEntry) * (size_t)bk->cap);
    if (!bk->entries) die("OOM lex bucket realloc");
  }
  bk->entries[bk->n++] = e;
}

typedef enum {
  OP_EMPTY=0, OP_LEFT, OP_RIGHT, OP_UNIFY,
  OP_REQUIRE_LEFT, OP_REQUIRE_RIGHT,
  OP_MAKE_GAP, OP_RELCLAUSE_OBL
} Op;

typedef enum {
  POST_NONE=0,
  POST_PROPAGATE_IDX_TO_RIGHT=1
} Post;

typedef struct {
  int lhs;
  int rhs_len;
  int rhs1;
  int rhs2;
  double weight;
  Op op;
  // args: for REQUIRE key/val, for MAKE_GAP type string id, for RELCLAUSE_OBL none
  int arg_key;
  int arg_val;
  int arg_type; // for MAKE_GAP: type id
  int post_flags;
} Rule;

typedef struct {
  Rule *r;
  int n;
  int cap;
} Grammar;

static void grammar_init(Grammar *g) {
  g->n = 0;
  g->cap = 128;
  g->r = (Rule*)malloc(sizeof(Rule) * (size_t)g->cap);
  if (!g->r) die("OOM grammar");
}

static void grammar_add(Grammar *g, Rule r) {
  if (g->n >= g->cap) {
    g->cap *= 2;
    g->r = (Rule*)realloc(g->r, sizeof(Rule) * (size_t)g->cap);
    if (!g->r) die("OOM grammar realloc");
  }
  g->r[g->n++] = r;
}

static Op op_from(const char *s) {
  if (!s) die("Regla sin op");
  if (strcmp(s,"EMPTY")==0) return OP_EMPTY;
  if (strcmp(s,"LEFT")==0) return OP_LEFT;
  if (strcmp(s,"RIGHT")==0) return OP_RIGHT;
  if (strcmp(s,"UNIFY")==0) return OP_UNIFY;
  if (strcmp(s,"REQUIRE_LEFT")==0) return OP_REQUIRE_LEFT;
  if (strcmp(s,"REQUIRE_RIGHT")==0) return OP_REQUIRE_RIGHT;
  if (strcmp(s,"MAKE_GAP")==0) return OP_MAKE_GAP;
  if (strcmp(s,"RELCLAUSE_OBL")==0) return OP_RELCLAUSE_OBL;
  die("op desconocido: %s", s);
  return OP_EMPTY;
}

/* ============================================================
 * Carga lexicon.json / grammar.json (formato neutral)
 * ============================================================ */

static int tok_skip(const jsmntok_t *toks, int i) {
  // Salta un token y todo su subárbol.
  int j = i + 1;
  if (toks[i].type == JSMN_OBJECT) {
    for (int k=0;k<toks[i].size;k++) {
      j = tok_skip(toks, j); // key
      j = tok_skip(toks, j); // value
    }
  } else if (toks[i].type == JSMN_ARRAY) {
    for (int k=0;k<toks[i].size;k++) j = tok_skip(toks, j);
  }
  return j;
}

static int obj_find_key(const char *json, const jsmntok_t *toks, int obj_i, const char *key) {
  if (toks[obj_i].type != JSMN_OBJECT) return -1;
  int j = obj_i + 1;
  for (int k=0;k<toks[obj_i].size;k++) {
    int key_i = j;
    int val_i = j + 1;
    if (jsoneq(json, (jsmntok_t*)&toks[key_i], key)) return val_i;
    j = tok_skip(toks, val_i);
  }
  return -1;
}

static void load_lexicon_from_json(Symtab *st, Lexicon *lx, const char *path) {
  size_t len=0;
  char *json = read_file(path, &len);

  jsmn_parser p;
  jsmn_init(&p);
  // tokens upper bound: coarse
  size_t ntoks = len / 6 + 256;
  jsmntok_t *toks = (jsmntok_t*)calloc(ntoks, sizeof(jsmntok_t));
  if (!toks) die("OOM toks lexicon");
  int n = jsmn_parse(&p, json, len, toks, ntoks);
  if (n < 0) die("JSON parse lexicon falló (%d)", n);

  Arena ta; arena_init(&ta, 1<<20);

  int root = 0;
  int entries_i = obj_find_key(json, toks, root, "entries");
  if (entries_i < 0 || toks[entries_i].type != JSMN_OBJECT) die("lexicon.json: falta entries{}");

  int j = entries_i + 1;
  for (int k=0;k<toks[entries_i].size;k++) {
    int word_i = j;
    int arr_i  = j + 1;
    char *word = json_tok_strdup(&ta, json, (jsmntok_t*)&toks[word_i]);
    if (toks[arr_i].type != JSMN_ARRAY) die("lexicon.json: entries.%s no es array", word);

    LexBucket *bk = lexicon_ensure_bucket(lx, word);

    int a = arr_i + 1;
    for (int e=0;e<toks[arr_i].size;e++) {
      int obj = a;
      if (toks[obj].type != JSMN_OBJECT) die("lexicon.json: entry no es objeto");
      int pos_i = obj_find_key(json, toks, obj, "pos");
      int w_i   = obj_find_key(json, toks, obj, "weight");
      int f_i   = obj_find_key(json, toks, obj, "feats");
      if (pos_i<0 || w_i<0 || f_i<0) die("lexicon.json: entry incompleta (pos/weight/feats)");

      char *pos = json_tok_strdup(&ta, json, (jsmntok_t*)&toks[pos_i]);
      double wt = json_tok_todouble(json, (jsmntok_t*)&toks[w_i]);

      Feats feats = feats_empty();
      if (toks[f_i].type != JSMN_OBJECT) die("lexicon.json: feats no es objeto");
      if (toks[f_i].size > 0) {
        feats.n = toks[f_i].size;
        feats.a = (Feat*)malloc(sizeof(Feat) * (size_t)feats.n);
        if (!feats.a) die("OOM feats lex");
        int fj = f_i + 1;
        for (int z=0; z<toks[f_i].size; z++) {
          int kk = fj;
          int vv = fj + 1;
          char *kstr = json_tok_strdup(&ta, json, (jsmntok_t*)&toks[kk]);
          char *vstr = json_tok_strdup(&ta, json, (jsmntok_t*)&toks[vv]);
          feats.a[z].key = sym_intern(st, kstr);
          feats.a[z].val = sym_intern(st, vstr);
          fj = tok_skip(toks, vv);
        }
        feats_sort(&feats);
      }

      LexEntry le;
      le.pos = sym_intern(st, pos);
      le.weight = wt;
      // feats van a arena en runtime; acá quedan heap -> se copian en init lexical
      le.feats = feats;
      lexbucket_add(bk, le);

      a = tok_skip(toks, obj);
    }

    j = tok_skip(toks, arr_i);
  }

  free(toks);
  free(json);
  // arena ta se deja vivo para strings temporales; el lexicon ya duplicó words con xstrdup.
}

static void load_grammar_from_json(Symtab *st, Grammar *gr, const char *path) {
  size_t len=0;
  char *json = read_file(path, &len);

  jsmn_parser p; jsmn_init(&p);
  size_t ntoks = len / 6 + 256;
  jsmntok_t *toks = (jsmntok_t*)calloc(ntoks, sizeof(jsmntok_t));
  if (!toks) die("OOM toks grammar");
  int n = jsmn_parse(&p, json, len, toks, ntoks);
  if (n < 0) die("JSON parse grammar falló (%d)", n);

  Arena ta; arena_init(&ta, 1<<20);

  int root = 0;
  int rules_i = obj_find_key(json, toks, root, "rules");
  if (rules_i < 0 || toks[rules_i].type != JSMN_ARRAY) die("grammar.json: falta rules[]");

  int a = rules_i + 1;
  for (int i=0;i<toks[rules_i].size;i++) {
    int obj = a;
    if (toks[obj].type != JSMN_OBJECT) die("grammar.json: regla no es objeto");

    int lhs_i = obj_find_key(json, toks, obj, "lhs");
    int rhs_i = obj_find_key(json, toks, obj, "rhs");
    int w_i   = obj_find_key(json, toks, obj, "weight");
    int op_i  = obj_find_key(json, toks, obj, "op");
    int args_i= obj_find_key(json, toks, obj, "args");
    int post_i= obj_find_key(json, toks, obj, "post");

    if (lhs_i<0 || rhs_i<0 || w_i<0 || op_i<0) die("grammar.json: regla incompleta");

    char *lhs = json_tok_strdup(&ta, json, (jsmntok_t*)&toks[lhs_i]);
    double wt = json_tok_todouble(json, (jsmntok_t*)&toks[w_i]);
    char *op  = json_tok_strdup(&ta, json, (jsmntok_t*)&toks[op_i]);

    Rule r;
    memset(&r, 0, sizeof(r));
    r.lhs = sym_intern(st, lhs);
    r.weight = wt;
    r.op = op_from(op);
    r.post_flags = POST_NONE;

    if (toks[rhs_i].type != JSMN_ARRAY) die("grammar.json: rhs no es array");
    if (toks[rhs_i].size < 1 || toks[rhs_i].size > 2) die("grammar.json: rhs len debe ser 1 o 2");
    r.rhs_len = toks[rhs_i].size;
    int rr = rhs_i + 1;
    char *rhs1 = json_tok_strdup(&ta, json, (jsmntok_t*)&toks[rr]);
    r.rhs1 = sym_intern(st, rhs1);
    if (r.rhs_len == 2) {
      rr = tok_skip(toks, rr);
      char *rhs2 = json_tok_strdup(&ta, json, (jsmntok_t*)&toks[rr]);
      r.rhs2 = sym_intern(st, rhs2);
    } else r.rhs2 = -1;

    // args
    r.arg_key = r.arg_val = r.arg_type = -1;
    if (args_i >= 0 && toks[args_i].type == JSMN_OBJECT) {
      int key_i = obj_find_key(json, toks, args_i, "key");
      int val_i = obj_find_key(json, toks, args_i, "value");
      int type_i= obj_find_key(json, toks, args_i, "type");
      if (key_i >= 0) {
        char *k = json_tok_strdup(&ta, json, (jsmntok_t*)&toks[key_i]);
        r.arg_key = sym_intern(st, k);
      }
      if (val_i >= 0) {
        char *v = json_tok_strdup(&ta, json, (jsmntok_t*)&toks[val_i]);
        r.arg_val = sym_intern(st, v);
      }
      if (type_i >= 0) {
        char *t = json_tok_strdup(&ta, json, (jsmntok_t*)&toks[type_i]);
        r.arg_type = sym_intern(st, t);
      }
    }

    // post
    if (post_i >= 0 && toks[post_i].type == JSMN_ARRAY) {
      int pj = post_i + 1;
      for (int k=0;k<toks[post_i].size;k++) {
        char *ps = json_tok_strdup(&ta, json, (jsmntok_t*)&toks[pj]);
        if (strcmp(ps, "PROPAGATE_IDX_TO_RIGHT") == 0) r.post_flags |= POST_PROPAGATE_IDX_TO_RIGHT;
        pj = tok_skip(toks, pj);
      }
    }

    grammar_add(gr, r);
    a = tok_skip(toks, obj);
  }

  free(toks);
  free(json);
}

/* ============================================================
 * Tokenización + split oraciones
 * ============================================================ */

typedef struct { char *raw; char *text; int index; } Token;

typedef struct {
  Token *t;
  int n;
  int cap;
} TokList;

static void toklist_init(TokList *tl) {
  tl->n = 0; tl->cap = 32;
  tl->t = (Token*)malloc(sizeof(Token) * (size_t)tl->cap);
  if (!tl->t) die("OOM toklist");
}

static void toklist_add(TokList *tl, char *raw, char *text, int index) {
  if (tl->n >= tl->cap) {
    tl->cap *= 2;
    tl->t = (Token*)realloc(tl->t, sizeof(Token) * (size_t)tl->cap);
    if (!tl->t) die("OOM toklist realloc");
  }
  tl->t[tl->n++] = (Token){ raw, text, index };
}

static int is_word_byte(unsigned char c) {
  // ASCII alpha o bytes >= 0xC0 (UTF-8 multibyte lead/cont)
  if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')) return 1;
  if (c >= 0xC0) return 1;
  return 0;
}

static char *substr_dup(Arena *a, const char *s, int start, int end) {
  int n = end - start;
  char *p = (char*)arena_alloc(a, (size_t)n + 1);
  memcpy(p, s + start, (size_t)n);
  p[n] = '\0';
  return p;
}

static const char *CLITICS[] = {"me","te","se","lo","la","los","las","le","les","nos","os"};
static int NCL = 11;

static void tokenize(Arena *a, const char *text, TokList *out) {
  toklist_init(out);
  int idx = 0;

  int n = (int)strlen(text);
  int i=0;
  while (i<n) {
    unsigned char c = (unsigned char)text[i];
    if (!is_word_byte(c)) { i++; continue; }

    int start = i;
    i++;
    while (i<n) {
      unsigned char d = (unsigned char)text[i];
      if (is_word_byte(d) || d=='-' || d=='\'') i++;
      else break;
    }
    int end = i;
    char *raw = substr_dup(a, text, start, end);
    char *low = arena_strdup(a, raw);
    ascii_lower_inplace(low);

    // contracciones al/del
    if (strcmp(low, "al") == 0) {
      char *r1 = arena_strdup(a, "a");
      char *t1 = arena_strdup(a, "a");
      char *r2 = arena_strdup(a, "el");
      char *t2 = arena_strdup(a, "el");
      toklist_add(out, r1, t1, idx++);
      toklist_add(out, r2, t2, idx++);
      continue;
    }
    if (strcmp(low, "del") == 0) {
      char *r1 = arena_strdup(a, "de");
      char *t1 = arena_strdup(a, "de");
      char *r2 = arena_strdup(a, "el");
      char *t2 = arena_strdup(a, "el");
      toklist_add(out, r1, t1, idx++);
      toklist_add(out, r2, t2, idx++);
      continue;
    }

    // enclítico (1)
    // preferimos el más largo
    int best = -1;
    for (int k=0;k<NCL;k++) {
      if (ends_with(low, CLITICS[k])) {
        if (best < 0 || (int)strlen(CLITICS[k]) > (int)strlen(CLITICS[best])) best = k;
      }
    }
    if (best >= 0) {
      int cllen = (int)strlen(CLITICS[best]);
      int baselen = (int)strlen(low) - cllen;
      if (baselen > 2) {
        char *base = substr_dup(a, low, 0, baselen);
        if (ends_with(base,"ar") || ends_with(base,"er") || ends_with(base,"ir") ||
            ends_with(base,"ando") || ends_with(base,"iendo")) {
          // raw split: usamos longitudes en bytes (ASCII base/cl)
          // Como clíticos son ASCII, el corte byte-wise es consistente.
          int rawlen = (int)strlen(raw);
          char *raw_base = substr_dup(a, raw, 0, rawlen - cllen);
          char *raw_cl   = substr_dup(a, raw, rawlen - cllen, rawlen);
          char *txt_base = arena_strdup(a, raw_base);
          ascii_lower_inplace(txt_base);
          char *txt_cl   = arena_strdup(a, raw_cl);
          ascii_lower_inplace(txt_cl);
          toklist_add(out, raw_base, txt_base, idx++);
          toklist_add(out, raw_cl, txt_cl, idx++);
          continue;
        }
      }
    }

    toklist_add(out, raw, low, idx++);
  }
}

typedef struct { char **s; int n; int cap; } StrList;
static void strlist_init(StrList *sl) {
  sl->n=0; sl->cap=16;
  sl->s=(char**)malloc(sizeof(char*)*(size_t)sl->cap);
  if(!sl->s) die("OOM strlist");
}
static void strlist_add(StrList *sl, char *x) {
  if (sl->n>=sl->cap){ sl->cap*=2; sl->s=(char**)realloc(sl->s,sizeof(char*)*(size_t)sl->cap); if(!sl->s) die("OOM strlist realloc"); }
  sl->s[sl->n++]=x;
}
static void split_sentences(Arena *a, const char *corpus, StrList *out) {
  strlist_init(out);
  int n=(int)strlen(corpus);
  int start=0;
  for(int i=0;i<=n;i++){
    char c = corpus[i];
    if (c=='.' || c=='\n' || c=='\0') {
      int end=i;
      while (start<end && isspace((unsigned char)corpus[start])) start++;
      while (end>start && isspace((unsigned char)corpus[end-1])) end--;
      if (end>start) {
        char *s = substr_dup(a, corpus, start, end);
        strlist_add(out, s);
      }
      start=i+1;
    }
  }
}

/* ============================================================
 * Árbol
 * ============================================================ */

typedef struct Node {
  int label; // symbol id (POS o categoría); para leaf_token usamos label = sym("TOK")
  int is_leaf_token;
  char *leaf_raw; // si is_leaf_token
  Feats feats;
  double score;
  struct Node *left;
  struct Node *right;
  struct Node *child; // unario: usa child
} Node;

static Node *node_leaf(Arena *a, int tok_label_sym, const char *raw, double score) {
  Node *n = (Node*)arena_alloc(a, sizeof(Node));
  n->label = tok_label_sym;
  n->is_leaf_token = 1;
  n->leaf_raw = arena_strdup(a, raw);
  n->score = score;
  n->feats = feats_empty();
  return n;
}

static Node *node_preterm(Arena *a, int pos_sym, Node *leaf, Feats feats, double score) {
  Node *n = (Node*)arena_alloc(a, sizeof(Node));
  n->label = pos_sym;
  n->is_leaf_token = 0;
  n->child = leaf;
  n->feats = feats;
  n->score = score;
  return n;
}

static Node *node_unary(Arena *a, int lhs_sym, Node *child, Feats feats, double score) {
  Node *n = (Node*)arena_alloc(a, sizeof(Node));
  n->label = lhs_sym;
  n->child = child;
  n->feats = feats;
  n->score = score;
  return n;
}

static Node *node_binary(Arena *a, int lhs_sym, Node *left, Node *right, Feats feats, double score) {
  Node *n = (Node*)arena_alloc(a, sizeof(Node));
  n->label = lhs_sym;
  n->left = left;
  n->right = right;
  n->feats = feats;
  n->score = score;
  return n;
}

static void node_replace_feat_value(Node *n, int from_val, int to_val) {
  if (!n) return;
  for (int i=0;i<n->feats.n;i++) if (n->feats.a[i].val == from_val) n->feats.a[i].val = to_val;
  if (n->child) node_replace_feat_value(n->child, from_val, to_val);
  if (n->left) node_replace_feat_value(n->left, from_val, to_val);
  if (n->right) node_replace_feat_value(n->right, from_val, to_val);
}

static void node_pretty_rec(Symtab *st, Node *n, int indent, FILE *out) {
  if (!n) return;
  for (int i=0;i<indent;i++) fputs("  ", out);

  if (n->is_leaf_token) {
    fprintf(out, "%s\n", n->leaf_raw);
    return;
  }

  fprintf(out, "%s", sym_str(st, n->label));
  if (n->feats.n > 0) {
    fputs(" [", out);
    for (int i=0;i<n->feats.n;i++) {
      if (i) fputs(", ", out);
      fprintf(out, "%s=%s", sym_str(st, n->feats.a[i].key), sym_str(st, n->feats.a[i].val));
    }
    fputs("]", out);
  }
  fprintf(out, "  (score=%.3f)\n", n->score);

  if (n->child) {
    node_pretty_rec(st, n->child, indent+1, out);
  } else {
    node_pretty_rec(st, n->left, indent+1, out);
    node_pretty_rec(st, n->right, indent+1, out);
  }
}

/* ============================================================
 * CKY + beam: chart
 * ============================================================ */

typedef struct {
  int cat;
  Feats feats;
  unsigned long feats_h;
  double score;
  Node *node;
} Item;

typedef struct {
  int cat;
  Item **items;          // pointers into arena items
  unsigned long *hashes; // feats hashes (dedupe)
  int n;
  int cap;
} CatBucket;

typedef struct {
  CatBucket *b;
  int n;
  int cap;
} Cell;

static void cell_init(Cell *c) {
  c->n = 0; c->cap = 8;
  c->b = (CatBucket*)calloc((size_t)c->cap, sizeof(CatBucket));
  if (!c->b) die("OOM cell");
}

static CatBucket *cell_get_bucket(Cell *c, int cat) {
  for (int i=0;i<c->n;i++) if (c->b[i].cat == cat) return &c->b[i];
  return NULL;
}

static CatBucket *cell_ensure_bucket(Cell *c, int cat) {
  CatBucket *bk = cell_get_bucket(c, cat);
  if (bk) return bk;
  if (c->n >= c->cap) {
    c->cap *= 2;
    c->b = (CatBucket*)realloc(c->b, sizeof(CatBucket) * (size_t)c->cap);
    if (!c->b) die("OOM cell realloc");
    memset(c->b + (c->cap/2), 0, sizeof(CatBucket) * (size_t)(c->cap/2));
  }
  bk = &c->b[c->n++];
  bk->cat = cat;
  bk->n = 0; bk->cap = 8;
  bk->items = (Item**)malloc(sizeof(Item*) * (size_t)bk->cap);
  bk->hashes= (unsigned long*)malloc(sizeof(unsigned long) * (size_t)bk->cap);
  if (!bk->items || !bk->hashes) die("OOM catbucket");
  return bk;
}

static int bucket_has_hash(CatBucket *bk, unsigned long h) {
  for (int i=0;i<bk->n;i++) if (bk->hashes[i] == h) return 1;
  return 0;
}

static void bucket_insert_sorted(CatBucket *bk, Item *it, int beam, int *pruned) {
  // insert descending by score
  if (bk->n >= bk->cap) {
    bk->cap *= 2;
    bk->items = (Item**)realloc(bk->items, sizeof(Item*) * (size_t)bk->cap);
    bk->hashes= (unsigned long*)realloc(bk->hashes,sizeof(unsigned long)*(size_t)bk->cap);
    if (!bk->items || !bk->hashes) die("OOM bucket realloc");
  }
  int pos = bk->n;
  bk->items[bk->n] = it;
  bk->hashes[bk->n] = it->feats_h;
  bk->n++;

  while (pos > 0 && bk->items[pos-1]->score < bk->items[pos]->score) {
    Item *tmp = bk->items[pos-1]; bk->items[pos-1] = bk->items[pos]; bk->items[pos] = tmp;
    unsigned long th = bk->hashes[pos-1]; bk->hashes[pos-1] = bk->hashes[pos]; bk->hashes[pos] = th;
    pos--;
  }

  if (bk->n > beam) {
    int removed = bk->n - beam;
    bk->n = beam;
    if (pruned) *pruned += removed;
  }
}

static void cell_add_item(Cell *c, Item *it, int beam, int *pruned) {
  CatBucket *bk = cell_ensure_bucket(c, it->cat);
  if (bucket_has_hash(bk, it->feats_h)) return;
  bucket_insert_sorted(bk, it, beam, pruned);
}

/* ============================================================
 * Operaciones de reglas (op DSL)
 * ============================================================ */

static int apply_op(Arena *a, Symtab *st, const Rule *r, const Feats *L, const Feats *R, Feats *out) {
  switch (r->op) {
    case OP_EMPTY: *out = feats_empty(); return 1;
    case OP_LEFT:  *out = feats_copy(a, L); return 1;
    case OP_RIGHT: *out = feats_copy(a, R); return 1;
    case OP_UNIFY: return feats_unify(a, st, L, R, out);

    case OP_REQUIRE_LEFT:
      if (r->arg_key < 0 || r->arg_val < 0) return 0;
      return feats_require(a, st, L, r->arg_key, r->arg_val, out);

    case OP_REQUIRE_RIGHT:
      if (r->arg_key < 0 || r->arg_val < 0) return 0;
      return feats_require(a, st, R, r->arg_key, r->arg_val, out);

    case OP_MAKE_GAP: {
      // idx=?i, gap=type
      int k_idx = sym_intern(st, "idx");
      int v_qi  = sym_intern(st, "?i");
      int k_gap = sym_intern(st, "gap");
      int v_type= r->arg_type;
      if (v_type < 0) return 0;

      Feats f;
      f.n = 2;
      f.a = (Feat*)arena_alloc(a, sizeof(Feat) * 2);
      f.a[0] = (Feat){k_idx, v_qi};
      f.a[1] = (Feat){k_gap, v_type};
      feats_sort(&f);
      *out = f;
      return 1;
    }

    case OP_RELCLAUSE_OBL: {
      // require right obl=yes, output copy(left) + gap=obl
      int k_obl = sym_intern(st, "obl");
      int v_yes = sym_intern(st, "yes");
      Feats tmp;
      if (!feats_require(a, st, R, k_obl, v_yes, &tmp)) return 0;

      Feats base = feats_copy(a, L);
      int k_gap = sym_intern(st, "gap");
      int v_obl = sym_intern(st, "obl");
      Feats req;
      req.n = 1;
      req.a = (Feat*)arena_alloc(a, sizeof(Feat));
      req.a[0] = (Feat){k_gap, v_obl};
      feats_sort(&req);
      return feats_unify(a, st, &base, &req, out);
    }
  }
  return 0;
}

/* ============================================================
 * Guess OOV (mismo espíritu)
 * ============================================================ */

typedef struct { LexEntry *a; int n; int cap; } LexVec;
static void lexvec_init(LexVec *v){ v->n=0; v->cap=8; v->a=(LexEntry*)malloc(sizeof(LexEntry)*(size_t)v->cap); if(!v->a) die("OOM lexvec"); }
static void lexvec_push(LexVec *v, LexEntry e){ if(v->n>=v->cap){ v->cap*=2; v->a=(LexEntry*)realloc(v->a,sizeof(LexEntry)*(size_t)v->cap); if(!v->a) die("OOM lexvec realloc"); } v->a[v->n++]=e; }

static int is_det_word(const char *w) {
  return (strcmp(w,"el")==0 || strcmp(w,"la")==0 || strcmp(w,"los")==0 || strcmp(w,"las")==0);
}

static void guess_lex(Arena *a, Symtab *st, const Token *tk, LexVec *out) {
  lexvec_init(out);
  const char *w = tk->text;

  // PropN por capitalización ASCII
  if (tk->raw[0] && (tk->raw[0] >= 'A' && tk->raw[0] <= 'Z') && !is_det_word(w)) {
    LexEntry e;
    e.pos = sym_intern(st, "PropN");
    e.weight = 0.03;
    e.feats.n = 1;
    e.feats.a = (Feat*)malloc(sizeof(Feat));
    e.feats.a[0] = (Feat){sym_intern(st,"num"), sym_intern(st,"sg")};
    feats_sort(&e.feats);
    lexvec_push(out, e);
  }
  if (ends_with(w, "mente")) {
    LexEntry e;
    e.pos = sym_intern(st, "Adv");
    e.weight = -0.03;
    e.feats = feats_empty();
    lexvec_push(out, e);
  }

  // no finitos
  if (ends_with(w,"ar") || ends_with(w,"er") || ends_with(w,"ir")) {
    LexEntry vi = { sym_intern(st,"Vi"), -0.12, feats_empty() };
    vi.feats.n=2; vi.feats.a=(Feat*)malloc(sizeof(Feat)*2);
    vi.feats.a[0]=(Feat){sym_intern(st,"fin"), sym_intern(st,"no")};
    vi.feats.a[1]=(Feat){sym_intern(st,"obl"), sym_intern(st,"no")};
    feats_sort(&vi.feats);
    lexvec_push(out, vi);

    LexEntry vt = { sym_intern(st,"Vt"), -0.14, feats_empty() };
    vt.feats.n=2; vt.feats.a=(Feat*)malloc(sizeof(Feat)*2);
    vt.feats.a[0]=(Feat){sym_intern(st,"fin"), sym_intern(st,"no")};
    vt.feats.a[1]=(Feat){sym_intern(st,"obl"), sym_intern(st,"no")};
    feats_sort(&vt.feats);
    lexvec_push(out, vt);
  }
  if (ends_with(w,"ando") || ends_with(w,"iendo")) {
    LexEntry vi = { sym_intern(st,"Vi"), -0.14, feats_empty() };
    vi.feats.n=2; vi.feats.a=(Feat*)malloc(sizeof(Feat)*2);
    vi.feats.a[0]=(Feat){sym_intern(st,"fin"), sym_intern(st,"no")};
    vi.feats.a[1]=(Feat){sym_intern(st,"obl"), sym_intern(st,"no")};
    feats_sort(&vi.feats);
    lexvec_push(out, vi);

    LexEntry vt = { sym_intern(st,"Vt"), -0.16, feats_empty() };
    vt.feats.n=2; vt.feats.a=(Feat*)malloc(sizeof(Feat)*2);
    vt.feats.a[0]=(Feat){sym_intern(st,"fin"), sym_intern(st,"no")};
    vt.feats.a[1]=(Feat){sym_intern(st,"obl"), sym_intern(st,"no")};
    feats_sort(&vt.feats);
    lexvec_push(out, vt);
  }

  if (out->n == 0) {
    LexEntry n;
    n.pos = sym_intern(st, "N");
    n.weight = -0.35;
    n.feats.n = 2;
    n.feats.a = (Feat*)malloc(sizeof(Feat)*2);
    n.feats.a[0] = (Feat){sym_intern(st,"gen"), sym_intern(st,"?g")};
    n.feats.a[1] = (Feat){sym_intern(st,"num"), sym_intern(st,"?n")};
    feats_sort(&n.feats);
    lexvec_push(out, n);
  }
}

/* ============================================================
 * Unary closure
 * ============================================================ */

static void unary_closure(Arena *a, Symtab *st, Grammar *gr, Cell *cell, int beam, int *pruned, int *unary_apps) {
  int changed = 1;
  while (changed) {
    changed = 0;
    for (int ri=0; ri<gr->n; ri++) {
      Rule *r = &gr->r[ri];
      if (r->rhs_len != 1) continue;
      CatBucket *bk = cell_get_bucket(cell, r->rhs1);
      if (!bk) continue;

      int before = cell_get_bucket(cell, r->lhs) ? cell_get_bucket(cell, r->lhs)->n : 0;

      for (int i=0;i<bk->n;i++) {
        Item *ch = bk->items[i];
        Feats pf;
        Feats empty = feats_empty();
        if (!apply_op(a, st, r, &ch->feats, &empty, &pf)) continue;
        (*unary_apps)++;

        Item *it = (Item*)arena_alloc(a, sizeof(Item));
        it->cat = r->lhs;
        it->feats = pf;
        feats_sort(&it->feats);
        it->feats_h = feats_hash(&it->feats);
        it->score = ch->score + r->weight;
        it->node = node_unary(a, r->lhs, ch->node, it->feats, it->score);
        cell_add_item(cell, it, beam, pruned);
      }

      int after = cell_get_bucket(cell, r->lhs) ? cell_get_bucket(cell, r->lhs)->n : 0;
      if (after != before) changed = 1;
    }
  }
}

/* ============================================================
 * Parse + stats
 * ============================================================ */

typedef struct {
  char *sentence;
  int tokens;
  int oov;
  int parsed;
  int n_parses;
  double best_score;
  double time_ms;

  int chart_items_total;
  int chart_items_max_cell;
  int pruned;
  int unary_apps;
  int ambiguous_cells;

  int sanity_s_has_vpfin;
  int sanity_sin_vpnf;
  int sanity_enclitic_only_nf;

  char **notes;
  int n_notes;
  char *best_tree; // pretty string (malloc)
} ParseStats;

static void notes_add(ParseStats *st, const char *s) {
  st->notes = (char**)realloc(st->notes, sizeof(char*)*(size_t)(st->n_notes+1));
  st->notes[st->n_notes++] = xstrdup(s);
}

static void walk_sanity(Node *n, int sym_S, int sym_VP_FIN, int *found) {
  if (!n) return;
  if (!n->is_leaf_token && n->label == sym_S) {
    // check children include VP_FIN
    Node *c1 = n->child;
    if (!c1) {
      if (n->left && n->left->label == sym_VP_FIN) *found = 1;
      if (n->right && n->right->label == sym_VP_FIN) *found = 1;
    } else {
      // unary S->VP_FIN
      if (c1->label == sym_VP_FIN) *found = 1;
    }
  }
  if (n->child) walk_sanity(n->child, sym_S, sym_VP_FIN, found);
  if (n->left)  walk_sanity(n->left, sym_S, sym_VP_FIN, found);
  if (n->right) walk_sanity(n->right, sym_S, sym_VP_FIN, found);
}

static int has_desc_label(Node *n, int label) {
  if (!n) return 0;
  if (!n->is_leaf_token && n->label == label) return 1;
  if (n->child && has_desc_label(n->child, label)) return 1;
  if (n->left  && has_desc_label(n->left, label)) return 1;
  if (n->right && has_desc_label(n->right,label)) return 1;
  return 0;
}

static int sanity_s_has_vpfin(Symtab *st, Node *tree) {
  int sym_S = sym_intern(st, "S");
  int sym_VP_FIN = sym_intern(st, "VP_FIN");
  int found = 0;
  walk_sanity(tree, sym_S, sym_VP_FIN, &found);
  return found;
}

static int sanity_sin_takes_vpnf(Symtab *st, Node *tree) {
  int sym_Pinf = sym_intern(st, "Pinf");
  int sym_VP_NF= sym_intern(st, "VP_NF");
  // if any Pinf lacks VP_NF descendant => fail
  // brute walk: find Pinf nodes and check descendant
  // (simple recursion)
  if (!tree) return 1;
  if (!tree->is_leaf_token && tree->label == sym_Pinf) {
    if (!has_desc_label(tree, sym_VP_NF)) return 0;
  }
  if (tree->child && !sanity_sin_takes_vpnf(st, tree->child)) return 0;
  if (tree->left  && !sanity_sin_takes_vpnf(st, tree->left)) return 0;
  if (tree->right && !sanity_sin_takes_vpnf(st, tree->right)) return 0;
  return 1;
}

static int sanity_enclitic_only_nf(Symtab *st, Node *tree) {
  int sym_VP = sym_intern(st, "VP");
  int sym_Cl = sym_intern(st, "Cl");
  int sym_Vt = sym_intern(st, "Vt");
  int sym_Vi = sym_intern(st, "Vi");
  // fail if VP -> (Vt|Vi) + Cl
  if (!tree) return 1;
  if (!tree->is_leaf_token && tree->label == sym_VP && tree->left && tree->right) {
    if (!tree->right->is_leaf_token && tree->right->label == sym_Cl) {
      if (!tree->left->is_leaf_token && (tree->left->label == sym_Vt || tree->left->label == sym_Vi)) return 0;
    }
  }
  if (tree->child && !sanity_enclitic_only_nf(st, tree->child)) return 0;
  if (tree->left  && !sanity_enclitic_only_nf(st, tree->left)) return 0;
  if (tree->right && !sanity_enclitic_only_nf(st, tree->right)) return 0;
  return 1;
}

static char *node_pretty_to_string(Symtab *st, Node *tree) {
  // write to mem via tmp file stream-like using open_memstream (GNU); fallback simple fixed buffer.
#if defined(__GNUC__)
  char *buf = NULL;
  size_t sz = 0;
  FILE *m = open_memstream(&buf, &sz);
  if (!m) return NULL;
  node_pretty_rec(st, tree, 0, m);
  fclose(m);
  return buf;
#else
  // fallback: fixed big buffer
  size_t cap = 1<<20;
  char *buf = (char*)malloc(cap);
  if (!buf) return NULL;
  FILE *m = fmemopen(buf, cap, "w");
  if (!m) { free(buf); return NULL; }
  node_pretty_rec(st, tree, 0, m);
  fclose(m);
  // ensure null-terminated
  buf[cap-1] = '\0';
  return buf;
#endif
}

static void free_lexvec(LexVec *v) {
  for (int i=0;i<v->n;i++) free(v->a[i].feats.a);
  free(v->a);
}

static void parse_sentence(Symtab *st, Lexicon *lx, Grammar *gr,
                           const char *sentence, int topk, int beam,
                           int include_tree, ParseStats *out) {
  memset(out, 0, sizeof(*out));
  out->sentence = xstrdup(sentence);

  double t0 = now_ms();

  Arena a; arena_init(&a, 1<<22); // arena por oración

  TokList toks;
  tokenize(&a, sentence, &toks);
  int n = toks.n;
  out->tokens = n;
  if (n == 0) {
    out->parsed = 0;
    notes_add(out, "empty");
    out->time_ms = now_ms() - t0;
    return;
  }

  // chart n x (n+1)
  Cell **chart = (Cell**)malloc(sizeof(Cell*) * (size_t)n);
  if (!chart) die("OOM chart");
  for (int i=0;i<n;i++) {
    chart[i] = (Cell*)malloc(sizeof(Cell) * (size_t)(n+1));
    if (!chart[i]) die("OOM chart row");
    for (int j=0;j<=n;j++) cell_init(&chart[i][j]);
  }

  int pruned = 0;
  int unary_apps = 0;

  int sym_TOK = sym_intern(st, "TOK");
  int sym_N = sym_intern(st, "N");
  int sym_PropN = sym_intern(st, "PropN");
  int sym_Pron = sym_intern(st, "Pron"); // por compatibilidad (no lo usamos)
  int k_idx = sym_intern(st, "idx");

  // lex init
  for (int i=0;i<n;i++) {
    Token *tk = &toks.t[i];
    Cell *cell = &chart[i][i+1];

    LexBucket *bk = lexicon_get_bucket(lx, tk->text);
    if (!bk) out->oov++;

    // entries from lexicon
    if (bk) {
      for (int e=0;e<bk->n;e++) {
        LexEntry le = bk->entries[e];

        // copy feats to arena
        Feats lf;
        lf.n = le.feats.n;
        lf.a = (Feat*)arena_alloc(&a, sizeof(Feat) * (size_t)lf.n);
        memcpy(lf.a, le.feats.a, sizeof(Feat) * (size_t)lf.n);
        feats_sort(&lf);

        // if N/PropN/Pron -> idx default
        if (le.pos == sym_N || le.pos == sym_PropN || le.pos == sym_Pron) {
          if (feat_find(&lf, k_idx) < 0) {
            char tmp[32]; snprintf(tmp, sizeof(tmp), "t%d", tk->index);
            int v = sym_intern(st, tmp);
            Feats req;
            req.n=1; req.a=(Feat*)arena_alloc(&a, sizeof(Feat));
            req.a[0] = (Feat){k_idx, v};
            feats_sort(&req);
            Feats merged;
            if (feats_unify(&a, st, &lf, &req, &merged)) lf = merged;
          }
        }

        Node *leaf = node_leaf(&a, sym_TOK, tk->raw, le.weight);
        Node *pre  = node_preterm(&a, le.pos, leaf, lf, le.weight);

        Item *it = (Item*)arena_alloc(&a, sizeof(Item));
        it->cat = le.pos;
        it->feats = lf;
        feats_sort(&it->feats);
        it->feats_h = feats_hash(&it->feats);
        it->score = le.weight;
        it->node = pre;

        cell_add_item(cell, it, beam, &pruned);
      }
    }

    // guessed
    LexVec gv;
    guess_lex(&a, st, tk, &gv);
    for (int e=0;e<gv.n;e++) {
      LexEntry le = gv.a[e];

      Feats lf;
      lf.n = le.feats.n;
      lf.a = (Feat*)arena_alloc(&a, sizeof(Feat) * (size_t)lf.n);
      memcpy(lf.a, le.feats.a, sizeof(Feat) * (size_t)lf.n);
      feats_sort(&lf);

      if (le.pos == sym_N || le.pos == sym_PropN || le.pos == sym_Pron) {
        if (feat_find(&lf, k_idx) < 0) {
          char tmp[32]; snprintf(tmp, sizeof(tmp), "t%d", tk->index);
          int v = sym_intern(st, tmp);
          Feats req;
          req.n=1; req.a=(Feat*)arena_alloc(&a, sizeof(Feat));
          req.a[0] = (Feat){k_idx, v};
          feats_sort(&req);
          Feats merged;
          if (feats_unify(&a, st, &lf, &req, &merged)) lf = merged;
        }
      }

      Node *leaf = node_leaf(&a, sym_TOK, tk->raw, le.weight);
      Node *pre  = node_preterm(&a, le.pos, leaf, lf, le.weight);

      Item *it = (Item*)arena_alloc(&a, sizeof(Item));
      it->cat = le.pos;
      it->feats = lf;
      feats_sort(&it->feats);
      it->feats_h = feats_hash(&it->feats);
      it->score = le.weight;
      it->node = pre;

      cell_add_item(cell, it, beam, &pruned);
    }
    free_lexvec(&gv);

    unary_closure(&a, st, gr, cell, beam, &pruned, &unary_apps);
  }

  // CKY spans
  for (int span=2; span<=n; span++) {
    for (int i=0; i<=n-span; i++) {
      int j = i + span;
      Cell *cell = &chart[i][j];

      for (int k=i+1; k<j; k++) {
        Cell *L = &chart[i][k];
        Cell *R = &chart[k][j];
        if (L->n == 0 || R->n == 0) continue;

        for (int ri=0; ri<gr->n; ri++) {
          Rule *rule = &gr->r[ri];
          if (rule->rhs_len != 2) continue;

          CatBucket *lb = cell_get_bucket(L, rule->rhs1);
          if (!lb) continue;
          CatBucket *rb = cell_get_bucket(R, rule->rhs2);
          if (!rb) continue;

          for (int a_i=0; a_i<lb->n; a_i++) {
            Item *ib = lb->items[a_i];
            for (int c_i=0; c_i<rb->n; c_i++) {
              Item *ic = rb->items[c_i];

              Feats pf;
              if (!apply_op(&a, st, rule, &ib->feats, &ic->feats, &pf)) continue;

              double score = ib->score + ic->score + rule->weight;

              Node *right_node = ic->node;
              if (rule->post_flags & POST_PROPAGATE_IDX_TO_RIGHT) {
                int idx_pos = feat_find(&ib->feats, k_idx);
                if (idx_pos >= 0) {
                  int idx_val = ib->feats.a[idx_pos].val;
                  int from = sym_intern(st, "?i");
                  node_replace_feat_value(right_node, from, idx_val);
                }
              }

              Item *it = (Item*)arena_alloc(&a, sizeof(Item));
              it->cat = rule->lhs;
              it->feats = pf;
              feats_sort(&it->feats);
              it->feats_h = feats_hash(&it->feats);
              it->score = score;
              it->node = node_binary(&a, rule->lhs, ib->node, right_node, it->feats, it->score);

              cell_add_item(cell, it, beam, &pruned);
            }
          }
        }
      }

      unary_closure(&a, st, gr, cell, beam, &pruned, &unary_apps);
    }
  }

  // final S
  int sym_S = sym_intern(st, "S");
  Cell *final = &chart[0][n];
  CatBucket *sb = cell_get_bucket(final, sym_S);

  out->pruned = pruned;
  out->unary_apps = unary_apps;

  // chart metrics
  int total_items=0, max_cell=0, amb_cells=0;
  for (int i=0;i<n;i++) {
    for (int j=i+1;j<=n;j++) {
      Cell *c = &chart[i][j];
      if (c->n >= 2) amb_cells++;
      int cell_items=0;
      for (int b=0;b<c->n;b++) cell_items += c->b[b].n;
      total_items += cell_items;
      if (cell_items > max_cell) max_cell = cell_items;
    }
  }
  out->chart_items_total = total_items;
  out->chart_items_max_cell = max_cell;
  out->ambiguous_cells = amb_cells;

  if (!sb || sb->n == 0) {
    out->parsed = 0;
    notes_add(out, "NO_PARSE");
  } else {
    out->parsed = 1;
    out->n_parses = sb->n < topk ? sb->n : topk;
    out->best_score = sb->items[0]->score;

    Node *best = sb->items[0]->node;
    out->sanity_s_has_vpfin = sanity_s_has_vpfin(st, best);
    out->sanity_sin_vpnf = sanity_sin_takes_vpnf(st, best);
    out->sanity_enclitic_only_nf = sanity_enclitic_only_nf(st, best);

    if (!out->sanity_s_has_vpfin) notes_add(out, "WARN: S sin VP_FIN visible");
    if (!out->sanity_sin_vpnf) notes_add(out, "WARN: 'sin' sin VP_NF bajo Pinf");
    if (!out->sanity_enclitic_only_nf) notes_add(out, "WARN: enclítico con verbo finito");

    if (include_tree) out->best_tree = node_pretty_to_string(st, best);
  }

  out->time_ms = now_ms() - t0;

  // cleanup chart (cells heap)
  for (int i=0;i<n;i++) {
    for (int j=0;j<=n;j++) {
      // free buckets arrays
      Cell *c = &chart[i][j];
      for (int b=0;b<c->n;b++) {
        free(c->b[b].items);
        free(c->b[b].hashes);
      }
      free(c->b);
    }
    free(chart[i]);
  }
  free(chart);

  // toks + arena: se liberan al final de función
  free(toks.t);
  free(a.buf);
}

/* ============================================================
 * Export JSON (simple, con escape mínimo)
 * ============================================================ */

static void json_escape(FILE *f, const char *s) {
  fputc('"', f);
  for (; *s; s++) {
    unsigned char c = (unsigned char)*s;
    if (c == '\"' || c == '\\') { fputc('\\', f); fputc(c, f); }
    else if (c == '\n') fputs("\\n", f);
    else if (c == '\r') fputs("\\r", f);
    else if (c == '\t') fputs("\\t", f);
    else fputc((int)c, f);
  }
  fputc('"', f);
}

typedef struct {
  ParseStats *rows;
  int n;
  int cap;
  int beam;
  int topk;
  double coverage;
  double avg_tokens;
  double avg_oov;
  double total_time_ms;
  double avg_time_ms;
} EvalSummary;

static void summary_init(EvalSummary *s) {
  s->n=0; s->cap=16;
  s->rows=(ParseStats*)malloc(sizeof(ParseStats)*(size_t)s->cap);
  if(!s->rows) die("OOM summary");
}
static void summary_add(EvalSummary *s, ParseStats st) {
  if(s->n>=s->cap){ s->cap*=2; s->rows=(ParseStats*)realloc(s->rows,sizeof(ParseStats)*(size_t)s->cap); if(!s->rows) die("OOM summary realloc"); }
  s->rows[s->n++]=st;
}

static void write_summary_json(const char *path, EvalSummary *sum) {
  FILE *f = fopen(path, "wb");
  if (!f) die("No puedo escribir %s", path);

  fprintf(f, "{\n");
  fprintf(f, "  \"sentences\": %d,\n", sum->n);
  fprintf(f, "  \"coverage\": %.6f,\n", sum->coverage);
  fprintf(f, "  \"avgTokens\": %.6f,\n", sum->avg_tokens);
  fprintf(f, "  \"avgOov\": %.6f,\n", sum->avg_oov);
  fprintf(f, "  \"totalTimeMs\": %.6f,\n", sum->total_time_ms);
  fprintf(f, "  \"avgTimeMs\": %.6f,\n", sum->avg_time_ms);
  fprintf(f, "  \"beam\": %d,\n", sum->beam);
  fprintf(f, "  \"topK\": %d,\n", sum->topk);

  fprintf(f, "  \"rows\": [\n");
  for (int i=0;i<sum->n;i++) {
    ParseStats *st = &sum->rows[i];
    fprintf(f, "    {\n");
    fprintf(f, "      \"sentence\": "); json_escape(f, st->sentence); fprintf(f, ",\n");
    fprintf(f, "      \"tokens\": %d,\n", st->tokens);
    fprintf(f, "      \"oovTokens\": %d,\n", st->oov);
    fprintf(f, "      \"parsed\": %s,\n", st->parsed ? "true" : "false");
    fprintf(f, "      \"nParsesReturned\": %d,\n", st->n_parses);
    if (st->parsed) fprintf(f, "      \"bestScore\": %.6f,\n", st->best_score);
    else fprintf(f, "      \"bestScore\": null,\n");
    fprintf(f, "      \"timeMs\": %.6f,\n", st->time_ms);
    fprintf(f, "      \"chartItemsTotal\": %d,\n", st->chart_items_total);
    fprintf(f, "      \"chartItemsMaxCell\": %d,\n", st->chart_items_max_cell);
    fprintf(f, "      \"prunedByBeam\": %d,\n", st->pruned);
    fprintf(f, "      \"unaryApplications\": %d,\n", st->unary_apps);
    fprintf(f, "      \"ambiguousCells\": %d,\n", st->ambiguous_cells);

    fprintf(f, "      \"sanitySHasVpFin\": %s,\n", st->sanity_s_has_vpfin ? "true":"false");
    fprintf(f, "      \"sanitySinTakesVpNf\": %s,\n", st->sanity_sin_vpnf ? "true":"false");
    fprintf(f, "      \"sanityEncliticOnlyNf\": %s,\n", st->sanity_enclitic_only_nf ? "true":"false");

    fprintf(f, "      \"notes\": [");
    for (int k=0;k<st->n_notes;k++) {
      if (k) fprintf(f, ", ");
      json_escape(f, st->notes[k]);
    }
    fprintf(f, "],\n");

    fprintf(f, "      \"bestTree\": ");
    if (st->best_tree) json_escape(f, st->best_tree);
    else fprintf(f, "null");
    fprintf(f, "\n");

    fprintf(f, "    }%s\n", (i==sum->n-1) ? "" : ",");
  }
  fprintf(f, "  ]\n");
  fprintf(f, "}\n");

  fclose(f);
}

/* ============================================================
 * CLI + evaluación
 * ============================================================ */

static void usage(void) {
  puts(
    "Uso:\n"
    "  ./parser_v7 [--lex lexicon.json] [--grammar grammar.json]\n"
    "             [--file corpus.txt | --text \"...\"]\n"
    "             [--beam 16] [--topk 1] [--trees] [--print] [--json out.json]\n"
    "\n"
    "Ejemplos:\n"
    "  ./parser_v7 --file corpus.txt --print --json out.json\n"
    "  ./parser_v7 --text \"Los científicos lo estudiaron durante décadas sin comprenderlo.\" --trees --print\n"
  );
}

static const char *argval(int *i, int argc, char **argv) {
  if (*i + 1 >= argc) die("Falta valor para %s", argv[*i]);
  (*i)++;
  return argv[*i];
}

int main(int argc, char **argv) {
  const char *lex_path = "lexicon.json";
  const char *grammar_path = "grammar.json";
  const char *file_path = "corpus.txt";
  const char *text = NULL;
  const char *json_out = NULL;
  int beam = 16;
  int topk = 1;
  int trees = 0;
  int print = 0;

  for (int i=1;i<argc;i++) {
    if (strcmp(argv[i], "--help")==0 || strcmp(argv[i], "-h")==0) { usage(); return 0; }
    else if (strcmp(argv[i], "--lex")==0) lex_path = argval(&i, argc, argv);
    else if (strcmp(argv[i], "--grammar")==0) grammar_path = argval(&i, argc, argv);
    else if (strcmp(argv[i], "--file")==0) file_path = argval(&i, argc, argv);
    else if (strcmp(argv[i], "--text")==0) text = argval(&i, argc, argv);
    else if (strcmp(argv[i], "--json")==0) json_out = argval(&i, argc, argv);
    else if (strcmp(argv[i], "--beam")==0) beam = atoi(argval(&i, argc, argv));
    else if (strcmp(argv[i], "--topk")==0) topk = atoi(argval(&i, argc, argv));
    else if (strcmp(argv[i], "--trees")==0) trees = 1;
    else if (strcmp(argv[i], "--print")==0) print = 1;
    else die("Arg desconocido: %s", argv[i]);
  }

  Symtab st; symtab_init(&st);
  Lexicon lx; lexicon_init(&lx);
  Grammar gr; grammar_init(&gr);

  load_lexicon_from_json(&st, &lx, lex_path);
  load_grammar_from_json(&st, &gr, grammar_path);

  char *corpus = NULL;
  size_t corpus_len = 0;
  if (text) {
    corpus = xstrdup(text);
  } else {
    corpus = read_file(file_path, &corpus_len);
  }

  Arena sa; arena_init(&sa, 1<<20);
  StrList sents;
  split_sentences(&sa, corpus, &sents);

  EvalSummary sum;
  summary_init(&sum);
  sum.beam = beam;
  sum.topk = topk;

  int parsed = 0;
  int total_tokens = 0;
  int total_oov = 0;
  double total_time = 0.0;

  for (int i=0;i<sents.n;i++) {
    ParseStats strow;
    parse_sentence(&st, &lx, &gr, sents.s[i], topk, beam, trees, &strow);

    if (print) {
      puts("==============================================================================");
      puts(strow.sentence);
      printf("tokens=%d  oov=%d  parsed=%d  parses=%d  score=%s  time_ms=%.1f\n",
             strow.tokens, strow.oov, strow.parsed, strow.n_parses,
             strow.parsed ? "yes" : "null", strow.time_ms);
      printf("chart_items=%d  max_cell=%d  pruned=%d  unary_apps=%d  amb_cells=%d\n",
             strow.chart_items_total, strow.chart_items_max_cell,
             strow.pruned, strow.unary_apps, strow.ambiguous_cells);
      if (strow.n_notes) {
        fputs("notes: ", stdout);
        for (int k=0;k<strow.n_notes;k++) {
          if (k) fputs("; ", stdout);
          fputs(strow.notes[k], stdout);
        }
        fputc('\n', stdout);
      }
      if (trees && strow.best_tree) {
        puts(strow.best_tree);
      }
    }

    parsed += strow.parsed ? 1 : 0;
    total_tokens += strow.tokens;
    total_oov += strow.oov;
    total_time += strow.time_ms;

    summary_add(&sum, strow);
  }

  sum.coverage = (sents.n == 0) ? 0.0 : (double)parsed / (double)sents.n;
  sum.avg_tokens = (sents.n == 0) ? 0.0 : (double)total_tokens / (double)sents.n;
  sum.avg_oov = (sents.n == 0) ? 0.0 : (double)total_oov / (double)sents.n;
  sum.total_time_ms = total_time;
  sum.avg_time_ms = (sents.n == 0) ? 0.0 : total_time / (double)sents.n;

  if (!print) {
    printf("SUMMARY: sentences=%d coverage=%.3f avg_time_ms=%.1f beam=%d top_k=%d\n",
           sents.n, sum.coverage, sum.avg_time_ms, beam, topk);
  } else {
    puts("==============================================================================");
    printf("SUMMARY\nsentences=%d  coverage=%.3f  avg_tokens=%.2f  avg_oov=%.2f  avg_time_ms=%.1f  beam=%d  top_k=%d\n",
           sents.n, sum.coverage, sum.avg_tokens, sum.avg_oov, sum.avg_time_ms, beam, topk);
  }

  if (json_out) {
    write_summary_json(json_out, &sum);
    printf("Wrote JSON: %s\n", json_out);
  }

  // cleanup minimal (mucho queda para SO; pero liberamos lo más grande)
  free(corpus);
  free(sa.buf);
  free(sents.s);

  // notas/strings de rows
  for (int i=0;i<sum.n;i++) {
    ParseStats *r = &sum.rows[i];
    free(r->sentence);
    for (int k=0;k<r->n_notes;k++) free(r->notes[k]);
    free(r->notes);
    free(r->best_tree);
  }
  free(sum.rows);

  return 0;
}
