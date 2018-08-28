/* A Lisp version 0 interpreter
   Copyright (c) 2010 James Craig Burley <james@jcb-sc.com>

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License , or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, write to the Free Software
   Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/* Cloned and wildly modified from
   http://www.sonoma.edu/users/l/luvisi/sl3.c by Andru Luvisi.

   Zero version implements "first" version of Lisp via Lisp code on
   top of builtin functions quote, atom, eq, cons, car, cdr, and cond,
   along with builtin recursive evaluations of the arguments to those
   functions where appropriate.

   This version incorporate the map.c source file, to comprise a
   single source file, easing use of c2go to convert it to Go and
   build it from there.
*/

#define __USE_XOPEN2K8 1
#include <assert.h>
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>

/** 
 * Copyright (c) 2014 rxi
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the MIT license. See LICENSE for details.
 */

#define MAP_VERSION "0.1.0"

struct map_node_s;
typedef struct map_node_s map_node_t;

typedef struct map_base_s {
  map_node_t **buckets;
  unsigned nbuckets, nnodes;
} map_base_t;

typedef struct map_iter_s {
  unsigned bucketidx;
  map_node_t *node;
} map_iter_t;


#define map_t(T)\
  struct { map_base_t base; T *ref; T tmp; }


#define map_init(m)\
  memset(m, 0, sizeof(*(m)))


#define map_deinit(m)\
  map_deinit_(&(m)->base)


#define map_get(m, key)\
  ( (m)->ref = map_get_(&(m)->base, key) )


#define map_set(m, key, value)\
  ( (m)->tmp = (value),\
    map_set_(&(m)->base, key, &(m)->tmp, sizeof((m)->tmp)) )


#define map_remove(m, key)\
  map_remove_(&(m)->base, key)


#define map_iter(m)\
  map_iter_()


#define map_next(m, iter)\
  map_next_(&(m)->base, iter)


void map_deinit_(map_base_t *m);
void *map_get_(map_base_t *m, const char *key);
int map_set_(map_base_t *m, const char *key, void *value, int vsize);
void map_remove_(map_base_t *m, const char *key);
map_iter_t map_iter_(void);
const char *map_next_(map_base_t *m, map_iter_t *iter);


typedef map_t(void*) map_void_t;
typedef map_t(char*) map_str_t;
typedef map_t(int) map_int_t;
typedef map_t(char) map_char_t;
typedef map_t(float) map_float_t;
typedef map_t(double) map_double_t;

struct map_node_s {
  unsigned hash;
  void *value;
  map_node_t *next;
  /* char key[]; */
  /* char value[]; */
};


static unsigned map_hash(const char *str) {
  unsigned hash = 5381;
  while (*str) {
    hash = ((hash << 5) + hash) ^ *str++;
  }
  return hash;
}


static map_node_t *map_newnode(const char *key, void *value, int vsize) {
  map_node_t *node;
  int ksize = strlen(key) + 1;
  int voffset = ksize + ((sizeof(void*) - ksize) % sizeof(void*));
  node = malloc(sizeof(*node) + voffset + vsize);
  if (!node) return NULL;
  memcpy(node + 1, key, ksize);
  node->hash = map_hash(key);
  node->value = ((char*) (node + 1)) + voffset;
  memcpy(node->value, value, vsize);
  return node;
}


static int map_bucketidx(map_base_t *m, unsigned hash) {
  /* If the implementation is changed to allow a non-power-of-2 bucket count,
   * the line below should be changed to use mod instead of AND */
  return hash & (m->nbuckets - 1);
}


static void map_addnode(map_base_t *m, map_node_t *node) {
  int n = map_bucketidx(m, node->hash);
  node->next = m->buckets[n];
  m->buckets[n] = node;
}


static int map_resize(map_base_t *m, int nbuckets) {
  map_node_t *nodes, *node, *next;
  map_node_t **buckets;
  int i; 
  /* Chain all nodes together */
  nodes = NULL;
  i = m->nbuckets;
  while (i--) {
    node = (m->buckets)[i];
    while (node) {
      next = node->next;
      node->next = nodes;
      nodes = node;
      node = next;
    }
  }
  /* Reset buckets */
  buckets = realloc(m->buckets, sizeof(*m->buckets) * nbuckets);
  if (buckets != NULL) {
    m->buckets = buckets;
    m->nbuckets = nbuckets;
  }
  if (m->buckets) {
    memset(m->buckets, 0, sizeof(*m->buckets) * m->nbuckets);
    /* Re-add nodes to buckets */
    node = nodes;
    while (node) {
      next = node->next;
      map_addnode(m, node);
      node = next;
    }
  }
  /* Return error code if realloc() failed */
  return (buckets == NULL) ? -1 : 0;
}


static map_node_t **map_getref(map_base_t *m, const char *key) {
  unsigned hash = map_hash(key);
  map_node_t **next;
  if (m->nbuckets > 0) {
    next = &m->buckets[map_bucketidx(m, hash)];
    while (*next) {
      if ((*next)->hash == hash && !strcmp((char*) (*next + 1), key)) {
        return next;
      }
      next = &(*next)->next;
    }
  }
  return NULL;
}


void map_deinit_(map_base_t *m) {
  map_node_t *next, *node;
  int i;
  i = m->nbuckets;
  while (i--) {
    node = m->buckets[i];
    while (node) {
      next = node->next;
      free(node);
      node = next;
    }
  }
  free(m->buckets);
}


void *map_get_(map_base_t *m, const char *key) {
  map_node_t **next = map_getref(m, key);
  return next ? (*next)->value : NULL;
}


int map_set_(map_base_t *m, const char *key, void *value, int vsize) {
  int n, err;
  map_node_t **next, *node;
  /* Find & replace existing node */
  next = map_getref(m, key);
  if (next) {
    memcpy((*next)->value, value, vsize);
    return 0;
  }
  /* Add new node */
  node = map_newnode(key, value, vsize);
  if (node == NULL) goto fail;
  if (m->nnodes >= m->nbuckets) {
    n = (m->nbuckets > 0) ? (m->nbuckets << 1) : 1;
    err = map_resize(m, n);
    if (err) goto fail;
  }
  map_addnode(m, node);
  m->nnodes++;
  return 0;
  fail:
  if (node) free(node);
  return -1;
}


void map_remove_(map_base_t *m, const char *key) {
  map_node_t *node;
  map_node_t **next = map_getref(m, key);
  if (next) {
    node = *next;
    *next = (*next)->next;
    free(node);
    m->nnodes--;
  }
}


map_iter_t map_iter_(void) {
  map_iter_t iter;
  iter.bucketidx = UINT_MAX;
  iter.node = NULL;
  return iter;
}


const char *map_next_(map_base_t *m, map_iter_t *iter) {
  if (!iter->node || !(iter->node = iter->node->next)) {
    do {
      if (++iter->bucketidx >= m->nbuckets) {
        return NULL;
      }
      iter->node = m->buckets[iter->bucketidx];
    } while (iter->node == NULL);
  }
  return (char*) (iter->node + 1);
}

static bool quiet = false;

#ifdef __GNUC__
#define __UNUSED__ __attribute__((__unused__))
#endif

#define TRACING 1
#if TRACING
#define TRACE(where...) where,
#define TRACEPARAMS(params...) params,
#else
#define TRACE(where...)
#define TRACEPARAMS(params...)
#endif

#define PRINT_ERROR_AND_EXIT(MSG, CODE)				\
  do								\
    {								\
      char *m = (MSG);						\
      if (m) fprintf(stderr, "%s\n", m);			\
      if (!quiet)                                               \
        fprintf(stderr, "allocations: %lld; total: %lld\n",	\
                allocations, allocations_total);                \
      exit((CODE));						\
    } while (0)

#define FPRINT_ERROR_AND_EXIT(CODE, F...)			\
  do								\
    {								\
      fprintf(stderr, ## F);					\
      if (!quiet)                                               \
        fprintf(stderr, "allocations: %lld; total: %lld\n",	\
                allocations, allocations_total);                \
      exit((CODE));						\
    } while (0)

/* Heap. */

static long long int allocations = 0;
static long long int allocations_total = 0;

char *string_duplicate(char const *str)
{
  size_t l = strlen(str) + 1;
  char *dup = malloc(l);

  if (!dup)
    PRINT_ERROR_AND_EXIT("insufficient memory for duplicating a string", 998);

  memcpy(dup, str, l);

  ++allocations;
  allocations_total += strlen(str);

  return dup;
}

/* Forward references. */

typedef struct Object_s *(*compiled_fn)(TRACEPARAMS(char const *) struct Object_s *, struct Object_s *);

struct Symbol_s {
  char const *name;
};

union Cdr_u {
  struct Object_s *obj;
  struct Symbol_s *sym;
  compiled_fn fn;
};

/* Objects (lists, atoms, etc.). */

typedef struct Object_s {
  struct Object_s *car;  /* Head item in this list, unless a BUILTIN_CAR node. */
  union Cdr_u cdr;  /* Tail list, unless car is a BUILTIN_CAR node. */
} Object;

#define BUILTIN_CAR(obj)			\
  static Object obj;				\
  static struct Object_s *p_##obj = &obj
/* If CAR of an Object is not one of these, it is a List. */
BUILTIN_CAR(atomic);  /* Object.cdr is a struct Symbol_s *. */
BUILTIN_CAR(compiled);  /* Object.cdr is a compiled_fn. */
#undef BUILTIN_CAR

struct Object_s *f_quote(TRACEPARAMS(char const *what) struct Object_s *args, struct Object_s *env);

#define OBJECT(list) static struct Object_s *p_##list = NULL
OBJECT(environment);  /* Current evaluation environment (list of bindings). */

OBJECT(nil); /* Input/output as (). */

OBJECT(quote);  /* Also input as '. */
OBJECT(atom);
OBJECT(eq);
OBJECT(cons);
OBJECT(car);
OBJECT(cdr);
OBJECT(cond);
OBJECT(eval);
OBJECT(apply);

OBJECT(defglobal);

OBJECT(dot_symbol_dump);
#undef OBJECT

bool nilp(struct Object_s *list)
{
  return list == p_nil;
}

/* Whether object is an atom in the traditional Lisp sense */
bool atomp(struct Object_s *list)
{
  return nilp(list) || list->car == p_atomic;
}

/* Whether object is an atom in this implementation */
bool atomicp(struct Object_s *list)
{
  return !nilp(list) && list->car == p_atomic;
}

bool compiledp(struct Object_s *list)
{
  return !atomp(list) && list->car == p_compiled;
}

bool listp(struct Object_s *list)
{
  return !atomp(list) && !compiledp(list);
}

bool finalp(struct Object_s *list)
{
  return listp(list) && nilp(list->cdr.obj);
}

struct Object_s *list_car(struct Object_s *list)
{
  assert (listp(list));
  return list->car;
}

struct Object_s *list_cdr(struct Object_s *list)
{
  assert (listp(list));
  return list->cdr.obj;
}

struct Symbol_s *object_symbol(struct Object_s *atom)
{
  assert (atomicp(atom));
  return atom->cdr.sym;
}

compiled_fn object_compiled(struct Object_s *compiled)
{
  assert (compiledp(compiled));
  return compiled->cdr.fn;
}

static struct Object_s *object_new(struct Object_s *car, struct Object_s *cdr)
{
  struct Object_s *obj;

  obj = (struct Object_s *) malloc(sizeof(Object));
  if (!obj)
    PRINT_ERROR_AND_EXIT("no more memory", 998);

  ++allocations;
  allocations_total += sizeof(Object);

  obj->car = car;
  obj->cdr.obj = cdr;

  return obj;
}

static struct Object_s *object_new_compiled(compiled_fn fn)
{
  struct Object_s *obj;

  obj = (struct Object_s *) malloc(sizeof(Object));
  if (!obj)
    PRINT_ERROR_AND_EXIT("no more memory", 998);

  ++allocations;
  allocations_total += sizeof(Object);

  obj->car = p_compiled;
  obj->cdr.fn = fn;

  return obj;
}

#define object_new_atomic(SYM) object_new(p_atomic, (struct Object_s *) (SYM))
#define object_new_t() object_new_atomic(p_sym_t)
#define object_new_quote() object_new_atomic(p_sym_quote)

/* Symbols. */

#define symbol_name(SYMBOL) ((SYMBOL)->name)

static struct Symbol_s *symbol_new(char const *name)
{
  struct Symbol_s *sym;

  sym = (struct Symbol_s *) malloc(sizeof(struct Symbol_s));
  if (!sym)
    PRINT_ERROR_AND_EXIT("no more memory", 998);

  ++allocations;
  allocations_total += sizeof(struct Symbol_s);

  sym->name = name;

  return sym;
}

/* Map of symbols (keys) to values. */

typedef map_t(struct Symbol_s *) map_sym_t;
static map_sym_t map_sym;

static struct Symbol_s *symbol_lookup(char const *name)
{
  void **p_sym = (void **) map_get(&map_sym, name);

  return p_sym ? *p_sym : NULL;
}

bool symbol_strdup = true;

static struct Symbol_s *symbol_sym(char const *name)
{
  struct Symbol_s *sym = symbol_lookup(name);

  if (sym)
    return sym;

  sym = symbol_new(symbol_strdup ? string_duplicate(name) : name);

  map_set(&map_sym, name, (struct Symbol_s *) sym);

  return sym;
}

static void
symbol_dump(void)
{
  map_iter_t iter = map_iter(&map_sym);

  const char *key;
  while ((key = map_next(&map_sym, &iter))) {
    printf("%s -> %p\n", key, symbol_lookup(key));
  }
}

static struct Symbol_s *p_sym_t = NULL;
static struct Symbol_s *p_sym_quote = NULL;

/* Environment (bindings). */

static struct Object_s *binding_new(struct Object_s *sym, struct Object_s *val)
{
  assert (atomicp(sym));

  return object_new(sym, val);
}

#define environment_new(SYM, VAL, ENV) (object_new(binding_new((SYM), (VAL)), (ENV)))

/* Bindings; each binding is either an atom (meaning its symbol is
   explicitly unbound) or a key/value cons (the symbol is in the car,
   its binding is in the cdr). */

struct Object_s *binding_lookup(TRACEPARAMS(char const *what __UNUSED__) struct Symbol_s *key, struct Object_s *bindings)
{
  if (nilp(bindings))
    return p_nil;

  assert (listp(bindings));

  /* Originally this used a recursive algorithm, but tail-recursion
     optimization wasn't being done by gcc -g -O0, and it was annoying
     to find oneself inside such deep stack traces during debugging.
  */
  for (; !nilp(bindings); bindings = list_cdr(bindings))
  {
    struct Object_s *binding = list_car(bindings);

    if (atomicp(binding) && object_symbol(binding) == key)
      return p_nil;

    {
      struct Object_s *symbol = list_car(binding);

      if (atomicp(symbol) && object_symbol(symbol) == key)
	return binding;
    }
  }

  return p_nil;
}

/* Input */

static char *token_lookahead;
static int lookahead_valid = 0;

typedef struct buffer_s {
  size_t size;
  size_t used;
  char *contents;
} *buffer;

static buffer buffer_new(size_t initial_size)
{
  buffer buf = (buffer ) malloc(sizeof(*buf));
  char *contents = (char *) malloc(initial_size);

  if (!buf || !contents)
    PRINT_ERROR_AND_EXIT("cannot allocate buffer for lexemes", 998);

  buf->size = initial_size;
  buf->used = 0;
  buf->contents = contents;

  return buf;
}

static void buffer_append(buffer buf, char ch)
{
  if (buf->used >= buf->size)
    PRINT_ERROR_AND_EXIT("lexeme too long for this build", 997);  /* TODO: realloc() */
  buf->contents[buf->used++] = ch;
}

static char *buffer_tostring(buffer buf)
{
  buffer_append(buf, '\0');
  return buf->contents;
}

#define buffer_clear(buf) ((buf)->used = 0)

static void token_putback(char *token)
{
  assert (lookahead_valid == 0);
  token_lookahead = token;
  lookahead_valid = 1;
}

static char *token_get(FILE *input, buffer buf) {
  int ch;

  buffer_clear(buf);

  if (lookahead_valid)
    {
      lookahead_valid = 0;
      return token_lookahead;
    }

  do
    {
      if ((ch = getc(input)) == EOF)
	PRINT_ERROR_AND_EXIT(NULL, 0);
      if (ch == ';')
	while ((ch = getc(input)) != EOF && ch != '\n')
	  ;
    } while(isspace(ch));

  buffer_append(buf, ch);

  if(strchr("()\'", ch))
    return buffer_tostring(buf);

  for (;;) {
    if ((ch = getc(input)) == EOF)
      PRINT_ERROR_AND_EXIT(NULL, 0);
    
    if (strchr("()\'", ch) || isspace(ch))
      {
	ungetc(ch, input);
	return buffer_tostring(buf);
      }

    buffer_append(buf, ch);
  }
}

struct Object_s *list_read(FILE *input, buffer buf);
struct Object_s *object_read(FILE *input, buffer buf)
{
  char *token;

  token = token_get(input, buf);

  if (!strcmp(token, "("))
    return list_read(input, buf);

  if (!strcmp(token, "\'"))
    {
      struct Object_s *tmp = object_read(input, buf);

      return object_new(object_new_atomic(p_sym_quote),
			object_new(tmp, p_nil));
    }

  if (!strcmp(token, ")"))
    PRINT_ERROR_AND_EXIT("unbalanced close paren", 1);

  return object_new_atomic(symbol_sym(token));
}

struct Object_s *list_read(FILE *input, buffer buf) {
  char *token = token_get(input, buf);
  struct Object_s *tmp;

  if (!strcmp(token, ")"))
    return p_nil;

  if (!strcmp(token, "."))
    {
      tmp = object_read(input, buf);
      if (strcmp(token_get(input, buf), ")"))
	PRINT_ERROR_AND_EXIT("missing close parenthese for simple list", 3);
      return tmp;
  }
  token_putback(token);

  /* Make sure we first read the object before going on to read the rest of the list. */ 
  tmp = object_read(input, buf);
  return object_new(tmp, list_read(input, buf));
}

/* Output. */

/* true if object is (quote arg) */
/* TODO: Decide whether this look up quote in the current env to do the check */
static bool quotep(struct Object_s *obj)
{
  if (!listp(obj) || !finalp(list_cdr(obj)))
    return false;

  {
    struct Object_s *car = list_car(obj);

    return compiledp(car) && object_compiled(car) == f_quote;
  }
}

void object_write(FILE *output, struct Object_s *obj)
{
  if (nilp(obj))
    {
      fprintf(output, "()");
      return;
    }

  if (atomicp(obj))
    {
      fprintf(output, "%s", symbol_name(object_symbol(obj)));
      return;
    }

  if (compiledp(obj))
    {
      fprintf(output, "*COMPILED*");  /* TODO: Print name of function. */
      return;
    }

  if (quotep(obj))
    {
      fprintf(output, "'");
      object_write(output, list_car(list_cdr(obj)));
      return;
    }

  fprintf(output, "(");
  for (;;)
    {
      object_write(output, list_car(obj));

      if (finalp(obj))
	{
	  fprintf(output, ")");
	  return;
	}

      obj = list_cdr(obj);

      if (!listp(obj))
	{
	  fprintf(output, " . ");
	  object_write(output, obj);
	  fprintf(output, ")");
	  return;
	}

      fprintf(output, " ");
    }
}

/* Evaluation */

struct Object_s *binding_for(TRACEPARAMS(char const *what) struct Symbol_s *sym, struct Object_s *env)
{
  struct Object_s *tmp;

  tmp = binding_lookup(TRACE(what) sym, env);
  if (nilp(tmp))
    {
      /* TODO: Throw an exception etc. */
      fprintf(stderr, "Unbound symbol \"%s\"\n", symbol_name(sym));
      assert ("unbound symbol" == NULL);
    }

  return list_cdr(tmp);
}

struct Object_s *apply(TRACEPARAMS(char const *what) struct Object_s *func, struct Object_s *me, struct Object_s *forms, struct Object_s *env);

/* Does not support traditional lambdas or labels; just the built-ins and
   our "unique" apply.  */
struct Object_s *eval(TRACEPARAMS(char const *what) struct Object_s *exp, struct Object_s *env)
{
  if (nilp(exp) || compiledp(exp))
    return exp;

  if (atomicp(exp))
    return binding_for(TRACE(what) object_symbol(exp), env);

  assert (listp(exp));

  {
    struct Object_s *func = eval(TRACE(what) list_car(exp), env);
    struct Object_s *forms = list_cdr(exp);

#if TRACING
    if (atomp(list_car(exp)))
      what = symbol_name(object_symbol(list_car(exp)));
#endif

    if (compiledp(func))
      {
	compiled_fn fn;

	fn = object_compiled(func);

	return (*fn)(TRACE(what) forms, env);
      }

    return apply(TRACE(what) func, func, forms, env);
  }
}

/* Need to solve these problems:

   (defun x (a) (progn (setq a 6) (a)))
   (setq a 5)
   (x a)

   It should return 5, but if defun is implemented naively, it
   might return 6.

   (defun y (a b c) (cons (a b c)))
   (setq a 5)
   (setq b 6)
   (setq c 7)
   (y c a b)

   That should return (7 5 6).

   Consider: Make the built-in form work as much like a compiled form
   as possible.  The compiled form is passed in an arglist and the
   current environment, allowing it to manipulate both to a high
   degree.  The compiler lexically binds those arguments to parameter
   names, so an instance of the compiled function doesn't put those
   parameter names into the environment.  So, either implement some
   form of lexical binding of names here, or consider a nameless
   solution such as having a particular form eval to the argument list
   and another to the environment.  This means explicitly supporting
   an eval() function that, unlike traditional Lisp, takes an env
   argument.
*/

void assert_zedbap(struct Object_s *zedba)
{
  assert (listp(zedba));

  assert (listp(list_car(zedba)));
  assert (atomicp(list_car(list_car(zedba))));  /* mename */
  assert (atomicp(list_car(list_cdr(list_car(zedba)))));  /* formlistparamname */
  assert (atomicp(list_car(list_cdr(list_cdr(list_car(zedba))))));  /* envparamname */
  assert (finalp(list_cdr(list_cdr(list_car(zedba)))));  /* envparamname */

  assert (finalp(list_cdr(zedba)));  /* body */
}

/* Apply a zedba, which is an self/arglist/env macro version of the
   classic lambda.  The form of a zedba is

   ((mename formlistparamname envparamname) body)

   where body is to be evaluated with formlistparamname bound to the
   list of forms of the invocation, envparamname to the environment
   for the evaluation of the containing expression, and mename bound
   to the zedba itself (for easy recursive references).  Note that the
   caller might want to pass something else to be bound to mename, in
   case this proves useful (e.g. a limit on the # of recursive
   invocations could be implemented this way), so this is allowed.  */

struct Object_s *apply(TRACEPARAMS(char const *what) struct Object_s *func, struct Object_s *me, struct Object_s *forms, struct Object_s *env)
{
  struct Object_s *meparamname;
  struct Object_s *formlistparamname;
  struct Object_s *envparamname;

  assert_zedbap(func);
  assert_zedbap(me);

  {
    struct Object_s *params = list_car(func);

    assert (listp(params));

    meparamname = list_car(params);

    assert (listp(list_cdr(params)));

    formlistparamname = list_car(list_cdr(params));

    assert (finalp(list_cdr(list_cdr(params))));

    envparamname = list_car(list_cdr(list_cdr(params)));
  }

  return eval(TRACE(what) list_car(list_cdr(func)),
	      environment_new(meparamname, func,
			      environment_new(formlistparamname, forms,
					      environment_new(envparamname, env,
							      env))));
}

/* (quote form) => form */
struct Object_s *f_quote(TRACEPARAMS(char const *what __UNUSED__) struct Object_s *args, struct Object_s *env __UNUSED__)
{
  assert (finalp(args));

  return list_car(args);
}

/* (atom atom) => t if atom is an atom (including nil), nil otherwise */
struct Object_s *f_atom(TRACEPARAMS(char const *what) struct Object_s *args, struct Object_s *env)
{
  assert (finalp(args));

  {
    struct Object_s *arg = eval(TRACE(what) list_car(args), env);

    return atomp(arg) ? object_new_t() : p_nil;
  }
}

/* (eq left-atom right-atom) => t if args are equal, nil otherwise */
struct Object_s *f_eq(TRACEPARAMS(char const *what) struct Object_s *args, struct Object_s *env)
{
  assert (listp(args));
  assert (finalp(list_cdr(args)));

  {
    struct Object_s *left = eval(TRACE(what) list_car(args), env);
    struct Object_s *right = eval(TRACE(what) list_car(list_cdr(args)), env);

    assert (atomp(left));
    assert (atomp(right));

    if (left == right)  /* All nils are equal to each other in this implementation */
      return object_new_t();

    if (nilp(left) || nilp(right))
      return p_nil;

    return object_symbol(left) == object_symbol(right) ? object_new_t() : p_nil;
  }
}

/* (cons car-arg cdr-arg) => (car-arg cdr-arg) */
struct Object_s *f_cons(TRACEPARAMS(char const *what) struct Object_s *args, struct Object_s *env)
{
  assert (listp(args));
  assert (finalp(list_cdr(args)));

  {
    struct Object_s *car = eval(TRACE(what) list_car(args), env);
    struct Object_s *cdr = eval(TRACE(what) list_car(list_cdr(args)), env);

    return object_new(car, cdr);
  }
}

/* (car cons-arg) : cons-arg is a list => car of cons-arg */
struct Object_s *f_car(TRACEPARAMS(char const *what) struct Object_s *args, struct Object_s *env __UNUSED__)
{
  assert (finalp(args));

  {
    struct Object_s *arg = eval(TRACE(what) list_car(args), env);

    assert (listp(arg));

    return list_car(arg);
  }
}

/* (cdr cons-arg) : cons-arg is a list => cdr of cons-arg */
struct Object_s *f_cdr(TRACEPARAMS(char const *what) struct Object_s *args, struct Object_s *env __UNUSED__)
{
  assert (finalp(args));

  {
    struct Object_s *arg = eval(TRACE(what) list_car(args), env);

    assert (listp(arg));

    return list_cdr(arg);
  }
}

/* (cond ifthen-args ...) : each ifthen-args is an ifthen-pair; each
   ifthen-pair is a list of form (if-arg then-form) => eval(then-form)
   for the first if-arg in the list that is not nil (true), otherwise
   nil. */
struct Object_s *f_cond(TRACEPARAMS(char const *what) struct Object_s *args, struct Object_s *env __UNUSED__)
{
  if (nilp(args))
    return p_nil;

  assert (listp(args));

  {
    struct Object_s *pair = list_car(args);

    assert (listp(pair));
    assert (finalp(list_cdr(pair)));

    {
      struct Object_s *if_arg = list_car(pair);
      struct Object_s *then_form = list_car(list_cdr(pair));

      if (!nilp(eval(TRACE(what) if_arg, env)))
	return eval(TRACE(what) then_form, env);

      return f_cond(TRACE(what) list_cdr(args), env);
    }
  }
}

/* (defglobal) => global environment
   (defglobal newenv) => '() with newenv as the new global environment (SIDE EFFECT)
   (defglobal key value) => '() with new global environment prepended (via cons)
   			    with (key . value) (SIDE EFFECT)
 */
struct Object_s *f_defglobal(TRACEPARAMS(char const *what) struct Object_s *args, struct Object_s *env __UNUSED__)
{
  if (nilp(args))
    return p_environment;

  assert (listp(args));

  /* This form allows direct replacement of global environment.
     E.g. (defglobal (cdr (defglobal))) pops off the top binding. */  
  if (nilp(list_cdr(args)))
    p_environment = eval(TRACE(what) list_car(args), env);
  else
    {
      assert (finalp(list_cdr(args)));

      {
	struct Object_s *sym = eval(TRACE(what) list_car(args), env);
	struct Object_s *form = eval(TRACE(what) list_car(list_cdr(args)), env);

	assert (atomicp(sym));

	p_environment = environment_new(object_new_atomic(object_symbol(sym)),
					form,
					p_environment);
      }
    }

  return p_nil;
}

/* (eval arg [env]) => arg evaluated with respect to environment env (default is current env) */
struct Object_s *f_eval(TRACEPARAMS(char const *what) struct Object_s *args, struct Object_s *env)
{
  assert (listp(args));
  assert (nilp(list_cdr(args)) || finalp(list_cdr(args)));

  return eval(TRACE(what) eval(TRACE(what) list_car(args), env),
	      nilp(list_cdr(args)) ? env : eval(TRACE(what) list_car(list_cdr(args)), env));
}

/* (apply zedba me forms [env]) => zedba invoked with reference to
   (presumably) itself, forms to be bound to zedba's arguments, and
   environment for such bindings (default is current env) */
struct Object_s *f_apply(TRACEPARAMS(char const *what) struct Object_s *args, struct Object_s *env)
{
  assert (listp(args));

  {
    struct Object_s *func = eval(TRACE(what) list_car(args), env);
    struct Object_s *rest = list_cdr(args);

#if TRACING
    if (atomp(list_car(args)))
      what = symbol_name(object_symbol(list_car(args)));
#endif

    assert (listp(rest));

    {
      struct Object_s *me = eval(TRACE(what) list_car(rest), env);
      struct Object_s *new_rest = list_cdr(rest);

      rest = new_rest;
      assert (listp(rest));

      {
	struct Object_s *forms = eval(TRACE(what) list_car(rest), env);
	struct Object_s *new_rest = list_cdr(rest);

	rest = new_rest;
	assert (nilp(rest) || finalp(rest));

	return apply(TRACE(what) func, me, forms, nilp(rest) ? p_nil : eval(TRACE(what) list_car(rest), env));
      }
    }
  }
}

/* (.symbol_dump) : dump symbol names along with their struct Symbol_s * objects */
struct Object_s *f_dot_symbol_dump(TRACEPARAMS(char const *what __UNUSED__) struct Object_s *args, struct Object_s *env __UNUSED__)
{
  assert (!args);

  symbol_dump();

  return p_nil;
}

static struct Object_s *initialize_builtin(char const *sym, compiled_fn fn)
{
  struct Object_s *tmp;

  p_environment = environment_new(object_new_atomic(symbol_sym(sym)),
				  (tmp = object_new_compiled(fn)),
				  p_environment);

  return tmp;
}

void initialize()
{
  p_sym_t = symbol_sym("t");
  p_sym_quote = symbol_sym("quote");

  symbol_strdup = false;

#define BUILTIN(sym) p_##sym = initialize_builtin(#sym, f_##sym);
  BUILTIN(quote);
  BUILTIN(atom);
  BUILTIN(eq);
  BUILTIN(cons);
  BUILTIN(car);
  BUILTIN(cdr);
  BUILTIN(cond);
  BUILTIN(eval);
  BUILTIN(apply);

  BUILTIN(defglobal);  /* TODO: Decide on a better name. */
#undef BUILTIN

  p_dot_symbol_dump = initialize_builtin(".symbol_dump", f_dot_symbol_dump);

  symbol_strdup = true;
}

#define MAXTOKENSIZE 1000

int
main(int argc, char **argv)
{
  if (argc > 1 && !strcmp(argv[1], "-q"))
    quiet = true;

  map_init(&map_sym);

  buffer buf;

  initialize();
  buf = buffer_new(MAXTOKENSIZE);
  for (;;) {
    struct Object_s *obj = object_read(stdin, buf);
    if (!quiet) {
      object_write(stdout, eval(TRACE("stdin") obj, p_environment));
      fprintf(stdout, "\n");
      fflush(stdout);
    }
  }
  return 0;
}

void debug_output(struct Object_s *obj)
{
  object_write(stdout, obj);
  fprintf(stdout, "\n");
  fflush(stdout);
}
