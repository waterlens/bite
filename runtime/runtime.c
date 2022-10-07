#include "runtime.h"
#include <gc/gc.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

void _dummy(object *_1, string *_2, array *_3, closure *_4, tagged *_5,
            tuple *_6) {}

void runtime_panic(const char *s) {
  puts(s);
  exit(1);
}

tuple *alloc_tuple(size_t n) {
  if (n > 8)
    runtime_panic("the number of tuple fields exceeds 8");
  tuple *p = GC_MALLOC(sizeof(tuple) + n * sizeof(void *));
  p->header.kind = Tuple;
  p->size = n;
  return p;
}

size_t get_tuple_size(tuple *t) { return t->size; }

void *extract_tuple_field(tuple *t, size_t n) {
  if (n >= t->size)
    runtime_panic("extraction overflow");
  return t->elements[n];
}

tagged *alloc_tagged(size_t tag, size_t n) {
  if (n > 8)
    runtime_panic("the number of ctor fields exceeds 8");
  tagged *p = GC_MALLOC(sizeof(tagged) + n * sizeof(void *));
  p->header.kind = Tuple;
  p->tag = tag;
  p->size = n;
  return p;
}

void *extract_tagged_field(tagged *t, size_t tag, size_t n) {
  if (tag != t->tag)
    runtime_panic("inconsistent tag");
  if (n >= t->size)
    runtime_panic("extraction overflow");
  return t->elements[n];
}

i64 *create_boxed_i64(uint64_t value) {
  i64 *p = GC_MALLOC(sizeof(i64));
  p->header.kind = Int64;
  p->data = value;
  return p;
}
