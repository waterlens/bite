#include "runtime.h"
#include <gc/gc.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

void _dummy(object *_1, string *_2, array *_3, closure *_4, tagged *_5,
            tuple *_6) {}

void runtime_panic(const char *s) {
  puts(s);
  exit(1);
}

tuple *alloc_tuple(size_t n) {
  if (n > 4)
    runtime_panic("the number of tuple fields exceeds 8");
  tuple *p = GC_MALLOC(sizeof(tuple) + n * sizeof(void *));
  p->header.kind = Tuple;
  p->size = n;
  return p;
}

tuple *make_tuple_1(object *_1) {
  tuple *t = alloc_tuple(1);
  t->elements[0] = _1;
  return t;
}

tuple *make_tuple_2(object *_1, object *_2) {
  tuple *t = alloc_tuple(1);
  t->elements[0] = _1;
  t->elements[1] = _2;
  return t;
}

tuple *make_tuple_3(object *_1, object *_2, object *_3) {
  tuple *t = alloc_tuple(1);
  t->elements[0] = _1;
  t->elements[1] = _2;
  t->elements[2] = _3;
  return t;
}

tuple *make_tuple_4(object *_1, object *_2, object *_3, object *_4) {
  tuple *t = alloc_tuple(1);
  t->elements[0] = _1;
  t->elements[1] = _2;
  t->elements[2] = _3;
  t->elements[3] = _4;
  return t;
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

tagged *make_tagged_0(size_t tag) {
  tagged *t = alloc_tagged(tag, 1);
  return t;
}

tagged *make_tagged_1(size_t tag, object *_1) {
  tagged *t = alloc_tagged(tag, 1);
  t->elements[0] = _1;
  return t;
}

tagged *make_tagged_2(size_t tag, object *_1, object *_2) {
  tagged *t = alloc_tagged(tag, 2);
  t->elements[0] = _1;
  t->elements[1] = _2;
  return t;
}

tagged *make_tagged_3(size_t tag, object *_1, object *_2, object *_3) {
  tagged *t = alloc_tagged(tag, 3);
  t->elements[0] = _1;
  t->elements[1] = _2;
  t->elements[2] = _3;
  return t;
}

tagged *make_tagged_4(size_t tag, object *_1, object *_2, object *_3,
                      object *_4) {
  tagged *t = alloc_tagged(tag, 4);
  t->elements[0] = _1;
  t->elements[1] = _2;
  t->elements[2] = _3;
  t->elements[3] = _4;
  return t;
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

uint64_t extract_boxed_i64(i64 *value) { return value->data; }