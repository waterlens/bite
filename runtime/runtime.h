#include <stdint.h>

enum object_kind {
  Reserved = 0,
  Str,
  Tuple,
  Tagged,
  Closure,
  Array,
};

#define packed_struct struct __attribute__((packed))

typedef packed_struct object {
  int32_t meta;
  int8_t kind;
  int8_t reserved1;
  int16_t reserved2;
}
object;

typedef packed_struct string {
  object header;
  uint32_t len;
  uint8_t data[];
}
string;

typedef packed_struct array {
  object header;
  void *data;
  uint32_t size;
  uint32_t capacity;
}
array;

typedef packed_struct closure {
  object header;
  void *env;
  void *f;
}
closure;

typedef packed_struct tagged {
  object header;
  uint32_t tag;
  uint32_t size;
  void *elements[];
}
tagged;

typedef packed_struct tuple {
  object header;
  uint32_t size;
  uint32_t reserved;
  void *elements[];
}
tuple;
