#include <stdlib.h>
#include "jv.h"

jv_kind jv_get_kind_w (jv* x) {
  return jv_get_kind(*x);
}

jv* jv_copy_w (jv* x) {
  jv* jvPtr = malloc(sizeof(jv));
  jv copy = jv_copy(*x);
  *jvPtr = copy;
  return jvPtr;
}

void jv_free_w (jv* x) {
  jv_free(*x);
}

int jv_get_refcnt_w (jv* x) {
  return jv_get_refcnt(*x);
}

int jv_equal_w (jv* jv1, jv* jv2) {
  return jv_equal(*jv1, *jv2);
}

int jv_identical_w (jv* a, jv* b) {
  return jv_identical(*a, *b);
}

int jv_contains_w (jv* a, jv* b) {
  return jv_contains(*a, *b);
}

jv* jv_invalid_w (void) {
  jv *ptr = malloc(sizeof *ptr);
  *ptr = jv_invalid();
  return ptr;
}

jv* jv_invalid_with_msg_w (jv* x) {
  jv *ptr = malloc(sizeof *ptr);
  *ptr = jv_invalid_with_msg(*x);
  return ptr;
}

jv* jv_invalid_get_msg_w (jv* x) {
  jv *ptr = malloc(sizeof *ptr);
  *ptr = jv_invalid_get_msg(*x);
  return ptr;
}

int jv_invalid_has_msg_w (jv* x) {
  return jv_invalid_has_msg(*x);
}


jv* jv_null_w (void) {
  jv *ptr = malloc(sizeof *ptr);
  *ptr = jv_null();
  return ptr;
}

jv* jv_true_w (void) {
  jv *ptr = malloc(sizeof *ptr);
  *ptr = jv_true();
  return ptr;
}

jv* jv_false_w (void) {
  jv *ptr = malloc(sizeof *ptr);
  *ptr = jv_false();
  return ptr;
}

jv* jv_bool_w (int i) {
  jv *ptr = malloc(sizeof *ptr);
  *ptr = jv_bool(i);
  return ptr;
}

////////////////////////////////////////////////////////////////////////////////

jv* jv_parse_w (const char* str) {
  jv *jvPtr = malloc(sizeof *jvPtr);
  jv result = jv_parse(str);
  *jvPtr = result;
  return jvPtr;
}

jv* jv_string_w (const char* str) {
  jv *jvPtr = malloc(sizeof *jvPtr);
  jv result = jv_string(str);
  *jvPtr = result;
  return jvPtr;
}

const char* jv_string_value_w (jv* x) {
  return jv_string_value(*x);
}

jv* jv_dump_string_w (jv* x, int opts) {
  jv* jvPtr = malloc(sizeof(jv));
  jv result;
  // does this free x? Yes
  result = jv_dump_string(*x, opts);
  *jvPtr = result;
  return jvPtr;
}
