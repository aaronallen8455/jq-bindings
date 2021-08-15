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

jv* jv_number_w (double d) {
  jv *ptr = malloc(sizeof *ptr);
  *ptr = jv_number(d);
  return ptr;
}

double jv_number_value_w (jv* x) {
  return jv_number_value(*x);
}

int jv_is_integer_w(jv* x) {
  return jv_is_integer(*x);
}

jv* jv_array_w() {
  jv *ptr = malloc(sizeof *ptr);
  *ptr = jv_array();
  return ptr;
}

jv* jv_array_sized_w(int s) {
  jv *ptr = malloc(sizeof *ptr);
  *ptr = jv_array_sized(s);
  return ptr;
}

int jv_array_length_w (jv* x) {
  return jv_array_length(*x);
}

jv* jv_array_get_w (jv* j, int i) {
  jv *ptr = malloc(sizeof *ptr);
  *ptr = jv_array_get(*j, i);
  return ptr;
}

void jv_array_set_w (jv* j, int i, jv* e) {
  *j = jv_array_set(*j, i, *e);
}

void jv_array_append_w(jv* arr, jv* val) {
  *arr = jv_array_append(*arr, *val);
}

void jv_array_concat_w(jv* a, jv* b) {
  *a = jv_array_concat(*a, *b);
}

jv* jv_array_slice_w (jv* arr, int start, int end) {
  jv *ptr = malloc(sizeof *ptr);
  *ptr = jv_array_slice(*arr, start, end);
  return ptr;
}

jv* jv_array_indexes_w (jv* a, jv* b) {
  jv *ptr = malloc(sizeof *ptr);
  *ptr = jv_array_indexes(*a, *b);
  return ptr;
}

jv* jv_string_w (const char* str) {
  jv *jvPtr = malloc(sizeof *jvPtr);
  jv result = jv_string(str);
  *jvPtr = result;
  return jvPtr;
}

// jv jv_string_sized(const char*, int);

// jv jv_string_empty(int len);

int jv_string_length_bytes_w(jv* x) {
  return jv_string_length_bytes(*x);
}

int jv_string_length_codepoints_w(jv* x) {
  return jv_string_length_codepoints(*x);
}

// unsigned long jv_string_hash(jv);

const char* jv_string_value_w (jv* x) {
  return jv_string_value(*x);
}

jv* jv_string_indexes_w(jv* j, jv* k) {
  jv *ptr = malloc(sizeof(*ptr));
  *ptr = jv_string_indexes(*j, *k);
  return ptr;
}

jv* jv_string_slice_w(jv* j, int start, int end) {
  jv *ptr = malloc(sizeof(*ptr));
  *ptr = jv_string_slice(*j, start, end);
  return ptr;
}

void jv_string_concat_w(jv* j, jv* k) {
  *j = jv_string_concat(*j, *k);
}

//jv jv_string_vfmt(const char*, va_list) JV_VPRINTF_LIKE(1);
//jv jv_string_fmt(const char*, ...) JV_PRINTF_LIKE(1, 2);
//jv jv_string_append_codepoint(jv a, uint32_t c);
//jv jv_string_append_buf(jv a, const char* buf, int len);

void jv_string_append_str_w(jv* a, const char* str) {
  *a = jv_string_append_str(*a, str);
}

jv* jv_string_split_w(jv* j, jv* sep) {
  jv *ptr = malloc(sizeof(*ptr));
  *ptr = jv_string_split(*j, *sep);
  return ptr;
}

//jv jv_string_explode(jv j);
//jv jv_string_implode(jv j);

jv* jv_object_w() {
  jv *ptr = malloc(sizeof(*ptr));
  *ptr = jv_object();
  return ptr;
}

jv* jv_object_get_w(jv* object, jv* key) {
  jv *ptr = malloc(sizeof(*ptr));
  *ptr = jv_object_get(*object, *key);
  return ptr;
}

int jv_object_has_w(jv* object, jv* key) {
  return jv_object_has(*object, *key);
}

void jv_object_set_w(jv* object, jv* key, jv* value) {
  *object = jv_object_set(*object, *key, *value);
}

void jv_object_delete_w(jv* object, jv* key) {
  *object = jv_object_delete(*object, *key);
}

int jv_object_length_w(jv* object) {
  return jv_object_length(*object);
}

void jv_object_merge_w(jv* a, jv* b) {
  *a = jv_object_merge(*a, *b);
}

void jv_object_merge_recursive_w(jv* a, jv* b) {
  *a = jv_object_merge_recursive(*a, *b);
}

jv* jv_parse_w (const char* str) {
  jv *jvPtr = malloc(sizeof *jvPtr);
  jv result = jv_parse(str);
  *jvPtr = result;
  return jvPtr;
}

jv* jv_dump_string_w (jv* x, int opts) {
  jv* jvPtr = malloc(sizeof(jv));
  *jvPtr = jv_dump_string(*x, opts);
  return jvPtr;
}
