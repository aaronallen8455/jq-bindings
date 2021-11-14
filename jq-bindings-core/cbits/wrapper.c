#include <stdlib.h>
#include "jv.h"
#include "jq.h"

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
  free(x);
}

int jv_get_refcnt_w (jv* x) {
  return jv_get_refcnt(*x);
}

int jv_equal_w (jv* jv1, jv* jv2) {
  int r = jv_equal(*jv1, *jv2);
  free(jv1);
  free(jv2);
  return r;
}

int jv_identical_w (jv* a, jv* b) {
  int r = jv_identical(*a, *b);
  free(a);
  free(b);
  return r;
}

int jv_contains_w (jv* a, jv* b) {
  int r = jv_contains(*a, *b);
  free(a);
  free(b);
  return r;
}

jv* jv_invalid_w (void) {
  jv *ptr = malloc(sizeof *ptr);
  *ptr = jv_invalid();
  return ptr;
}

jv* jv_invalid_get_msg_w (jv* x) {
  jv *ptr = malloc(sizeof *ptr);
  *ptr = jv_invalid_get_msg(*x);
  free(x);
  return ptr;
}

jv* jv_null_w (void) {
  jv *ptr = malloc(sizeof *ptr);
  *ptr = jv_null();
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
  int r = jv_array_length(*x);
  free(x);
  return r;
}

jv* jv_array_get_w (jv* j, int i) {
  jv *ptr = malloc(sizeof *ptr);
  *ptr = jv_array_get(*j, i);
  return ptr;
}

void jv_array_set_w (jv* j, int i, jv* e) {
  *j = jv_array_set(*j, i, *e);
  free(e);
}

void jv_array_append_w(jv* arr, jv* val) {
  *arr = jv_array_append(*arr, *val);
  free(val);
}

void jv_array_concat_w(jv* a, jv* b) {
  *a = jv_array_concat(*a, *b);
  free(b);
}

jv* jv_array_slice_w (jv* arr, int start, int end) {
  jv *ptr = malloc(sizeof *ptr);
  *ptr = jv_array_slice(*arr, start, end);
  free(arr);
  return ptr;
}

jv* jv_array_indexes_w (jv* a, jv* b) {
  jv *ptr = malloc(sizeof *ptr);
  *ptr = jv_array_indexes(*a, *b);
  free(a);
  free(b);
  return ptr;
}

jv* jv_string_w (const char* str) {
  jv *jvPtr = malloc(sizeof *jvPtr);
  *jvPtr = jv_string(str);
  return jvPtr;
}

jv* jv_string_sized_w (const char* str, int len) {
  jv *jvPtr = malloc(sizeof *jvPtr);
  *jvPtr = jv_string_sized(str, len);
  return jvPtr;
}

int jv_string_length_bytes_w(jv* x) {
  int r = jv_string_length_bytes(*x);
  free(x);
  return r;
}

int jv_string_length_codepoints_w(jv* x) {
  int r = jv_string_length_codepoints(*x);
  free(x);
  return r;
}

const char* jv_string_value_w (jv* x) {
  return jv_string_value(*x);
}

jv* jv_string_indexes_w(jv* j, jv* k) {
  jv *ptr = malloc(sizeof(*ptr));
  *ptr = jv_string_indexes(*j, *k);
  free(j);
  free(k);
  return ptr;
}

jv* jv_string_slice_w(jv* j, int start, int end) {
  jv *ptr = malloc(sizeof(*ptr));
  *ptr = jv_string_slice(*j, start, end);
  free(j);
  return ptr;
}

void jv_string_concat_w(jv* j, jv* k) {
  *j = jv_string_concat(*j, *k);
  free(k);
}

void jv_string_append_str_w(jv* a, const char* str) {
  *a = jv_string_append_str(*a, str);
}

jv* jv_string_split_w(jv* j, jv* sep) {
  jv *ptr = malloc(sizeof(*ptr));
  *ptr = jv_string_split(*j, *sep);
  free(j);
  free(sep);
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
  free(object);
  free(key);
  return ptr;
}

int jv_object_has_w(jv* object, jv* key) {
  int r = jv_object_has(*object, *key);
  free(object);
  free(key);
  return r;
}

void jv_object_set_w(jv* object, jv* key, jv* value) {
  *object = jv_object_set(*object, *key, *value);
  free(key);
  free(value);
}

void jv_object_delete_w(jv* object, jv* key) {
  *object = jv_object_delete(*object, *key);
  free(key);
}

int jv_object_length_w(jv* object) {
  int r = jv_object_length(*object);
  free(object);
  return r;
}

void jv_object_merge_w(jv* a, jv* b) {
  *a = jv_object_merge(*a, *b);
  free(b);
}

void jv_object_merge_recursive_w(jv* a, jv* b) {
  *a = jv_object_merge_recursive(*a, *b);
  free(b);
}

void jv_setpath_w (jv* x, jv* path, jv* v) {
  *x = jv_setpath(*x, *path, *v);
  free(path);
  free(v);
}

void jv_getpath_w(jv* x, jv* path) {
  *x = jv_getpath(*x, *path);
  free(path);
}

jv* jv_parse_w (const char* str) {
  jv *jvPtr = malloc(sizeof *jvPtr);
  jv result = jv_parse(str);
  *jvPtr = result;
  return jvPtr;
}

jv* jv_parse_sized_w (const char* str, int len) {
  jv *jvPtr = malloc(sizeof *jvPtr);
  jv result = jv_parse_sized(str, len);
  *jvPtr = result;
  return jvPtr;
}

jv* jv_load_file_w (const char* file, int raw) {
  jv *jvPtr = malloc(sizeof *jvPtr);
  *jvPtr = jv_load_file(file, raw);
  return jvPtr;
}

jv* jv_dump_string_w (jv* x, int opts) {
  jv* jvPtr = malloc(sizeof(jv));
  *jvPtr = jv_dump_string(*x, opts);
  free(x);
  return jvPtr;
}

jv* jv_keys_w (jv* x) {
  jv* jvPtr = malloc(sizeof(jv));
  *jvPtr = jv_keys(*x);
  free(x);
  return jvPtr;
}

// JQ programs

void jq_start_w (jq_state* jq, jv* x, int flags) {
  jq_start(jq, *x, flags);
  free(x);
}

jv* jq_next_w (jq_state* jq) {
  jv* ptr = malloc(sizeof(jv));
  *ptr = jq_next(jq);
  return ptr;
}

void jq_teardown_w (jq_state* jq) {
  jq_teardown(&jq);
}
