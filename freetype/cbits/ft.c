#include <stddef.h>
#include <memory.h>
#include "ft.h"

const char * hs_get_error_string(FT_Error e) {
#undef FTERRORS_H_
#define FT_ERROR_START_LIST switch ( e ) {
#define FT_NOERRORDEF(e,s) case 0: return s;
#define FT_ERRORDEF(e,v,s) case v: return s;
#define FT_ERROR_END_LIST default: return NULL; }

#include FT_ERRORS_H
}

static void * alloc_func(FT_Memory memory, long size) {
  return calloc(1,size); /* https://www.freetype.org/freetype2/docs/reference/ft2-user_allocation.html */
}

static void free_func(FT_Memory memory, void * block) {
  if (block != memory) free(block);
}

static void * realloc_func(FT_Memory memory, long cur_size, long new_size, void * block) {
  return realloc(block, new_size);
}

struct FT_MemoryRec_ hs_memory = {
  NULL,
  alloc_func,
  free_func,
  realloc_func
};
