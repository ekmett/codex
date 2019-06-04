#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_TYPES_H
#include FT_SYSTEM_H

extern const char * hs_get_error_string(FT_Error e);

extern struct FT_MemoryRec_ hs_memory;

typedef struct memory_face_data {
  const char * data;
  FT_Library lib;
} memory_face_data;

extern void finalize_memory_face_data(void *);
