#include <ft2build.h>
#include FT_TYPES_H
#include FT_SYSTEM_H

extern const char * hs_get_error_string(FT_Error e);

extern struct FT_MemoryRec_ hs_memory;

typedef struct { FT_Library lib; void * data; } memory_face_env;
