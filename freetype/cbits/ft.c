#include <stddef.h>
#include "ft.h"

const char * get_error_string(FT_Error e) {
#undef FTERRORS_H_
#define FT_ERROR_START_LIST switch ( e ) {
#define FT_NOERRORDEF(e,s) case 0: return s;
#define FT_ERRORDEF(e,v,s) case v: return s;
#define FT_ERROR_END_LIST default: return NULL; }

#include FT_ERRORS_H
}
