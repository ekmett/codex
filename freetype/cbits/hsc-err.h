#ifndef INCLUDED_ERR_H
#define INCLUDED_ERR_H

#include <stdio.h>
#include <ft2build.h>

/* drop the FT_ prefix */
#define xstr(s) str(s)+3
#define str(s) #s

static inline void hsc_err_exports() {
#undef  FTERRORS_H_
#undef  FT_ERROR_START_LIST
#define FT_ERROR_START_LIST
#undef  FT_NOERRORDEF
#define FT_NOERRORDEF(e,s) hsc_printf("  , %s\n",xstr(e));
#undef  FT_ERRORDEF
#define FT_ERRORDEF(e,v,s) hsc_printf("  , %s\n",xstr(e));
#undef  FT_ERROR_END_LIST
#define FT_ERROR_END_LIST

#include FT_ERRORS_H
}

static inline void hsc_err_patterns() {
#undef  FTERRORS_H_
#undef  FT_ERROR_START_LIST
#define FT_ERROR_START_LIST
#undef  FT_NOERRORDEF
#define FT_NOERRORDEF(e,s) hsc_printf("-- %s\npattern %s :: Error\npattern %s = Error 0\n\n",s,xstr(e),xstr(e));
#undef  FT_ERRORDEF
#define FT_ERRORDEF(e,v,s) hsc_printf("-- %s\npattern %s :: Error\npattern %s = Error %d\n\n",s,xstr(e),xstr(e),v);
#undef  FT_ERROR_END_LIST
#define FT_ERROR_END_LIST

#include FT_ERRORS_H
}
#endif
