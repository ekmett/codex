#ifndef INCLUDED_ERR_H
#define INCLUDED_ERR_H

#include <stdio.h>
#include <ft2build.h>

/* drop the FT_ prefix */
#define xstr(s) str(s)+3
#define str(s) #s

static inline const char * err_exports() {
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

  return "";
}

static inline const char * err_patterns() {
#undef  FTERRORS_H_
#undef  FT_ERROR_START_LIST
#define FT_ERROR_START_LIST
#undef  FT_NOERRORDEF
#define FT_NOERRORDEF(e,s) hsc_printf("-- | %s\npattern %s :: Error\npattern %s = 0\n\n",s,xstr(e),xstr(e));
#undef  FT_ERRORDEF
#define FT_ERRORDEF(e,v,s) hsc_printf("-- | %s\npattern %s :: Error\npattern %s = %d\n\n",s,xstr(e),xstr(e),v);
#undef  FT_ERROR_END_LIST
#define FT_ERROR_END_LIST

#include FT_ERRORS_H

  return "";
}
#endif
