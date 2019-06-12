#include <stdint.h>
#include "parsnip.h"

/* find the first occurence of a given character or the location of the terminating null in a string. */

char * strchr0 (char *s, HsChar hc) {
  char c = (char)hc;
  for (; ((uint64_t)s & 7) != 0; ++s)
    if (*s == c || *s == '\0')
      return s;

  uint64_t *p = (uint64_t*)s;
  uint64_t magic = (uint64_t)(-1) / 0xff * 0xfe << 1 >> 1 | 1;

  uint64_t mask = (uint64_t)c;
  mask |= mask << 8;
  mask |= mask << 16;
  mask |= mask << 32;

  for (;;) {
    uint64_t w = *p++;
    if ((((w + magic) ^ ~w) & ~magic) != 0 || ((((w ^ mask) + magic) ^ ~(w ^ mask)) & ~magic) != 0) {
      s = (char*)(p-1);
      if (*s == c || *s == '\0') return s;
      if (*++s == c) return s;
      if (*s == '\0') return s;
      if (*++s == c) return s;
      if (*s == '\0') return s;
      if (*++s == c) return s;
      if (*s == '\0') return s;
      if (*++s == c) return s;
      if (*s == '\0') return s;
      if (*++s == c) return s;
      if (*s == '\0') return s;
      if (*++s == c) return s;
      if (*s == '\0') return s;
      if (*++s == c) return s;
      if (*s == '\0') return s;
    }
  }
}
