#define STB_TRUETYPE_IMPLEMENTATION
#include <stdlib.h>
#include "stb_rect_pack.h"
#include "stb_truetype.h"
#include "truetype.h"

// we transfer ownership of the font to stbtt, so we must free it properly
// void free_font(struct stbtt_font_info * font) {
//  free(font->data);
//}
