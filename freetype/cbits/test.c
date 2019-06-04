#include "hsc-struct.h"

typedef struct {
  int height,width,size,x_ppem,y_ppem;
} FT_Bitmap_Size;

int main(int argc, char ** argv) {
  hsc_struct(bitmapsize,BitmapSize,FT_Bitmap_Size,height,Int16,width,Int16,size,Pos,x_ppem,Pos,y_ppem,Pos);
}
