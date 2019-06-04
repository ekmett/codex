#include <stdio.h>
#include <stdarg.h>

void show_struct(const char * hsPrefix, const char * hsTyName, const char * cTyName, int cTySize, int cTyAlign, ...) {
  printf("data %s = %s\n", hsTyName, hsTyName);
  char next = '{';
  va_list args;
  va_start(args,cTyAlign);
  const char * p = va_arg(args, const char *);
  int q = va_arg(args, int);
  const char * r = va_arg(args, const char *);
  while (p && r) {
    printf("  %c %s_%s :: %s -- %d\n",next,hsPrefix,p,r,q);
    next = ',';
    p = va_arg(args, const char *);
    q = va_arg(args, int);
    r = va_arg(args, const char *);
  }
  printf("  %c deriving (Eq,Show)\n\ninstance Storable %s where\n", next == ',' ? '}' : ' ',hsTyName);
  printf("  sizeOf _ = %d\n", cTySize);
  printf("  alignment _ = %d\n", cTyAlign);
  printf("  peek p = %s\n",hsTyName);

  next = '$';
  va_start(args,cTyAlign);
  p = va_arg(args, const char *);
  q = va_arg(args, int);
  r = va_arg(args, const char *);
  while (p && r) {
    printf("    <%c> peekByteOff p %d\n",next,q);
    next = '*';
    p = va_arg(args, const char *);
    q = va_arg(args, int);
    r = va_arg(args, const char *);
  }
  printf("  poke p %s{..} = do\n",hsTyName);
  va_start(args,cTyAlign);
  p = va_arg(args, const char *);
  q = va_arg(args, int);
  r = va_arg(args, const char *);
  while (p && r) {
    printf("    pokeByteOff p %d %s_%s\n",q,hsPrefix,p);
    p = va_arg(args, const char *);
    q = va_arg(args, int);
    r = va_arg(args, const char *);
  }
  puts("\n");
}

#define loop0(x, s,t,dummy)
#define loop2(x,s,t,a,b) s(x,a),t(b)
#define loop4(x,s,t,a,b,c,d) s(x,a),t(b),s(x,c),t(d)
#define loop6(x,s,t,a,b,c,d,e,f) s(x,a),t(b),s(x,c),t(d),s(x,e),t(f)
#define loop8(x,s,t,a,b,c,d,e,f,g,h) s(x,a),t(b),s(x,c),t(d),s(x,e),t(f),s(x,g),t(h)
#define loop10(x,s,t,a,b,c,d,e,f,g,h,i,j) s(x,a),t(b),s(x,c),t(d),s(x,e),t(f),s(x,g),t(h),s(x,i),t(j)
#define loop12(x,s,t,a,b,c,d,e,f,g,h,i,j,k,l) s(x,a),t(b),s(x,c),t(d),s(x,e),t(f),s(x,g),t(h),s(x,i),t(j),s(x,k),t(l)
#define loop14(x,s,t,a,b,c,d,e,f,g,h,i,j,k,l,m,n) s(x,a),t(b),s(x,c),t(d),s(x,e),t(f),s(x,g),t(h),s(x,i),t(j),s(x,k),t(l),s(x,m),t(n)
#define loop16(x,s,t,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) s(x,a),t(b),s(x,c),t(d),s(x,e),t(f),s(x,g),t(h),s(x,i),t(j),s(x,k),t(l),s(x,m),t(n),s(x,o),t(p)
#define loop18(x,s,t,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) s(x,a),t(b),s(x,c),t(d),s(x,e),t(f),s(x,g),t(h),s(x,i),t(j),s(x,k),t(l),s(x,m),t(n),s(x,o),t(p),s(x,q),t(r)

#define num_args_h1(dummy,x18,x17,x16,x15,x14,x13,x12,x11,x10,x9,x8,x7,x6,x5,x4,x3,x2,x1,x0,...) x0
#define num_args(...) num_args_h1(dummy,##__VA_ARGS__,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0)

#define loop_all_h3(x,s,t,n,...) loop##n(x,s,t,__VA_ARGS__)
#define loop_all_h2(x,s,t,n,...) loop_all_h3(x,s,t,n,__VA_ARGS__)
#define loop_all(x,s,t,...) loop_all_h2(x,s,t,num_args(__VA_ARGS__), __VA_ARGS__)

#define OFFSET_OF(TYPE, ELEMENT) ((size_t)&(((TYPE *)0)->ELEMENT))
#define ALIGNMENT_OF( t ) OFFSET_OF( struct { char x; t test; }, test )

#define str(s) #s
#define struct_field_name(cTy,field) str(field),OFFSET_OF(cTy,field)
#define struct_type_name(hFieldTy) #hFieldTy
#define hsc_struct(hsPrefix, hsTyName, cTyName, ...) show_struct(#hsPrefix, #hsTyName, #cTyName, sizeof(cTyName), ALIGNMENT_OF(cTyName), loop_all(cTyName,struct_field_name,struct_type_name,##__VA_ARGS__), NULL,0,NULL,NULL,0)

