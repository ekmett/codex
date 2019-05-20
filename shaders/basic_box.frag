#version 330

uniform vec2 iResolution = vec2(640.,480.);
uniform vec2 iMouse;

const int N = 5;

layout (location = 0) out vec4 fragColor;

float sdfCircle(vec2 p, float r) {
  return length(p) - r;
}

float sdfRect(in vec2 p, in vec2 wh) {
  vec2 d = abs(p) - wh;
  return length(max(d,vec2(0))) + min(max(d.x,d.y),0.0);
}

float sdfAnnular( in float sdf, in float r ) {
  return abs(sdf) - r;
}

float sdfUnion( in float d1, in float d2 ) {
  return min(d1, d2);
}

// This would be a pain to specify different N for every poly so there has to be
// a nicer way. This will probably live in its own module and the guts can be
// macro'd in maybe? I don't know much about mangling GLSL source with built in
// things.
//
float sdfPoly( in vec2[N] v, in vec2 p )
{
  const int num = v.length();
  float d = dot(p-v[0],p-v[0]);
  float s = 1.0;
  for( int i=0, j=num-1; i<num; j=i, i++ )
    {
      // distance
      vec2 e = v[j] - v[i];
      vec2 w =    p - v[i];
      vec2 b = w - e*clamp( dot(w,e)/dot(e,e), 0.0, 1.0 );
      d = min( d, dot(b,b) );

      // winding number from http://geomalgorithms.com/a03-_inclusion.html
      bvec3 cond = bvec3( p.y>=v[i].y, p.y<v[j].y, e.x*w.y>e.y*w.x );
      if( all(cond) || all(not(cond)) ) s*=-1.0;
    }
  return s*sqrt(d);
}

mat2 rot2d(in float rads) {
  return mat2(
              cos(rads), -sin(rads),
              sin(rads), cos(rads)
              );
}

vec2 opTx( in vec2 p, in mat2 t) {
  return inverse(t) * p;
}

void main() {
  vec2[N] pointer = vec2[]( vec2(0),
                            vec2(-0.03, 0.1),
                            vec2(0.0, 0.07),
                            vec2(0.03, 0.1),
                            vec2(0)
                            );

  float r = .2;
  float pen_width = 0.01;

  // center of screen
	vec2 p = (2.0 * gl_FragCoord.xy - iResolution.xy) / iResolution.y;
  vec2 m = vec2(1.0, -1.0) * (-(2.0 * iMouse - iResolution.xy) / iResolution.y);

  // build a box
  vec2 boxwh = vec2(.7, .5);
  float box = sdfAnnular(sdfRect(p, vec2(.7, .5)), pen_width);

  // build a title bar for the box
  vec2 titlebarwh = vec2(boxwh.x, .09);
  float titlebar = sdfRect( p + vec2(.0, - boxwh.y + titlebarwh.y), titlebarwh);

  // build a little window
  float titlebox = sdfUnion(box, titlebar);

  // build a cursor
  float cursor = sdfPoly(pointer, opTx(p + m, rot2d(radians(135.))));

  // draw all the things
  float d = sdfUnion(titlebox, cursor);

  // colours, yay!
  vec3 col = vec3(1.0) - sign(d) * vec3(1.0, 1.0, 1.0);

  fragColor = vec4(col, 1.0);
}
