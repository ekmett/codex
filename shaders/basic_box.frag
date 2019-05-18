#version 130

uniform vec2 iResolution = vec2(640.,480.);

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

void main() {

  // not quite internalised this yet
  // left  = +x
  // right = -x
  // up    = -y
  // down  = +y

  float r = .2;
  float pen_width = 0.01;

  // center of screen
  vec2 p = -1.0 + 2.0*gl_FragCoord.xy / iResolution.xy;
  p.x *= iResolution.x / iResolution.y;

  // build a box
  vec2 boxwh = vec2(.7, .5);
  float box = sdfAnnular(sdfRect(p, vec2(.7, .5)), pen_width);

  // build a title bar for the box
  vec2 titlebarwh = vec2(boxwh.x, .09);
  float titlebar = sdfRect( p + vec2(.0, - boxwh.y + titlebarwh.y), titlebarwh);

  // union shapes!
  float d = sdfUnion(box, titlebar);

  // colours, yay!
  vec3 col = vec3(1.0) - sign(d) * vec3(0.1, 0.4, 0.7);

  gl_FragColor = vec4(col, 1.0);
}
