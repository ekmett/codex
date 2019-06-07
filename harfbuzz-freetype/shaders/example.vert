#version 410
in vec4 a; // x0,s0,x1,s1
in vec4 b; // y0,t0,y1,t2
out vec2 texcoord;

void main(void) {
  vec2 xs = ((gl_VertexID & 1) == 1) ? a.zw : a.xy;
  vec2 yt = ((gl_VertexID & 2) == 2) ? b.zw : b.xy;
  texcoord = vec2(xs.y,yt.y);
  gl_Position = vec4(xs.x,yt.x,0,1);
}
