#version 410

layout (binding = 0) uniform sampler2D tex;

layout (binding = 1, std140) uniform Font {
  vec3 pixel;
  mat4 pvm;
}

layout (location = 0) in Input {
  vec3 vertex;
  vec4 color;
  vec2 tex_coord;
  float ashift;
  float agamma;
}

layout (location = 0) out Output {
  vec4 vcolor;
  vec2 vtex_coord;
  float vshift;
  float vgamma;
}

void main() {
  vshift = ashift;
  vgamma = agamma;
  vcolor = color;
  vtex_coord = tex_coord;
  gl_Position = pvm*vec4(vertex,1.0);
}
