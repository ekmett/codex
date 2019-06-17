#version 410
uniform sampler2D tex;
in vec2 texcoord;
out vec4 color;
void main(void) {
  color = texture(tex,texcoord).rrrr;
}
