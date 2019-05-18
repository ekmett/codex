const int SAMPLENUM = 1;

float foo (float a, float d) {
  float smoothing = 1.0/fontSize;
  return 1. - smoothstep(0.5 - smoothing, 0.5 + smoothing, (a - d));
}

float multisample_gaussian3x3 (mat3 arr, float d) {
  return foo(arr[0][0], d) * 0.011
    + foo(arr[0][1], d) * 0.084
    + foo(arr[0][2], d) * 0.011
    + foo(arr[1][0], d) * 0.084
    + foo(arr[1][1], d) * 0.62
    + foo(arr[1][2], d) * 0.084
    + foo(arr[2][0], d) * 0.011
    + foo(arr[2][1], d) * 0.084
    + foo(arr[2][2], d) * 0.011;
}

void main() {
  float smoothing = zoom * 1.0/fontSize;

  float dx = 1./2048.;
  float dy = 1./2048.;

  vec2 uv2 = uv;

  uv2.x /= (glyphsTextureSize / glyphLoc[2]); // width
  uv2.y /= (glyphsTextureSize / glyphLoc[3]); // width
  uv2.x += glyphLoc.x / glyphsTextureSize;
  uv2.y += glyphLoc.y / glyphsTextureSize;

  //uv2.x += 0.1;
  vec4 img = texture2D(glyphsTexture, uv2);
  //float s = img.r;

  vec4 red   = rgb2lch(vec4(1.0,0.0,0.0,1.0));
  vec4 white = rgb2lch(vec4(1.0));

  vec4 cd = white;

  float realZoom = glyphZoom * zoom; // texture is scaled for tests!

  mat3 samples;

  samples[0][0] = texture2D(glyphsTexture, vec2(uv2.x + dx * (-1.*realZoom/2.), uv2.y + dy * (-1.*realZoom/2.))).r;
  samples[0][1] = texture2D(glyphsTexture, vec2(uv2.x + dx * ( 0.*realZoom/2.), uv2.y + dy * (-1.*realZoom/2.))).r;
  samples[0][2] = texture2D(glyphsTexture, vec2(uv2.x + dx * ( 1.*realZoom/2.), uv2.y + dy * (-1.*realZoom/2.))).r;
  samples[1][0] = texture2D(glyphsTexture, vec2(uv2.x + dx * (-1.*realZoom/2.), uv2.y + dy * ( 0.*realZoom/2.))).r;
  samples[1][1] = texture2D(glyphsTexture, vec2(uv2.x + dx * ( 0.*realZoom/2.), uv2.y + dy * ( 0.*realZoom/2.))).r;
  samples[1][2] = texture2D(glyphsTexture, vec2(uv2.x + dx * ( 1.*realZoom/2.), uv2.y + dy * ( 0.*realZoom/2.))).r;
  samples[2][0] = texture2D(glyphsTexture, vec2(uv2.x + dx * (-1.*realZoom/2.), uv2.y + dy * ( 1.*realZoom/2.))).r;
  samples[2][1] = texture2D(glyphsTexture, vec2(uv2.x + dx * ( 0.*realZoom/2.), uv2.y + dy * ( 1.*realZoom/2.))).r;
  samples[2][2] = texture2D(glyphsTexture, vec2(uv2.x + dx * ( 1.*realZoom/2.), uv2.y + dy * ( 1.*realZoom/2.))).r;

  float s = pow(multisample_gaussian3x3(samples, realZoom/150.0),realZoom/2.);

  float alpha = color.a * (1. - smoothstep(0.5 - smoothing, 0.5 + smoothing, img.r));
  //float alpha = s;
  gl_FragColor = vec4(color.rgb, alpha);
  //gl_FragColor = vec4((img.rgb - 0.5)*2.0, 1.0);
}
