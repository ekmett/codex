vec4 texture2DRect (sampler2D sampler, float size, vec2 pixel) {
  float x = (0.5 + pixel.x)/size;
  float y = (0.5 + pixel.y)/size;
  return texture2D(sampler,vec2(x,y));
}

float texture2DAs1D (sampler2D sampler, float size, float idx) {
  float el   = floor(idx/4.0);
  float comp = idx - el*4.0;
  float y    = floor(el/size);
  float x    = el - y*size;
  vec4  val  = texture2DRect(sampler, size, vec2(x,y));

  if      (comp == 0.0) { return val.r; }
  else if (comp == 1.0) { return val.g; }
  else if (comp == 2.0) { return val.b; }
  else if (comp == 3.0) { return val.a; }
  else                  { return -1.0;  }
}

void main() {

  vec2 p = local.xy;
  float s = 9999.0;
  vec4 red   = rgb2lch(vec4(1.0,0.0,0.0,1.0));
  vec4 white = rgb2lch(vec4(1.0));
  vec4 cd = white;

  p -= vec2(spread, spread);

  //p -= vec2(1.0);
  vec2 origin = vec2(0.0);
  float idx = floor(offset+0.5);
  bool isInside = false;

  for (float i=0.0; i<1000.0; i++) {
    if (idx>=nextOffset) { break; }
    float cmd = texture2DAs1D(commands, size, idx);
    if        (cmd == 0.0) { break;

    } else if (cmd == 1.0) { // Move
      idx++; float dx = texture2DAs1D(commands, size, idx);
      idx++; float dy = texture2DAs1D(commands, size, idx);
      p -= vec2(dx,dy) - origin;
      origin = vec2(dx,dy);

    } else if (cmd == 2.0) { // Line
      idx++; float tx = texture2DAs1D(commands, size, idx);
      idx++; float ty = texture2DAs1D(commands, size, idx);
      vec2 tp = vec2(tx,ty);
      tp -= origin;
      origin += tp;
      float ns       = sdf_quadraticCurve           (p, tp, tp);
      bool  interior = quadraticCurve_interiorCheck (p, tp, tp);
      isInside = interiorChec_union(isInside, interior);
      s = sdf_union(s,ns);
      p -= tp;

    } else if (cmd == 3.0) { // Quadratic Curve
      idx++; float cx = texture2DAs1D(commands, size, idx);
      idx++; float cy = texture2DAs1D(commands, size, idx);
      idx++; float tx = texture2DAs1D(commands, size, idx);
      idx++; float ty = texture2DAs1D(commands, size, idx);
      vec2 cp = vec2(cx,cy);
      vec2 tp = vec2(tx,ty);
      cp -= origin;
      tp -= origin;
      origin += tp;
      float ns       = sdf_quadraticCurve           (p, cp, tp);
      bool  interior = quadraticCurve_interiorCheck (p, cp, tp);
      isInside = interiorChec_union(isInside, interior);
      s = sdf_union(s,ns);
      p -= tp;
    }
    idx++;
  }

  if (isInside) { s = -s; }
  float d = s/(2.*spread) + .5;
  //d = s/spread;
  gl_FragColor = vec4(vec3(d),1.0);
  //return sdf_shape(d, 0, vec4(0.0), cd);
}
