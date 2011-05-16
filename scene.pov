#include "colors.inc"

global_settings {
  max_trace_level 256
  radiosity {
  }
}

background { rgb <0, 0.2, 0.4> }

camera {
  location <0, 0, 0>
  look_at <0, 0, 1>
  angle 58
  #include "point-of-view.inc"
}

light_source {
  <0, 10000, 0>
  color 2.0 * White
}

#declare plant = union {
  #include "scene.inc"
}

object { 
  plant
  scale < 1.0, 1.0,-1.0>
}

plane {
  y, -1
  texture {
    pigment { rgb <0.2, 0.15, 0.0 > }
    finish { ambient 0.5 }
  }
}
