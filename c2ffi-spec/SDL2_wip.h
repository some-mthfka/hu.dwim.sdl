/*
SDL2_wip.h: Some basic geometric primatives written using SDL2's 
RenderGeometry function

Inspired by the work by:
Copyright (C) 2012-2014  Andreas Schiffler
Additions for BBC BASIC (C) 2016-2020 Richard Russell

This software is provided 'as-is', without any express or implied
warranty. In no event will the authors be held liable for any damages
arising from the use of this software.

Permission is granted to anyone to use this software for any purpose,
including commercial applications, and to alter it and redistribute it
freely, subject to the following restrictions:

1. The origin of this software must not be misrepresented; you must not
claim that you wrote the original software. If you use this software
in a product, an acknowledgment in the product documentation would be
appreciated but is not required.

2. Altered source versions must be plainly marked as such, and must not be
misrepresented as being the original software.

3. This notice may not be removed or altered from any source
distribution.

Andreas Schiffler -- aschiffler at ferzkopp dot net
Richard Russell -- richard at rtrussell dot co dot uk
Avon -- avon61002 at g mail dot com

*/
#ifndef _SDL2_wip_h
#define _SDL2_wip_h

#include <math.h>
#ifndef M_PI
#define M_PI 3.1415926535897932384626433832795
#endif

#include <SDL2/SDL.h>

#ifdef __cplusplus
extern "C"
{
#endif

  int point (SDL_Renderer *renderer, float x, float y, Uint8 r, Uint8 g,
             Uint8 b, Uint8 a);
  int lineRaw (SDL_Renderer *renderer, float x1, float y1, float x2, float y2,
               Uint8 r, Uint8 g, Uint8 b, Uint8 a);
  int linesRaw (SDL_Renderer *renderer, float *vx, float *vy,
                Sint16 num_points);
  int rectRaw (SDL_Renderer *renderer, float x1, float y1, float x2, float y2,
               Uint8 r, Uint8 g, Uint8 b, Uint8 a);
  int rectsRaw (SDL_Renderer *renderer, float *vx, float *vy,
                Sint16 num_points);
  int filledRectRaw (SDL_Renderer *renderer, float x1, float y1, float x2,
                     float y2, Uint8 r, Uint8 g, Uint8 b, Uint8 a);
  int filledRectsRaw (SDL_Renderer *renderer, float *vx, float *vy,
                      Sint16 num_points, Uint8 r, Uint8 g, Uint8 b, Uint8 a);

  int hLine (SDL_Renderer *renderer, float x1, float y, float x2,
             float thickness, Uint8 cap_type, Uint8 r, Uint8 g, Uint8 b,
             Uint8 a);
  int vLine (SDL_Renderer *renderer, float x, float y1, float y2,
             float thickness, Uint8 cap_type, Uint8 r, Uint8 g, Uint8 b,
             Uint8 a);
  int line (SDL_Renderer *renderer, float x1, float y1, float x2, float y2,
            float thickness, Uint8 cap_type, Uint8 r, Uint8 g, Uint8 b,
            Uint8 a);
  int _polylineSegment (SDL_Renderer *renderer, float x1, float y1, float x2,
                        float y2, float x3, float y3, float thickness,
                        Uint8 joint_type, Uint8 cap_type, Uint8 start_cap,
                        Uint8 end_cap, Uint8 r, Uint8 g, Uint8 b, Uint8 a);
  int polyline (SDL_Renderer *renderer, float *vx, float *vy, Sint16 num_points,
                float thickness, Uint8 joint_type, Uint8 cap_type, Uint8 r,
                Uint8 g, Uint8 b, Uint8 a);
  int aaRect (SDL_Renderer *renderer, float x1, float y1, float x2, float y2,
              float stroke_thickness, Uint8 joint_type, Uint8 stroke_r,
              Uint8 stroke_g, Uint8 stroke_b, Uint8 stroke_a, Uint8 filled,
              Uint8 fill_r, Uint8 fill_g, Uint8 fill_b, Uint8 fill_a);
  int regularPolygon (SDL_Renderer *renderer, float xc, float yc, float rad,
                      Sint16 num_sides, float stroke_thickness,
                      Uint8 joint_type, Uint8 stroke_r, Uint8 stroke_g,
                      Uint8 stroke_b, Uint8 stroke_a, Uint8 filled,
                      Uint8 fill_r, Uint8 fill_g, Uint8 fill_b, Uint8 fill_a);
  int convexPolygon (SDL_Renderer *renderer, float *vx, float *vy,
                     Sint16 num_points, float stroke_thickness,
                     Uint8 joint_type, Uint8 stroke_r, Uint8 stroke_g,
                     Uint8 stroke_b, Uint8 stroke_a, Uint8 filled, Uint8 fill_r,
                     Uint8 fill_g, Uint8 fill_b, Uint8 fill_a);
  int polygon (SDL_Renderer *renderer, float *vx, float *vy, Sint16 num_points,
               float stroke_thickness, Uint8 joint_type, Uint8 stroke_r,
               Uint8 stroke_g, Uint8 stroke_b, Uint8 stroke_a, Uint8 filled,
               Uint8 fill_r, Uint8 fill_g, Uint8 fill_b, Uint8 fill_a);

  int arc ();
  int bezier ();
  int bSpline ();
  int NUBS ();
  int hermite ();
  int catmullRom ();

  int circle ();
  int ellipse ();
  int polyBezier ();

#ifdef __cplusplus
}
#endif

#endif /* SDL2_wip.h */
