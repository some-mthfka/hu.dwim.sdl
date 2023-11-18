#ifndef _SDL2_wip_h
#define _SDL2_wip_h

#include <SDL2/SDL.h>

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


#endif /* SDL2_wip.h */
