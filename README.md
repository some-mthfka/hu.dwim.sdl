# hu.dwim.sdl

## Fork

This is a fork of [hu.dwim.sdl](https://github.com/hu-dwim/hu.dwim.sdl). These are the changes:

- `hu.dwim.sdl.ffi`, where all the bindings lived, was replaced by several packages, one for each SDL2 library:
  - `cl-moar-sdl2.core` (bindings for SDL2),
  - `cl-moar-sdl2.gfx` (bindings for SDL2_gfx),
  - `cl-moar-sdl2.ttf` (bindings for SDL2_ttf),
  - `cl-moar-sdl2.image` (bindings for SDL_image).
- Everything is now exported in the generated files (used to be nothing). Argument names probably don't have to be, but it's a bit problematic (see `ffi-name-export-predicate` in [](source/ffi_prelude.lisp)).
- The generated names are lispy now. Instead of `|SDL_Init|` you now simply write `sdl-init`.  See `ffi-name-transformer` in [](source/ffi_prelude.lisp) for details, but basically: 
  - all underscores are replaced with dashes; 
  - camel case is converted to dashes; 
  - Abbreviations are treated nicely (such as `GL`, `GUID`, `RGBA` etc.);
  - all constants and enum members are braced with pluses, e.g. `+have-pow+`;
  - and these table substitutions take place: 
    - `SDL_Log` -> `SDL-LOG`
    - `SDL_log` -> `SDL-LOG*`
    - `SDL_TRUE` -> `TRUE`
    - `SDL_FALSE` -> `FALSE`.

I skipped _most_ the functions listed at https://wiki.libsdl.org/ToDo, which includes C function wrappers/equivalents (such `SDL_ceil`, `SDL_sscanf`, `SDL_strupr`, `SDL_strstr`, `SDL_strrchr` and many, many other hissing, stumpy sounds). Some functions I wasn't sure about. Some weren't documented. See *skip/core* for details. However, I didn't skip the `GameController` functions (they are a draft) as they had documentation, but you should treat them as a draft here as well.

It shouldn't be too problematic to add support for `sdl2_mixer` or `sdl2_net`, but I didn't bother with the specs.

## What

It's a Common Lisp [FFI](https://en.wikipedia.org/wiki/Foreign_function_interface)
for http://libsdl.org/ (SDL2).

## Why

The alternative projects are partial, while this one uses
[cffi/c2ffi](https://github.com/cffi/cffi) to automatically generate the
CFFI bindings for the various subsystems of [SDL2](http://libsdl.org/).

It only requires vanilla CFFI when used, no extra dependencies.

## Who

Written by [attila@lendvai.name](mailto:attila@lendvai.name).

## Where

The primary communication channel is the facilities on
[the project's GitHub page](https://github.com/hu-dwim/hu.dwim.sdl).

## How

The project uses [CFFI/C2FFI](https://github.com/cffi/cffi).
Its ASDF extension does two things:

1. If needed it can invoke [c2ffi](https://github.com/rpav/c2ffi) to process a C header file
and emit a c2ffi spec file (a json file) that contains every detail needed for a given platform
to generate its FFI. Yours truely has run this phase and checked in the
resulting spec files into the [c2ffi-spec/](c2ffi-spec/) directory, so that
users don't need to have a working c2ffi executable and the SDL dev headers
installed.

2. Based on the spec file it generates the CFFI forms into a lisp file (placed next to the spec file)
and continues as if it was just another lisp file written by hand. (These lisp files
could also be checked in the repo, but for now they are not.)

## Status

It contains a complete FFI for ```sdl.h```, ```sdl-gfx.h```, ```sdl-ttf.h```, and ```sdl-image.h```.
Not much has been added yet to lispify the SDL API, but the CFFI binding part is complete.
