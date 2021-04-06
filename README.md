# hu.dwim.sdl

## Fork

https://bugzilla.libsdl.org/show_bug.cgi?id=3219

This is a fork of [hu.dwim.sdl](https://github.com/hu-dwim/hu.dwim.sdl). The details are in the subsections below, but here's the summary:

- `hu.dwim.sdl.ffi` was removed in favour of seperate packages, one for each sdl module.
- Everything is now exported in the generated files.
- The generated names are lispy now.
- Automatic conversion and error checking for _all_ functions.

New dependencies: `cl-ppcre` for regexps and `parachute` for tests.

### Packages

- `hu.dwim.sdl/core` (bindings for SDL2),
- `hu.dwim.sdl/gfx` (bindings for SDL2_gfx),
- `hu.dwim.sdl/ttf` (bindings for SDL2_ttf),
- `hu.dwim.sdl/image` (bindings for SDL_image).
 
### Naming

Instead of `|SDL_Init|` you now simply write `sdl-init`.  See `ffi-name-transformer` in [](source/ffi_prelude.lisp) for details, but basically: 
  - all underscores are replaced with dashes; 
  - camel case is converted to dashes; 
  - Abbreviations are treated nicely (such as `GL`, `GUID`, `RGBA` etc.);
  - all constants and enum members are braced with pluses, e.g. `+have-pow+`;
  - and these table substitutions take place: 
    - `SDL_Log` -> `SDL-LOG`
    - `SDL_log` -> `SDL-LOG*`
    - `SDL_TRUE` -> `TRUE`
    - `SDL_FALSE` -> `FALSE`.
 
I skipped _most_ the functions listed at https://wiki.libsdl.org/ToDo, which includes C function wrappers/equivalents (such `SDL_ceil`, `SDL_sscanf`, `SDL_strupr`, `SDL_strstr`, `SDL_strrchr` and many, many other hissing, stumpy sounds). Some functions I wasn't sure about. Some weren't documented. See `hu.dwim.sdl::*skip/core*` for details. However, I didn't skip the `GameController` functions (they are a draft) as they had documentation (so, they may change here as well).

#### Conventions

I think the following conventions would be good.

##### `<function-name>*`: Convenience functions for passed return values ()

Instead of allocating foreign objects to pass to a function for value return,
make a wrapping to do it for you, e.g. `ttf-size-text` takes a pointers to width
and height variables which it will set, our goal is to make a function
`ttf-size-text*` that does it for us thus eliminating the need for these
arguments, ignores the original return value (which would be checked by our call
to the original function anyway) and returns the width and height instead as
values (in the same order as they come).

```
(with-foreign-objects ((w :int) (h :int))
  (ttf-size-text font text w h)
  (values (mem-aref w :int) (mem-aref h :int)))
```
 
becomes simply

```
(ttf-size-text* font text)
```

There's a macro in [](source/extra-bits.lisp) that makes it easy enough to do:

```
(defun-with-passed-return-values ttf-size-text * * :int :int)
```

But these have to be specified by hand, and the types do too (they shouldn't
really have to be, but I don't know how to get this info from cffi).

You can find/add these definitions in [](source/core-extras.lisp) /
[](source/ttf-extras.lisp) etc. The function is exported too, along with it's
multiple variant, but the idea is to add these definitions to this codebase.
 
Note that the resulting definitions end up in the same package as the original function.

##### `<function-name>*gc

For the garbage collected version of a function. There aren't any GC additions
so far, though. But I think they should be named seperately to avoid any
overhead for those who don't want it.

##### TODO `%<original-function-name>`

It wouldn't be bad if CFFI could generate multiple functions per one C function, or even simply give an option to include the original unmodified function but with a custom name e.g. `sdl-init` and `%sdl-init`. This could be useful when one doesn't want the overhead of conversion or error checking.

### Automatic Conversion and Error checking of Return Values

- NULL signals an error
- certain enum value signals an error
- constant (such as 0) signals an error
- negative number signals an error
- string starts with smth errors
- `SDL_bool` is converted to `t`/`nil`.
- `SDL_bool` is converted to `t`/`nil`, signaling error on `nil`.
- Bool-like functions where 0 means FALSE

`SDL_DequeueAudio` and `SDL_RWwrite` are exceptions: the user passes a number and they return another number, and if the number they return is less than the one you passed, that's an error and you call `sdl-error`. I don't think C2FFI supports this case, but if it does, this will be fixed.

Figuring out if a function does any error checking / conversion is simple: go to its definition and look at the return type. The typedefs for return values are unique for each function with custom expansion to make it possible to set up an expand method individually. These typedefs have the format: `<function name>/<expansion procedure>/<actual type>`. It will be clear from the `<expansion procedure>` what you are dealing with. You shouldn't really have to know about this long type name, the actual type should be enough.

### Example

The following example opens a window and shows a rectangle for two seconds and then closes the window.
 
``` 
(defpackage #:sdl2-example
  (:use :cl)
  (:local-nicknames (sdl hu.dwim.sdl/core)))

(in-package :sdl2-example)

(progn
  (hu.dwim.sdl::defun-with-passed-return-values sdl:sdl-get-window-size * :int :int)
  (sdl:sdl-init sdl:+sdl-init-video+)
  (let* ((window (sdl:sdl-create-window "a flying square"
                                        sdl:+sdl-windowpos-undefined+
                                        sdl:+sdl-windowpos-undefined+
                                        0 0
                                        (logior sdl:+sdl-window-resizable+
                                                sdl:+sdl-window-fullscreen-desktop+)))
         (renderer (sdl:sdl-create-renderer window -1 sdl:+sdl-renderer-accelerated+)))
    (dotimes (time 120)
      ;; clear
      (sdl:sdl-set-render-draw-color renderer 255 255 255 255)
      (sdl:sdl-render-clear renderer)
      (sdl:sdl-set-render-draw-color renderer 0 0 0 255)
      ;; draw first rectangle, using SDL primitives with manual rectangle management
      (cffi:with-foreign-object (rect '(:struct sdl:sdl-rect))
        (cffi:with-foreign-slots ((sdl:x sdl:y sdl:w sdl:h) rect (:struct sdl:sdl-rect))
          (multiple-value-bind (total-x total-y) (sdl:sdl-get-window-size* window)
            (setf sdl:x (- (floor total-x 2) 100)
                  sdl:y (- (floor total-y 2) 100)
                  sdl:w 200
                  sdl:h 200))
          (sdl:sdl-render-draw-rect renderer rect)))
      ;; show
      (sdl:sdl-render-present renderer)
      (sdl:sdl-delay 16))
    (sdl:sdl-destroy-renderer renderer)
    (sdl:sdl-destroy-window window))
  (sdl:sdl-quit))
```

TODO Is there a way to generate annotated struct makers that would show the actual fields? That would require the means of getting the fields of a given struct (if one had to generated these manually).

### Status: beta

I think everything is pretty solid, but there may be errors in judgement when sorting functions in the conversion lists: there could be some stray functions that may not really need error checking, or, on the contrary, may benefit from it. If there's a clear cut case of this and you see it, don't rely on it, it will be considered a bug and will be fixed.

All the breaking changes will be listed in this file.

### Alternative projects

- [cl-sdl2](https://github.com/lispgames/cl-sdl2)
- [cl-sdl2-ttf](https://github.com/Failproofshark/cl-sdl2-ttf)
- [cl-sdl2-gfx](https://github.com/Zulu-Inuoe/cl-sdl2-gfx)
- [sdl2-gfx](https://github.com/mabragor/sdl2-gfx)
- [cl-sdl2-image](https://github.com/lispgames/cl-sdl2-image)

To add to the How section below, the libraries listed above all use
[cl-autowrap](https://github.com/rpav/cl-autowrap) and this library is using
[cffi](https://common-lisp.net/project/cffi/) with its ASDF-integrated c2ffi. They both use c2ffi

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
