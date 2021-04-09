# hu.dwim.sdl

## Fork

This is a fork of [hu.dwim.sdl](https://github.com/hu-dwim/hu.dwim.sdl). The
details are in the subsections below, but here's the summary:

- `hu.dwim.sdl.ffi` was removed in favour of seperate packages, one for each sdl module.
- Everything is now exported in the generated files.
- The generated names are lispy now.
- Automatic boolean conversion and error checking: for _all_ functions.

New dependencies: `cl-ppcre` for regexps and `parachute` for some tests.

Also, now, the generated files call stuff from the `hu.dwim.sdl` package. I
don't think this to be a big deal, though.

### Packages

- `hu.dwim.sdl/core` (bindings for SDL2),
- `hu.dwim.sdl/gfx` (bindings for SDL2_gfx),
- `hu.dwim.sdl/ttf` (bindings for SDL2_ttf),
- `hu.dwim.sdl/image` (bindings for SDL_image).
 
### Naming

Instead of `|SDL_CreateWindow|` you now simply write `create-window`.  See
`ffi-name-transformer` in [name-translation](source/name-translation.lisp) for details, but
basically:
  - all underscores are replaced with dashes; 
  - camel case is converted to dashes; 
  - Abbreviations are treated nicely (such as `GL`, `GUID`, `RGBA` etc.);
  - all constants and enum members are braced with pluses, e.g. `+have-pow+`;
  - and these table substitutions take place: 
    - `SDL_Log` -> `LOG`
    - `SDL_log` -> `LOG*`
  - `SDL_`, `TTF_`, `GFX_`, `IMG_` prefixes were removed.
 
#### Conventions

I think the following conventions would be good.

##### `<function-name>*`: Convenience functions for passed return values

Instead of allocating foreign objects to pass to a function for value return,
make a wrapping to do it for you, e.g. `ttf-size-text` takes a pointers to width
and height variables which it will set, our goal is to make a function
`ttf-size-text*` that does it for us, eliminating the need for these arguments:

```
(with-foreign-objects ((w :int) (h :int))
  (ttf-size-text font text w h)
  (values (mem-ref w :int) (mem-ref h :int)))
```
becomes simply

```
(ttf-size-text* font text)
```

For the list of functions for which this is done (and for a place to add more)
see [passed-return-value-lists.lisp](source/passed-return-value-lists.lisp).

##### `make-<struct-name>` and `make*<struct-name>`: construction macros

In place of `cffi:foreign-alloc` + cffi:with-foreign-slots and `setf`'ing the
slots yourself, you can just do this:

```
(let ((r (sdl:make-rect :x 0 :y 0 :w 10 :h 10)))
  ;; ...
  (cffi:foreign-free r))
```
or this, if you want to leave some slots uninitialized:
 
```
(let ((r (sdl:make*rect)))
  ;; ...
  (cffi:foreign-free r))
```

##### `with-<struct-name>` and `with*<struct-name>`: scoped construction macros

The following are like the make constructs above, but `cffi:foreign-free` is
done automatically:
 
```
(sdl:with-rect (r :x 0 :y 0 :w 10 :h 10)
  ;; ...
  )
```

```
(sdl:with*rect (r)
  ;; ...
  )
```
 
##### `with-<struct-name>*` and `with*<struct-name>*`

These are just like `with-<struct-name>` and `with*<struct-name>` except they
are for multiple forms:

``` 
(sdl:with-point* ((a :x 0 :y 0)
                  (b :x 5 :y 5))
  ;; ...
  )
```

##### `with-sdl-slots`

This macro lets you easily access slots of a struct. Here's an example:

```
(defpackage #:sdl2-example
  (:use :cl)
  (:local-nicknames (sdl hu.dwim.sdl/core))
  (:import-from #:hu.dwim.sdl
                #:with-sdl-slots))

(in-package :sdl2-example)

(sdl:with-point (a :x 0 :y 5)
  (with-sdl-slots ((x y) a (:struct rect))
    (values x y)))
```

With `cffi:with-foreign-slots` you would have to either do this:
 
```
(sdl:with-point (a :x 0 :y 5)
  (cffi:with-foreign-slots ((sdl:x sdl:y) a (:struct sdl:rect))
    (values sdl:x sdl:y)))
```

or import `x`, `y` and `rect` to access them here without the package-resolving
prefix.

In contrast, `with-sdl-slots` finds `rect` in one of the sdl packages at
macroexpansion time, places `x` and `y` into the package where `rect` was found
and sets up a `symbol-macrolet` to replace `x` with `hu.dwim.sdl/core:x` and `y`
with `hu.dwim.sdl/core:y`, while internally still using
`cffi:with-foreign-slots`.

##### **TODO** `%<original-function-name>`

It wouldn't be bad if CFFI could generate multiple functions per one C function,
or even simply give an option to include the original unmodified function but
with a custom name e.g. `init` and `%init`. This could be useful when one
doesn't want the overhead of conversion or error checking. Alternatively,
another file could be generated with its own CFFI options.
 
##### (maybe) `<function-name>-gc`

With `<function-name>*gc` standing for a garbage collected version of `<function-name>*`.

There aren't any GC additions so far.

Name clashes with new SDL functions: very unlikely, I think.
 
### Automatic Conversion and Error checking of Return Values

Functions in SDL2 return error codes, and there are several types.  They aren't
indicated in any way, other than on wiki, and that's what I did. Some were easy
to pick with regexps, others I looked through manually.  The assortment is
exhaustive and is meant to stay that way.

It should have been easier has [this
issue](https://bugzilla.libsdl.org/show_bug.cgi?id=3219) been resolved, filed
for the exact same purpose of this library, but it hasn't been.

The following expansions of return values take place:

- NULL signals an error
- certain enum value signals an error
- constant (such as 0) signals an error
- negative number signals an error
- string starts with something indicating an error
- bool-like value like `SDL_bool` and where `0` indicates false are converted to
  `nil` on `0`/`SDL_FALSE` and `t` otherwise.
- same as above, but where false signals an error
 
These are fairly easy to add should there be more cases.

The values are expanded based on the actual return type of the function - the
expansion methods are defined at the time of return type name conversion and the
code with custom type details is inserted into the generated file, right before
the function definiton in question.

Each function has it's own error condition: `<function-name>-error` and it is
exported.  All error conditions inherit from `hu.dwim.sdl:sdl-error`. The actual
errors are quite informative, naming the return value, its type, and showing
the `SDL_GetError` output.

Finding out if a function does any error checking / conversion is simple: go to
its definition and look at the return type. The typedefs for return values are
unique for each function with custom expansion to make it possible to set up an
expand method individually. These typedefs have the format: `<function
name>/<expansion procedure>/<actual type>`. It will be clear from the
`<expansion procedure>` what you are dealing with. Otherwise you shouldn't
really have to know or care about this long type name, the actual type should be
enough.

Another way is to go to
[type-conversion-lists](source/type-conversion-lists.lisp) and see for yourself.
 
I skipped _most_ of the functions listed at https://wiki.libsdl.org/ToDo, which
includes C function wrappers/equivalents (e.g. `SDL_sscanf`). Some functions I
wasn't sure about. Some weren't documented. See `hu.dwim.sdl::*skip/core*` for
details. However, I didn't skip the `GameController` functions (they are a
draft) as they had documentation.

Also, `SDL_DequeueAudio` and `SDL_RWwrite` are exceptions: the user passes a
number and they return another number, and if the number returned is less than
the one passed, that's an error and the user calls `SDL_GetError`. I don't think
C2FFI supports this case. Anyway, I redefined these manually with error checks
in [core-extras](core-extras.lisp). I haven't tested them and, in fact, the
current specs are out of date and don't have them at all. Unmodified versions
prefixed with % should be available for these.

**There are many functions, some were picked with regexps, others were picked by
hand, some could've been misjudged: there could have been errors. If something
doesn't work, always make sure to go and check the generated definition along
with the relevant SDL documentation.**

As a precaution, if you generate new specs with new functions in the sense that
they aren't in the conversion lists, you will get a warned about them, and if
those functions have unknown abbreviations, you will be told about that as well.

### Example

The following example opens a window and shows a rectangle and then, after two
seconds, closes the window.
 
``` 
(defpackage #:sdl2-example
  (:use :cl)
  (:local-nicknames (sdl hu.dwim.sdl/core)))

(in-package :sdl2-example)

(progn
  (sdl:init sdl:+init-video+)
  (let* ((window (sdl:create-window "a square"
                                    sdl:+windowpos-undefined+
                                    sdl:+windowpos-undefined+
                                    0 0
                                    (logior sdl:+window-resizable+
                                            sdl:+window-fullscreen-desktop+)))
         (renderer (sdl:create-renderer window -1 sdl:+renderer-accelerated+)))
    (dotimes (time 120)
      ;; clear
      (sdl:set-render-draw-color renderer 255 255 255 255)
      (sdl:render-clear renderer)
      ;; draw a rectangle, using SDL primitives with manual rectangle management
      (sdl:set-render-draw-color renderer 0 0 0 255)
      (multiple-value-bind (total-x total-y) (sdl:get-window-size* window)
        (sdl:with-rect (rect :x (- (floor total-x 2) 100)
                             :y (- (floor total-y 2) 100)
                             :w 200
                             :h 200)
          (sdl:render-draw-rect renderer rect)))
      ;; show
      (sdl:render-present renderer)
      (sdl:delay 16))
    (sdl:destroy-renderer renderer)
    (sdl:destroy-window window))
  (sdl:quit))
```

### Status: beta

**WIP** Things aren't very stable at the moment, API is not frozen.

I think everything is pretty solid, but there may be errors in judgement about
the functions in the conversion lists: there could be some stray functions that
may not really need error checking or conversion, or, on the contrary, those
that may. If there's a clear cut case of such an error that you find, don't rely
on it, it will be likely considered a bug and will be fixed. So, please, report
dubious cases.

All the breaking changes will be listed in this file.

#### Problems

- The specs in the repo are out of date by 5 years. I didn't figure out how to
  regenerate them myself, cffi was giving me some pesky error, while the `c2ffi`
  utility worked fine if I supplied `/usr/include` to it, but my attempts to do
  this within the asdf definition were short-lived.
- Definitions for `SDL_DequeueAudio` and `SDL_RWwrite` were done manually to add
  special cases of error checking (see above about them), and I didn't test
  them. The original definitions should be available with % prefix, though.
- This fork relies on support prologue code pull request being merged in
  `cffi`. The change is only a few lines of code and shouldn't be a problem.

### Alternative projects

- [cl-sdl2](https://github.com/lispgames/cl-sdl2)
- [cl-sdl2-ttf](https://github.com/Failproofshark/cl-sdl2-ttf)
- [cl-sdl2-gfx](https://github.com/Zulu-Inuoe/cl-sdl2-gfx)
- [sdl2-gfx](https://github.com/mabragor/sdl2-gfx)
- [cl-sdl2-image](https://github.com/lispgames/cl-sdl2-image)

To add some details to the *How* section below, the libraries listed above all
use [cl-autowrap](https://github.com/rpav/cl-autowrap) and this library is using
[cffi](https://common-lisp.net/project/cffi/) with its ASDF-integrated
c2ffi. They both use the `c2ffi` utility to generate the specs, but to the best
of my knowledge, `cffi` has these advantages:
- it generates a file which you can "goto definition", while `cl-autowrap`
  provides a macro, which isn't so easy to work with, and
- it provides a way to build compiler-macro return value expansions (used
  extensively in this project for return value conversion and checking).

## What

It's a Common Lisp [FFI](https://en.wikipedia.org/wiki/Foreign_function_interface)
for http://libsdl.org/ (SDL2).

## Why

The alternative projects are partial, while this one uses
[cffi/c2ffi](https://github.com/cffi/cffi) to automatically generate
the complete CFFI bindings for the various subsystems of
[SDL2](http://libsdl.org/).

It only requires vanilla CFFI when used, no extra dependencies.

## Who

Written by [attila@lendvai.name](mailto:attila@lendvai.name).

## Where

The primary communication channel is the facilities on
[the project's GitHub page](https://github.com/hu-dwim/hu.dwim.sdl).

## How

This project uses [CFFI/C2FFI](https://github.com/cffi/cffi), whose
ASDF extension does two things:

1. When needed, it can invoke [c2ffi](https://github.com/rpav/c2ffi)
to process a C header file and emit a c2ffi spec file (a json file)
that contains every detail needed to generate an FFI for a given
platform. But yours truely has run this phase, and checked in the
resulting spec files into the [c2ffi-spec/](c2ffi-spec/)
directory. This way users don't need to have a working c2ffi
executable, nor the SDL dev headers installed.

2. Based on the above mentioned spec file, it generates the CFFI forms
into a lisp file (placed next to the spec file), and continues as if
it was just another lisp file written by hand. (These lisp files could
also be committed into the repo, but for now they are not, because
their regeneration is automatic and painless.)

## Status

It contains a complete FFI for `sdl.h`, `sdl-gfx.h`, `sdl-ttf.h`, and `sdl-image.h`.
Not much has been added yet to lispify the SDL API, but the CFFI binding part is complete.
