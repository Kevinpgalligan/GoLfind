## GoLfind
Finding Game of Life states that eventually turn into a picture, such as the Mona Lisa.

Write-up: https://kevingal.com/blog/mona-lisa-gol.html

![demonstration of Life transforming into Mona Lisa's face](https://github.com/Kevinpgalligan/MonaLisaGoL/blob/master/mona.gif)

Comes with code for simulating Life, doing backsearch (i.e. finding the parent state of a Life state) and creating GIFs / animations of the Life simulation. Even if you haven't used Common Lisp before, it should be straightforward to follow the examples (below) and play around with it.

I created [a tool](https://kevingal.com/apps/pixelate.html) for converting images to black & white, which might come in handy if you want to Life-ify your own pictures.

## Setup
#### Step 1: prerequisites
You'll need:
* An implementation of Common Lisp.
* Quicklisp (the Common Lisp package manager).

If you don't have these, the fastest way to get started is to install [Portacle](https://portacle.github.io/), which comes with everything you need.

##### Step 2: download
Clone this repository into your Quicklisp local-projects folder. For Portacle users, this should be `~/portacle/all/quicklisp/local-projects/`.

```
$ cd ~/portacle/all/quicklisp/local-projects/
$ git clone https://github.com/Kevinpgalligan/GoLfind.git
```

##### Step 3: manual sketch installation
The [sketch](https://github.com/vydd/sketch) library requires a manual installation step, unfortunately. You'll need to install some SDL2-related dependencies, as described in its README. Shortcut for Debian users: `sudo apt-get install libsdl2-dev libsdl2-ttf-dev libsdl2-image-dev`.

##### Step 4: load
From within your REPL, run `(ql:quickload 'golfind)`. You should now be able to play with the examples below.

If anything doesn't work or you're confused by these instructions, please let me know and I'll try to help.

## Usage examples
From the REPL...

```common-lisp
CL-USER> (in-package golfind)
#<PACKAGE "GOLFIND">
GOLFIND> (defparameter custom-life
    (life-from-lists (list (list LIVE DEAD)
                     (list DEAD LIVE))))
CUSTOM-LIFE
GOLFIND> custom-life
#S(LIFE :GRID #2A((T NIL) (NIL T)) :ROWS 2 :COLS 2)
GOLFIND> (defparameter some-life (random-life 4 4))
SOME-LIFE
GOLFIND> some-life
#S(LIFE
   :GRID #2A((T T NIL NIL) (NIL T NIL T) (NIL T T NIL) (NIL T T T))
   :ROWS 4
   :COLS 4)
GOLFIND> (life-get-cell some-life 0 1)
T
GOLFIND> (life-get-cell some-life 0 2)
NIL
GOLFIND> (defparameter next-life (life-next-state some-life))
NEXT-LIFE
GOLFIND> next-life
#S(LIFE
   :GRID #2A((NIL NIL NIL NIL)
             (NIL NIL NIL T)
             (NIL NIL NIL NIL)
             (NIL NIL NIL T))
   :ROWS 4
   :COLS 4)
GOLFIND> (find-life-parent-state next-life)
[...trimmed MiniSAT output...]
Memory used           : 7.43 MB
CPU time              : 0.024131 s

SATISFIABLE
[...trimmed MiniSAT output...]
#S(LIFE
   :GRID #2A((NIL NIL T NIL) (NIL NIL NIL NIL) (T NIL NIL T) (NIL NIL NIL NIL))
   :ROWS 4
   :COLS 4)
T
GOLFIND> ; the parent we found is different to some-life, next-life has multiple parents
; No value
GOLFIND> ; run an animation
GOLFIND> (run-life some-life :pixels-per-cell 50 :frames-per-state 20)
#<LIFE-ANIMATE {10036EE1D3}>
GOLFIND> ; load a black & white image as a Life state
GOLFIND> (defparameter pic-as-life (load-png-as-life "/home/kevin/Imágenes/gol.png"))
PIC-AS-LIFE
GOLFIND> pic-as-life
#S(LIFE
   :GRID #2A((NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
              [...insert big-ass array here...]
   :ROWS 348
   :COLS 348)
GOLFIND> ; save it as a gif
GOLFIND> (life-to-gif pic-as-life "~/mygif.gif" :num-states 5 :pixels-per-cell 3)
#P"/home/kevin/mygif.gif"
```
