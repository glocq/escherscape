Escherscape
============

A small game-ish program that procedurally generates a 3D environment around you as you move.

This is kind of an exercise in Haskell and creative coding:
* The whole world is specified in a declarative manner, by defining a few symbols in a single module. The idea is to allow the programmer to specify *what the world should be*, rather than tell the computer *how it should go about generating it*.
* This was an opportunity for me to get acquainted with Haskell lenses, concurrency with the STM monad, and the Raylib library.
* I try to use idiomatic Haskell, whatever that means. I'm afraid the code may be a little cryptic at times, but it is fairly well documented.

As of now, the program only generates regularly spaced small trees — so, not very interesting —, but the program can be easily modified to generate custom environments with a great deal of freedom (see section *Generating your own worlds* below), which I plan to do.

On my list of things to do:
* Shaders (right now there is no lighting/color variations)
* Interesting world with advanced non-IO based randomness
* Non-boring motion, use of the mouse
* Collision

Building
---------

0. You need to have the `cabal` build tool installed (see https://www.haskell.org/cabal/).
1. Clone this repository: (or alternatively, you can just download the .zip from Github's web interface)
```
git clone https://github.com/glocq/escherscape
```
2. Navigate to the directory:
```
cd path/to/escherscape
```
3. Build and run the program at once:
```
cabal run
```

Using
------

All you can do is move around.

As of now, navigation is (AZERTY) keyboard-based:
* Z/S to move forward/backward,
* Q/D to move left/right,
* Space/Shift to move up/down,
* Left/right arrow to rotate left/right,
* Up/down arrow to look up/down.


Generating your own worlds
---------------------------

Central to this program is the `Chunks` module; this is what you need to edit should you want to define your own worlds. All you need is to make 3D models in e.g. Blender, and then define the three following symbols:
* `ChunkCoordinates`: A type synonym for chunk identifiers (typically, tuples of integers). I think it needs to be a member of `Ord`.
* `visibleChunks :: Vector3 -> Set ChunkCoordinates`: The set of chunks that a player at a given position can see.
* `chunkAt :: ModelMap -> ChunkCoordinates -> Scene`: The chunk located at the given coordinates (needs to be translated accordingly!).

The argument of `chunkAt` of type `ModelMap` is a structure that allows you to access your 3D models. Here's how it works: say you've made a model of a tree in a 3D modeling program.
1. Export your model in the .glb format under `assets/models/tree.glb`.
2. The model automatically gets loaded in a `ModelMap` upon starting the program. The key of the `ModelMap` is just the model's file name with the `.glb` extension removed — here, `tree`.
3. The `ModelMap` is given as argument to `chunkAt` for you to use when defining the function.
4. Your tree model is then available in that `modelMap` as `modelMap ! "tree"`.
