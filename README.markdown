Escherscape
============

A small game-ish program that procedurally generates a 3D environment around you as you move.

This is kind of an exercise in Haskell and creative coding:
* The whole world is specified in a declarative manner, by defining a few symbols in a single module. The idea is to allow the programmer to specify *what the world should be*, rather than tell the computer *how it should go about generating it*.
* In particular, purity and laziness are taken advantage of for random generation.
* This was an opportunity for me to get acquainted with Haskell lenses, concurrency with the STM monad, and the Raylib library.
* I try to use combinators rather than explicit recursion whenever possible, to flex my Haskell muscles.

As of now, the program only generates regularly spaced trees of varying size — so, not very interesting —, but the program can be easily modified to generate custom environments with a great deal of freedom (see section *Generating your own worlds* below), which I plan to do.

On my list of things to do:
* Non-boring motion, use of the mouse
* Less boring shaders (though I like the look of my basic one)
* Collision

Building
---------

### Prerequisites

* You need to have the Haskell toolchain installed. The standard way to do so is
  with [GHCup](https://www.haskell.org/ghcup/).
* If you are on Linux, you will also need to install X11 development packages;
  see https://github.com/Anut-py/h-raylib?tab=readme-ov-file#linux

### Building

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
* `chunkAt :: ModelMap -> ShaderMap -> ChunkCoordinates -> Scene`: The chunk located at the given coordinates (needs to be translated accordingly!).

The argument of `chunkAt` of type `ModelMap` is a structure that allows you to access your 3D models. Here's how it works: say you've made a model of a tree in a 3D modeling program.
1. Export your model in the .glb format under `assets/models/tree.glb`.
2. The model automatically gets loaded in a `ModelMap` upon starting the program. The key of the `ModelMap` is just the model's file name with the `.glb` extension removed — here, `tree`.
3. The `ModelMap` is given as argument to `chunkAt` for you to use when defining the function.
4. Your tree model is then available in that `modelMap` as `modelMap ! "tree"`.

Custom shaders work in a similar way. Say you want to use custom shaders `custom.vert` and/or `custom.frag`.
1. Save your shader(s) under `assets/shaders/custom.vert` and/or `assets/shaders/custom.frag`. **The extensions need to be `.vert` and `.frag`, otherwise your shader will not be loaded!**
2. The second argument of `chunkAt`, of type `ShaderMap`, now contains your shader(s). You can invoke them and assign them to some model `model` as follows: `model |* (shaderMap ! "custom")`

Uniforms are not yet supported.
