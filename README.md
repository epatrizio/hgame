# hgame - A sample game in Haskell language

This project is a video game written in Haskell.
This is a minimalist implementation of a fighting game inspired by the famous Street Fighter.\
In this context, the objective is educational by practicing functional programming in "Haskelian" style!

*This project is part of my professional training.*

## Technical environment

* [Haskell language](https://www.haskell.org)
* [Stack](https://haskellstack.org), the cross-platform program for developing Haskell projects
  * For a new project, the command *'stack new projectName --resolver lts-18.24'* initialize a complete structure
* [Hspec](https://hspec.github.io), a testing framework
* [QuickCheck](https://hackage.haskell.org/package/QuickCheck), an automatic testing environment
* [SDL2](https://hackage.haskell.org/package/sdl2), SDL library binding in Haskell
  * [SDL](https://www.libsdl.org), Simple DirectMedia Layer

## Execution and play ;)

The Makefile contains all the stack commands (clean, build, run and tests).

To start fighting (2 players), it's very easy!
Run the following command and follow the instructions on the console.\
Have fun!

```console
make run
```
