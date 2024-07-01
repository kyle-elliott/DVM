# DVM64: A Symbolic Execution Engine

DVM64 is a C# project that serves as a symbolic execution engine, translating AMD64 instructions into Z3 expressions. It leverages various libraries such as Iced.Intel and Microsoft.Z3 to achieve this.

## Overview

Project is relatively simple to run against whatever you want, just set the appropriate path and relative address to what you want to analyze and run. It resonably emulates memory operations, reading from the binary, arithmetic, flags, stack read and write, both full sized and partials.

## Inspiration

I was originally inspired by a friend who wanted to remain anonymous who worked on a project for VMP2 and decided to try my hand against VMP3 using the approach shown in this project.
Huge shoutout to [Nitr0-G](https://github.com/Nitr0-G/Rework-part-of-z3-proving) and his project here which is where to concept came from.

## Problems

The usual sorts of problems that you have in a trace-based devirtualization effort with some bonus ones
- (Probably) incorrect handeling of instruction semantics
- Path explosion on non-opaque branches (never got to far enough into this to bother writing logic for it)
- Actually compiling back to something useful from Z3 AST
- ??? Probably some other things

## Shouts
- Colton
- NaC-L
- Brainlets
