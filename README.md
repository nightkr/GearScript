GearScript
==========

[![Build Status](https://travis-ci.org/teozkr/GearScript.svg?branch=master)](https://travis-ci.org/teozkr/GearScript)

A typed language that compiles to TorqueScript. This type information is also used to make certain parts of the TorqueScript language more consistent.

The language
------------

The documentation is very much a WIP at the moment, but you can see the [Design](Design) directory for some my current design drafts and motivations. In the meantime, try reading the source code. It's implemented using the [Parsec](https://hackage.haskell.org/package/parsec) parser combinator language, which has the benefit of being quite close to [EBNF](http://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_Form) notation.

Usage
-----

GearScript is in no way ready for prime-time yet, but if you want to play around with it anyway then it can be used as follows:

### Installation

```bash
# Initialize Cabal sandbox to keep dependencies separate from your regular Haskell setup
$ cabal sandbox init
# Install GS and dependencies
$ cabal install --jobs
```

### Compile stuff

The CLI is just provisional and will change once the compiler 'works'

```bash
# Compiles Examples/Example1.gs to Examples/Example1.cs
$ gearscript Examples/Example1
```
