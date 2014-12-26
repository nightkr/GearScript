GearScript
==========

[![Build Status](https://travis-ci.org/teozkr/GearScript.svg?branch=master)](https://travis-ci.org/teozkr/GearScript)

A typed language that compiles to TorqueScript. This type information is also used to make certain parts of the TorqueScript language more consistent. See the [Design](Design) directory for more detailed specs and motivations.

Usage
-----

GearScript is in no way ready for prime-time yet, but if you want to play around with it anyway then it can be used as follows:

### Install

```bash
# Initialize Cabal sandbox to keep dependencies separate from your regular Haskell install
$ cabal sandbox init
# Install GS and dependencies
$ cabal install --jobs
```