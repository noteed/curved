# Curved - Network metrics service

This project is in early stage.

Curved is a network monitoring package. Its aim it to collect and serve
metrics. It is in particular a drop-in replacement for
[Graphite](http://graphite.readthedocs.org/).

Currently Curved runs on Linux only.

## Install

The development version can be installed by cloning the Git repository and
using cabal:

    > git clone git://github.com/noteed/curved.git
    > cd curved && cabal install

## Features

- Listen on port 2006 (TCP), just like Graphite.
- Listen on port 7002, just like Graphite (i.e. supports the Python Pickle
  binary format).
- Proof-of-concpet web server with D3.js rendering.

## TODO

- Instrument Curved (with EKG).
- The cache-query request is always responding with the empty list.
- Other 7002 queries are not implemented.
- Modularity:
  - Any program should be able to store metrics in cache or to disk using the
    Whisper format, and/or serve them as carbon-cache does, or directly as a
    web server, or push them to Graphite/Curved.
- Turn the Pickle format code into its own package.
- Listen in UPD in addition to TCP.
- Turn the Web server into its own package.
- Turn the Whisper format code into its own package.

