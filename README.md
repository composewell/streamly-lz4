# streamly-lz4

This library uses <https://github.com/lz4/lz4.git> to implement streamly
combinators that perform lz4 compression.

This library is inspired by <https://github.com/bigmac2k/lz4-conduit>

To read more about LZ4 go to <https://github.com/lz4/lz4>

## Installation

This library comes with a `default.nix` which can be used to setup the
environment. Using `default.nix` is highly recommended.

Currently, `streamly` is pulled from master at
`9fefed37d5da5aa103f93ded65b1335903651e26`. Until a newer version is released,
this package cannot be released on hackage.

To install this package properly, you might need add the following to your
`cabal.project` file,
```
...

source-repository-package
    type: git
    location: https://github.com/composewell/streamly
    tag: 9fefed37d5da5aa103f93ded65b1335903651e26

source-repository-package
    type: git
    location: https://github.com/composewell/streamly-lz4
    tag: <latest commit of streamly-lz4>

...
```

If you're using stack then you'll need to add these packages to your `extra-deps`,
```
...

extra-deps:
- github: composewell/streamly
  commit: 9fefed37d5da5aa103f93ded65b1335903651e26
- github: composewell/streamly-lz4
  commit: <latest commit of streamly-lz4>

...
```

## Running benchmarks

Benchmarks use corpora provided by the canterbury corpus
<https://corpus.canterbury.ac.nz/>

`download-corpora.sh` automatically downloads and unpacks all the required
benchmarks.

You need to run it from the top level directory like so,
```
$ ./download-corpora.sh
```

You can use `-f fusion-plugin` to build benchmarks with the
[fusion-plugin](https://github.com/composewell/fusion-plugin).
