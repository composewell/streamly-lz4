# streamly-lz4

This library uses <https://github.com/lz4/lz4.git> to implement streamly
combinators that perform lz4 compression.

This library is inspired by <https://github.com/bigmac2k/lz4-conduit>

To read more about LZ4 go to <https://github.com/lz4/lz4>

## Known Limitations

### Endianness

The library does not specifically consider the endianness while serializing and
deserializing. If the data is compressed on a little-endian machine, it cannot
be decompressed on a big-endian machine and vice-versa.

### Build on Windows using stack

Currently, the library fails to build on windows using stack due to some
limitations of stack itself.

## Running benchmarks

Benchmarks use corpora provided by the canterbury corpus
<https://corpus.canterbury.ac.nz/>

`download-corpora.sh` automatically downloads and unpacks all the required
benchmarks.

You need to run it from the top level directory like so,
```
$ ./download-corpora.sh
$ cabal run bench-lz4 -- --quick
$ cabal run --flag fusion-plugin bench-lz4 -- --quick
```

You can use `--flag fusion-plugin` to build benchmarks with the
[fusion-plugin](https://github.com/composewell/fusion-plugin).

### Benchmarking an external corpus

`BENCH_STREAMLY_LZ4_FILE` is looked up for file to benchmark.  This should
contain the absolute path of the target file.

`BENCH_STREAMLY_LZ4_STRATEGY` is looked up for which benchmarking combinator to
run on the target file.

It can contain values of the following structure,
- `c+speed+bufsize`
- `d+bufsize`
- `r+bufsize`

`c` corresponds to compress, `d` for decompress and `r` for resizing.

Example,
```
$ export BENCH_STREAMLY_LZ4_FILE="path/to/file/"
$ export BENCH_STREAMLY_LZ4_STRATEGY="c+400+640000"
$ cabal bench
```

The commands above will run the compression bench suite on **path/to/file/** with
the acceleration value of **500** read as arrays of size **640000** bytes.

*Note*: For the decompression and resizing bench suite a compressed file is
expected as input.
