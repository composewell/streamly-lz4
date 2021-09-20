# streamly-lz4

This library uses the LZ4 compression algorithm
<https://github.com/lz4/lz4> to compress and decompress a data stream
using [Haskell Streamly](https://streamly.composewell.com/).

## Running benchmarks

`download-corpora.sh` downloads and unpacks the [caterbury
corpus](https://corpus.canterbury.ac.nz/) used for benchmarking.

Run the following commands from the top level directory in the repo:
```
$ ./download-corpora.sh
$ cabal run bench-lz4 -- --quick
```

To use [fusion-plugin](https://github.com/composewell/fusion-plugin) for
benchmarks:

```
$ cabal run --flag fusion-plugin bench-lz4 -- --quick
```

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

The commands above runs the compression bench suite on **path/to/file/** with
the acceleration value of **500** read as arrays of size **640000** bytes.

*Note*: For the decompression and resizing bench suite a compressed file is
expected as input.
