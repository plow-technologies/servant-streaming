# servant-streaming

`servant-streaming` introduces `servant` combinators that work with the
[`streaming`](https://hackage.haskell.org/package/streaming) package (and, by
extension, with [`pipes`](https://hackage.haskell.org/package/pipes),
[`conduit`](https://hackage.haskell.org/package/conduit) and
[`io-streams`](https://hackage.haskell.org/package/io-streams)). This allows
you to deal with large request or responses without consuming excessive memory.

The `servant-streaming-server` package provides instances for `servant`
servers, and the `servant-streaming-client` package, for `servant` clients.
