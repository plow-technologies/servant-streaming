# servant-streaming

`servant-streaming` introduces `servant` combinators that work with the
[`streaming`](https://hackage.haskell.org/package/streaming) package (and, by
extension, with [`pipes`](https://hackage.haskell.org/package/pipes),
[`conduit`](https://hackage.haskell.org/package/conduit) and
[`io-streams`](https://hackage.haskell.org/package/io-streams)). This allows
you to deal with large request or responses without consuming excessive memory.
(The haddocks for `streaming` contain one-liners for connecting to other
 streaming libraries mentioned above).

The `servant-streaming-server` package provides instances for `servant`
servers; the `servant-streaming-client` package, for `servant` clients; and the
`servant-streaming-docs` package, instances for `servant` docs.
