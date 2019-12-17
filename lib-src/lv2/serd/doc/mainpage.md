Serd is a lightweight and dependency-free C library for RDF serialisation which
supports [Turtle], [NTriples], [NQuads], and [TriG].

The complete API is documented in the [serd](@ref serd) group.  It revolves
around two main types: @ref SerdReader, which reads text and fires callbacks,
and @ref SerdWriter, which writes text when driven by corresponding functions.
Both work in a streaming fashion but still support pretty-printing, so the pair
can be used to pretty-print, translate, or otherwise process arbitrarily large
documents very quickly.  The stream context is maintained by @ref SerdEnv,
which stores the current base URI and set of namespace prefixes.

[Turtle]: http://www.w3.org/TR/turtle/
[NTriples]: http://www.w3.org/TR/n-triples/
[NQuads]: http://www.w3.org/TR/n-quads/
[TriG]: http://www.w3.org/TR/trig/
