# Different Tries in Haskell

Some tries implemented as a learning exercise. I'll probably turn this into a blog post or something!

The binary trie in `BinaryTrie` is based on the big-endian PATRICIA trie described in [Fast Mergeable Integer Maps][fast-maps] by Chris Okasaki.

`Trie` generalizes to PATRICIA tries with different spans. A PATRICIA trie with a span `s` branches on `s` bits of the key at each level, meaning each internal node has `2^s` children. This creates an interesting tradeoff between time and memory efficiency: for sparse trees, smaller values of `s` produce deeper trees with less wasted memory while larger values produce shallower trees with more wasted memory. In practice this means smaller spans result in slower operations but increased memory efficiency.

My `Trie` type is parametrized on its span. Here is an example trie with two entries and a span of `6`:

    let trie = Trie.fromList [(7, "abc"), (1243, "def")] :: Trie 6 String

This makes it easy to know exactly what branching factor any given `Trie` has, but harder to configure this value at runtime.

[fast-maps]: http://ittc.ku.edu/~andygill/papers/IntMap98.pdf
