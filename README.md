# jq-bindings

This project contains Haskell bindings for `libjq`, the C library that powers
the `jq` command line utility.

Linear types are employed for safe resource management in the face of `libjq`'s
rather unorthodox emulation of referential transparency.
