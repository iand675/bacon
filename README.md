bacon
=====

Degrees of Kevin Bacon (where a degree is distance from Kevin Bacon on wikipedia)

to build
========

Requires ghc && cabal

cabal sandbox init
cabal install --only-dependencies
cabal build

to run
======
./dist/build/bacon/bacon
