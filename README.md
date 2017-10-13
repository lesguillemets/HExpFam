# HExpFams

# Building

Make sure `stack` is installed.

```terminal
$ git clone https://github.com/lesguillemets/HExpFams/
$ cd HExpFams # I intend to rename the repository when appropreate
$ # You can remove stack.yaml and `stack --resolver=<your favourite resolver> init` if you want.
$ stack build
```

`stack haddock HExpFams` to build the documentation. `stack test` to run the tests.
