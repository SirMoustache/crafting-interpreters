These are [some notes](./notes/README.md) and code while I'm trying to go through [Crafting Interpreters](https://craftinginterpreters.com) Book by [Bob Nystrom](https://twitter.com/munificentbob). To make things even more complicated I tried to use a language I know nothing about - Haskell.

## How to run

To run Haskell code, you need to install [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/).

````bash

```bash
stack setup
stack build
stack exec crafting-interpreters-exe
````

If you want to launch a REPL:

```bash
stack ghci
```

Run tests:

```bash
stack test
```

## Links

- [Haskell Tool Stack docs](https://docs.haskellstack.org/en/stable/README)
- [Stack user guide](https://docs.haskellstack.org/en/stable/GUIDE)
- [Crafting Interpreters book](https://craftinginterpreters.com)
- [Crafting Interpreters book repo ](https://github.com/munificent/craftinginterpreters)
