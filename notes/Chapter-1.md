# Chapter 1: Introduction

Hello world in Haskell.

```haskell
main :: IO ()
main = putStrLn "Hello, World!"
```

Read command line arguments in Haskell

```haskell
import System.Environment
import Data.List

main = do
  args <- getArgs                 -- IO [String]
  putStrLn "The arguments are:"
  mapM putStrLn args
```

Or read line

```haskell
import System.IO

main = do
  line <- getLine                 -- IO [String]
  putStrLn "The line is:"
  putStrLn line
```

```bash
$ ./args foo bar
The arguments are:
foo
bar
```
