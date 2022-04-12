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

## Links

- [Crafting Interpreters: Introduction](https://craftinginterpreters.com/introduction.html)
- [Doubly linked list Wiki article](https://en.wikipedia.org/wiki/Doubly_linked_list) Wiki
- [Implementing Doubly Linked Lists in Haskell](https://www.reddit.com/r/haskell/comments/2nepr0/implementing_doubly_linked_lists_in_haskell/) Reddit post
 - [how to implement doubly linked lists](https://stackoverflow.com/questions/10386616/how-to-implement-doubly-linked-lists) stackoverflow answer