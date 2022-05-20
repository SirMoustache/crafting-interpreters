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

```bash
$ ./args foo bar
The arguments are:
foo
bar
```

Or read line

```haskell
import System.IO

main = do
  line <- getLine                 -- IO [String]
  putStrLn "The line is:"
  putStrLn line
```

Haskell Lists

```haskell
-- [] is an empty list
-- [1,2,3] is a list with three elements
-- [1..10] is a list range with 10 elements
-- [1,2..10] is a list range with 10 elements
-- [1,3..10] is a list range with 9 elements

-- x:[] prepend to the list
-- 1 : 2 : 3 : [] is a list with three elements [1,2,3]

-- List Comprehension
-- [x*2 | x <- [1..5]] is a list with 5 elements [2,4,6,8,10]

-- List Comprehension with guard condition
-- [x | x <- [1..10], x `mod` 2 == 0] is a list with 5 elements [2,4,6,8,10]

-- List Comprehension with multiple guards conditions
-- [x | x <- [1..10], x `mod` 2 == 0, x > 5] is a list with 2 elements [6,8]

-- List Comprehension with multiple lists
-- [(x, y) | x <- [1, 2, 3], y <- ['a', 'b']]
-- is a list [(1, 'a'), (1, 'b'), (2, 'a'), (2, 'b'), (3, 'a'), (3, 'b')]

-- List concatenation
-- [1,2,3] ++ [4,5,6] is a list with 6 elements [1,2,3,4,5,6]
```

Importing List functions

```haskell
import Data.List
```

Haskell Functions

Guards

```haskell
factorial :: Int -> Int
factorial n
  | n == 0 = 1
  | n > 0 = n * factorial (n - 1)
  | otherwise = error "Factorial of negative number"
```

Accumulator tail recursion

```haskell
factorial :: Int -> Int
factorial n = factorial' n 1
  where
    factorial' 0 acc = acc
    factorial' n acc = factorial' (n - 1) (n * acc)
```

Pattern Matching

```haskell
-- Pattern matching is a way to match a value against a pattern
-- and extract a value from the pattern if the pattern matches
isZero :: Int -> Bool
isZero 0 = True
isZero _ = False
```

Lambda functions

```haskell
-- It is a function that is defined in terms of a single expression
-- The expression is the body of the function
-- The expression is evaluated lazily
squareAll :: [Int] -> [Int]
squareAll [] = []
squareAll (xs) = map (\x -> x * x) xs
-- \x -> x * x
```

## Links

- [Crafting Interpreters: Introduction](https://craftinginterpreters.com/introduction.html)
- [Doubly linked list Wiki article](https://en.wikipedia.org/wiki/Doubly_linked_list) Wiki
- [Implementing Doubly Linked Lists in Haskell](https://www.reddit.com/r/haskell/comments/2nepr0/implementing_doubly_linked_lists_in_haskell/) Reddit post
- [how to implement doubly linked lists](https://stackoverflow.com/questions/10386616/how-to-implement-doubly-linked-lists) stackoverflow answer
- [Hello, World!](https://riptutorial.com/haskell#hello--world-) in Haskell
