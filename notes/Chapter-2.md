# Chapter 2: A Map of the Territory

## The Parts of a Language

### Scanning (lexical analysis)

Tokenize the source code. A scanner (or lexer) takes in the linear stream of characters and chunks them together into a series of something more akin to “words”. In programming languages, each of these words is called a token.

### Parsing

Creating a sintax tree. A parser takes the flat sequence of tokens and builds a tree structure that mirrors the nested nature of the grammar. These trees have a couple of different names—parse tree or abstract syntax tree—depending on how close to the bare syntactic structure of the source language they are.

### Static analysis (Type checking)

Check the type of the sintax tree. The first bit of analysis that most languages do is called binding or resolution. For each identifier, we find out where that name is defined and wire the two together. This is where scope comes into play—the region of source code where a certain name can be used to refer to a certain declaration.

### Intermediate representations

In the middle, the code may be stored in some intermediate representation (IR) that isn’t tightly tied to either the source or destination forms (hence “intermediate”). Instead, the IR acts as an interface between these two languages

### Optimization

Once we understand what the user’s program means, we are free to swap it out with a different program that has the same semantics but implements them more efficiently—we can optimize it.

A simple example is constant folding: if some expression always evaluates to the exact same value, we can do the evaluation at compile time and replace the code for the expression with its result. If the user typed in this:

`pennyArea = 3.14159 * (0.75 / 2) * (0.75 / 2);`

we could do all of that arithmetic in the compiler and change the code to:

`pennyArea = 0.4417860938;`

### Code generation

We have applied all of the optimizations we can think of to the user’s program. The last step is converting it to a form the machine can actually run. In other words, generating code (or code gen), where “code” here usually refers to the kind of primitive assembly-like instructions a CPU runs and not the kind of “source code” a human might want to read.

We have a decision to make. Do we generate instructions for a real CPU (**machine code**) or a virtual one (**bytecode**)?

### Runtime

We have finally hammered the user’s program into a form that we can execute. The last step is running it. If we compiled it to machine code, we simply tell the operating system to load the executable and off it goes. If we compiled it to bytecode, we need to start up the VM and load the program into that.

**Compiling** is an implementation technique that involves translating a source language to some other—usually lower-level—form. When you generate bytecode or machine code, you are compiling. When you transpile to another high-level language, you are compiling too.

When we say a language implementation **“is a compiler”**, we mean it **translates source code to some other form but doesn’t execute it**. The user has to take the resulting output and run it themselves.

Conversely, when we say an implementation **“is an interpreter”**, we mean it takes in source code and executes it immediately. **It runs programs “from source”**.

## Links

- [Crafting Interpreters: A Map of the Territory](https://craftinginterpreters.com/a-map-of-the-territory.html)
- [A crash course in just-in-time (JIT) compilers](https://hacks.mozilla.org/2017/02/a-crash-course-in-just-in-time-jit-compilers/)
