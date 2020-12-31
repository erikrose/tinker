# Tinker

My playing-around language. This is a practice run to figure out LLVM and how to structure things before I take a serious run at the language I wish I could program in. Or maybe I'll approach it iteratively. We'll see.

For starters, I'm shooting for a language about as high-level as Python but trading some of its little-used dynamicity for speed and machine-code compilation. A good language for prototyping but which runs fast and deploys easily. A language easier to reason about than Python. A language at least as terse.

## Properties

Everything is vague and loosely held at the moment. Consider this a sketch.

* It compiles to machine code.
* Circular imports are fine. It might make the build slower and use more RAM, but I'm not going to impose the busywork of toposorting right-the-hell-now if you're in a hurry.
* Scopes (for instance, module top-levels) are immutable
  * So you can do forward references. Toposorting is for computers.
* Screw colons. I'm always forgetting them. We have line breaks.
* Goal: the only reason to write out a type name for a variable is documentation. Clear cases (at least) should be inferred.
* Everything's an expression (including `if` but maybe not assignment). This gives terseness and simplicity (no special ternary expression needed; can factor an `a = ` out of a match clause).
* Still no variable declarations, folks. They're noise to read and bookkeeping to do. Being able to use tighter-scoped vars as a crutch inside a function that should be made shorter anyway is not sufficient justification. Having vars go out of scope as soon as they're no longer used should be a matter for the optimizer (and should matter only for embedded systems). (But should we be able to rebind a var to a string later in a function if it was an int earlier?)
* Definitely want to be able to pass functions around. (Thus, compile-time call graphs will not always be exhaustive, limiting some optimizations. However, I imagine we can still do optimizations for static function calls.)
* I love iterators. Probably have those.
* Perhaps avoid breaking-returns. Though it sure is nice to be able to grep for return statements when trying to figure out what a function returns. And explicit returns are also nice because you don't then end up accidentally returning stuff which callers then come to depend on. Or unintentionally making your return type non-void. Alright, I've talked myself out of it.
* List comps with filters. Map/reduce. All those things that let you focus syntactically on what you want, not how to get it. Resolve all applicable questions in favor of what lets you focus on what you want rather than how to get it. That's what you care about when trying to understand a program, which is how most time is spent.
* ? Block-structured or function-scoped? Probably the latter. OCaml-style block structuring weirds language, making var bindings start new blocks, which people can't tolerate the indentation of, so then they weird the indentation, and then the block representation has to live in your head, and semicolons crop up in nonobvious places. Function scoping is a nice middleground where you can have an open playground (for fast development and prototyping) but bounded by the limits of the function.
* Probably don't autocurry. It makes for confusing error messages for noobs.
* D has GC but lets you turn it off for sections. Study it. V has ARC (actually avoiding most instances of refcounting) and then GC for cycles. But atm it makes you decide which ref is weak.

## Types
### Possibilities
* No classes, just typed structs (or interfaces?) and type- (or interface?)-dispatched functions
* ADTs
* Generics through compile-time functors. Or maybe Hindley-Milner makes that pointless.

### Deciding among possibilities
* I'd like for things to be retrofittable. IOW, be able to attach new routines to a stdlib type without having to stick new interface declarations in the stdlib.

## Syntax

Sketches. Don't take them too seriously.

```
to add a:int and b:int
    a + b

to count-words str:text
    num = 0
    for char in str do
        if char == ' ' then
            num = num + 1  # Might lower to `set num to (num + 1)`.

to count-words string:text
    sum((if char == ' ' 1 else 0) for char in string, 0)

fun num_words(string str)
    length(char for char in str if char == ' ')

fun sum(iterable Iterable<T>, default T)
    total = 0  -- All symbols are infix operators.
    for item in iterable
        total = total + item
      else
        default

fn documented_types(big_doodad:BigDoodad)
    sub_doodad:PigglyWiggly = big_doodad.owner  -- Compiler proves that documented types are right.

type tipe is
    double_type or
    int_type or
    string_type with length:int or
    string_ptr_type

type expr is
    double with value:float or
    int with value:int or
    call with function_name:text and args:expr[] or
    string with value:text

match expression to
    call with function_name and args in  -- Trying to make it look like a function definition, because it works similarly: there's a block inside with bound params. I also like that you can copy and paste the line from the type definition.
        generate_call function_name with args
    int with value in
        generate_int value
```

## Status

Right now, "Hello, world" is hard-coded into the compiler in the form of AST expressions. There's no parser yet, because I don't know what I want the syntax to look like. To compile and run "Hello, world", run `make run`. You'll need OCaml, the llvm opam package, and the LLVM headers installed.

## To do
* √ Be able to declare externals so we don't need any C.
    * √ We'd need strings too, though (or at least "bytes"), for printf's format string. So add externals support and support for multiple types.
* Compile all the way to an executable without needing other shell commands. (Call LLVM's linker internally.)
    * https://lld.llvm.org/
    * "You can embed LLD in your program to eliminate dependencies on external linkers. All you have to do is to construct object files and command line arguments just like you would do to invoke an external linker and then call the linker’s main function, lld::elf::link, from your code."
* √ Be able to do multiple statements.
* √ Study what I've written to get an understanding of the LLVM API.
* √ Add ifs (multiple basic blocks).
* Decide on dispatch. Will it be hard for a human to find where a function's code is?
* Do non-primitive types, like ML enums, probably with type erasure, which would mean an IR or at least symbol tables to keep track of what types things are.
* We might be able to get away with just recursion for type inference, as long as we explicitly declare function args and return types (at least temporarily): https://mukulrathi.co.uk/create-your-own-programming-language/intro-to-type-checking/
* Design the language.