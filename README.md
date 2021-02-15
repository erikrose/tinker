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
* Everything's an expression (including `if` and, for now, assignment). This gives terseness and simplicity (no special ternary expression needed; can factor an `a = ` out of a match clause).
* Definitely want to be able to pass functions around. (Thus, compile-time call graphs will not always be exhaustive, limiting some optimizations. However, I imagine we can still do optimizations for static function calls.)
* I love iterators. Probably have those.
* Perhaps avoid breaking-returns. Though it sure is nice to be able to grep for return statements when trying to figure out what a function returns. And explicit returns are also nice because you don't then end up accidentally returning stuff which callers then come to depend on. Or unintentionally making your return type non-void through inference. Alright, I've talked myself out of it.
* List comps with filters. Map/reduce. All those things that let you focus syntactically on what you want, not how to get it. Resolve all applicable questions in favor of what lets you focus on what you want rather than how to get it. That's what you care about when trying to understand a program, which is how most time is spent.
* Still no variable declarations, folks. They're noise to read and bookkeeping to do. Being able to use tighter-scoped vars as a crutch inside a function that should be made shorter anyway is not sufficient justification. Having vars go out of scope as soon as they're no longer used should be a matter for the optimizer (and should matter only for embedded systems). (But should we be able to rebind a var to a string later in a function if it was an int earlier?)
* ? Block-structured or function-scoped? Probably the latter. OCaml-style block structuring weirds language, making var bindings start new blocks, which people can't tolerate the indentation of, so then they weird the indentation, and then the block representation has to live in your head, and semicolons crop up in nonobvious places. (But it buys you a lack of undefined-var errors, trivially.) Function scoping is a nice middleground where you can have an open playground (for fast development and prototyping) but bounded by the limits of the function.
    Perhaps those are extremes and the middleground is just not having var declarations introduce new blocks, like C.
* Pure enough to use for config. For example, it should be able to express metadata for its own modules, like language version #.
* Probably exceptions. I don't see how to do Options without a lot of boilerplate (`.unwrap()` and `?` all over the place in Rust) that makes programs hard to read and just lays a lot of profitless bookkeeping on the user. "Just add question marks until the compiler is happy!" —-In that case, have you really improved anything over just throwing exceptions?
* Probably don't autocurry. It makes for confusing error messages for noobs. Or at least improve the errors so they point out that you passed the wrong number of args to a function (if the curried interpretation doesn't otherwise make sense).
* D has GC but lets you turn it off for sections. Study it. V has ARC (actually avoiding most instances of refcounting) and then GC for cycles. But atm it makes you decide which ref is weak.
* One of the aesthetic goals is to declutter. Syntax that is "for the machine" or "for the compiler" should be minimized. (This nudges us away from super-low-level applications, but perhaps a Sufficiently Advanced Compiler or special-purpose notation could keep those avenues open.) Declarations are minimal. GC exists. Exceptions rather than explicit error checking. Even whitespace over braces might be an example of this. The intention of minimizing clutter is to maximize the amount of mental space a developers can dedicate to their applications. Clutter should *not* be reduced past the point where it makes code harder to read.
* You can't keep people from making a mess by force. (Style rule enforcement can make code *appear* clean but can't do much for comprehensibility beyond a surface polish.) But what we can do is give them the capability to *not* make a mess. Let them omit type declarations when they're just noise. Let them add them when they're informative. Letting humans, which are the only parties with perspective, decide what's important to state is the only path to the highest level of comprehensibility.

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


-- Scoping:
fun foo
    if goo
        g = 8
    g  -- Could be unbound. But compiler can complain. (Assuming function scope.)

fun foo
    if goo
        g = 8
      else
        g = 9
    g  -- Compiler can tell this is bound, assuming function scope.

fun foo
    print g  -- Compiler can complain about read before write.
    g = 9

fun foo
    result = match bar to
        thing with thong and thang
            nums = [1 2 3 4 5 6]
            each nums giving (to $0 * 2)  -- like map()
        bing with bong
            capitalize bong
            total = reduce bong from 0 adding ((accum, cur) => accum + (ord cur))
            [total]
    if result isa bing
        print total  -- Compile error: could be unbound. Compiler isn't smart enough to prove it's bound. Acceptable? If compiler remains that stupid, could satisfy it by adding a total=0 initializer at the top of the function.
        -- I feel like functions should be areas of "free play". A language should scale to arbitrarily large programs but not arbitrarily large functions (which is what block-scoping helps allow). Programs can get unboundedly large, but functions shouldn't. Functions, not blocks, are the main units of programmatic decomposition. Blocks aren't first-class in any language I can think of; they're always parametrized, making them functions.
        -- Anyway, the cost of going with function-scoping is that you have to declare nonlocals you want to write (or have a special = operator for that). That's a rarity. (It's also something of an antipattern: it promotes hidden mutations that would better be expressed through return values. Maybe don’t even support it.) The dividend is you don't need to declare vars (or read the usually-just-noise declarations). Declarations exist solely to broaden the scope of a var, assuming a language in which vars are otherwise assumed to be local to the block they're mentioned in.
        -- It's important to allow shadowing of nonlocals in a function; otherwise, you can't paste functions around without reading them carefully to make sure they don't overwrite nonlocals.
        -- A read in an if is fine if the if has the same or a logically equivalent condition as one which previously wrote. For ifs which write and are nested, the whole chain of nested if conditions (up to the point where the writes and the reads share a common encloser) will have to be satisfied...or something. Maybe I can just use unification and get some more milage out of the unifier I'm going to need anyway for Hindley-Milner.
```

## Status

Right now, "Hello, world" is hard-coded into the compiler in the form of AST expressions. There's no parser yet, because I don't know what I want the syntax to look like. To install dependencies...

```
opam install ctypes-foreign llvm ounit2
```

Also install the LLVM headers.

To compile and run "Hello, world", run `make run`.

## To do
* √ Be able to declare externals so we don't need any C.
    * √ We'd need strings too, though (or at least "bytes"), for printf's format string. So add externals support and support for multiple types.
* Compile all the way to an executable without needing other shell commands. (Call LLVM's linker internally.)
    * https://lld.llvm.org/
    * "You can embed LLD in your program to eliminate dependencies on external linkers. All you have to do is to construct object files and command line arguments just like you would do to invoke an external linker and then call the linker’s main function, `lld::elf::link`, from your code."
* √ Be able to do multiple statements.
* √ Study what I've written to get an understanding of the LLVM API.
* √ Add ifs (multiple basic blocks).
* Vars. A var assigned-to from a function and not declared nonlocal/global/whatever-else-I-add is scoped to that function. There is no block-level scoping. This avoids having to make or read var declarations, read over `let` keywords, or slide `:=` operators around as a function evolves. In short, we borrow Python's assignment heuristic. Upsides: you can move code from an outer function to an inner one without changing it, as long as it only reads variables. (You don't have to add `^` annotations like `print(foo^)` (if `^` meant "look in the enclosing scope").) Writes to a var from outside can be accomplished by declaring the var `nonlocal`. This is better than a special assignment operator because you can do it once and for all rather than changing every writing reference you've copied and pasted from the outer function.

    It's worth contrasting var declarations with import statements. The latter are useful while reading code. They tell the reader where deeply imported local symbols are defined, in case they want to go read the code or docs. They shorten callsites if you deeply import. Var declarations don't provide as much information: just "it belongs to this block" and sometimes "it is of this type". We're dispensing with the strictures of the former for ease of expression, and the second we will accomplish some other way, perhaps `someVar:someType = nonObviousReturnValue()`.

    Function scoping--or in fact, any scoping in which the introduction of a new var does not introduce a new block--does make it more challenging to detect reads from undefined vars. (I don't want runtime errors for something so common and, because the cause is branches, possibly undetected until a rare branch fires.) The compiler will have to check every branch of every `if` in a function to make sure every one defines all non-pre-existing vars read afterward. Same for loops that could `break` before assigning to a var. If the compiler can't prove we write before read, it throws an error. You can satisfy it by initializing the var before the `if` or loop. Hopefully the compiler will get smarter and smarter, letting us remove more and more such legalistic initializations. But in any case it's a reduction from saying `let` in front of each first write of a var.

    A var with the same name as one in an enclosing function or module, if assigned to from the inner function, shadows the outer one.

    To implement: for each function, find the vars written to within it. Put those into a set. (This is faster than spidering all over the function anew for each ref.) As we codegen each var ref, look into those sets to figure out what stack frame to (if not already done) add the var to.

    We'll put all the `alloca` instructions in the entry block of the function so we can depend on `mem2reg` to convert them to register accesses.

    Sum types can be represented by alloca-ing enough space for the largest alternative and also making space (whether as a separate pseudovar or part of a struct that contains the enum and the var value) for the enum value. Make sure it works nicely with nested enumerations, like `foo:(int|Snoo);  Snoo = double|string`.

    * √ Complain on the possibility of undefined var reads. Maybe for this it could be good enough just to assert that every read in a CFG node has a write in a dominator or in the same node but before the read. (With Lengauer-Tarjan, we can find dominators in O(m log n).) Or assert that every read hits a write via every path back to the entry block. Definitely write this against the CFG, lest we get some control structure later that has a nonobvious, nonlexical relationship to control flow. Or would it be possible and simpler to have a big match clause where each syntactical construct makes sure a given var is found in each of its (potential) branches? We could cache for speed.

        When making example CFGs for trying out analysis ideas, using `rand()` calls as the condition in `if` expressions comes in handy: it lets you pick best-case and worst-case paths at your discretion without painstakingly constructing code that does whichever you were looking for. Sometimes it lets you halve the amount of code just by choosing different branches each time around a loop.
    * Support other types than int. I don't see any other way to do this than to write an honest-to-goodness constraint solver for type inference. Perhaps it's possible that we wouldn't need one just for inference, but we'd sure need one for type checking. If somebody is inconsistent with types, we need that unification failure to notice the error. Here's a great summary of unification for type inference, even including some OCaml code: https://www.cs.cornell.edu/courses/cs3110/2011sp/Lectures/lec26-type-inference/type-inference.htm. Start with local inference for practice, then expand. A unification of a set of pairs is a single set of var bindings that unifies every pair.

        * Make func an expr so it can have a type.
        * Make funcs first-class so we have something useful to do with function expressions.
            * What do we do about function names? Munge them all?  Probably at first. Even if we have a `fun foo(a, b, c)` syntax, it can desugar to `foo = fun(a, b, c)`.
            * How do we give functions names for external calls? Infer from global assignment varname?
            * So that the linker can hook up the symbols, we have to have at least the addresses right at link time. I suppose we could put the values in place later with an `initialize()`.
            * Remember that we'll have to use external linkage (and thus names) for anything we want to get at from another module, not just from another language. And we'll probably go with 1 module = 1 file. Why complicate things? (We want to have multiple modules anyway for the effective caching that separate compilation brings.)
            * Does w^x mean we can't have a module-level initializer?
            * Maybe to start, we can have only global functions (unmunged), outlaw inner functions for now (just unneeded complexity keeping us from committing), but let us still assign those global functions to vars. Then do unification before I forget how. Next, write a test to make sure it complains on nested functions (mostly to make sure I know OCaml) and then see if passing a func ptr around and calling it works. Working out how we want to do reads to globals will require a lot of language design: we'd have to change our read-before-write detector to take globals into account, for instance. Let's just make inner functions work. Or meh, maybe the former is easier; with the latter, we have to change Ast.Call anyway to take vars. So we might as well just change Var resolution to see globals and the detector to put up with them too. But then we still have to change Call anyway. So locals it is. And we'll just have the type checker make sure the number and types of args are right.
            * After that, do inner functions, which will mean passing closures around (whenever a function contains free vars) instead of just func ptrs: records like `{env, func_ptr}` and generating `call func_ptr(env, …)`.
        * Descend expr tree to create a constraint (a pair) for each expr (and subexpr).
        * Unify the pairs to infer the types.
        * Here's how you handle locals. I guess you have to alpha rename and feed in all the constraints at once, local and global. You can't demand to do the globals first and then one set of locals at a time, because there might be info in the local scope that needs to percolate out and combine with an arg value in another local scope to inform the return type on a third function. Figure out a fully qualified name for symbols or just use the Var instance itself or generate a unique int (to pair with the var name) or something.
* Raise an error if a function returns a different type than declared. (This should be taken care of by the unifier.)
* GC
* Loops. All looping constructs can be lowered to an infinite loop plus a break statement. Infinite loops can be lowered to recursion with TCO. Not sure about the breaks.
* Decide on dispatch. Will it be hard for a human to find where a function's code is?
* Do non-primitive types, like ML enums. We won't have type erasure on enums because we'll have to be able to distinguish among variants in `match` clauses.
* We might be able to get away with just recursion for type inference, as long as we explicitly declare function args and return types (at least temporarily): https://mukulrathi.co.uk/create-your-own-programming-language/intro-to-type-checking/
* Make protos unnecessary (except for externals, I guess).
* Design the language.
* Exceptions. Undesirable in a pure language because they can pop out any time unexpectedly. So require you catch them--and in the direct-calling stack frame? That makes them basically inferred option types without exceptions' distinguishing characteristic: multi-frame stack unwinding.

    If we have exceptions, the `catch` clauses should probably look like `match` clauses and be able to pattern-match and destructure thrown values. A thrown value can be any old object with a constructor: anything that can be matched against.

    For applications that cannot tolerate exceptions, we can have a `no_except` signifier applied to a function that cues the compiler to throw an error if any exception that could be raised within a function is not caught.
