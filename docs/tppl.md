# TPPL
## Introduction and Overview

The objective of this project is to build a simple temporal logic-based probabilistic programming language.  In particular, we would like to express probability distribution functions in a declarative temporal setting, with the goal of being able to represent random operations over time in an intuitive and easy-to-interpret style.  The latter goal is particularly important in some regard, since the intention of this project is partly to make a usable language in a relatively short amount of time that can represent reasonably interesting probabilistic programs.

### Examples

It is perhaps worth identifying what makes an "interesting" probabilistic program.  Intuitively, we rely on the notion of FOL interacting with probabilistic distributions to obtain reasonably complicated behaviors.  For example, the following code provides both an intuition and, indeed, the motivation for this project, using custom temporal syntax:

```
x = uniform // x holds for all subsequent operations
y <- uniform <!> gt(x, .5) // y holds the value until updated, which is uniform until x > .5
--- // advance time
dist(y) // note that this and the following statement are _interchangeable_
y <- y | (uniform * 2 <!> leq(x, .5)) // y updates to hold for all subsequent operations, which is mult(uniform, 2) until x <= .5
--- // advance time
dist(y)
```

This program creates the interaction between `x` and `y` where the value of `x` determines the distribution of `y` in some way.  The resulting distribution of `y` is non-obvious, particularly to those unfamiliar with the dubious syntax, yet this interaction describes a feature of code that shows up reasonably frequently when reasoning about temporal probability.  In particular, the notion of two variables interacting in a logical sense requires reasonable effort to interpret.

A less-complicated while still interesting example would be as follows:

```
assert(x <.. mult(uniform, 2))  // assert that x eventually follows the uniform distribution doubled
x <X uniform // x holds the uniform value for the next timestep
--- // advance time
x <X mult(x, 2) // updates x in the next timestep to be double itself
--- // advance time
x <.. uniform // now requires that x hold the regular old uniform distribution after "cleaning out" the old result this timestep

// The assertion is satisfied since x at at least one point has the distribution of uniform doubled
```

### Discussion

The intention of the output of this language with such a program would be to show the distribution of `y` and verify the assertion on `x`, probably visually using some internal interpreter.  In particular, the language would indicate the distribution produced by this arrangement over the range of non-zero values at that moment in time.  Note that, to determine the range, some sort of interval analysis may be necessary.

Practically, the language would intend to evaluate these expressions symbolically (if possible) and experimentally (if not).  The choice between the two may be reasonable opaque to the programmer, so adding in a message about which the interpreter ends up selecting would be helpful, along with a log of the reasons for making such a decision.  Another goal may be to have the interpreter attempt both symbolic and experimental solutions simultaneously, and resort to experimental results if the symbolic solution is looking improbable.

For temporal analysis, LTL is the obvious solution, but some extensions may be needed to reason about assertions and assignments in a reasonable way.  How this will be implemented will require some serious thought.

## Syntax

TPPL (name pending) tentatively will have syntax super made-up syntax for temporal reasoning, with standard probabilistic extensions.  An incomplete description of the grammar is as follows:

```
x := {variables}
f := {propositions}
n := {number}

c := c1\nc2         // no semicolons!  Whitespace aware!
    | ---           // timestep
    | x = e         // global assignment
    | x <X e        // next assignment
    | x <- e        // updateable assignment in the future (variant of NEXT that holds until updated)
    | x <.. e       // x must eventually be e (though may not be immediately if a contradiction!)
    | f(e1, ...)    // prepositional invocation
    | dist(x)       // distribution of variable (must be a command!)
    | assert(c)     // note that you must assert a command, which is no executed
    | f(x1, ...) = e // preposition creation, must hold globally (for now?)
    | print(e)      // print (must be a command!)

e := n
    | true
    | false
    | x
    | f(e1, ...)        // prepositional invocation
    | e1 + e2           // adds things together (along with other operations)           
    | e1 | e2           // e1 or e2
    | e1 & e2           // e1 and e2
    | e1 <!> e2         // e1 weak until e2 (implies negation)
    | e1 <!!> e2        // e1 strong until (e2 must eventually hold)
    | uniform           // uniform from 0 to 1
    | uniform(e1, e2)   // uniform from e1 to e2
    | normal            // normal distribution with mean 0 and std 1
    | ...               // blah blah blah
```

In summary, we have commands and expressions.  Commands are essentially operations and assignments, including basic I/O (it may be worth restricting user input for this language).  Types of functions and variables are inferred, so no need to include a type.  The user can define propositions (read: functions) using `f(x1, ...) = e` notation, but cannot define custom types with this basic version of the language.  There are a bunch of weird operators that I'm still fiddling with, open to ideas though!  The timestep command is particularly important, and shamelessly stolen from Dahlia.

The expressions on display are mostly straightforward operators and function calls, though the `\` operators bear specific mention.  These operations are intended to give full control of temporal logic in relating expressions to each other, perhaps at the cost of readability!  Note that these operations are generally negatory -- this is in-line with traditional LTL operators, though may be counter-intuitive to use, unfortunately.  It may be worth putting some thought into reworking these operators in particular.

Also notable are the core syntactic `uniform` and `normal` expressions, which respectively indicate standard probability distribution functions.  Other obvious choices include `binomial`, `poisson`, and several others, which should be easy enough to add at some point.  The objective of having these as part of the core syntax is to make them very clearly part of the core language design, and to allow direct interpreter support.

## Core Operations

There's a lot of weird operations at play in this language, so it's important to be clear about each of their meanings with respect to semantics.  It's worth noting that TPPL does not have intuitive semantics compared to "regular" declarative or even imperative languages, as operations are based on assertions of statements that relate two objects which may change over time.  In particular, all operations can be thought of as assertions for a given timestep (as delimited by `---`).

### Assignment

The most important operations (in some sense) are those of assignment.  Saying `x = uniform`, for example, binds `x` to be uniform for the remainder of the program (reassigning x in the future is a compile-time error).  Stating `x <- uniform`, on the other hand, simply binds `uniform` to the value of `x` from this timestep until reassignment occurs (using an UNTIL operation, presumably).  Note that multiple temporal assignments in the same timestep will produce an error!

```
x <- 1
x <- 2 // compile-time error since this is a contradiction!
```

Finally, the `<X` operation is notable for being highly restrictive -- only providing a value to x in the _next_ timestep.  Careful use of this operator allows for helpful errors

```
x <X 3
---
---
print(x) // x is undefined since two timesteps have passed
```

I would like a "valid for the current timestep" operator, but I'm worried about overloading, and perhaps this is overkill, but it could be nice.  Just something to indicate that a variable lives in the current timestep, for assigning something very local, for instance.  Finally, the `<..` operator is uniquely powerful for reasoning about assertions, and not really useful elsewhere.  The other main use is to assign a value of a variable when a contradiction no longer exists -- for example, we might have

```
x <X 1
---
x <.. 2
---
print(x)
```

Which will of course print 2 since x at this point is assigned 2.  How this interacts with other temporal assignments is unclear to me -- it is fairly unrestrictive, so has an interesting use as a "default value" for variables:

```
x <.. 0
x <X 3
print(x)
---
print(x)
---
print(x)
```

Prints 0, 3, 0, since the default value of x is 0, while the 2nd timestep has x being 3.

### Temporal Expressions

Perhaps the core temporal expression is `e1 <!> e2`, which indicates that e1 holds true _until_ e2 holds true.  This can lead to some complicated behavior:

```
x <.. false
y <.. uniform
y = normal <!> x
---
x <X true
dist(y)
---
dist(y)
---
dist(y)
```

Produces the three distributions: `normal, uniform, uniform`, since `y` does not _revert_ when x becomes false.  Note that no contradiction is generated by the assignment `y = ...` since the condition given does not violate the requirement that `y` is eventually `uniform`.

More subtle, perhaps is the `<!!>` operator, which places an additional restriction that the condition must eventually be true (and thus remove the current value)

```
x = false
y <.. uniform
y = normal <!!> x
```

The above code produces a runtime error, since `x` will never be true (indeed, this may even produce a compile-time error depending on how aggressive the analysis on the constant of `x`).  Besides being included for completeness, this operator is useful for asserting that the statement will eventually fail to hold, essentially acting as a sort of temporal assertion on the validity of the associated value.

### Probabilistic Expressions

The only core supported probabilistic expressions are those of `uniform` and `normal`, though more are intended to be added.  These both create distributions, which can only be evaluated with `dist`.  Note that `print` will fail on distributions, as will be covered further in the section on Types.  Distributions are notable in that they invoke interval analysis on any timestep they exist in, and are only compatible with other distributions.  Probabilistic expressions can be operated on by constants (of course):

```
x <- uniform + 1 // equivalent, of course, to uniform(1, 2)
---
dist(x)
x <- x - 1 // returns x to the uniform dist
---
dist(x)
```

Note that this behavior can quickly get out of hand for the compiler to evaluate.  As a result, symbolic evaluation will depend heavily on a bunch of internal decision making.  I don't really have a good sense for the bounds on symbolic evaluation (besides the obvious `input` problem).  Here's an example which may be difficult to evaluate:

```
foo(x) = leq(x, .1) & uniform
foo(x) = gt(x, .1) & foo(x / 2) + 1
x <- foo(uniform)
---
dist(x)
```

Evaluating x symbolically, while potentially valid, seems...tricky even using a fix-point.  How to handle cases like this requires some serious thought and may even be generally invalid.

Finally, something that's worth thinking about are probabilistic temporal assignments, which could totally be included.  I have no idea what to make of something like:

```
x <-::.3 true
x <.. false
---
print(x)
```

Is this a distribution?  Probably!  Makes the `<..` operator feel much more useful at least.

### Temporal Predicates

An interesting aside is the potential existence of temporal predicates (which are currently unspecified):

```
foo(x) <- x + 1
---
y <- foo(3)
foo(x) <- x - 1
---
print(y)
```

It's not entirely clear what should happen here (or if this should even be supported) but is an interesting thought experiment if `y` is actually 2.  I'm...not entirely sure what should be supported, particularly with respect to some weird probabilistic features:

```
foo(x) <.. uniform
foo(x) <- normal <!> gt(x, .5)
---
y = foo(uniform)
foo(x) <- normal <!> gt(x, .3)
---
z = foo(uniform)
---
dist(y)
dist(z)
```

It feels like y and z should have different distributions, but also, they rely on the same predicate, so idk.

## Types

Alright, finally, a discussion of the actual types involved.  All types are inferred, as usual, though the temporal and probability stuff gets complicated.  This is a very rough overview of the ideas rather than a deep dive into each operation, obviously this will need to be cleaned up later

### Core types

There are three basic types: `bool`, `float`, and `int`, which are all obvious enough to be not worth mentioning much about.  Print accepts both, and input must produce an int (for now, cause I'm too lazy to specify generics yet).

More interesting, perhaps, is the `pdf` type, which is the result of the `uniform` or `normal` expressions.  The `pdf` type holds no information up-front about the distribution, since this analysis is done at runtime, but does notate that the underlying value is a distribution (of course).  Expressions of type `pdf` can be operated on by `int`, `float`, and other `pdf` expressions.  Sampling a `pdf` always produces a `float`.

### Temporal Types

Much more complicated are temporal types and what information they produce.  All temporal types are used to indicate whether or not there _must_ be a contradiction, and whether or not a variable can have a definition at a given point in time.  An example of an undefined variable at time 3:

```
x <X 3
---
---
print(x)
```

How will this work?  I haven't worked out the details yet, but basically each assignment gives both the core type of the variable and the temporal properties of the variable.  For example, `x = 3` gives the `global int` temporal type to `x` from the current time forward, while `x <X uniform` gives the `next pdf` type to `x` from the current time.  Each timestep "advances" the types in the way you might expect -- for example, the following types hold

```
x = 2 // global int
y <X true // next bool
---
// y becomes "current bool"
---
// y becomes "undefined"
print(y) // and thus, and error
```

The types I'm imagining are as follows:

| Type      | Assignment    | Description   |
| ---       | ---           | ---           |
| global    | `= | <..`     | forever       |
| next      | `<X`          | next state    |
| current   | N/A           | current state |
| undefined | N/A           | no core type  |
| until     | `<-`          | reassignment  |
| future    | `<-`          | edge case     |

Note that the type of a variable can change with reassignment given no contradiction, during which time the variable has type `current` and also type `future`, noted with a `pair` type:

```
x <- 3 // x is future int
---
// x becomes until int
x <- true // x becomes pair<future bool, current int>
---
print(x) // perfectly valid, prints true
```

## Interval Analysis

I have no idea how this will work, so just making a note that it needs some serious thought.  See the examples in "Probabilistic Expressions" for some weird cases.  Note that the decision to do interval analysis will be made at runtime, not compile time.
