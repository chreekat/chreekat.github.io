---
published: true
layout: post
categories: functional-programming
title: Ditching the Imperative
date: 2012-03-01 11:40
comments: true
---

Problems
---

I still have a bad habit of busting out code that wants to branch a lot, because
my inner dialogue is still imperative. "If this is true, then if that is true, then
for all of those, do these things."

My intuition is slowly changing. I know to substitute "for all of those, do these
things" into "map these over those", but I still struggle with the "if, if,
else, if, else". This is especially true with switching statements.

For instance, right now I have

```haskell

  let a = "cromulent" in do
    x <- getX
    y <- getY
    z <- getZ
    case requestType of
        "x" ->
            res <- runX
            case errorCondition res of
                Nothing ->
                    case otherCondition of
                        Foo -> do
                            doThingsWithX_and_Y
                            ...
                        Bar -> do
                            doThingsWithA_and_Z
                            ...
                _ -> ...
        "y" ->
            res <- runY
            case errorCondition res of
                Nothing ->
                    case yetAnotherCondition of
                        Baz -> do
                            doThingsWithZ_and_Y
                            ...
                        Quux -> do
                            doThingsWithX_and_A
                            ...
                _ -> ...
        _ -> blarg

```

I find that to be incredibly ugly code. Why? Two reasons. One: I may no longer
be a C programmer (if I ever was one) but I still agree with Linus that "If you
need more than three levels of indent, you're screwed anyway, and should fix
your program." This code has seven!

I could fix this first problem by hacking off some of the indent levels and
putting them in a new function, i.e.

```haskell

  let a = "cromulent" in do
    x <- getX
    y <- getY
    z <- getZ
    case requestType of
        "x" -> processX x y z
        "y" -> processY x y z
        _ -> blarg

processX x y z a = ...
    res <- runX
    case errorCondition res of
        Nothing ->
            case otherCondition of
                Foo -> do
                    doThingsWithX_and_Y
                    ...
                Bar -> do
                    doThingsWithA_and_Z
                    ...
        _ -> ...

processY x y z a = ...
    res <- runY
    case errorCondition res of
        Nothing ->
            case yetAnotherCondition of
                Baz -> do
                    doThingsWithZ_and_Y
                    ...
                Quux -> do
                    doThingsWithX_and_A
                    ...
        _ -> ...

```

However, that brings me to the second reason I think this is ugly code. Even
when chopped into bitty functions, it's still too imperative. If I simply chop
out segments of the logic and put them in small functions, I haven't gotten rid
of the underlying problem, which is too-deep, "turn left then turn right" code.
Plus I've created a bunch of functions that are useless in general, and are so
strongly connected to their callers that in practice I end up needing to pass a
ton of variables.

Solutions
---

So let's fix up this code, and hopefully improve my intuition in the process.

One helpful idea occurred to me right away: use closures and define the shrimpy
functions under a 'where'. So the previous becomes:

```haskell

  let a = "cromulent" in do
    x <- getX
    y <- getY
    z <- getZ
    case requestType of
        "x" -> processX
        "y" -> processY
        _ -> blarg
  where
    processX = ...
        res <- runX
        case errorCondition res of
            Nothing ->
                case otherCondition of
                    Foo -> do
                        doThingsWithX_and_Y
                        ...
                    Bar -> do
                        doThingsWithA_and_Z
                        ...
            _ -> ...

    processY = ...
        res <- runY
        case errorCondition res of
            Nothing ->
                case yetAnotherCondition of
                    Baz -> do
                        doThingsWithZ_and_Y
                        ...
                    Quux -> do
                        doThingsWithX_and_A
                        ...
            _ -> ...

```

That strikes me as a reasonable change that alleviates the indentation problem.
Those specific, tightly coupled functions one must create now get access to
variables in the outer scope, and aren't visible outside of that scope.

But, I still feel this is too imperative. (There's another problem, too. This
won't compile for reasons I'll get to below. But let's stick to the ugly factor
for now.) Can I do better than "If this then do that else do this?"

Next insight: To make this more declarative and functional, I can take the case
switiching out of my *manual* control and put it into the capable hands of the
compiler. That means pattern matching on function arguments. So now:

```haskell

  let a = "cromulent" in do
    x <- getX
    y <- getY
    z <- getZ
    processRequest requestType
  where
    processRequest "x" = ...
        res <- runX
        case errorCondition res of
            Nothing ->
                case otherCondition of
                    Foo -> do
                        doThingsWithX_and_Y
                        ...
                    Bar -> do
                        doThingsWithA_and_Z
                        ...
            _ -> ...
    processRequest "y" = ...
        res <- runY
        case errorCondition res of
            Nothing ->
                case yetAnotherCondition of
                    Baz -> do
                        doThingsWithZ_and_Y
                        ...
                    Quux -> do
                        doThingsWithX_and_A
                        ...
    processRequest _ = blarg

```

This is shorter and more declarative. I could iterate the process to remove the
inner case statements, too.

The take away from this is: **when I feel like writing a case statement, I should
instead write a function with argument pattern-matching.**

Except there's a compilation problem, as mentioned above. When I was going on
about closures, and variables being available in inner functions, I confused
'let' and 'where' variables with 'do' variables. In the examples above, `x`, `y`,
and `z` are out of scope in `processRequest`! To see why, consider this smaller
example:

```haskell

broken = do
    x <- someAught
    runIt
  where
    runIt = someProcess x

```

This doesn't work because `x` is not local to the "scope" as defined by indenting
-- it's local to the monad that is running. `runIt` is defined external to the
action going on inside to the 'do', so it doesn't get to see `x`. If we
de-sugar, we see

```haskell

broken = someAught >= (\x -> runIt)
  where
    runIt = someProcess x -- Not in scope: `x'

```
This is annoying when, for example, all of my deeper code (like the bodies of
`processRequest` above) want the result of the same computations. If every
branch of code uses `runX` and `runY`, must I write the code to run those
actions in each body separately? Where's the code sharing?

I haven't figured a good solution to this yet.
