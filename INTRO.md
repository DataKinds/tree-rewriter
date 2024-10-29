# Rosin's Easy Intro

## Overview

Rosin is a _tree rewriting language_. That means you write rules which mutate trees.

What's a tree? Trees are *terms* represented with S-expressions:

```
(hello world! this (is (a (tree))))
```

Trees can be nested arbitrarily deep and have as many terms in a row as you'd like.


You interact with trees through _rules_. Rules are created by presenting Rosin with a tree that looks like this: `(pattern ~> template)`. Here's what that might look like:

```
((this (is :rest)) ~> (woah! it's :rest))
==> ROSIN SAYS: (defined "(this (is :rest)) ~> (woah! it's :rest)")
```

Everything prefaced with `==>` is used for some output of running the interpreter.

Rosin will apply the rules you present to it everywhere it can, until they can no longer be applied. 
```
this (is (a (tree)))
==> ROSIN SAYS: (woah! it's (a (tree)))
```

## Patterns and variables

The left side of the `~>` in a rule is a *pattern*. Barring all else, Rosin will only rewrite a term if it matches exactly what they saw in the pattern. There are a few exceptions to this. You may present Rosin with a *variable*, prefixed with `:`, which they will match against any term:
```
((my :a rule) ~> (my totally :a rule))
(my awesome rule)
==> ROSIN SAYS: (my totally awesome rule)
```

There is one other special type of pattern variable that we'll talk about in the later discussion on cons lists.

Sometimes, you may want Rosin to preferentially apply one rule instead of another rule, even if both patterns match. There are a few ways to do this. The first one is by presenting the rules in order. All else equal, Rosin will always prefer to match the first rule they saw:

```
((lil function f +0) ~> +0)
((lil function f :n) ~> +1)

(lil function f +2)
==> ROSIN SAYS: +1 
(lil function f +1)
==> ROSIN SAYS: +1
(lil function f +0)
==> ROSIN SAYS: +0
```

## Eager variables 

The other way to do this is through the use of *eager variables*. Rosin would very much like to match eager variables last, allowing all other computation to happen first. You write an eager variable with the prefix `:!` and Rosin will not allow the pattern to match if that variable could be matched by another rule. 

Let's demonstrate eager variables by building up a larger example:

```
(; (Comments! This just deletes all terms of the form (; ...) ))
((; :comment) ~>)
==> ROSIN SAYS: (defined "(; :comment) ~> ")
(; (This deletes the output Rosin generates for definitions, so we don't have to worry about showing it.))
((defined :str) ~>)

(; (Let's establish all the arithmetic we need for this example!))
((-1 +3) ~> +2)
((-1 +2) ~> +1)
((-1 +1) ~> +0)
((-1 +0) ~> +0)

(; (Now let's try to write a "function" that makes a 2-tree of depth N filled up with a certain value.))
(; (First, the base case... a depth 0 tree gives back the value directly.))
((give me a depth +0 tree of :x) ~> :x)
(; (Then, the recursive case.))
((give me a depth :n tree of :x) ~> (give me a depth (-1 :n) tree of (:x :x)))
(; (And let's execute the function.))
(give me a depth +2 tree of wow)
==> ROSIN HANGS.
```

Why is this? Let's use a *special accumulator* which prints the intermediate values to find out.

```
((puts :?>) ~> :?>)
((give me a depth +0 tree of :x) ~> :x)
((give me a depth :n tree of :x) ~> (puts (give me a depth (-1 :n) tree of (:x :x))))
(give me a depth +2 tree of wow)

==> ROSIN GINGERLY PRESENTS THE FOLLOWING TO STANDARD OUT
(give me a depth (-1 +2) tree of (wow wow))
(give me a depth (-1 (-1 +2)) tree of ((wow wow) (wow wow)))
(give me a depth (-1 (-1 (-1 +2))) tree of (((wow wow) (wow wow)) ((wow wow) (wow wow))))
(give me a depth (-1 (-1 (-1 (-1 +2)))) tree of ((((wow wow) (wow wow)) ((wow wow) (wow wow))) (((wow wow) (wow wow)) ((wow wow) (wow wow)))))
(give me a depth (-1 (-1 (-1 (-1 (-1 +2))))) tree of (((((wow wow) (wow wow)) ((wow wow) (wow wow))) (((wow wow) (wow wow)) ((wow wow) (wow wow)))) ((((wow wow) (wow wow)) ((wow wow) (wow wow))) (((wow wow) (wow wow)) ((wow wow) (wow wow))))))
... and so forth
```

Since Rosin was always able to match the `(give me a depth :n tree of :x)` rule, they never traversed into the tree to match a `(-1 ...)` rule. So the arithmetic was never evaluated as a result. We can give Rosin an eager variable in the recursive pattern and they'll refuse to match the unevaluated `(-1 ...)` tree because another rule exists which can match it. This is spelled `(give me a depth :!n tree of :x)`. Watch this:

```
((puts :?>) ~> :?>)
((give me a depth +0 tree of :x) ~> :x)
((give me a depth :!n tree of :x) ~> (puts (give me a depth (-1 :!n) tree of (:x :x))))
(give me a depth +2 tree of wow)
==> ROSIN RECURSES WITH A FEW INTERMEDIATE STEPS:
(give me a depth (-1 +2) tree of (wow wow))
(give me a depth (-1 +1) tree of ((wow wow) (wow wow)))
==> ROSIN THEN ULTIMATELY SAYS: ((wow wow) (wow wow))
```

In this way, Rosin is able to force something that resembles eager evaluation using these eager variables. Do note there is a performance cost here: a subsearch of the input trees is performed to ensure no rule matches the tree.

## Special accumulators

In the previous section, we presented Rosin with a rule that looks like this:

```
((puts :?>) ~> :?>)
```

This rule uses a *special accumulator* spelled `:?>` in which tells Rosin to write to the standard output. 

All special accumulators will match against certain terms, potentially appearing multiple times on the LHS. The *output* accumulator used above matches against any term, and it asks Rosin to write a visual representation of the matches to standard out.

This works how you would expect:
```
(puts hello)
==> ROSIN PRESENTS "hello" TO STANDARD OUT
```

Special accumulators can be matched multiple times on the LHS. This allows multiple values to be passed to a particular accumulator. In the case of the output accumulator `:?>`, these values are written to standard out in sequence.

```
((puts :?> :?>) ~> :?>)
(puts hello (world))
==> ROSIN PRESENTS "hello (world)" TO STANDARD OUT
```

Other special accumulators may have useful behavior when matched multiple times. For example, take the sum accumulator `:?+`. It only matches individual numbers (a plus sign followed by digits, see [Bare term datatypes](README.md#bare-term-datatypes)). When it matches multiple, Rosin adds them up and lets the result be filled in on the right hand side.

```
((:?+ plus :?+ plus :?+) ~> :?+)
(+5 plus +6 plus +7)
==> ROSIN SAYS: +18
(+5 plus (+0 plus +1 plus +2) plus +7)
==> ROSIN SAYS: +15
```

You may see [the full list of special accumulators in the README](README.md#special-accumulators).

## Cons Lists

Cons lists are lists formed by nested pairs of elements, called *cons cells*.

```
[I am a cons list! Check out my nesting...]
==> ROSIN SAYS: (I (am (a (cons (list! (Check (out (my (nesting... ())))))))))
```

The last piece of special pattern matching syntax allows you to explicitly match the tail of a list in a cons list. It used to look like this:

```
([head ..:tail] ~> ..:tail)
```

but is currently TODO.