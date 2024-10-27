# Rosin

## Easy Intro

### Overview

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

Everything prefaced with `==> ROSIN SAYS:` is the output of running the interpreter and added for dramatic effect.

Rosin will apply the rules you present to it everywhere it can, until they can no longer be applied. 
```
this (is (a (tree)))
==> ROSIN SAYS: (woah! it's (a (tree)))
```

### Patterns and variables

The left side of the `~>` in a rule is a *pattern*. Barring all else, Rosin will only rewrite a term if it matches exactly what they saw in the pattern. There are a few exceptions to this. You may present Rosin with a *variable*, prefixed with `:`, which they will match against any term:
```
((my :a rule) ~> (my totally :a rule))
(my awesome rule)
==> ROSIN SAYS: (my totally awesome rule)
```

There is one other special type of variable that we'll talk about in the later discussion on cons lists.

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

### Eager variables 

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

## The Nitty Gritty

### Bare term datatypes

Inside of the S-expressions, there are a few datatypes that can occur as tree leaves. The trees are parameterized over these datatypes:

| Name | Spelling | Description |
|------|----------|-------------|
| Number | `\+-?[0987654321.]+` | Bignum. Not stored as text.
| Symbol | `[^[]():/ \t]+` | 
| Pattern variable | `:[^[]():/ \t]+` | 

### Special accumulators

These special accumulators take some sort of action on the matched values. Some of them match eagerly, which is indicated by a bang `!`. All are prefaced with a colon `:` to indicate they match like pattern variables.

| Name | Spelling | Matches | Behavior |
|------|----------|---------|----------|
| Sum | `:?+` | Only numbers | Sums up all matched terms, produces one number | 
| Negate | `:?-` | Only numbers | Negates (additive inverse) all matched terms, produces as many terms as matched | 
| Product | `:?*` | Only numbers | Multiplies all matched terms,  produces one number | 
| Output | `:?>` | Anything | Writes the term to standard out, produces it unchanged | 
| Input | `:?<` | Anything | Still TODO | 
| Pack | `:?!@` | Only branches | Converts a cons list to a S-expression. This accumulator matches eagerly, so all computation must be completed before packing. | 
| Unpack | `:?!%` | Only branches | Converts an S-expression to a cons list. This accumulator matches eagerly, so all computation must be completed before unpacking. | 


## Planned features
* Allow all special accumulators to be used either eagerly or non-eagerly
* Input accumulator

---

tree rewriter is totally in progress. it is currently a tree rewriting language supporting syntactic sugar for cons lists, special named accumulators for executing side effects and doing arithmetic, and bigint support. it is currently declarative: a file is read for rewriting rules first, then passed back over to rewrite all the trees.

it is planned to support regex matching rules on trees. it is also planned to allow live redefinition of rules (homoiconicity) a-la [modal](https://wiki.xxiivv.com/site/modal). in fact, this project's primary motivation is to be my test bed for what a slightly more feature-rich [modal](https://wiki.xxiivv.com/site/modal) could look like. 

enter the main directory and run with `stack run sample/main.tree`. 

the syntax is comprised of sexpr trees with special named terms. a term starting with `.` is an integer. a term starting with `:` is a rewrite pattern variable. a term starting with `:?` is a special accumulator. a cons list may have a final term starting with `..:` on the LHS of a rewrite term to match the tail of the list. 