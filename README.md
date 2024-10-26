# Rosin

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

The other way to do this is through the use of *eager variables*. Rosin would very much like to match eager variables last, allowing all other computation to happen first. You write an eager variable with the prefix `:!` and Rosin will not allow the pattern to match if that variable could be matched by another rule. 

Let's demonstrate eager variables by building up a larger example:

```
(; (Comments! This just deletes all terms of the form (; ...) ))
((; :comment) ~>)
==> ROSIN SAYS: (defined "(; :comment) ~> ")

(; (Let's establish all the arithmetic we need for this example!))
((-1 +3) ~> +2)
==> ROSIN SAYS: (defined "(-1 +3) ~> +2")
((-1 +2) ~> +1)
==> ROSIN SAYS: (defined "(-1 +2) ~> +1")
((-1 +1) ~> +0)
==> ROSIN SAYS: (defined "(-1 +1) ~> +0")
((-1 +0) ~> +0)
==> ROSIN SAYS: (defined "(-1 +0) ~> +0")

(; (Now let's try to write a "function" that makes a 2-tree of depth N filled up with a certain value.))
(; (First, the base case: a depth 0 tree gives back the value.))
((give me a depth +0 tree of :x) ~> :x)
==> ROSIN SAYS: (defined "(give me a depth +0 tree of :x) ~> :x")
((give me a depth :n tree of :x) ~> (give me a depth (-1 :n) tree of (:x :x)))
==> ROSIN SAYS: (defined "(give me a depth :n tree of :x) ~> (give me a depth (-1 :n) tree of (:x :x))")
(; (Without eager variables, ))
```


---

tree rewriter is totally in progress. it is currently a tree rewriting language supporting syntactic sugar for cons lists, special named accumulators for executing side effects and doing arithmetic, and bigint support. it is currently declarative: a file is read for rewriting rules first, then passed back over to rewrite all the trees.

it is planned to support regex matching rules on trees. it is also planned to allow live redefinition of rules (homoiconicity) a-la [modal](https://wiki.xxiivv.com/site/modal). in fact, this project's primary motivation is to be my test bed for what a slightly more feature-rich [modal](https://wiki.xxiivv.com/site/modal) could look like. 

enter the main directory and run with `stack run sample/main.tree`. 

the syntax is comprised of sexpr trees with special named terms. a term starting with `.` is an integer. a term starting with `:` is a rewrite pattern variable. a term starting with `:?` is a special accumulator. a cons list may have a final term starting with `..:` on the LHS of a rewrite term to match the tail of the list. 