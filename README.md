# Rosin

Rosin is a *tree rewriting language*. This means you pass the interpreter rewrite rules and input trees, and the interpreter runs computations by applying these rewrite rules.

Interested in learning more? Visit [INTRO.md](./INTRO.md).

## Installation and usage

Using a relatively recent version of [Haskell Stack](https://docs.haskellstack.org/en/stable/), run the following command to build and install Rosin:
```
stack install
```

Then, the interpreter will be available in Stack's standard install directory. Run it like any other program on your PATH:
```
rosin sample/main.tree
```


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
| Output | `:?>` | Anything | Writes the matched term to standard out, produces it unchanged | 
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