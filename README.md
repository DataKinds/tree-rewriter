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

You should see a help prompt that looks like this: 

```
$ rosin --help
Rosin (they/them) is a tree rewriting language

Usage: rosin [INFILE] [-p|--print-output] [-v|--verbose]

  Invoke Rosin on input, either from standard input or from a file.

Available options:
  INFILE                   Rosin file to read and interpret
  -p,--print-output        Whether to print the final state of the input tree.
  -v,--verbose             Whether to print verbose debugging information.
                           Implies -p.
  -h,--help                Show this help text

Created by at/DataKinds in 2024. Comes with one warranty: if you can prove that
Rosin caused you physical or otherwise material injury, the current maintainer
will arrive and dispense one (1) sad platitude regarding your condition.
```

Note that running it without arguments will cause Rosin to attempt to read from standard input.

## The Nitty Gritty

### Template Haskell

This repo does contain a Template Haskell implementation of the langugage you can embed into your programs. Use with caution, this whole repo is currently very unstable.

See [blob/main/src/TH.hs](blob/main/src/TH.hs) for details. `Main.hs` also contains the following:

```
ensureTHCompilation :: [Tree RValue]
ensureTHCompilation = [
        [a|hello ~> world|],
        [a|(+1 :asd) ~> (+3 :asd)|],
        [a|(+1 :a +3) ~> (+3 :a +1)|],
        [a|[+1 +2 three four :a :b] ~> [:a :b]|],
        [a|[+1 +2 three four :a ..:b] ~> [:a ..:b]|],
        [a|(if true then :a else :b) ~> :a|],
        [a|(if false then :a else :b) ~> :b|],
        [a|(true) ~> true|],
        [a|(false) ~> false|],
        [a|(hello :world)|],
        [a|(hello world! (true) reversethis (+4 +1  +2 +3 +5))|],
        [a|(hello world)|],
        rbranch [rsym "hello", rsym "world!", rbranch [rsym "true"], rsym "reverse this", rbranch [rnum 4, rnum 1, rnum 2, rnum 3, rnum 5]]
    ]
```


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
* Allow all special accumulators to be used either eagerly (`!`) or non-eagerly.
* Implement Input accumulator.
* Parse strings and allow regex in pattern variables which match strings.
* Improved performance!

---

tree rewriter is totally in progress. it is currently a tree rewriting language supporting syntactic sugar for cons lists, special named accumulators for executing side effects and doing arithmetic, and bigint support. it is currently declarative: a file is read for rewriting rules first, then passed back over to rewrite all the trees.

it is planned to support regex matching rules on trees. it is also planned to allow live redefinition of rules (homoiconicity) a-la [modal](https://wiki.xxiivv.com/site/modal). in fact, this project's primary motivation is to be my test bed for what a slightly more feature-rich [modal](https://wiki.xxiivv.com/site/modal) could look like. 

enter the main directory and run with `stack run sample/main.tree`. 

the syntax is comprised of sexpr trees with special named terms. a term starting with `.` is an integer. a term starting with `:` is a rewrite pattern variable. a term starting with `:?` is a special accumulator. a cons list may have a final term starting with `..:` on the LHS of a rewrite term to match the tail of the list. 