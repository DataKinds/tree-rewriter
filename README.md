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

## Running the test suite

Once installed, you may run the included regression tests using the makefile: 
```
make test
```

## The Nitty Gritty

### Template Haskell

This repo does contain a Template Haskell implementation of the langugage you can embed into your programs. Use with caution, this whole repo is currently very unstable. This feature is also likely going to be dropped in the coming months as the TH usecase has diverged from the standalone usecase -- the runtime here is fairly heavy.

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
        branch [sym "hello", sym "world!", branch [sym "true"], sym "reverse this", branch [num 4, num 1, num 2, num 3, num 5]]
    ]
```


### Bare term datatypes

Inside of the S-expressions, there are a few datatypes that can occur as tree leaves. The trees are parameterized over these datatypes:

| Name | Spelling | Description |
|------|----------|-------------|
| Number | `\+-?[0987654321.]+` | Bignum. Not stored as text.
| Symbol | `[^[]():/ \t]+` | 
| Pattern variable | `:[^[]():/ \t]+` | 
| String | Anything between unescaped quotes | UTF-8 compatible
| Regex | Anything between unescaped forward slashes | Matches via [ICU](https://unicode-org.github.io/icu/userguide/strings/regexp.html#regular-expression-metacharacters)

### Special accumulators

These special accumulators take some sort of action on the matched values. You may place a bang `!` after the `?` sigil to make the special accumulator match eagerly. Some of these accumulators are much more useful when made eager, like Pack `?!@`.

| Name | Spelling | Matches | Behavior |
|------|----------|---------|----------|
| Sum | `?+` | Only numbers | Sums up all matched terms, produces one number | 
| Negate | `?-` | Only numbers | Negates (additive inverse) all matched terms, produces as many terms as matched | 
| Product | `?*` | Only numbers | Multiplies all matched terms,  produces one number | 
| Output | `?>` | Anything | Writes the matched term to standard out, produces it unchanged | 
| Input | `?<` | Anything | Still TODO | 
| Pack | `?@` | Only branches | Walks an S-expression in DFS order and flattens a nested structure. This converts a cons list to a S-expression. | 
| Unpack | `?%` | Only branches | Converts an S-expression to a cons list. | 


## Planned features
* [ ] Implement Input accumulator.
* [ ] Support capture groups from multiple regex matches in one rule.
* [ ] Support variable templating on multiset matches.
* [ ] Better [tutorial](./INTRO.md) or reference spec
* [ ] Real code written in Rosin
* [ ] Improved performance!

## Comparison with Modal/Thuesday

Rosin takes a large amount of inspiration from [Modal](https://wryl.tech/projects/modal.html), a tree rewriting language by *wryl*, and [Thuesday](https://wiki.xxiivv.com/site/modal), an extension to Modal by Hundred Rabbits. Rosin would also not exist in its current form without a great deal of input from *wryl* and others on the PLT and the Concatenative Discord servers. Here are a few notable differences:

* Modal/Thuesday are fundamentally *string rewriting* systems, whereas Rosin is fundamentally a *tree rewriting* system. This has a handful of knock on effects:
  * Modal is less picky about matching patterns offset within trees, since rules match via a linear scan of the input. So, a Modal rule like `<> (my awesome rule) (my very awesome rule)` will rewrite `(this gets modified by my awesome rule)` to `(this gets modified by my very awesome rule)`. But a Rosin rule like `((my awesome rule) ~> (my very awesome rule))` will NOT match against `(this gets modified by my awesome rule)`. 
    * Want to match offsets inside trees in Rosin? Use the [unpack accumulator](README.md#special-accumulators) with a pattern that matches cons lists.
  * In theory, Rosin may be faster than Modal for small rewrites on very large data structures. The current Modal interpreter has a constant overhead of copying a size 0x4000 memory region on every rewrite, even when no rules apply (see https://git.sr.ht/~rabbits/modal/tree/master/item/src/modal.c). Rosin has a constant overhead of walking the entire input tree, but small rewrites will only swap pointers in the parsed tree (and eventually trigger a GC sweep) under Haskell's data model. No data copying required. I have not measured it, but I suspect two things to be true, making this currently a moot point: 
    1. Rosin's overall constant overhead per rewrite step is likely much higher thanks to Haskell's generally unperformant runtime & my overuse of singly-linked lists, and
    2. The size of input tree needed to outweigh that constant overhead is larger than the 0x4000-byte region that Modal allocates to work in.
* Rosin does not currently allow rules to be destroyed after they're created. 
  * Modal allows this through the `><` operation. 
  * This will not always be the case -- rule destruction may be allowed in the future if I want the feature figure out a nice way to do it. Other than that,
* Rosin includes a wider set of "batteries included" features, such as cons list syntax and regex matching.
* Rosin uses [eager variables](./INTRO.md#eager-variables) to sequence computation. 
  * Modal does not include any evaluation sequencing primitives -- instead, it is recommended to use a quasiquoting rule like `<> (quote ?x) (quote ?x)` which causes computation to jump back to the beginning.
  * Rosin will always try to match closest to the top of the input data, so Rosin has "quasiquote" semantics by default. Forcing evaluation happens at the rule definition site instead of requiring the callee to explicitly quote or mark the data as eager.
* The goal of Rosin differs from that of Modal. 
  * Much of the work around Modal and successive languages (Nova, Mira) is centered around finding a new basis for computation. This basis must necessarily be very low-dependency and depend on very few primitives.
  * There is a focus on permacomputing -- the development of systems more intuitive, universal, resillient, low-resource-usage, such that computing becomes natural and sustainable. 
  * Rosin is at odds with this. It takes the intuitive model of computing developed above and tacks on all the nuts and bolts I can find. I aim to provide a maximalist rewriting environment that straddles the line of "simple, intuitive base" with a low-frustration programming experience. 
  * Additionally, Haskell's rich repertoire of libraries and declaritivity lets me iterate on the language faster, despite being a possibly poor choice for a language runtime where speed is a concern.
