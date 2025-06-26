# How does Rosin execute code?
This document will serve as a *prescriptive* model of Rosin's execution. Whether the interpreter currently follows this model is a question of correctness and completeness.
## State in Rosin
Rosin holds a few pieces of state in order to execute on input it receives. The state is as follows:

* The **dictionary**, which holds the active Rosin rules.
* The **tree**, which is an S-expression that is fed into Rosin to begin execution. We may refer to contiguous slices of this **tree** as **subtrees**. The leaf values of this **tree** are referred to as **terms**. Any element of the **tree** may be **tagged** with arbitrary data as required by this spec.
* The **pointer**, which points to the currently focused **subtree**. The **pointer** must be able to move up, down, left, or right within the **tree**.
* The **bag**, which is a multiset that contains **subtrees**. We may call these subtrees **items**.
* The **epoch number**, which starts at 0 and is incremented every time a **rule** is introduced or the **bag** is changed.
* The **done marker**, which is a boolean that is used to determine whether execution is complete.

TL;DR: Rosin input is a **tree** (S-expression) of **terms**. It remembers rewrite **rules** in the **dictionary**. It's also got a **bag** full of **items** that are actually **subtrees**.
## Rules in Rosin
**Rules** are split into two distinct sections. A **rule** has one or more **pattern**s, which match against either the **pointer** or the **multiset**. Once all patterns match, the rule applies one or more **effect**s. **Effects** may replace the **pointer**, add or remove **items** from the **bag**, or carry out side effects. 

Here is an example rule in Rosin syntax:

```
(hello ~> world & (bag item) |> (awesome item))
```

This rule has exactly two **patterns**: the first **pattern** (`hello ~>`) matches a single **term** `hello`, and the second pattern (`(bag item) |>`) matches an **item** `(bag item)`. The first pattern is a **tree pattern** and the second pattern is a **bag pattern**.

This rule has four **effects**: `~> world` encodes the effect of replacing the **pointer** with the **term** `world`. This is technically two effects: clearing the **subtree** under the **pointer**, then replacing the **subtree** under the **pointer**. This allows for rules like `(hello ~>)` which *only* have the effect of clearing the **pointer**. Likewise, `|> (awesome item)` encodes the effect of replacing the matched **item** with the new **item** `(awesome item)`. This decomposes to two effects for the same reason as above. One effect will remove the matched **item** from the **bag**, the other effect will push `(awesome item)` to the **bag**.

Matching all **patterns** in a **rule** then carrying out all **effects** in a **rule** may be called **applying a rule**.

### Eager patterns
Rosin supports **eager patterns** (TODO: maybe *delayed patterns* is a better name?) for ordering **rules**, where specific **subtrees** inside **patterns** can be marked as **eager**, meaning they'll refuse to match if any **subtree** matches another **rule**. This has the effect of *eagerly evaluating* the **subtree** before applying the **eager pattern**. 

This may be implemented naively with O(n^2) time complexity, but this spec rejects that in favor of the **epoch number**: **eager patterns** should only match if all matching **subtrees** are tagged with the current **epoch number**! This brings the time complexity down to O(n) on the size of the **subtree** in exchange for less predictable **eager pattern** semantics.

## The rewrite loop
Rosin will set up a few things before execution begins:
* Setting the **epoch number** to 0.
* Filling out the **dictionary** with built-in rules.
* Placing the **pointer** at the first element of the **tree**, when read in DFS order.

Rosin also expects to be able to get the **next pointer**, which is the next pointer in the **tree** when the tree is read in DFS order, or an exception if the **pointer** is at the end of the **tree**.

Rosin applies **rules** in a loop until it can no longer apply any **rule** across the entire **tree**. The exact procedure for doing this is as follows:

1. Set the **done marker** to true
2. Try consuming the **pointer** as a **rule definition** (i.e. the `(x ~> y & a |> b)` structure given above).
	1. If this consuming is successful, add one to the **epoch number** and set the **done marker** to false.
3. Try applying all **rules** in the **dictionary** to the **pointer**, in order of when they were added to the **dictionary**.
	1. If a **bag effect** is carried out, add one to the **epoch number**.
	2. If any rule applies, set the **done marker** to false and jump back to step 3. This heuristic allows faster processing.
	3. If no rules could be applied, **tag** all nodes in the **pointer**'s **subtree** with the **epoch number**. This ensures that eager patterns match correctly.
4. Try applying all **rules** in the **dictionary** which do not have a **tree pattern** as many times as they will apply.
	1. If any rule applies, set the **done marker** to false and add one to the **epoch number**.
5. Get the **next pointer**.
	1. If the current **pointer** is at the last element AND the **done marker** is true, finish execution.
	2. If the current **pointer** is at the last element AND the **done marker** is false, set the **pointer** to the first element in the **tree** and jump to step 1.