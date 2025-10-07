# How does Rosin execute code?
This document will serve as a *prescriptive* model of Rosin's execution. Whether the interpreter currently follows this model is a question of correctness and completeness.
## State in Rosin
Rosin holds a few pieces of state in order to execute on input it receives. The state is as follows:

* The **dictionary**, which holds the active Rosin rules.
* The **tree**, which is an S-expression that is fed into Rosin to begin execution. We may refer to contiguous slices of this **tree** as **subtrees**. The leaf values of this **tree** are referred to as **terms**. Any element of the **tree** may be **tagged** with arbitrary data as required by this spec.
* The **pointer**, which points to the currently focused **subtree**. The **pointer** must be able to move up, down, left, or right within the **tree**.
* The **bag**, which is a multiset that contains **subtrees**. We may call these subtrees **items**.
<!-- * The **epoch number**, which starts at 0 and is incremented every time a **rule** is introduced or the **bag** is changed. -->
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

### Pattern variables

A **pattern** relies on **pattern variables** to capture values out from **subtree**s. **Pattern variables** are denoted by a colon (`:`) and may appear inside both **tree patterns** and **bag patterns**. A few examples:

* `((pop :x) ~>)` matches a **subtree** of the form `(pop it)`, where `:x` binds to `it`.
* `((get rid of a :y) ~> & :y |)` matches a **subtree** of the form `(get rid of a (fruit salad))`, where `:y` binds to `(fruit salad)` but must also remove a `(fruit salad)` from the **bag**.

Note that the order a **pattern variable** is bound depends on the order of the **patterns** in the **rule**. Notice how the following rule differs from the one above:

* `(:y | & (get rid of a :y) ~>)` matches something of the **bag** and binds it to `:y`. It must then match a **subtree** of the form `(get rid of a :y)`, where `:y` is given by the **subtree** taken from the **bag** above. Since there is no defined order for the bag to return its items, this is not a very useful rule.


### Eager patterns
Rosin supports **eager patterns** for enforcing an execution order on **rules**, where specific **subtrees** inside **patterns** can be marked as **eager**, meaning they'll fully evaluate (see [The rewrite loop](#the-rewrite-loop)) any matching **subtree** before applying the **eager pattern**. This is denoted by a `:!` in place of a `:`. Any side effects caused by evaluating the **subtree** will be carried out. Only one instance of a given **pattern variable** must be marked as eager, and the evaluation will carry out exactly once per unique **pattern variable**. For example:

* `(apple ~> & | slices) ((salad :fruit) ~> eaten) (salad apple)` gives back `eaten` without modifying the bag, because the first rule never matches.
* `(apple ~> & | slices) ((salad :!fruit) ~> eaten) (salad apple)` gives back `eaten` but with `slices` in the bag, because `:!fruit` eagerly forced the first rule to evaluate on the **subtree** `apple`.


## The rewrite loop
Rosin will set up a few things before execution begins:
<!-- * Setting the **epoch number** to 0. -->
* Filling out the **dictionary** with built-in rules.
* Placing the **pointer** at the first element of the **tree**, when read in DFS order.

Rosin expects to be able to get the **next pointer**, which is the next pointer in the **tree** when the tree is read in DFS order, or an exception if the **pointer** is at the end of the **tree**.

Rosin expects to be able to **consume** a tree, which involves removing the **pointer**'s focused **subtree**, moving the 

Rosin applies **rules** in a loop until it can no longer apply any **rule** across the entire **tree**. The exact procedure for doing this is as follows:

1. Set the **done marker** to true
2. Carry out rule application until rules no longer match:
	1. Try applying all **single-use rules** in the **dictionary** to the **pointer**, in order of when they were added to the **dictionary**. More recent rules should be applied with higher priority.
		1. If any **rule** applies, set the **done marker** to false. 
	2. Try applying all **rules** in the **dictionary** to the **pointer**, in order of when they were added to the **dictionary**. More recent rules should be applied with higher priority.
		1. If any **rule** applies, set the **done marker** to false. 
	3. If any **rule** applied in 2.1 or 2.2, jump to 2.
3. Try consuming the **pointer** as a **rule definition** (i.e. the `(x ~> y & a |> b)` structure given above).
	1. If this consuming is successful, set the **done marker** to false.
4. Modify the pointer or wrap up:
	1. If the current **pointer** is at the last element AND the **done marker** is true, finish execution.
	2. If the current **pointer** is at the last element AND the **done marker** is false, set the **pointer** to the first element in the **tree** and jump to step 1.
	3. If the **done marker** is true (i.e. no rules applied), set the **pointer** to the **next pointer**.

This procedure ensures a couple of assumptions are always preserved in favor of making Rosin easier to reason about:

* Given the same input **tree** and same **bag**, **rule** application follows a strict hierarchy of priorities: single-use rules will always apply before multi-use rules, and within a category of rules the most recently consumed rule will apply before an older rule.
* Rules which do not match anything in the tree (like `(apple |> salad)`) will apply to completion -- until they can no longer be applied -- before Rosin will move the **pointer**.
* Aside from the above two points, the only way to enforce ordering on **rule** execution is through **eagerness**.