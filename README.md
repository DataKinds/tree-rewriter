# tree-rewriter

tree rewriter is totally in progress. it is currently a tree rewriting language supporting syntactic sugar for cons lists, special named accumulators for executing side effects and doing arithmetic, and bigint support. it is currently declarative: a file is read for rewriting rules first, then passed back over to rewrite all the trees.

it is planned to support regex matching rules on trees. it is also planned to allow live redefinition of rules (homoiconicity) a-la [modal](https://wiki.xxiivv.com/site/modal). in fact, this project's primary motivation is to be my test bed for what a slightly more feature-rich [modal](https://wiki.xxiivv.com/site/modal) could look like. 

enter the main directory and run with `stack run sample/main.tree`. 