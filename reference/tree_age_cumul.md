# Calculate age in a tppl_tree

`tree_age_cumul` takes an tppl_tree and returns a json with branch
length accumulation as node's age.

## Usage

``` r
tree_age_cumul(tree, root_index, age = "branch-length")
```

## Arguments

- tree:

  an tppl_tree create with
  [tp_phylo_to_tppl_tree](http://treeppl.org/treepplr/reference/tp_phylo_to_tppl_tree.md).

- root_index:

  the index of the root in the tppl_tree.

- age:

  a string that determine the way the age of the node are calculated
  (default "branch-length").

  - "root-to-tip" : Root's age = 0.0, cumulative branch-length from root

  - "tip-to-root" : Leaf's age = 0.0, cumulative branch-length from leaf

## Value

A tppl_tree
