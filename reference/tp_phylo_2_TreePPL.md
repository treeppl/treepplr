# Convert phylo obj to TreePPL tree

`tp_phylo_2_TreePPL` takes an object of class `phylo` and returns a
TreePPL json str ready to print in a data file for TreePPL use.

## Usage

``` r
tp_phylo_2_TreePPL(phylo_tree, age = "")
```

## Arguments

- phylo_tree:

  an object of class
  [ape::phylo](https://rdrr.io/pkg/ape/man/read.tree.html).

- age:

  a string that determine the way the age of the node are calculated
  (default "branch-length").

  - "branch-length" : Root's age = NA, branch-length from the parent
    branch

  - "top-down" : Root's age = 0.0, cumulative branch-length from root

  - "down-top" : Leaf's age = 0.0, cumulative branch-length from leaf

## Value

A TreePPL json str
