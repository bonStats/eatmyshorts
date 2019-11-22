#' eatmyshorts: Visualisation for BART models
#'
#' Visualisation methods and functions for exploring Bayesian Additive
#'   Regression Trees
#'
#' @docType package
#' @name eatmyshorts
#' @importFrom readr read_table
#' @importFrom purrr map_df map2
#' @import dplyr
#' @importFrom tidygraph tbl_graph
#'
NULL

utils::globalVariables(
  c(
    "new_node",
    "new_parent",
    "node",
    "var",
    "leaf",
    "unique_tree_id",
    "iter",
    "tree_id",
    "is_leaf",
    "label",
    "leaf_value",
    "tier"
  )
)



