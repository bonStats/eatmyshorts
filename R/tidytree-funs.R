#' Get posterior tree draws into `tbl_tree` format from `tidytree` package.
#'
#' Creates a `tbl_tree` grouped by iteration (`iter`) and tree id (`tree_id`).
#' See Details below.
#'
#' @param model BART model.
#' @param extra_cols Extra columns to be included.
#' @param label_digits Rounding for labels.
#'
#' @return A tibble with required columns for `tbl_tree`:
#'   `node`, `parent`, `label`.
#'   And columns `iter`, `tree_id` are used to differentiate trees and iterations and are always included.
#'   Remaining columns are optional (see details).
#'
#' @details List of potential columns returned:
#'   \describe{
#'   \item{iter}{Integer describing unique MCMC iteration.}
#'   \item{tree_id}{Integer. Unique tree id with each `iter`.}
#'   \item{node}{Integer describing node in tree. Unique to each `tree`-`iter`.}
#'   \item{parent}{Integer describing parent node in tree.}
#'   \item{label}{Label for the node.}
#'   \item{tier}{Position in tree hierarchy.}
#'   \item{var}{Variable for split.}
#'   \item{cut}{Numeric. Value of decision rule for `var`.}
#'   \item{is_leaf}{Logical. `TRUE` if leaf, `FALSE` if stem.}
#'   \item{leaf_value}{Numeric (mean function) value of leaf}
#'   \item{child_left}{Integer. Left child of node.}
#'   \item{child_right}{Integer. Right child of node.}
#'   }
#'
#' @export
#'
as_tidytree <- function(model, extra_cols, label_digits = 2) {
  keep_cols <- c("iter", "tree_id", "node", "parent", "label")

  if (!missing(extra_cols)) {
    stopifnot(is.character(extra_cols))
    keep_cols <- unique(c(keep_cols, extra_cols))
  }

  out <- dplyr::select(
    get_posterior_trees(model, label_digits = label_digits)$tree,
    dplyr::one_of(keep_cols)
  )

  class(out) <- c("grouped_df", "tbl_tree", "tbl_df", "tbl", "data.frame")

  return(out)
}
