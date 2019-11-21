utils::globalVariables(c("new_node", "new_parent"))

#' Get posterior tree draws into `tbl_graph` format from `tidygraph` package.
#'
#' Creates a list of `tbl_graph`'s. Each element of list corresponds to a particular MCMC iteration (`iter`) and tree id (`tree_id`).
#' See [tbl_graph][tidygraph::tbl_graph] for details.
#'
#' @param model BART model.
#' @param extra_cols Extra columns to be included.
#' @param label_digits Rounding for labels.
#'
#' @return List of `tbl_graph`.
#'
#' @export
as_tidygraph_list <- function(model, extra_cols, label_digits = 2){

  keep_cols <- c("iter", "tree_id", "node", "parent", "label")

  if(!missing(extra_cols)){
    stopifnot(is.character(extra_cols))
    keep_cols <- unique(c(keep_cols, extra_cols))
  }

  res <- dplyr::select(
    get_posterior_trees(model, label_digits = label_digits)$tree,
    dplyr::one_of(keep_cols)
  )

  # reorder
  res <- dplyr::select(res, -iter, -tree_id, everything() )

  # new sequential id for nodes (1,2,3...)
  res <- dplyr::mutate(res,
                new_node = seq_along(node),
                new_parent = new_node[match(parent, node)],
                node = new_node,
                parent = new_parent)
  res <- dplyr::select(res, -new_node, - new_parent)

  node_list <- dplyr::group_split(select(res, -parent), keep = T)
  edge_list <- purrr::map(dplyr::group_split(dplyr::select(res, iter, tree_id, parent, node), keep = F), ~ dplyr::filter(., !is.na(parent)))

  tbl_graph_list <- purrr::map2(node_list, edge_list, ~ tidygraph::tbl_graph(nodes = .x, edges = .y, directed = T))

  return(tbl_graph_list)
}
