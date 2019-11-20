utils::globalVariables(c("node", "var", "leaf", "unique_tree_id", "iter", "tree_id", "is_leaf", "label", "leaf_value", "tier"))

#' Get posterior tree draws into tibble format
#'
#' Tibble grouped by iteration (\code{iter}) and tree id (\code{tree_id}).
#' All information calculated by method is included in output.
#' See \code{as_tidytree} for more user-friendly method.
#'
#' @param model BART model.
#' @param label_digits Rounding for labels.
#'
#' @return A tibble with columns to \describe{
#'   \item{iter}{Integer describing unique MCMC iteration.}
#'   \item{tree_id}{Integer. Unique tree id with each \code{iter}.}
#'   \item{node}{Integer describing node in tree. Unique to each \code{tree}-\code{iter}.}
#'   \item{parent}{Integer describing parent node in tree.}
#'   \item{label}{Label for the node.}
#'   \item{tier}{Position in tree hierarchy.}
#'   \item{var}{Variable for split.}
#'   \item{cut}{Numeric. Value of decision rule for \code{var}.}
#'   \item{is_leaf}{Logical. \code{TRUE} if leaf, \code{FALSE} if stem.}
#'   \item{leaf_value}{}
#'   \item{child_left}{Integer. Left child of node.}
#'   \item{child_right}{Integer. Right child of node.}
#'   }
#'
#' @export
#'
get_posterior_trees <- function(model, label_digits = 2){

  UseMethod("get_posterior_trees")

}

posterior_trees_BART <- function(model, label_digits){

  var_names <- names(model$treedraws$cutpoints)

  cut_points_tb <- purrr::map_df(
    model$treedraws$cutpoints,
    ~ dplyr::tibble(cut = ., cut_id = 1:length(.)),
    .id =  "var"
    )

  out <- list()

  # first line contains mcmc draws
  fline <- strsplit(readr::read_lines(file = model$treedraws$trees, n_max = 1), " ")[[1]]
  out$n_mcmc <- as.integer(fline[1])
  out$n_tree <- as.integer(fline[2])
  out$n_var <- as.integer(fline[3])

  out$trees <- suppressWarnings(
      readr::read_table2(
        file = model$treedraws$trees,
        col_names = c("node", "var", "cut", "leaf"),
        col_types =
          readr::cols(
            node = readr::col_integer(),
            var = readr::col_integer(),
            cut = readr::col_integer(),
            leaf = readr::col_double()
          ),
        skip = 1,
        na = c(""),
        progress = F
      )
    )

  # indexing and tier
  out$trees <- dplyr::mutate(
    out$trees,
    tier = as.integer(floor(log2(node))),
    cut_id = cut + 1L, # R indexing at 1
    var = var_names[var + 1L] # R indexing at 1
  )

  # define tree id and mcmc iteration number
  out$trees <-  dplyr::mutate(
    out$trees,
    unique_tree_id = cumsum(is.na(var) & is.na(cut) & is.na(leaf)),
    iter = (unique_tree_id - 1L) %/% out$n_tree + 1L,
    tree_id = (unique_tree_id - 1L) %% out$n_tree + 1L,
    unique_tree_id = NULL
  )

  # remove information about tree groups (was stored as missing lines)
  out$trees <- dplyr::filter(out$trees, stats::complete.cases(out$trees))

  # add cut information
  out$trees <- dplyr::left_join(
    dplyr::select(out$trees,-cut),
    cut_points_tb,
    by = c("var", "cut_id")
    )

  # add children information
  out$trees <- dplyr::group_by(out$trees, iter, tree_id)
  out$trees <- dplyr::mutate(
    out$trees,
    child_left = child_left(node),
    child_right = child_right(node)
    )

  # remove leaf info if no children
  out$trees <- dplyr::mutate(
    dplyr::ungroup(out$trees),
    is_leaf = is.na(child_left) & is.na(child_right),
    leaf_value = ifelse(is_leaf, leaf, NA_real_),
    cut = ifelse(is_leaf, NA_real_, cut), # is leaf, then no cut for stem
    var = ifelse(is_leaf, NA_character_, var), # is leaf, then no var for cut
    label = ifelse(
      is_leaf,
      as.character(round(leaf_value, digits = label_digits)),
      paste(var, ">", round(cut, digits = label_digits))
      ),
    parent = parent(node)
    )

  # regroup
  out$trees <- select(
    dplyr::group_by(out$trees, iter, tree_id),
    iter,
    tree_id,
    node,
    parent,
    label,
    tier,
    var,
    cut,
    is_leaf,
    leaf_value,
    child_left,
    child_right
  )

  return(out)

}

#' @export
get_posterior_trees.wbart <- function(model, label_digits = 2){

  posterior_trees_BART(model, label_digits = label_digits)

}

#' @export
get_posterior_trees.pbart <- function(model, label_digits = 2){

  posterior_trees_BART(model, label_digits = label_digits)

}

#' @export
get_posterior_trees.lbart <- function(model, label_digits = 2){

  posterior_trees_BART(model, label_digits = label_digits)

}

#' @export
get_posterior_trees.mbart <- function(model, label_digits = 2){

  posterior_trees_BART(model, label_digits = label_digits)

}

#' @export
get_posterior_trees.mbart2 <-function(model, label_digits = 2){

  posterior_trees_BART(model, label_digits = label_digits)

}

child_left <- function(nodes){

  # must be grouped by iter and tree to apply
  pot_child <- nodes*2L
  pot_child[!pot_child %in% nodes] <- NA_integer_

  return(pot_child)

}

child_right <- function(nodes){

  # must be grouped by iter and tree to apply
  pot_child <- nodes*2L + 1L
  pot_child[!pot_child %in% nodes] <- NA_integer_

  return(pot_child)

}

parent <- function(nodes){

  parents <- nodes %/% 2L
  parents[parents == 0L] <- NA_integer_

  return(parents)

}
