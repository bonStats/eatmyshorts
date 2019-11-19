utils::globalVariables(c("node", "var", "leaf", "unique_tree_id", "iter", "tree_id"))

#' Get posterior tree draws into tibble format
#'
#' Tibble grouped by iteration (\code{iter}) and tree id (\code{tree_id}).
#'
#' @param model BART model
#'
#' @return A tibble
#'
#' @export
#'
posterior_trees <- function(model){

  UseMethod("posterior_trees")

}

posterior_trees_BART <- function(model){

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
    var = var + 1L, # R indexing at 1
    cut = cut + 1L # R indexing at 1
  )

  # define tree id and mcmc iteration number
  out$trees <-  dplyr::mutate(out$trees,
    unique_tree_id = cumsum(is.na(var) & is.na(cut) & is.na(leaf)),
    iter = (unique_tree_id - 1L) %/% out$n_tree + 1L,
    tree_id = (unique_tree_id - 1L) %% out$n_tree + 1L,
    unique_tree_id = NULL
  )

  # remove information about tree groups (was stored as missing lines)
  out$trees <- dplyr::filter(out$trees, stats::complete.cases(out$trees))

  # add children information
  out$trees <- dplyr::group_by(out$trees, iter, tree_id)
  out$trees <- dplyr::mutate(out$trees,
                             child_left = child_left(node),
                             child_right = child_right(node))

  # remove leaf info if no children
  out$trees <- dplyr::mutate(dplyr::ungroup(out$trees),
                             leaf = ifelse(is.na(child_left) & is.na(child_right), leaf, NA_real_),
                             parent = parent(node))

  # regroup
  out$trees <- dplyr::group_by(out$trees, iter, tree_id)

  return(out)

}

#' @export
posterior_trees.wbart <- function(model){

  posterior_trees_BART(model)

}

#' @export
posterior_trees.pbart <- function(model){

  posterior_trees_BART(model)

}

#' @export
posterior_trees.lbart <- function(model){

  posterior_trees_BART(model)

}

#' @export
posterior_trees.mbart <- function(model){

  posterior_trees_BART(model)

}

#' @export
posterior_trees.mbart2 <- function(model){

  posterior_trees_BART(model)

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
