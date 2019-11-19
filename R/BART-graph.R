#' Get posterior tree draws into XXX format
#'
#' @param model BART model
#'
#' @return List of XXX objects.
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
        cols(
          node = col_integer(),
          var = col_integer(),
          cut = col_integer(),
          leaf = col_double()
        ),
      skip = 1,
      na = c(""),
      progress = F
      )
    )

  # indexing and tier
  out$trees <- mutate(
    out$trees,
    tier = floor(log2(node)),
    var = var + 1L, # R indexing at 1
    cut = cut + 1L # R indexing at 1
  )

  # define tree id and mcmc iteration number
  out$trees <-  mutate(out$trees,
    unique_tree_id = cumsum(is.na(var) & is.na(cut) & is.na(leaf)),
    iter = (unique_tree_id - 1L) %/% out$n_tree + 1L,
    tree_id = (unique_tree_id - 1L) %% out$n_tree + 1L,
    unique_tree_id = NULL
  )

  # remove information about tree groups (was stored as missing lines)
  out$trees <- filter(out$trees, stats::complete.cases(out$trees))

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
