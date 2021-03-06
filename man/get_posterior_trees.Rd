% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tree-extract-BART.R
\name{get_posterior_trees}
\alias{get_posterior_trees}
\title{Get posterior tree draws into tibble format}
\usage{
get_posterior_trees(model, label_digits = 2)
}
\arguments{
\item{model}{BART model.}

\item{label_digits}{Rounding for labels.}
}
\value{
A tibble with columns to \describe{
\item{iter}{Integer describing unique MCMC iteration.}
\item{tree_id}{Integer. Unique tree id with each \code{iter}.}
\item{node}{Integer describing node in tree. Unique to each \code{tree}-\code{iter}.}
\item{parent}{Integer describing parent node in tree.}
\item{label}{Label for the node.}
\item{tier}{Position in tree hierarchy.}
\item{var}{Variable for split.}
\item{cut}{Numeric. Value of decision rule for \code{var}.}
\item{is_leaf}{Logical. \code{TRUE} if leaf, \code{FALSE} if stem.}
\item{leaf_value}{}
\item{child_left}{Integer. Left child of node.}
\item{child_right}{Integer. Right child of node.}
}
}
\description{
Tibble grouped by iteration (\code{iter}) and tree id (\code{tree_id}). All information
calculated by method is included in output. See \code{as_tidytree} for more
user-friendly method.
}
