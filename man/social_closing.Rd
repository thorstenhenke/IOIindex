% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ioi-index.R
\name{social_closing}
\alias{social_closing}
\alias{social_closing.list}
\alias{social_closing.network}
\alias{social_closing.igraph}
\alias{social_closing.matrix}
\alias{social_closing.default}
\alias{social_opening.list}
\alias{social_integration.list}
\title{Social closing coefficient}
\usage{
social_closing(net, ..., depth = 0)

\method{social_closing}{list}(net, vname, mode = c("out", "in"), ...,
  depth = 0)

\method{social_closing}{network}(net, vname, mode = c("out", "in"), ...)

\method{social_closing}{igraph}(net, vname, mode = c("out", "in"), ...)

\method{social_closing}{matrix}(net, v, mode = c("out", "in"), ...)

\method{social_closing}{default}(net, depth = 0, ...)

\method{social_opening}{list}(net, vname, mode = c("out", "in"), ...,
  depth = 0)

\method{social_integration}{list}(net, vname, mode = c("out", "in"), ...,
  depth = 0)
}
\arguments{
\item{net}{Network of type matrix, igraph, network or a list of objects of these datatypes.
The list objects can even be mixed, meaning that some objects are of type network, some of type
igraph, etc. The networks can either be directed or undirected. In case of a directed network it
is recommended to specify whether the indices should be calculated based on the incoming network
ties or outgoing network ties.}

\item{...}{Potential additional paramters. .}

\item{depth}{this paramter should not be manipulated! It controls the recursion depth of the function
in case net is of type list.}

\item{vname}{character variable indicating which variable should be used for grouping. This parameter
can only be used if net is of type igraph, network or list. Please note, that in case you are
using the list input, the variable has to be present in each of the list objects. This parameter
can only be used if net is of type igraph, network or list.}

\item{mode}{This parameter will only be evaluated if the network is directed. In case of an undirected
network this parameter will be ignored.}

\item{v}{vector of node attributes. This parameter is only relevant if net is of type matrix. Otherwise
this parameter should be ignored and left as it is.}
}
\value{
a numeric vector of social closing indices for each indvidual in the network.
}
\description{
`social_closing()` calculates the social closing coefficient for a given network and node attribute.
}
\section{Methods (by class)}{
\itemize{
\item \code{list}: net is of type list

\item \code{network}: net is of type network

\item \code{igraph}: net is of type igraph

\item \code{matrix}: net is of type matrix

\item \code{default}: error case

\item \code{list}: net is of type list

\item \code{list}: net is of type list
}}

\examples{
\dontrun{
data("faux.magnolia.high", package = "ergm")
fmh <- faux.magnolia.high ; rm(faux.magnolia.high)
social_closing(fmh, "Grade")

x <- network::as.sociomatrix(fmh)
v <- network::get.vertex.attribute(fmh, "Grade")
social_closing(x, v)

gg <- igraph::graph_from_adjacency_matrix(x)
igraph::vertex_attr(gg, "Grade") <- v

social_closing(list(fmh, fmh), "Grade")
social_closing(list(gg, fmh), "Grade")
}
}
