\name{rpart.permutation.mergeResults}
\alias{rpart.permutation.mergeResults}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to merge results from a series of MPI client nodes. }
\description{
This internal function merges results from Message Passing Interface (MPI) client nodes.
}
\usage{
rpart.permutation.mergeResults(results)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{results}{ A result list as returned by the clusterCall function of \code{\link[Rmpi]{Rmpi}}.}
}
\value{
  A list with the following elements:
  \item{R}{Integer vector such that the ith element is the number of replicates having a better relative error than the original model for the i-split subtree.}
  \item{X}{As above, but using cross-validation error instead of relative error.}
  \item{N}{Integer vector such that the ith element is the total number of replicates reporting error values for the i-split subtree.}
}
\author{ Daniel S. Myers }

\keyword{tree}

