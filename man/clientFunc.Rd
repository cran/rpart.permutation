\name{rpart.permutation.clientFunc}
\alias{rpart.permutation.clientFunc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Worker function for MPI client nodes }
\description{
This internal function is executed on MPI client nodes and executes the
rpart calls needed to generate permutation replicates.
}
\usage{
rpart.permutation.clientFunc(dataset, responseidx, model, formula, nperms)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataset}{ The dataset on which the rpart models will be built. }
  \item{responseidx}{ The index in \code{dataset} of the response variable. }
  \item{model}{ The original rpart model. }
  \item{formula}{ The formula used to create \code{model}. }
  \item{nperms}{ Number of permutations to execute. }
}
\value{
  A list with the following elements:
  \item{R}{Integer vector such that the ith element is the number of replicates having a better relative error than the original model for the i-split subtree.}
  \item{X}{As above, but using cross-validation error instead of relative error.}
  \item{nseen}{Integer vector such that the ith element is the total number of replicates reporting error values for the i-split subtree}
}
\author{ Daniel S. Myers }

\keyword{tree}

