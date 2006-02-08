\name{rpart.permutation.addPvalues}
\alias{rpart.permutation.addPvalues}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to add P-values to an rpart object. }
\description{
	Computes P-values from raw permutation replicate counts.
}
\usage{
rpart.permutation.addPvalues(model, nseen, nbetterR, nbetterX)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{ An object as returned from the rpart function. }
  \item{nseen}{ An integer vector such that nseen[i] is the number of replicates seen for the subtree with i splits. }
  \item{nbetterR}{ An integer vector such that nbetterR[i] is the number of replicates for the subtree with i splits with a relative error better than the original model. }
  \item{nbetterX}{ As nbetterR, except for cross-validation error, not relative error. }
}
\value{
  The model passed in is returned. The cptable is augmented
  to include $P$-values for the relative and cross-validation errors.
}
\author{ Daniel S. Myers }

\keyword{tree}

