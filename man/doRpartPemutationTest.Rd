\name{doRpartPermutationTest}
\alias{doRpartPermutationTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to permutation test an rpart model.}
\description{
This function performs a permutation test of an rpart model.
}
\usage{
doRpartPermutationTest(dataset, responseidx, model, formula, nperms, nprocs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataset}{The dataset used to construct the rpart model.}
  \item{responseidx}{ The index of the response variable in the dataset. }
  \item{model}{ An rpart model fitted to the dataset. }
  \item{formula}{ The formula used to build the rpart model. }
  \item{nperms}{ The number of permutations to use. }
  \item{nprocs}{ The number of processors to use. If nprocs > 1, MPI will be used to parallelize the testing.}
}
\details{

This function performs a permutation test of an rpart model. Permutation testing
provides a means by which to test the null hypothesis that the relationship of
predictor variables to the response variable is random (i.e., no causal or
correlative association between predictor and response variable exists).
Described briefly, this function repeatedly regenerates the rpart model on a succession
of randomized datsets (i.e., datasets derived from the original dataset by
permuting the response variable). By generating many such permuted datasets
and determining a model for each, and we can determine the frequency of models
equal to or better than that which we observed from the original data.  This
frequency is our $P$-value, the probability of observing a model equal to or
better than that which we observed by chance alone.

}
\value{
	The original rpart object is returned. The cptable is augmented to show
	$P$-values for both relative and cross-validation error rates 
    at each level of splitting in the tree. Additionally, a third new 
	column gives the actual number of replicates used to generate the
	$P$-values at each level of splitting (the need for this column
	is explained in the technical report referenced below).
}
\references{ 
M. P. Cummings, D. S. Myers, and M. Mangelson. Applying Permutation Tests to
Tree-Based Statistical Models: Extending the R package rpart. 
Technical Report CS-TR-4581, UMIACS-TR-2004-24, Institute for Advanced Computer
Studies (UMIACS), April 2004.
}

\author{ Daniel S. Myers }

\examples{
data(iris);
fit <- rpart(Species~., data=iris);
fit <- doRpartPermutationTest(dataset=iris, responseidx=5, model=fit, 
						formula=Species~., nperms=100, nprocs=1);
printcp(fit);
#>    CP nsplit rel error xerror       xstd     Rpvalue     Xpvalue nreps
#>1 0.50      0      1.00   1.21 0.04836666          NA          NA    NA
#>2 0.44      1      0.50   0.80 0.06110101 0.008196721 0.008196721   122
#>3 0.01      2      0.06   0.11 0.03192700 0.009900990 0.009900990   101

}
\keyword{tree}

