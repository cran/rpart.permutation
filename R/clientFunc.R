"rpart.permutation.clientFunc" <-
function (dataset, responseidx, model, formula, nperms) 
{
    maxsplits <- model$cptable[length(model$cptable[, 2]), 2]
    nbetterX <- array(0, maxsplits)
    nbetterR <- array(0, maxsplits)
    nseen <- array(0, maxsplits)
    origerrorsR <- array(NA, maxsplits)
    origerrorsX <- array(NA, maxsplits)
    for (i in 1:length(model$cptable[, 2])) {
        nsplits <- model$cptable[i, 2]
        origerrorsR[nsplits] <- model$cptable[i, 3]
        origerrorsX[nsplits] <- model$cptable[i, 4]
    }
    underFullBins <- length(model$cptable[, 2]) - 1
    if (maxsplits == 0) {
        underFullBins = 0
    }
    permuted <- dataset
    ntot <- 0
    while (underFullBins > 0) {
        permuted[, responseidx] <- sample(dataset[, responseidx])
        fit <- rpart(formula, permuted, control = rpart.control(maxdepth = maxsplits + 
            1, cp = 0))
        nrows <- length(fit$cptable[, 1])
        ntot <- ntot + 1
        for (j in 2:nrows) {
            if (nrows < 2) {
                next
            }
            else {
            }
            nsplits <- fit$cptable[j, 2]
            relerror <- fit$cptable[j, 3]
            xerror <- fit$cptable[j, 4]
            if (!is.na(origerrorsR[nsplits])) {
                nseen[nsplits] <- nseen[nsplits] + 1
                if (relerror <= origerrorsR[nsplits]) {
                  nbetterR[nsplits] <- nbetterR[nsplits] + 1
                }
                if (xerror <= origerrorsX[nsplits]) {
                  nbetterX[nsplits] <- nbetterX[nsplits] + 1
                }
                if (nseen[nsplits] == nperms) {
                  underFullBins <- underFullBins - 1
                }
            }
        }
    }
    list(R = nbetterR, X = nbetterX, N = nseen, total = ntot)
}
