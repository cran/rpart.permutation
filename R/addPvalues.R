"rpart.permutation.addPvalues" <-
function (model, nseen, nbetterR, nbetterX) 
{
    model$cptable <- cbind(model$cptable, 1:length(model$cptable[, 
        1]))
    model$cptable <- cbind(model$cptable, 1:length(model$cptable[, 
        1]))
    model$cptable <- cbind(model$cptable, 1:length(model$cptable[, 
        1]))
    names <- dimnames(model$cptable)
    names[[2]][6] <- "Rpvalue"
    names[[2]][7] <- "Xpvalue"
    names[[2]][8] <- "nreps"
    dimnames(model$cptable) <- names
    model$cptable[1, 6] <- NA
    model$cptable[1, 7] <- NA
    model$cptable[1, 8] <- NA
    nrows <- length(model$cptable[, 1])
    for (i in 2:nrows) {
        nsplits <- model$cptable[i, 2]
        Rpvalue <- nbetterR[nsplits]/nseen[nsplits]
        Xpvalue <- nbetterX[nsplits]/nseen[nsplits]
        model$cptable[i, 6] <- Rpvalue
        model$cptable[i, 7] <- Xpvalue
        model$cptable[i, 8] <- nseen[nsplits]
    }
    model
}
