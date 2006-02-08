"rpart.permutation.mergeResults" <-
function (results) 
{
    Xbetter <- array(1, length(results[[1]]$X))
    Rbetter <- array(1, length(results[[1]]$R))
    Nseen <- array(1, length(results[[1]]$N))
    ntot <- 0
    for (i in 1:length(results)) {
        Xbetter <- results[[i]]$X + Xbetter
        Rbetter <- results[[i]]$R + Rbetter
        Nseen <- results[[i]]$N + Nseen
        ntot <- results[[i]]$total + ntot
    }
    list(X = Xbetter, R = Rbetter, N = Nseen, total = ntot)
}
