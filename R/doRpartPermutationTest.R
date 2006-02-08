".First.lib" <-
function (libname, pkgname)
{
    library(rpart)
}


"doRpartPermutationTest" <-
function (dataset, responseidx, model, formula, nperms, nprocs) 
{
	results <- list()
	permsPerClient <- ceiling(nperms / nprocs); 
	if (nprocs > 1) {
    	library(snow)
  		library(rsprng)
		# Boot the cluster
		cluster <- makeCluster(nprocs, type="MPI");
		clusterSetupSPRNG(cluster)
		clusterEvalQ(cluster, library(rpart));

		results <- clusterCall(cluster, rpart.permutation.clientFunc, 
							dataset, responseidx, model,
							formula, permsPerClient);

		stopCluster(cluster);
	} else {
		results[[1]] <- rpart.permutation.clientFunc(dataset, responseidx, model, formula, permsPerClient)
	}

	# We need to pairwise-add the nbetterR and nbetterX lists in results.
	merged <- rpart.permutation.mergeResults(results);

	model <- rpart.permutation.addPvalues(model, merged$N, merged$R, merged$X);

	return(model)
}
