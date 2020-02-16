library(PlantPopGenFIT)

set.seed(8972397)

nloc <- 5000
nind <- 3000

fixed_vec <- sample(rep(c(0,2),nloc*nind/2))

gt_in <- matrix( fixed_vec, nrow=nind)

mortality    = c(1)
dm_in        = cbind(rep(0,nind),rep(0,nind))
K            = c(nind)
dispersal    = matrix(c(1),nrow=1)
locus_effect = rep(0,nloc)
dominance_effect = rep(0.5,nloc)
n_p = 20
n_k = 0.2
phenotype_opt = c(0)
selfing=1
mu = 1e-7
t_steps   <- 6000
burn_in   <- simulation(tsteps=t_steps, mortality=mortality, demo=dm_in, gt=gt_in, K=K, dispersal=dispersal, selfing=selfing, locus_effect=locus_effect, dominance_effect=dominance_effect, phenotype_opt=phenotype_opt, n_p=n_p, n_k=n_k, mutation=mu)

K_sub    <- c(100,100,100)
isubset   <- sample(1:nind)[1:sum(K_sub)]
gt_sub   <- burn_in[[2]][isubset,]
dm_sub   <- burn_in[[1]][isubset,]
dm_sub[,1] <- c(rep(0,100), rep(1,100), rep(2,100) )

dispersal_sub <- as.matrix(diag(3))
dispersal_sub[dispersal_sub==0] <- 0.1
t_steps   <- 10
phenotype_opt = c(0,0,0)

run_recent  <- simulation(tsteps=t_steps, mortality=mortality, demo=dm_sub, gt=gt_sub, K=K_sub, dispersal=dispersal_sub, selfing=selfing, locus_effect=locus_effect, dominance_effect=dominance_effect, phenotype_opt=phenotype_opt, n_p=n_p, n_k=n_k, mutation=mu)

save.image("/home/jgb/Rpkgs/Ramet/sim/simulation.Rda")



