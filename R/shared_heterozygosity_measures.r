#' Calculate genotype counts for all pairs of individuals
#'
#' @param gt - genotype matrix for a set of individuals (in rows) [Required]
#' @return a list for each pair of individuals 
#' @author Jason Bragg (jasongbragg@gmail.com)
#' @export
shared_heterozygosity_measures <- function(gt) {

snames   <- rownames(gt)
nsamples <- nrow(gt)

ptables <- tabulate_all_pairs(gt)

chisq_mat  <- mat.or.vec(nsamples, nsamples)
mdshet_mat <- mat.or.vec(nsamples, nsamples)

rownames(chisq_mat)  <- snames
colnames(chisq_mat)  <- snames
rownames(mdshet_mat) <- snames
colnames(mdshet_mat) <- snames

for (ip in 1:length(ptables)) {

   ptables[[ ip ]]
   ip_names <- ptables[[ ip ]]$names

   g2tab <- ptables[[ ip ]]$table4cell

   xsq <- chisq.test(g2tab)$statistic

   ifs <- (g2tab[1,2] / sum(g2tab[,2]) ) 
   jfs <- (g2tab[2,1] / sum(g2tab[2,]) ) 

   chisq_mat[ip_names[1],ip_names[2]] <- xsq
   chisq_mat[ip_names[2],ip_names[1]] <- xsq
   mdshet_mat[ip_names[1],ip_names[2]] <- max(ifs,jfs)
   mdshet_mat[ip_names[2],ip_names[1]] <- max(jfs,ifs)

}

return(list(chisq_mat=as.dist(chisq_mat),mdshet_mat=as.dist(mdshet_mat)))
}
