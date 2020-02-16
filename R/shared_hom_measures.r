#' Calculate genotype counts for all pairs of individuals
#'
#' @param gt - genotype matrix for a set of individuals (in rows) [Required]
#' @return a list for each pair of individuals 
#' @author Jason Bragg (jasongbragg@gmail.com)
#' @export
shared_hom_measures <- function(gt) {

snames   <- rownames(gt)
nsamples <- nrow(gt)

ptables <- table_all_pairs(gt)

mdshom_mat <- mat.or.vec(nsamples, nsamples)

rownames(mdshom_mat) <- snames
colnames(mdshom_mat) <- snames

for (ip in 1:length(ptables)) {

   ptables[[ ip ]]
   ip_names <- ptables[[ ip ]]$names

   g3tab <- ptables[[ ip ]]$table9cell

   mdshom_mat[ip_names[1],ip_names[2]] <- (g3tab[1,3] + g3tab[3,1] ) / (g3tab[1,1] + g3tab[3,3] ) 
   mdshom_mat[ip_names[2],ip_names[1]] <- (g3tab[1,3] + g3tab[3,1] ) / (g3tab[1,1] + g3tab[3,3] )

}

return(list(mdshom_mat=as.dist(mdshom_mat)))
}
