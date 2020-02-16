#' make a dataset with known ramets
#'
#' @param gt - a genotype matrix 0, 1, 2 format [Required]
#' @param neach - number of ramets of each sample
#' @param het2hom - mutation rate het to hom
#' @param het2hom - mutation rate hom to het
#' @param het2hom - mutation rate hom to hom
#' @return a list consisting of two matrices 
#' @author Jason Bragg (jasongbragg@gmail.com)
#' @export
simulated_dataset <- function(gt, neach=1, het2hom=0.01, hom2het=0.001, hom2hom=0.001, simulate_source=TRUE) {

   total_samples <- nrow(gt)*neach
   
   ngt <- gt
   sgt <- cbind(rownames(gt), rownames(gt))

   for (i in 1:nrow(gt)) {

      nigt           <- mat.or.vec(neach, ncol(gt) )
      rownames(nigt) <- paste0(rownames(gt)[i], "_R", 1:neach) 

      # simulate real genotype
      if (simulate_source) {
         i_source_gt  <- mutate_genotypes(gt[i,], hom2het, het2hom, hom2hom)
      } else {
         i_source_gt  <- gt[i,]
      }

      for (j in 1:neach) {
         j_sample_gt  <- mutate_genotypes(i_source_gt, het2hom, hom2het, hom2hom)
         nigt[j,]     <- j_sample_gt
      }
      ngt <- rbind(ngt, nigt)
      sgt <- rbind(sgt, cbind(rownames(nigt), rep(rownames(gt)[i], neach)) )
   }

   sm <- ramet_sets_from_columns(sgt)

   return(list(gt=ngt, ramet_sets=sm))         
}
