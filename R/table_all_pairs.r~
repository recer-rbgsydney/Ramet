#' Calculate genotype counts for all pairs of individuals
#'
#' @param gt - genotype matrix for a set of individuals (in rows) [Required]
#' @return a list for each pair of individuals 
#' @author Jason Bragg (jasongbragg@gmail.com)
#' @export

table_all_pairs <- function( gt ) {

   population <- 1:nrow(gt)

   tables <- list()
   cat(" Tabulating all pairs...\n")
   for (ii in population) {

      cat(" sample ",ii, "of", length(population), "\n")
      for (ij in population) {

      if (ii < ij) {

         igt  <- which(!is.na(gt[ii,]))
         jgt  <- which(!is.na(gt[ij,]))

         gtp <- gt[c(ii, ij), ]

         plist <- table_genotypes(gtp)

         plist[[ "indices" ]] <- c(ii, ij)
         plist[[ "names" ]] <- rownames(gt)[c(ii, ij)]

         tables[[ length(tables)+1 ]] <- plist 
      }
   }
}

return(tables)

}


