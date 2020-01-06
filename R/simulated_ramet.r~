#' make a ramet with simulated sequencing errors 
#'
#' @param gv - a vector of genotypes, in 0, 1, 2 format [Required]
#' @param het2hom - mutation rate het to hom
#' @param het2hom - mutation rate hom to het
#' @param het2hom - mutation rate hom to hom
#' @return a list consisting of two matrices 
#' @author Jason Bragg (jasongbragg@gmail.com)
#' @export
simulated_ramet <- function(gv, het2hom=0.01, hom2het=0.001, hom2hom=0.001) {

   ngv   <- gv

   srgv  <- mutate_genotypes(gv, het2hom, hom2het, hom2hom)   
   sogv  <- mutate_genotypes(srgv, het2hom, hom2het, hom2hom)

   return(sogv)         
}
