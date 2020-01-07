#' Encode known ramet sets using a meta data file 
#'
#' @param ramet_genet - two columns: names of samples and genets [Required]
#' @return a list consisting of two matrices 
#' @author Jason Bragg (jasongbragg@gmail.com)
#' @export

ramet_sets_from_columns <- function(ramet_genet) { 
   multi_genets  <- names(which(table(ramet_genet[,2]) > 1)) 
   genets        <- ramet_genet[which(ramet_genet[,2] %in% multi_genets),2]
   names(genets) <- ramet_genet[which(ramet_genet[,2] %in% multi_genets),1]
   return(genets)
}

