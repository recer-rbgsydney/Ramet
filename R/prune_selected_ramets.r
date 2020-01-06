#' Remove chosen ramets 
#'
#' @param ps - a list of samples to prune [Required]
#' @param gt - genotype matrix, with missing data as NA
#' @return a list consisting of two matrices 
#' @author Jason Bragg (jasongbragg@gmail.com)
#' @export

prune_selected_ramets <- function(ps, gt) {

   im  <-  match(ps,rownames(gt))
   ngt <-  gt[-im,]

   return(ngt)
}

