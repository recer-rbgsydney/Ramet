#' Find sets of ramets, based on a distance matrix 
#' and a nominated threshold
#'
#' @param dm - distance matrix for a set of individuals [Required]
#' @param dthresh - threshold value for corresponding measure in matrix [Required]
#' @return a vector with assignments to ramet sets
#' @author Jason Bragg (jasongbragg@gmail.com)
#' @export

collect_ramet_sets <- function(dm, dthresh, lt=TRUE) {

   require(igraph)
   require(reshape2)
 
   if (lt) {
     g <- subset(subset(melt(as.matrix(dm)), value < dthresh ), value != 0)
   } else {
     g <- subset(subset(melt(as.matrix(dm)), value > dthresh ), value != 0)
   }

   ig <- graph_from_edgelist(as.matrix(g[,c(1,2)]), directed = FALSE)
   cig <- components(ig)$membership

   return(cig)
}
