#' Calculate genotype counts for all pairs of individuals
#'
#' @param gt  - genotype matrix for a set of individuals (in rows) [Required]
#' @param fam - vector assigning individuals to a family [Required]
#' @param tissue  - vector indicating if mother (M) or progeny (P) [Required]
#' @return a list for each pair of individuals 
#' @author Jason Bragg (jasongbragg@gmail.com)
#' @export

count_progeny <- function( pop, fam, tissue ) {

   families <- unique(fam)
   ifam     <- which(tissue=="M")
   ipro     <- which(tissue=="P")
   npro     <- length(ipro)
   nfam     <- length(families)

   out <- data.frame(population=as.character(rep("",nfam)), mother=as.character(rep("",nfam)), progeny=as.numeric(rep(0,nfam)))

   class(out$population) <- 'character'
   class(out$mother) <- 'character'

   c <- 1
   for (i in ifam) {

         n    <- length(which( fam==fam[i] & tissue == "P"))
         p    <- pop[ which( fam==fam[i] & tissue == "M") ]
         f    <- fam[ which( fam==fam[i] & tissue == "M") ]

         out$population[c] <- p
         out[c, 2] <- f
         out[c, 3] <- n

         c <- c + 1
   }


   return(out)

}

