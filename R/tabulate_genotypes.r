#' For a pair of individuals, count shared genotypic states
#'
#' @param gtp - genotype matrix for two individuals (in rows) [Required]
#' @return a list consisting of two matrices 
#' @author Jason Bragg (jasongbragg@gmail.com)
#' @export

tabulate_genotypes <- function( gtp ) {

         tables <- list()

         igt  <- which(!is.na(gtp[1,]))
         jgt  <- which(!is.na(gtp[2,]))

         shared_gt <- intersect(igt, jgt)

         # make tmp_data, both i and j are non-missing
         tmp_data <- gtp[ , shared_gt ]

         ihomRef <- which(tmp_data[1,] == 0)
         ihomAlt <- which(tmp_data[1,] == 2)

         jhomRef <- which(tmp_data[2,] == 0)
         jhomAlt <- which(tmp_data[2,] == 2)

         ihet  <- which(tmp_data[1,] == 1)
         jhet  <- which(tmp_data[2,] == 1)

         ijhet    <- intersect(ihet, jhet)

         ijhomAlt <- intersect(ihomAlt, jhomAlt)
         ijhomRef <- intersect(ihomRef, jhomRef)
 
         ihomAltjhomRef <- intersect(ihomAlt, jhomRef)
         ihomRefjhomAlt <- intersect(ihomRef, jhomAlt)

         ihetjhomAlt <- intersect(ihet, jhomAlt)
         ihetjhomRef <- intersect(ihet, jhomRef)
         ihomAltjhet <- intersect(ihomAlt, jhet)
         ihomRefjhet <- intersect(ihomRef, jhet)

         g3table <- mat.or.vec(3,3)

         g3table[1,1] <- length(ijhomAlt) 
         g3table[3,3] <- length(ijhomRef) 
         g3table[1,3] <- length(ihomAltjhomRef) 
         g3table[3,1] <- length(ihomRefjhomAlt) 

         g3table[2,1] <- length(ihetjhomAlt)
         g3table[1,2] <- length(ihomAltjhet)

         g3table[2,3] <- length(ihetjhomRef)
         g3table[3,2] <- length(ihomRefjhet)

         g3table[2,2] <- length(ijhet)

         g2table <- mat.or.vec(2,2)
         g2table[1,1] <- g3table[1,1] + g3table[3,3] + g3table[1,3] + g3table[3,1]
         g2table[1,2] <- g3table[2,1] + g3table[2,3]
         g2table[2,1] <- g3table[1,2] + g3table[3,2]
         g2table[2,2] <- g3table[2,2]

         pair <- list(table9cell=g3table, table4cell=g2table)

         return(pair)
}


