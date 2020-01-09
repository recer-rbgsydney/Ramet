#' For a pair of individuals, count shared genotypic states
#'
#' @param gtp - genotype matrix for two individuals (in rows) [Required]
#' @return a list consisting of two matrices 
#' @author Jason Bragg (jasongbragg@gmail.com)
#' @export

table_genotypes <- function( gtp ) {

         tables <- list()

         igt  <- gtp[1,]
         jgt  <- gtp[2,]

         ttt <- table(as.numeric(igt), as.numeric(jgt))  
         tcn  <- colnames(ttt) 
         trn  <- rownames(ttt)

         g3table <- mat.or.vec(3,3)
         rownames(g3table) <- c("2","1","0")
         colnames(g3table) <- c("2","1","0")

         for (i in 1:nrow(ttt)) {
            for (j in 1:ncol(ttt)) {
               if ( !(trn[i]=="NA" | tcn[j]=="NA") ) {
                  g3table[trn[i],tcn[j]] <- ttt[trn[i],tcn[j]]         
               }

            }
         }

         g2table <- mat.or.vec(2,2)
         g2table[1,1] <- g3table[1,1] + g3table[3,3] + g3table[1,3] + g3table[3,1]
         g2table[1,2] <- g3table[2,1] + g3table[2,3]
         g2table[2,1] <- g3table[1,2] + g3table[3,2]
         g2table[2,2] <- g3table[2,2]

         pair <- list(table9cell=g3table, table4cell=g2table)

         return(pair)
}


