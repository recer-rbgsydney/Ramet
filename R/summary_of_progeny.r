#' Calculate genotype counts for all pairs of individuals
#'
#' @param gt  - genotype matrix for a set of individuals (in rows) [Required]
#' @param fam - vector assigning individuals to a family [Required]
#' @param tissue  - vector indicating if mother (M) or progeny (P) [Required]
#' @return a list for each pair of individuals 
#' @author Jason Bragg (jasongbragg@gmail.com)
#' @export

summary_of_progeny <- function( gt, fam, tissue ) {

   i_no_fam    <- which(fam=="")
   i_no_tissue <- which(tissue=="")
   i_samples_rm <- union( i_no_fam, i_no_tissue )

   if ( length(i_samples_rm) > 0 ) {

      gt <- gt[ -i_samples_rm, ]
      fam <- fam[ -i_samples_rm]    
      tissue <- tissue[ -i_samples_rm]
   }



   families <- unique(fam)
   ipro     <- which(tissue=="P")
   npro     <- length(ipro)
   v        <- rep(0, npro)
   out <- data.frame(mother=as.character(rep("",npro)), progeny=as.character(rep("",npro)), imposs=v, mhet=v, phet=v, mhetpfix=v, mfixphet=v, mhetphet=v)

   class(out$mother) <- "character"
   class(out$progeny) <- "character"
   c <- 1
   for (i in ipro) {

         ifam <- fam[i]
         j    <- which( fam==ifam & tissue == "M")

         igt  <- which(!is.na(gt[i,]))
         jgt  <- which(!is.na(gt[j,]))

         gtp <- gt[c(i, j), ]

         plist <- table_genotypes(gtp)
         t9c   <- plist$table9cell

         out[c,1] <- rownames(gt)[j]
         out[c,2] <- rownames(gt)[i]
         out[c,3] <- t9c[1,3] + t9c[3,1]
         out[c,4] <- sum(t9c[,2])
         out[c,5] <- sum(t9c[2,])
         out[c,6] <- t9c[1,2] + t9c[3,2]
         out[c,7] <- t9c[2,1] + t9c[2,3]
         out[c,8] <- t9c[2,2] 
         c <- c + 1
   }


   return(out)

}

