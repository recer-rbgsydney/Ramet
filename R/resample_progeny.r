#' Calculate genotype counts for all pairs of individuals
#'
#' @param fam    - vector assigning individuals to a family [Required]
#' @param tissue  - vector indicating if mother (M) or progeny (P) [Required]
#' @return a list for each pair of individuals 
#' @author Jason Bragg (jasongbragg@gmail.com)
#' @export

resample_progeny <- function( pop, fam, tissue, nseed, nfam, npop=NULL ) {

   cp <- count_progeny(pop, fam, tissue)

   class(cp$population) <- 'character'
   class(cp$mother) <- 'character'

   pops     <- unique(cp$population)

   if (is.null(npop)) {

      npop = length(pops)

   }

   sout <- NULL

   spop <- sample(pops)[1:npop]

   for (p in spop) {

      # eligible fams
      efams <- cp$mother[which( cp$population == p & cp$progeny >= nseed)]

      if (length(efams) == nfam) {
          sfam <- efams
      } 

      if (length(efams) > nfam) {
          sfam <- sample(efams)[ 1:nfam ]
      }

      if (length(efams) < nfam) {
         cat("Warning: could not find enough families of nominated size...\nPopulation", p, efams, nfam); stop();
      }

      # for fams in sfam, choose seeds

      for (f in sfam) {

        # eligible seeds
        eseeds <- which( fam==f & tissue == "P")

          if (length(eseeds) == nseed) {
             sseed <- eseeds
          } 

         if (length(eseeds) > nseed) {
             sseed <- sample(eseeds)[ 1:nseed ]
         }

         if (length(eseeds) < nseed) {
            cat("Warning: could not find enough seeds for nominated family...\n"); stop();
         }
         sout <- c(sout, sseed)

      }

   }

   return(sort(sout))

}

