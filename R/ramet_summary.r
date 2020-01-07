#' Summarize measure values for ramets and non-ramets 
#'
#' @param ramet_set - a list of samples to prune [Required]
#' @param measure_matrix - genotype matrix, with missing data as NA
#' @return a list  
#' @author Jason Bragg (jasongbragg@gmail.com)
#' @export

ramet_summary <- function(ramet_set, measure_matrix) {

   require(reshape2)
   mm_mat <- as.matrix(measure_matrix)

   for (i in 1:nrow(mm_mat)) {
      for (j in 1:nrow(mm_mat)) {

         if (i <= j) {
            mm_mat[i,j] <- NA
         }

      }
   }

   mm_df  <- melt(mm_mat)
   mm_df  <- mm_df[  -which(is.na(mm_df[,3])), ] 

   mmc_df <- cbind(mm_df, (rep("nc", nrow(mm_df)))) 
   mmc_df[,4] <- as.character(mmc_df[,4])

   # loop through genets, mark clones as "c" 
   vs <- unique(ramet_set)

   for (i in 1:length(vs)) {

      inames <- names(rs)[ which(rs == vs[i]) ]
      if (length(inames > 1)) {

         for (j in 1:length(inames)) {
            for (k in 1:length(inames) ) {
               if (j < k) {
               
                  itmp <- which( (inames[ j ] == mmc_df[,1] & inames[ k ] == mmc_df[,2] ) |  (inames[ k ] == mmc_df[,1] & inames[ j ] == mmc_df[,2] ))
                  mmc_df[itmp, 4] <- "c"

               } 
            }

         }

      } 
   }
   mmc_df[,4] <- as.factor(mmc_df[,4])
   mean_c=mean(  mmc_df[,3][ which(mmc_df[,4] == "c")  ] )
   mean_nc=mean(  mmc_df[,3][ which(mmc_df[,4] == "nc")  ] )
   max_c=max(  mmc_df[,3][ which(mmc_df[,4] == "c")  ] )
   max_nc=max(  mmc_df[,3][ which(mmc_df[,4] == "nc")  ] )
   min_c=min(  mmc_df[,3][ which(mmc_df[,4] == "c")  ] )
   min_nc=min(  mmc_df[,3][ which(mmc_df[,4] == "nc")  ] )

   return(list(values=mmc_df, mean_c=mean_c, mean_nc=mean_nc, max_c=max_c, max_nc=max_nc, min_c=min_c, min_nc=min_nc))

}


