#' Convert a bed to GRanges
#'
#' The function accepts a set of sites in a bed file (no header, columns: chromosome,start, end, site) or a bed data.frame or bed data.table and converts to a \code{\link{GRanges}} object.
#'   
#' @section
#'  \code{bedToGRanges}    
#' @param bed
#' 

bedToGRanges= function(bed){
  require(GenomicRanges)
  # if this is a path to a file
  if(is.character(bed)){
    bed=read.table(bed, colClasses=c('character', 'numeric', 'numeric', 'character'))
    # "chr1" should be 1 like in other files.. grrrr...
    colnames(bed)=c('chr', 'bedStart', 'bedEnd', 'siteType')  
    bed$chr= gsub('chr', '', bed$chr)
    
    
  }
  return(GRanges(seqnames=bed$chr,  
                 IRanges(start=bed$bedStart, 
                         end=bed$bedEnd, 
                         names=bed$siteType)))
}
