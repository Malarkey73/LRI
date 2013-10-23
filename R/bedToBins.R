#' Join the nearest bins to a set of bed regions
#'
#' The function accepts  in a bin file (columns: cbin, chr, from.coord. to.coord, count) or a bin data.frame or bin data.table and converts to a \code{\link{GRanges}} object. Returns a data.table with 
#'   
#' @section
#'  \code{binToGRanges}    
#' @param bed
#' 

bedToBins=function(bed, bins){
  require(GenomicRanges)
  require(IRanges)
  require(data.table)
  # [1] because data.table has dual data.frame data.table class
  if(class(bed)[1] != "GRanges") bed = bedToGRanges(bed)
  if(class(bins)[1] != "GRanges") bins = binToGRanges(bins)
  nearest.wh=nearest(bed, bins)
  return(data.table(bin = names(bins[nearest.wh]), 
                    start=start(bins[nearest.wh]),
                    site=names(bed), 
                    chr=as.factor(as.character(seqnames(bed)))))
  
}
