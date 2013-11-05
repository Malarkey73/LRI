#' Convert a bin to GRanges
#'
#' The function accepts a set of sites in a bin file (columns: cbin, chr, from.coord. to.coord, count) or a bin data.frame or bin data.table and converts to a \code{\link{GRanges}} object.
#'   
#' @section
#'  \code{binToGRanges}    
#' @param bed
#' 

binToGRanges= function(bins){
  require(GenomicRanges)
  # if this is a path to a file
  if(is.character(bins)){
    bins = read.table(bins, header=T, colClasses=c('numeric','character', rep('numeric',3)))
        
  }
  
  return(GRanges(seqnames=bins$chr,  
                 IRanges(start=bins$from.coord, 
                         end=bins$to.coord, 
                         names=bins$cbin)))
}
