#' Calculate all inter-site combinations
#'
#' The function accepts a set of sites in a bed file (no header, columns: chromosome,start, end, site) and a set of bins in a bin file (header with columns: cbin, chr, from.coord. to.coord, count), plus a window distance. It will then calculate all possible inter site combinations within a chromosome and within the window distance. That is both bin positions (not the original bed position), both bin IDs (for querying the contact data), and both site types.
#' The name of the function essentially comes from the similarity to \code{\link{outer}} an all by all set of comparisons - though it is a mite more complicated.
#'   
#' @section
#'  \code{outerBedBins}    
#' @param .bedFile Path to your bed file
#' @param .binFile Path to your bin file.
#' @param window Restricted to inter-site combinations within window distance.
#' @export
#' @examples
#' # hint:
#' # devtools::install_github("assertthat")
#' # devtools::install_github("dplyr")
#' # for a bed file with 32138 sites, a bin file with 50441 bins, window = 1e7 (10Mb), resolution is 2.5kb
#'   system.time((DT= outerBedBins(bedFile, binFile, Nbase=1e7)))
#' #   user    system  elapsed 
#' #   86.733  21.083  342.534
#' # on an old iMac Core2 Duo, 2GB Ram



outerBedBins = function(bedFile, binFile, window=1e6)
{
  
require(GenomicRanges)
require(data.table)
require("dplyr")




Nbase =1e7
bed=read.table(bedFile, colClasses=c('character', 'numeric', 'numeric', 'character'))
# "chr1" should be 1 like in other files.. grrrr...
colnames(bed)=c('chr', 'bedStart', 'bedEnd', 'siteType')  
bed$chr= gsub('chr', '', bed$chr)

#Process the BIN data
bins = read.table(binFile, header=T, colClasses=c('numeric','character', rep('numeric',3)))
rm(bedFile, binFile)

# This is fucking magic... IRanges magic
bed.gr= GRanges(seqnames=bed$chr,  IRanges(start=bed$bedStart, end=bed$bedEnd, names=bed$siteType))
bin.gr= GRanges(seqnames=bins$chr,  IRanges(start=bins$from.coord, end=bins$to.coord, names=bins$cbin))
nearest.wh=nearest(bed.gr, bin.gr)

# I have now switched to the experimental dplyr - fucking NASA!
# essentially it indexes the data but keeps in memory so any subsetting ooperations should be 100s times faster
bedbin.dt = data.table(bin = names(bin.gr[nearest.wh]), start=start(bin.gr[nearest.wh]),site=names(bed.gr), chr=as.vector(seqnames(bed.gr)))

# this is adding an index by group I presume??? Questionable dplyr syntax
bedbin.dt = group_by(bedbin.dt, chr)
rm(nearest.wh,bin.gr,bed.gr)

# perhaps this outer fun can be speed up?? The unique will cut memory overhead whilst the loop proceeds
outerFun= function(dt)
  {
   unique(data.table(
    x=dt[rep(1:nrow(dt),each =nrow(dt)),],
    y=dt[rep(1:nrow(dt),times=nrow(dt)),]))
  }

# terrible name brilliant result - though a list??
outer.bedbin.dt = do(bedbin.dt, outerFun)

# collapse the list to a single data.table - rbindlist is much quicker than rbind
outer.bedbin.dt=rbindlist(outer.bedbin.dt)

# we only want to query distances within an Nbase window - this should cut it down further
outer.bedbin.dt = outer.bedbin.dt[abs(x.start-y.start) < window,]

return(outer.bedbin.dt)
}





