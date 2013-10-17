\name{outerBedBins}
\alias{outerBedBins}
\title{Calculate all inter-site combinations}
\usage{
  outerBedBins(bed, bins, window = 1e+06)
}
\arguments{
  \item{.bedFile}{Path to your bed file}

  \item{.binFile}{Path to your bin file.}

  \item{window}{Restricted to inter-site combinations
  within window distance.}
}
\value{
  DT An indexed data.table with positions, bins and site
  info.
}
\description{
  The function accepts a set of sites in a bed file (no
  header, columns: chromosome,start, end, site) and a set
  of bins in a bin file (header with columns: cbin, chr,
  from.coord. to.coord, count), plus a window distance. It
  will then calculate all possible inter site combinations
  within a chromosome and within the window distance. That
  is both bin positions (not the original bed position),
  both bin IDs (for querying the contact data), and both
  site types. The name of the function essentially comes
  from the similarity to \code{\link{outer}} an all by all
  set of comparisons - though it is a mite more
  complicated.
}
\section{\code{outerBedBins}}{

}
\examples{
# hint:
# devtools::install_github("assertthat")
# devtools::install_github("dplyr")
# for a bed file with 32138 sites, a bin file with 50441 bins, window = 1e7 (10Mb), resolution is 2.5kb
  system.time((DT= outerBedBins(bedFile, binFile, Nbase=1e7)))
#   user    system   elapsed
#   82.617  21.086   158.143
# on an old iMac Core2 Duo, 2GB Ram
}

