#' An SQL style join between a set of Bins and  Contacts
#'
#' Given a set of bins in a data.table (such as one created by \code{\link{outerBedBins}} ) - this creates an SQL join with an SQLite database of contacts (such as one created by \code{\link{contactsToSQLite}} ). The join is on columns "cbin1" and "cbin2", which should be in both the data.table and database. Additional columns in data.table or database will be included- this is an "INNER_JOIN".
#'   
#' @section
#'  \code{binsContactsJoin}    
#' @param .bins.DT a data.table of bins
#' @param .DBname Path and name of database
#' 
#' @return DT An indexed data.table with all columns. 
#' @export
#' @examples
#' 
#' #BinContactJoin(outer.DT, "mNSC_CT.sqlite")
#' 


binsContactJoin=function(bins.DT, DBname)
{
  src.sql=src_sqlite(DBname)
  x=tbl(src.sql, "Contact")
  x=collect(inner_join(x, bins.DT, by=c("cbin1", "cbin2"), copy=T))
  x$observed=as.numeric(x$observed)
  return(x)
}