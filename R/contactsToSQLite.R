#' Load contact data into and SQLite database
#'
#' This function maybe better run as a script if working over SSH - as it may time out. Some contact files contain up to a billion rows of data (e.g. 2.5Kb resolution, 10MB window) that's 50GB. So even if run as a script you may not want to do this on your laptop or an underpowered desktop. The database will be created in the working directory. Uses \code{\link{RSQLite}} and \code{\link{sqldf}}.
#' 
#' You will also probably want to prefilter the data to remove duplicates (there are 2 of everything) using bash/awk:
#'        nohup awk '/^[0-9]/ { if ($2<=$1) print; }' file.n.contact > temp.contact &
#'        mv temp.contact file.n_contact
#' #'   
#' @section
#'  \code{contactsToSQLite}
#' @param contactFile Path to your contact file
#' @param DBname the name of your database
#' @return DB contact data is written into a DB - nothing except a message is returned to R 
#' @export
#' @examples
#'   system.time((DT= outerBedBins(bedFile, binFile, Nbase=1e7)))
#' #   user    system   elapsed 
#' #   82.617  21.086   158.143
#' # on an old iMac Core2 Duo, 2GB Ram
#' 

contactsToSQLite= function(contactFile, DBname)
  {
  require(sqldf)
  sqldf(paste0("attach ", "\'",DBname, "\'", " as new"))
  db <- dbConnect(SQLite(), dbname=DBname)
  dbWriteTable(conn = db, name = "Contact", value = contactFile, row.names = FALSE, 
             header = FALSE, sep='\t', col.names= c("cbin1", "cbin2", "expected", "observed"))
  sqldf("CREATE INDEX bin1bin2index ON Contact (cbin1, cbin2)", dbname = DBname)
  # Analyse when tested microbenchamrked made no difference to SELECT speed on small files
  sqldf("ANALYZE Contact", dbname=DBname)
  return(paste("Created", DBname, "in working directory."))
  }


