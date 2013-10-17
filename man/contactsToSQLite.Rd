\name{contactsToSQLite}
\alias{contactsToSQLite}
\title{Load contact data into and SQLite database}
\usage{
  contactsToSQLite(contactFile, DBname, Analyse = FALSE)
}
\arguments{
  \item{contactFile}{Path to your contact file}

  \item{DBname}{the name of your database}
}
\value{
  DB contact data is written into a DB - nothing except a
  message is returned to R
}
\description{
  This function maybe better run as a script if working
  over SSH - as it may time out. Some contact files contain
  up to a billion rows of data (e.g. 2.5Kb resolution, 10MB
  window) that's 50GB. So even if run as a script you may
  not want to do this on your laptop or an underpowered
  desktop. The database will be created in the working
  directory. Uses \code{\link{RSQLite}} and
  \code{\link{sqldf}}.
}
\details{
  You will also probably want to prefilter the data to
  remove duplicates (there are 2 of everything) using
  bash/awk: nohup awk '/^[0-9]/ { if ($2<=$1) print; }'
  file.n.contact > temp.contact & mv temp.contact
  file.n_contact #'
}
\section{\code{contactsToSQLite}}{

}
\examples{
system.time((DT= outerBedBins(bedFile, binFile, Nbase=1e7)))
#   user    system   elapsed
#   82.617  21.086   158.143
# on an old iMac Core2 Duo, 2GB Ram
}

