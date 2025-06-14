% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepareData.R
\name{prepareData}
\alias{prepareData}
\title{prepareData}
\usage{
prepareData(
  cxn = NULL,
  force.extract = F,
  extract_user = NULL,
  extract_computer = NULL
)
}
\arguments{
\item{cxn}{A valid Oracle connection object. This parameter allows you to 
pass an existing connection, reducing the need to establish a new connection 
within the function. If provided, it takes precedence over the connection-
related parameters.}

\item{force.extract}{The default value is FALSE.  By default, existing data will be loaded.  If
\code{force.extract ==TRUE}, than a full extraction will take place, overwriting any existing
data.}
}
\description{
This function extracts and/or loads data from the MARFISSCI schema into a list 
object.  If the necessary data (i.e. .rdata files) already exists within C:/DFO-MPO/PESDData/Tunaverse
\code{data.dir} (and \code{force.extract} is set to FALSE), it will load those.  If 
\code{force.extract} is TRUE or the files don't exist, it will extract the necessry files into
the \code{data.dir}.
}
\author{
Mike McMahon, \email{Mike.McMahon@dfo-mpo.gc.ca}
}
