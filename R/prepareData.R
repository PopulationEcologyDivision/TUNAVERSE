#' @title prepareData
#' @description This function extracts and/or loads data from the MARFISSCI schema into a list 
#' object.  If the necessary data (i.e. .rdata files) already exists within the specified 
#' \code{data.dir} (and \code{force.extract} is set to FALSE), it will load those.  If 
#' \code{force.extract} is TRUE or the files don't exist, it will extract the necessry files into
#' the \code{data.dir}.
#' @param username default is \code{NULL} This is your username for accessing oracle objects. 
#' @param password default is \code{NULL} This is your password for accessing oracle objects. 
#' @param dsn default is \code{NULL} This is your data source name - likely something like "PTRAN". 
#' @param data.dir  The default is your working directory. If you are hoping to 
#' load existing data, this folder should identify the folder containing your 
#' *.rdata files.
#' @param usepkg default is \code{'rodbc'}. This indicates whether the connection to Oracle should
#' use \code{'rodbc'} or \code{'roracle'} to connect.  rodbc is slightly easier to setup, but
#' roracle will extract data ~ 5x faster.
#' @param force.extract The default value is FALSE.  By default, existing data will be loaded.  If
#' \code{force.extract ==TRUE}, than a full extraction will take place, overwriting any existing
#' data.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
prepareData <- function(username=NULL, password=NULL, dsn=NULL, data.dir=NULL, usepkg="rodbc", force.extract=F){
  if (is.null(data.dir))stop("Please provide a location where the extracted .rdata files can be stored and/or accessed")
  Mar.utils::get_data_tables(schema = "MARFISSCI", usepkg = usepkg,
                             fn.oracle.username = username, fn.oracle.password = password, fn.oracle.dsn = dsn,
                             data.dir=data.dir,
                             tables= allTables, force.extract = force.extract)
  
  rawData <- loadIntoList(allTables,removeOriginals = T)
  rawData <- dropFields(rawData)
  rawData <- enWidener(rawData)
  return(rawData)
}