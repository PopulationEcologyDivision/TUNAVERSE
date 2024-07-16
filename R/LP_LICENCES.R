#' @title LP_LICENCES
#' @description This function loads the specified licence data into the global environment.
#' @param type  this is a one element vector identifying the licence type which is matched against a list.
#' @param data_path  this is the location of the PLL_LICENCES, MAR_BFT_LICENCES and HP_LICENCES Rdata files.
#' @author  Alex Hanke, \email{Alex.Hanke@@dfo-mpo.gc.ca}
#' @importFrom dplyr %>%
#' @export
LP_LICENCES <- function(type="PLL_LICENCES", 
    data_path="R:/ATLSiteShares/SABS/LargePelagics/COMMERCIAL DATA/TUNAVERSE/data/") 
  {
    # Function to load specific data object
    # Check if data_path is a character string
    if (!is.character(data_path)) {
      stop("data_path must be a character string specifying the file path.")
    }
    
    # Check if object_name is a character string
    if (!is.character(type)) {
      stop("object_name must be a character string specifying the object to load.")
    }
    
    # Allowed object names
    allowed_objects <- c("PLL_LICENCES", "MAR_BFT_LICENCES", "HP_LICENCES")
    
    # Check if object_name is valid
    if (!type %in% allowed_objects) {
      stop(paste("Invalid object name. Choose from:", paste(allowed_objects, collapse = ", "), sep = " "))
    }
    
    # Load the data object using load()
    RdataFiles = c("PLL_LICENCES.Rdata",
               "MAR_BFT_LICENCES.Rdata",
               "HP_LICENCES.Rdata")
    
    data <- load(paste0(data_path,
            RdataFiles[match(type,allowed_objects)]),
            verbose=T)
    
    # Check if the object exists
    if (!exists(type, where = 1)) {
      warning(paste("Object:", object_name, "not found in the data file.", sep = " "))
      return(invisible())  # Return invisibly if object not found
    }
    
    # Assign the object to the global environment
    assign(type, get(type, pos = 1))
    message(paste("Loaded object:", type, sep = " "))
    
    # Return the loaded data (optional)
    # return(get(object_name, envir = data$environment))
  }
  