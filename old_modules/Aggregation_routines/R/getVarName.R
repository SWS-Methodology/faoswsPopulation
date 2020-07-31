##' Get Var Name
##' 
##'   
##' @description This function returns the name of a variable.
##'   
##' @param pattern Character string containing a regular expression (or character string).
##'   
##' @return A string.
##'   

getVarName <- function(pattern) {
  
  return(grep(pattern, names(swsContext.datasets[[1]]@dimensions), ignore.case = T, value = T) )
  
}