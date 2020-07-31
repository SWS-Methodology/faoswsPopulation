##' List of functions
##' 
##' @rdname listFunctions
##' @aliases listFunctions sum_var ratio_var 
##'   
##' @description These functions are used to get aggregates.
##'   
##' @param value The variable used to compute its aggregation.
##' @param value1 The intermediate variable used to compute an aggregation. It's the numerator.
##' @param value2 The intermediate variable used to compute an aggregation. It's the denominator.
##'   
##' @return An aggregated value for any element in the SWS.
##' @keywords internal
##'   

sum_var <- function(value){
  
  sum(value, na.rm = T)
  
}

ratio_var <- function(value1, value2) {
  
  
  sum(value1, na.rm = T)/sum(value2, na.rm = T)
  
}
