##' Aggregate Element
##' 
##'   
##' @description This function is used to compute the aggregate of any element in the SWS.
##'   
##' @param data The data set used to compute the aggregation.
##'   
##' @return An aggregated value.
##' @keywords internal
##'   


aggregateElement <- function(data) {
  
  # new version using Amanda's table
  data = as.data.table(data)
  calculated_elements <- ReadDatatable('calculated_elements', 
                                       columns = c('result_var_code', 'num_var_code', 'den_var_code', 'mult'))
  # we have to exclude those elements. They don't exist in SWS. Must be checked with Amanda/Carola.
  calculated_elements <- calculated_elements[!den_var_code %in% c(5319) & !result_var_code == '664']
  setnames(data, old = c(grep("element", colnames(data), ignore.case = T, value = T)), 
           new = c('measuredElement'))
  elements <- unique(data$measuredElement)
  
  d <- list()
  
  for (i in 1:length(elements)) {
    
    d[[i]] <- if(nrow(calculated_elements[result_var_code %in% elements[i]]) > 0) {
      
      var1_elem = calculated_elements[result_var_code %in% elements[i], num_var_code]
      var2_elem = calculated_elements[result_var_code %in% elements[i], den_var_code]
      mult = as.numeric(calculated_elements[result_var_code %in% elements[i], mult])

      data_subset <- data[measuredElement %in% c(var1_elem, var2_elem)]
      vars_dcast <- dcast.data.table(data_subset, 
                                     paste(country, "+", item, '+', years, '~', 'measuredElement'), 
                                     value.var = 'Value')
      
      vars_dcast <- na.omit(vars_dcast)
      ratio_var(vars_dcast[[var1_elem]], vars_dcast[[var2_elem]]) * mult

    } else if(nrow(calculated_elements[result_var_code %in% elements[i]]) == 0) {
      
      var_value = data[measuredElement == elements[i], Value]
      sum_var(var_value)
    }
    
  } 
  
  names(d) <- elements
  d <- bind_rows(d)
  return(d)
}
