##' Get key
##' Depending on whether item exists or not, the function returns the possible keys.
##' 
##' @return A list


getKey <- function() {
  
  if(length(item) == 1) {
    
    masterKeyAreaItem <- DatasetKey(
      domain = domain_,
      dataset = dataset_,
      dimensions = list(
        Dimension(name = country,
                  keys = GetCodeList(domain_, dataset_, country)[type == 'country', code]),
        Dimension(name = element, 
                  keys = elementSession),
        Dimension(name = item,
                  keys = GetCodeList(domain_, dataset_, item)[, code]),
        Dimension(name = years, 
                  keys = yearsSession)
      )
    )
    
    masterKeyArea <- DatasetKey(
      domain = domain_,
      dataset = dataset_,
      dimensions = list(
        Dimension(name = country,
                  keys = GetCodeList(domain_, dataset_, country)[type == 'country', code]),
        Dimension(name = element, 
                  keys = elementSession),
        Dimension(name = item,
                  keys = itemSession),
        Dimension(name = years, 
                  keys = yearsSession)
      )
    )
    
    masterKeyItem <- DatasetKey(
      domain = domain_,
      dataset = dataset_,
      dimensions = list(
        Dimension(name = country,
                  keys = countrySession),
        Dimension(name = element, 
                  keys = elementSession),
        Dimension(name = item,
                  keys = GetCodeList(domain_, dataset_, item)[, code]),
        Dimension(name = years, 
                  keys = yearsSession)
      )
    )
    
    list_key <- list(masterKeyAreaItem = masterKeyAreaItem,
                     masterKeyArea = masterKeyArea,
                     masterKeyItem = masterKeyItem)[]
    
  } else {
    
    masterKeyArea <- DatasetKey(
      domain = domain_,
      dataset = dataset_,
      dimensions = list(
        Dimension(name = country,
                  keys = GetCodeList(domain_, dataset_, country)[type == 'country', code]),
        Dimension(name = element, 
                  keys = elementSession),
        # Dimension(name = item,
        #           keys = itemSession),
        Dimension(name = years, 
                  keys = yearsSession)
      )
    )
    
    list_key <- list(masterKeyArea = masterKeyArea)[]
    
  }
}

