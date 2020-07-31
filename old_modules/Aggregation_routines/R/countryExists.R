countryExists <- function(countries, years, refcountries, refstarts, refends, warnNA=TRUE){

  if(any(is.na(countries)|is.na(years)) & warnNA){
    warning("NA in country or year - if you're subsetting, this will probably return nothing")
  }
  stopifnot(length(countries) == length(years))

  # Get list of codes and country start and end dates
  codeList <- data.table(country = as.character(refcountries),
                         refstart = as.numeric(refstarts),
                         refend = as.numeric(refends))

  # TODO check references are formatted correctly

  # All years not specified are effectively infinite in the past or future
  codeList[is.na(refstart), refstart := -Inf]
  codeList[is.na(refend), refend := Inf]

  # The as.character is in case years is a factor
  callDf <- data.table(country = as.character(countries), year = as.numeric(as.character(years)))
  mergedDf <- merge(callDf, codeList, by = "country", all.x = T, sort = F)

  mergedDf$refstart <- ifelse(is.na(mergedDf$refstart), -Inf, mergedDf$refstart)
  mergedDf$refend <- ifelse(is.na(mergedDf$refend), -Inf, mergedDf$refend)
  #Return logical for if the country exists
  mergedDf[,year >= refstart & year <= refend]
}
