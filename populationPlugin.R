suppressMessages({
  library(data.table)
  library(faosws)
  library(faoswsFlag)
  library(faoswsUtil)
  library(faoswsImputation)
#  library(faoswsProduction)
  library(faoswsProcessing)
  library(faoswsEnsure)
  library(magrittr)
  library(dplyr)
  library(plyr)
  library(tidyr)
  library(sendmailR)
#  library(faoswsStandardization)

})


R_SWS_SHARE_PATH <- Sys.getenv("R_SWS_SHARE_PATH")

if(CheckDebug()){
  library(faoswsModules)
  SETTINGS = ReadSettings("sws.yml")
  ## If you're not on the system, your settings will overwrite any others
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  ## Define where your certificates are stored
  SetClientFiles(SETTINGS[["certdir"]])
  ## Get session information from SWS. Token must be obtained from web interface
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = SETTINGS[["token"]])
}
# sessionKey = swsContext.datasets[[1]]

## Take the Token from the population_unpd dataset

message('population_unpd2fao plugin: Getting data')

country=  GetCodeList("population", "source_population_undp", "geographicAreaM49_unpd")[,code]
time=GetCodeList("population", "source_population_undp", "timePointYears")[,code]
time <- time[time != '0']

endYear <- max(as.numeric(time))
#startYear=finalData[, min(timePointYears)]
startYear= "1950"
## All the population elements
elements=c("511", "512", "513", "551", "561")

WPPyear <- swsContext.computationParams$wppy
WUPyear <- swsContext.computationParams$wupy

# time=c(startYear:endYear)

key = DatasetKey(domain = "population", dataset = "source_population_undp", dimensions = list(
  geographicAreaM49_unpd = Dimension(name = "geographicAreaM49_unpd", keys = country),
  measuredElement= Dimension(name = "measuredElement", keys = elements),
  timePointYears = Dimension(name = "timePointYears", keys = time)
))

timeWindow=c(startYear:endYear)

#for(i in seq_along(timeWindow)){

# message("Restricting the time key..")

#currentYear=as.character(timeWindow[i])
#key@dimensions$timePointYears@keys=currentYear

#message(paste0("Get the data and metadata for the year ", currentYear))
#Get data
sourceData = GetData(key)

# Areas having ad-hoc code created for 'unspecified areas'
adhoccodes <- c("011", "029", "005", "061", "154", "039", "021", "155", "057")

## Data manipulations

# Load datatablewith code conversion
pop_conv <- ReadDatatable('pop_code_conversion_unpd_fao')

sourceDataCheck <- nameData("population",
                           "source_population_undp",
                           sourceData,except = c("measuredElement","timePointYears"))

# Take only country codes from the dataset
sourceData2fao <- sourceData[geographicAreaM49_unpd %in% pop_conv[compliant == TRUE]$code]

# unique(sourceDataCheck[geographicAreaM49_unpd %in% unique(sourceData2fao$geographicAreaM49_unpd) &
#                   !geographicAreaM49_unpd_description %in% pop_conv$unpd_description, .(geographicAreaM49_unpd, geographicAreaM49_unpd_description)])

tot <- nrow(sourceData2fao[measuredElement == '511'])

message('population_unpd2fao plugin: Check for unspecified areas')

# Select countries where male population detail is not available
unavailable_countries <- unique(sourceData2fao[measuredElement == '511' &
                                                 !geographicAreaM49_unpd %in%
                                                 unique(sourceData2fao[measuredElement == '512']$geographicAreaM49_unpd)]$geographicAreaM49_unpd)

unavailable_countries_fem <-unique(sourceData2fao[measuredElement == '511' &
                                                    !geographicAreaM49_unpd %in%
                                                    unique(sourceData2fao[measuredElement == '513']$geographicAreaM49_unpd)]$geographicAreaM49_unpd)


unavailable_countries_urb <- unique(sourceData2fao[measuredElement == '511' &
                                                     !geographicAreaM49_unpd %in%
                                                     unique(sourceData2fao[measuredElement == '561']$geographicAreaM49_unpd)]$geographicAreaM49_unpd)

unavailable_countries_rur <- unique(sourceData2fao[measuredElement == '511' &
                                                     !geographicAreaM49_unpd %in%
                                                     unique(sourceData2fao[measuredElement == '551']$geographicAreaM49_unpd)]$geographicAreaM49_unpd)
#-- Check and messages ----

# Check the same countries are not available for male and female
if(any(!unavailable_countries %in% unavailable_countries_fem) | any(!unavailable_countries_fem %in% unavailable_countries)){
  cmale <- unavailable_countries[!unavailable_countries %in% unavailable_countries_fem]
  cfem <- unavailable_countries_fem[!unavailable_countries_fem %in% unavailable_countries]
  msg <- paste('Problem! Countries ',paste(c(cmale, cfem), collapse = ','),' are reporting only male or female population. ', sep ='')
} else {
  msg <- ''
}

if(tot != nrow(sourceData2fao[measuredElement == '512'])) {
  msgMF <- paste('Missing countries for male and female population: ',
                 paste(unique(sourceData2fao[measuredElement == '511' &
                                               !geographicAreaM49_unpd %in%
                                               unique(sourceData2fao[measuredElement == '512']$geographicAreaM49_unpd)]$geographicAreaM49_unpd),collapse = ', '),
                 'They will be included in the unspecified areas. ',
                 sep = '')
}else {
  msgMF <- ''
}

if(tot != nrow(sourceData2fao[measuredElement == '551'])) {
  msgRur <- paste('Missing rural countries: ',
                  paste(unique(sourceData2fao[measuredElement == '511' &
                                                !geographicAreaM49_unpd %in%
                                                unique(sourceData2fao[measuredElement == '551']$geographicAreaM49_unpd)]$geographicAreaM49_unpd),collapse = ', '),
                  '. ', sep = '')
} else {
  msgRur <- ''
}


if(tot != nrow(sourceData2fao[measuredElement == '561'])) {
  msgUrb <- paste('Missing urban countries: ',
                  paste(unique(sourceData2fao[measuredElement == '511' &
                                                !geographicAreaM49_unpd %in%
                                                unique(sourceData2fao[measuredElement == '561']$geographicAreaM49_unpd)]$geographicAreaM49_unpd),collapse = ', '),
                  '. ', sep = '')
} else{
  msgUrb <- ''
}

setnames(sourceData2fao, "geographicAreaM49_unpd", "geographicAreaM49")

#-- Unspecified areas calculation ----

ebx_hierar <- ReadDatatable('geographic_hierarchy_ebx5')
# Missing country code
excludedCodes <- ebx_hierar[code_l5 %in% unavailable_countries]

excludedCodesRU <- ebx_hierar[code_l5 %in% unavailable_countries_urb]

# Areas they belong to
unspec <- c(unique(excludedCodes[!is.na(code_l4) & code_l4 != '']$code_l4),
            unique(excludedCodes[is.na(code_l4) | code_l4 == '']$code_l3))

unspecRU <- c(unique(excludedCodesRU[!is.na(code_l4) & code_l4 != '']$code_l4),
              unique(excludedCodesRU[is.na(code_l4) | code_l4 == '']$code_l3))
# Check if any missing country is different from the ad-hoc codes

if(any(!unspec %in% adhoccodes)){
  areanotincluded <- unspec[!unspec %in% adhoccodes]
  missingUnspecCode <- paste("A new 'unspecified area' code is required for area: ", areanotincluded, sep ='')
} else {
  missingUnspecCode <- ''
}

if(any(!unspecRU %in% adhoccodes)){
  areanotincludedRU <- unspecRU[!unspecRU %in% adhoccodes]
  missingUnspecCodeRU <- paste("A new 'unspecified area' code is required for area: ", areanotincludedRU, sep ='')
} else {
  missingUnspecCodeRU <- ''
}

unspec_conv <- ReadDatatable('pop_unspecified_conversion')

# Get area data from source data
unspec_fig <- sourceData[ geographicAreaM49_unpd %in% unspec_conv$unpd_code & measuredElement %in% c('512', '513')]

unspec_figRU <- sourceData[ geographicAreaM49_unpd %in% unspec_conv$unpd_code & measuredElement %in% c('551', '561')]

# Get conversion codes
unspec_fig_conv <- merge(unspec_fig, unspec_conv[ ,.(sws_code, unpd_code)],
                         by.x = 'geographicAreaM49_unpd', by.y = 'unpd_code')
unspec_fig_conv$sws_code <- gsub('.01', '', unspec_fig_conv$sws_code)

unspec_fig_convRU <- merge(unspec_figRU, unspec_conv[ ,.(sws_code, unpd_code)],
                         by.x = 'geographicAreaM49_unpd', by.y = 'unpd_code')
unspec_fig_convRU$sws_code <- gsub('.01', '', unspec_fig_convRU$sws_code)


# Identify all the country in these areas
countriesinterested <- c(ebx_hierar[code_l4 %in% unspec ]$code_l5, ebx_hierar[code_l3 %in% unspec ]$code_l5)

countriesinterestedRU <- c(ebx_hierar[code_l4 %in% unspecRU ]$code_l5, ebx_hierar[code_l3 %in% unspecRU ]$code_l5)

# Countries for which data for male and female population are available
available_countries <- sourceData2fao[geographicAreaM49 %in% countriesinterested & measuredElement %in% c('512', '513')]

available_countriesRU <- sourceData2fao[geographicAreaM49 %in% countriesinterestedRU & measuredElement %in% c('551', '561')]

# Get hierarchy
adhocaggr <- ebx_hierar[code_l4 %in% unspec | code_l3 %in% unspec, ]
adhocaggrRU <- ebx_hierar[code_l4 %in% unspecRU | code_l3 %in% unspecRU, ]

# Reduce to one level of herarchy (mix level 3-4)
adhocaggr <- adhocaggr[is.na(code_l4) | code_l4 == '', code_l4 := code_l3 ]
adhocaggrRU <- adhocaggrRU[is.na(code_l4) | code_l4 == '', code_l4 := code_l3 ]

# Remove all columns not needed
adhocaggr[ , c("code_l1", "type_l1", "name_en_l1", "code_l2",
               "type_l2", "name_en_l2", "code_l3", "type_l3",
               "name_en_l3", "type_l4", "name_en_l4", "type_l5", "name_en_l5")] <- NULL

adhocaggrRU[ , c("code_l1", "type_l1", "name_en_l1", "code_l2",
               "type_l2", "name_en_l2", "code_l3", "type_l3",
               "name_en_l3", "type_l4", "name_en_l4", "type_l5", "name_en_l5")] <- NULL


# Compute aggregate from available countries
aggregateAvCountries <- merge(available_countries, adhocaggr,
                              by.x = 'geographicAreaM49', by.y = 'code_l5')
aggregateAvCountries[, Value := sum(Value), by = c('measuredElement', 'timePointYears', 'code_l4')]
aggregateAvCountries[ , geographicAreaM49 := NULL]
setkey(aggregateAvCountries)
aggregateAvCountries <- unique(aggregateAvCountries)


aggregateAvCountriesRU <- merge(available_countriesRU, adhocaggrRU,
                              by.x = 'geographicAreaM49', by.y = 'code_l5')
aggregateAvCountriesRU[, Value := sum(Value), by = c('measuredElement', 'timePointYears', 'code_l4')]
aggregateAvCountriesRU[ , geographicAreaM49 := NULL]
setkey(aggregateAvCountriesRU)
aggregateAvCountriesRU <- unique(aggregateAvCountriesRU)

# Difference from the UNPD aggregate and the just computed ones with available data
comparisonUNPDm49 <- merge(unspec_fig_conv, aggregateAvCountries,
                           by.x = c('sws_code', 'timePointYears', 'measuredElement'),
                           by.y = c('code_l4', 'timePointYears', 'measuredElement'),
                           all = T,suffixes = c('_unpd', '_avail'))
comparisonUNPDm49 <- comparisonUNPDm49[!is.na(Value_avail)]

comparisonUNPDm49RU <- merge(unspec_fig_convRU, aggregateAvCountriesRU,
                           by.x = c('sws_code', 'timePointYears', 'measuredElement'),
                           by.y = c('code_l4', 'timePointYears', 'measuredElement'),
                           all = T,suffixes = c('_unpd', '_avail'))
comparisonUNPDm49RU <- comparisonUNPDm49RU[!is.na(Value_avail)]


# Computed 'unspecified areas' values
comparisonUNPDm49[ , unspecified := Value_unpd - Value_avail ]
comparisonUNPDm49RU[ , unspecified := Value_unpd - Value_avail ]


unspecifiedTab <- comparisonUNPDm49[ , .(sws_code, timePointYears,
                                         measuredElement, unspecified,
                                         flagObservationStatus_avail, flagMethod_avail)]

unspecifiedTabRU <- comparisonUNPDm49RU[ , .(sws_code, timePointYears,
                                         measuredElement, unspecified,
                                         flagObservationStatus_avail, flagMethod_avail)]

unspecifiedTabRU$unspecified <- round(unspecifiedTabRU$unspecified, 6)
# Add '.01' suffix to create the 'unspecified area' code
unspecifiedTab[ , sws_code:= paste(sws_code, '.01', sep ='')]
unspecifiedTabRU[ , sws_code:= paste(sws_code, '.01', sep ='')]

# Make the table compliant with the rest of the data and attach
setnames(unspecifiedTab, c("sws_code", "timePointYears", "measuredElement",
                           "unspecified", "flagObservationStatus_avail", "flagMethod_avail"),
         c("geographicAreaM49", "timePointYears", "measuredElement",
           "Value", "flagObservationStatus", "flagMethod"))

setnames(unspecifiedTabRU, c("sws_code", "timePointYears", "measuredElement",
                           "unspecified", "flagObservationStatus_avail", "flagMethod_avail"),
         c("geographicAreaM49", "timePointYears", "measuredElement",
           "Value", "flagObservationStatus", "flagMethod"))

message('population_unpd2fao plugin: Unspecified areas calculated')


data_unspec <- rbind(sourceData2fao, unspecifiedTab)
data_unspec <- rbind(data_unspec, unspecifiedTabRU[Value !=0])

# "China, main" issue.
# The M49 code used for China is 1248, this is not true for UNPD who identifies China, main with the code 156

China1248 <- data_unspec[geographicAreaM49=="156", ]
China1248[geographicAreaM49=="156", geographicAreaM49:="1248"]

finalData <- rbind(data_unspec, China1248)

## Metadata manipulation
## filter metadata for M49 countries and transform China in 1248
#Get METAdata
# sourceMetaData <- data.table()
# for(i in seq_along(time)){
#   year <- time[i]
#   keyMD = DatasetKey(domain = "population", dataset = "source_population_undp", dimensions = list(
#     geographicAreaM49_unpd = Dimension(name = "geographicAreaM49_unpd", keys = country),
#     measuredElement= Dimension(name = "measuredElement", keys = elements),
#     timePointYears = Dimension(name = "timePointYears", keys = as.character(2015:2019))
#   ))
# 
#   sourceMetaData0=GetMetadata(keyMD)
#   sourceMetaData <- rbind(sourceMetaData0)
# 
#   print(paste('Metadata pulled year: ', year, sep =''))
# }
# 
# 
# sourceMetaDataWUP= sourceMetaData[Metadata_Value %in% Metadata_Value[grepl("WUP2", Metadata_Value)],]
# sourceMetaDataWPP= sourceMetaData[Metadata_Value %in% Metadata_Value[grepl("WPP2", Metadata_Value)],]
# sourceMetaData=rbind(sourceMetaDataWUP,sourceMetaDataWPP)
# 
# setnames(sourceMetaData, "geographicAreaM49_unpd", "geographicAreaM49")
# sourceMetaData[, geographicAreaM49:=as.character(geographicAreaM49)]
# sourceMetaData[, measuredElement:=as.character(measuredElement)]
# sourceMetaData[, timePointYears:=as.character(timePointYears)]
# sourceMetaData[, Metadata_Value:=as.character(Metadata_Value)]
# sourceMetaData[, Metadata_Language:=as.character(Metadata_Language)]
# 
# finalMetaData= sourceMetaData[ geographicAreaM49 %in% finalData$geographicAreaM49,  ]

message('population_unpd2fao plugin: Including metadata')

includemetadata <- copy(finalData)
includemetadata[, c('flagObservationStatus', 'flagMethod', 'Value')] <- NULL
includemetadata[measuredElement %in% c('511', '512', '513') , Metadata_Value := paste('WPP-', WPPyear, sep = '')]
includemetadata[measuredElement %in% c('551', '561') , Metadata_Value := paste('WUP-', WUPyear, sep = '')]
includemetadata[timePointYears > WPPyear, Metadata_Value := paste('WPP-', WPPyear, '. Medium fertility variant ', sep = '')]
includemetadata[geographicAreaM49 %in% unspec_conv$sws_code, Metadata_Value := 'Residual variable aggregating unreported areas']
includemetadata[,Metadata:="GENERAL"]
includemetadata[,Metadata:=as.character(Metadata)]
includemetadata[,Metadata_Element:="COMMENT"]
includemetadata[,Metadata_Language:="en"]
includemetadata[,Metadata_Group := c(1:dim(includemetadata)[1])]


MetaData1248 <- includemetadata[geographicAreaM49=="156", ]
MetaData1248[,geographicAreaM49:="1248"]

finalMetaData <- rbind(includemetadata, MetaData1248)

setcolorder(finalMetaData,c( "geographicAreaM49","measuredElement",
                             "timePointYears",  "Metadata",
                             "Metadata_Element", "Metadata_Language",
                             "Metadata_Group","Metadata_Value"))

## Checks

# finalDataCurrentYear=finalData[timePointYears==currentYear]
#finalDataCurrentYear[,timePointYears:=as.character(timePointYears)]


# finalMetaDataCurrentYear=finalMetaData[timePointYears==currentYear]
#finalMetaDataCurrentYear[,timePointYears:=as.character(timePointYears)]

message('population_unpd2fao plugin: Saving data')

# message(paste0("SavePopulation data for the year: ", currentYear))
SaveData("population","population_unpd",finalData, finalMetaData, waitTimeout = Inf)

## Initiate email
from = "sws@fao.org"
to = swsContext.userEmail
subject = "UNPD population data"
body = paste(paste0("Data have been properly synchronized from the source dataset. Time range: ",startYear,"-", endYear, ". ", sep = ''),
              msg, msgMF, msgRur, msgUrb, missingUnspecCode, missingUnspecCodeRU, sep = '')

sendmail(from = from, to = to, subject = subject, msg = body)

message("Data have been successfully imported from the Source Population dataset.")
