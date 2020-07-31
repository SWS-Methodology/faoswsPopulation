
##' Pull data from SWS folder, reshape and populate the 'Population UNPD (source)' dataset
##'
##' **Author: Francesca Rosa (modified by Charlotte Taglioni)**
##'
##' **Description:**
##'
##' Plugin to populate the 'Population UNPD (source)' dataset. It pulls original UNPD data from
##' the shared point on the server, reshape them and populate the 'Population UNPD (source)' dataset
##'
##' **Inputs:**
##'
##' * Initial parameters


suppressMessages({
  library(faosws)
  library(faoswsUtil)
  library(data.table)
#  library(igraph)
# library(faoswsBalancing)
# library(faoswsStandardization)
  library(dplyr)
  library(MASS)
  library(lattice)
  library(reshape2)
  library(sendmailR)
  library(openxlsx)
})

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
  R_SWS_SHARE_PATH <- getwd()
} else {
  R_SWS_SHARE_PATH <- Sys.getenv("R_SWS_SHARE_PATH")
}

sessionKey = swsContext.datasets[[1]]
CONFIG = GetDatasetConfig(swsContext.datasets[[1]]@domain, swsContext.datasets[[1]]@dataset)

setwd(file.path(R_SWS_SHARE_PATH, 'population'))
files <- list.files()

# Parameters
WPPmetadata <- as.character(swsContext.computationParams$WPP)
WUPmetadata <- as.character(swsContext.computationParams$WUP)
common_year <- as.numeric(swsContext.computationParams$Common_year) # The year present both in projections and data
startrow <- as.numeric(swsContext.computationParams$startrow) # add parameter

total_file <- files[grepl('oth|OTH', files)]
women_file <- files[grepl('emale|EMALE', files) & files != total_file]
men_file <- files[grepl('male|MALE', files) & !files %in% c(total_file, women_file)]
urban_file <- files[grepl('rban|RBAN', files) & !files %in% c(total_file, women_file, men_file)]
rural_file <- files[grepl('ural|URAL', files) & !files %in% c(total_file, women_file, men_file, urban_file)]

message('Packages and parameters loaded')
## Data coming from UN Population Division contains "Estimates" up to the current year ena "Projections"
## up to a long time horizon (2010). That's why for each dataset we import two different tables : the first contains
## "estimates", the second contains "projections". This info is reported in the metadata.

load_data <- function(myfile, startrow = startrow, common_year = common_year){

  sheetNames <- getSheetNames(myfile)

  estimatevar <- which(grepl('ESTIM|estim|Estimat|ata', sheetNames))

  estimates <- read.xlsx(myfile,
                         sheet = estimatevar, startRow = startrow)

  estimates <- as.data.table(estimates)

  variant <- names(estimates)[grepl('ariant|ARIANT', names(estimates))]

  countrycode <-names(estimates)[grepl('ountr', names(estimates))]
  countrycode <- countrycode[grepl('ode', countrycode)]
  lastcol <- names(estimates)[ncol(estimates)]
  years <- as.character(1950:as.numeric(lastcol))

  if(length(variant) == 0){
    estimates[, variant := NA]
    variant <- names(estimates)[grepl('ariant|ARIANT', names(estimates))]
  }

  estimates <- estimates[ , c(variant, countrycode, years), with = F]
  setnames(estimates, c(variant, countrycode), c("Variant","geographicAreaM49_unpd"))

  estimatesMelt <- melt(estimates, id.vars = c("Variant","geographicAreaM49_unpd"),
       variable.name= "timePointYears",
       value.name = "Value")

  ## Remove some spaces that in the csv file identifie the thousand separator
  estimatesMelt[,Value := gsub(" ", "", estimatesMelt[,Value])]

  ##Value and time must be numeric
  estimatesMelt[,Value := as.numeric(Value)]
  estimatesMelt[,timePointYears := as.numeric(as.character(timePointYears))]


  if(any(grepl('EDIUM|edium', sheetNames))){
  mediumvar <- which(grepl('EDIUM|edium', sheetNames))

  medium <- read.xlsx(myfile,
                      sheet = mediumvar, startRow = startrow)
  medium <- as.data.table(medium)


  variantm <- names(medium)[grepl('ariant|ARIANT', names(medium))]
  countrycodem <-names(medium)[grepl('ountr', names(medium))]
  countrycodem <- countrycodem[grepl('ode', countrycodem)]

  lastcolm <- names(medium)[ncol(medium)]
  firstcolm <- as.numeric(common_year)
  yearsm <- as.character(firstcolm:as.numeric(lastcolm))

  medium <- medium[ , c(variantm, countrycodem, yearsm), with = F]
  setnames(medium, c(variantm, countrycodem), c("Variant","geographicAreaM49_unpd"))

  mediumMelt <- melt(medium, id.vars = c("Variant","geographicAreaM49_unpd"),
                        variable.name= "timePointYears",
                        value.name = "Value")

  ## Remove some spaces that in the csv file identifie the thousand separator
  mediumMelt[,Value := gsub(" ", "", mediumMelt[,Value])]

  ##Value and time must be numeric
  mediumMelt[,Value := as.numeric(Value)]
  mediumMelt[,timePointYears := as.numeric(as.character(timePointYears))]

  if(nrow(setdiff(estimatesMelt[timePointYears == common_year,
                                .(geographicAreaM49_unpd, timePointYears, Value)],
                  mediumMelt[ timePointYears == common_year,
                              .(geographicAreaM49_unpd, timePointYears, Value)]))>0){warning("Check common_year data for TOT population!!!")}


  mediumMelt <- mediumMelt[timePointYears!=common_year]
  data <- rbind(estimatesMelt,mediumMelt)

  } else {
    data <- estimatesMelt
  }
  return(data)
}

message('Reading total data')
pop_WPP_tot <- load_data(myfile = total_file, startrow = startrow, common_year = common_year)

message('Reading female data')
pop_WPP_fem <- load_data(myfile = women_file, startrow = startrow, common_year = common_year)

message('Reading male data')
pop_WPP_male <- load_data(myfile = men_file, startrow = startrow, common_year = common_year)

message('Reading urban population data')
pop_WUP_urban <- load_data(myfile = urban_file , startrow = startrow, common_year = common_year)

message('Reading rural population data')
pop_WUP_rural <- load_data(myfile = rural_file , startrow = startrow, common_year = common_year)

# Add elements
pop_WPP_tot[, measuredElement := "511"]
pop_WPP_male[, measuredElement := "512"]
pop_WPP_fem[, measuredElement := "513"]
pop_WUP_urban[, measuredElement := "561"]
pop_WUP_rural[, measuredElement := "551"]

finalFile <- rbind(pop_WPP_tot, pop_WPP_fem, pop_WPP_male, pop_WUP_urban, pop_WUP_rural)

## Assign the Flags
finalFile[, flagObservationStatus := "X"]
finalFile[, flagMethod := "h"]
finalFile$geographicAreaM49_unpd <- as.character(finalFile$geographicAreaM49_unpd)

# Get countries in geographicAreaM49_unpd
swsunpd <- GetCodeList('population', 'source_population_undp', 'geographicAreaM49_unpd')
finalSWS <- finalFile[geographicAreaM49_unpd %in% swsunpd$code, ]
missingCountries <- unique(finalFile[!geographicAreaM49_unpd %in% swsunpd$code,
                                     ]$geographicAreaM49_unpd)
# Add metadata

fileMetadata <- copy(finalSWS)
fileMetadata[, c('flagObservationStatus', 'flagMethod', 'Value')] <- NULL

fileMetadata[measuredElement %in% c("511", "512", "513"), Metadata_Value := paste(WPPmetadata, Variant, sep="-")]
fileMetadata[measuredElement %in% c("561", "551"), Metadata_Value := paste(WUPmetadata, Variant, sep="-")]
fileMetadata[, Variant := NULL]

fileMetadata[, Metadata_Value := gsub("-NA", "", fileMetadata[, Metadata_Value])]

finalSWS[, Variant := NULL]

fileMetadata[, Metadata := "GENERAL"]
fileMetadata[, Metadata_Element := "COMMENT"]
fileMetadata[, Metadata_Language := "en"]
fileMetadata[, Metadata_Group := c(1:dim(fileMetadata)[1])]

setcolorder(fileMetadata, c("geographicAreaM49_unpd", "measuredElement",
                            "timePointYears", "Metadata",
                            "Metadata_Element", "Metadata_Language",
                            "Metadata_Group", "Metadata_Value"))

setcolorder(finalSWS, c("geographicAreaM49_unpd", "measuredElement",
                        "timePointYears", "Value", "flagObservationStatus",
                        "flagMethod"))

finalSWS[is.na(Value), c('Value', 'flagObservationStatus') := list(0, 'O')]
finalSWS[, geographicAreaM49_unpd := as.character(geographicAreaM49_unpd)]
fileMetadata[, geographicAreaM49_unpd := as.character(geographicAreaM49_unpd)]
finalSWS[, timePointYears := as.character(timePointYears)]
fileMetadata[, timePointYears := as.character(timePointYears)]

message("Saving Population data")
SaveData(domain = "population", dataset = "source_population_undp",
         data = finalSWS, metadata =  fileMetadata)

## Initiate email
from <- "sws@fao.org"
to <- swsContext.userEmail
subject <- "UNPD population data"
body <- paste0("Data have been properly uploaded from the source dataset.
               There are codes in the original data not in the codelist:",
               missingCountries,". Please check they are not relevant for the aggregate computation.")

sendmailR::sendmail(from = from, to = to, subject = subject, msg = body)

paste0("Upload completed successfully! ", "Email sent to ", swsContext.userEmail)

