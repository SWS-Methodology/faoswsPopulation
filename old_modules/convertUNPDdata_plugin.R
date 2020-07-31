
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
  library(igraph)
  library(faoswsBalancing)
  library(faoswsStandardization)
  library(dplyr)
  library(MASS)
  library(lattice)
  library(reshape2)
  library(sendmailR)
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

sessionKey = swsContext.datasets[[1]]
CONFIG = GetDatasetConfig(swsContext.datasets[[1]]@domain, swsContext.datasets[[1]]@dataset)

# Parameters
WPPmetadata <- as.character(swsContext.computationParams$WPP)
WUPmetadata <- as.character(swsContext.computationParams$WUP)
common_year <- as.numeric(swsContext.computationParams$Common_year) # The year present both in projections and data

total_file <- as.character(swsContext.computationParams$Total_file)
total_file_proj <- as.character(swsContext.computationParams$Total_file_proj)
women_file <- as.character(swsContext.computationParams$Women_file)
women_file_proj <- as.character(swsContext.computationParams$Women_file_proj)
men_file <- as.character(swsContext.computationParams$Men_file)
men_file_proj <- as.character(swsContext.computationParams$Men_file_proj)
urban_file <- as.character(swsContext.computationParams$Urban_file)
rural_file <- as.character(swsContext.computationParams$Rural_file)

message('Packages and parameters loaded')
## Data coming from UN Population Division contains "Estimates" up to the current year ena "Projections"
## up to a long time horizon (2010). That's why for each dataset we import two different tables : the first contains
## "estimates", the second contains "projections". This info is reported in the metadata.
##
################################################
#####           Total Population           #####
################################################

## Estimation

message('Reading population data')

pop_tot_pop_WPP <- fread(file.path(R_SWS_SHARE_PATH, "population", "total",  paste(total_file, ".csv", sep = "")),
                         header = TRUE, encoding = "UTF-8")

pop_tot_pop_WPP_melt= melt(pop_tot_pop_WPP, id.vars = c("Variant","Country_code"),
                           variable.name= "timePointYears",
                           value.name = "Value")

pop_tot_pop_WPP_melt=setnames(pop_tot_pop_WPP_melt,"Country_code", "geographicAreaM49")

##Eliminate spaces in the Value columns
pop_tot_pop_WPP_melt[,Value:=gsub(" ", "", pop_tot_pop_WPP_melt[,Value])]

##Value and time must be numeric
pop_tot_pop_WPP_melt[,Value:=as.numeric(Value)]
pop_tot_pop_WPP_melt[,timePointYears:=as.character(timePointYears)]
pop_tot_pop_WPP_melt[,timePointYears:=as.numeric(timePointYears)]


##Projection

message('Reading population projection')

pop_tot_pop_WPP_MediumVariantFertility <- fread(file.path(R_SWS_SHARE_PATH, "population", "total", paste(total_file_proj, ".csv", sep = "")),
                                                encoding = "UTF-8", header = TRUE)
##melt data
pop_tot_pop_WPP_MVF_melt <- melt(pop_tot_pop_WPP_MediumVariantFertility, id.vars = c("Variant", "Country_code"),
                                 variable.name = "timePointYears",
                                 value.name = "Value")


pop_tot_pop_WPP_MVF_melt <- setnames(pop_tot_pop_WPP_MVF_melt, "Country_code","geographicAreaM49")

## Remove some spaces that in the csv file identifie the thousand separator
pop_tot_pop_WPP_MVF_melt[,Value := gsub(" ", "", pop_tot_pop_WPP_MVF_melt[,Value])]

##Value and time must be numeric
pop_tot_pop_WPP_MVF_melt[,Value := as.numeric(Value)]
pop_tot_pop_WPP_MVF_melt[,timePointYears := as.character(timePointYears)]
pop_tot_pop_WPP_MVF_melt[,timePointYears := as.numeric(timePointYears)]



##Check common_year: common_year is in common between the two dataset used as input. Check if the results are exactly the same
pop_MVF_common_year <- pop_tot_pop_WPP_MVF_melt[timePointYears==common_year,]
pop_est_common_year <- pop_tot_pop_WPP_melt[timePointYears==common_year,]

pop_est_common_year[, Variant := NULL]
pop_MVF_common_year[, Variant := NULL]

if(nrow(setdiff(pop_MVF_common_year,pop_est_common_year))>0){warning("Check common_year data for TOT population!!!")}


pop_tot_pop_WPP_MVF_melt <- pop_tot_pop_WPP_MVF_melt[timePointYears!=common_year]
totPop <- rbind(pop_tot_pop_WPP_melt,pop_tot_pop_WPP_MVF_melt)
totPop[,item:="tot"]

################################################
#####         Female Population            #####
################################################
##Estimation
message('Reading female data')
WPP_POP_FEMALE <- fread(file.path(R_SWS_SHARE_PATH, "population", "female", paste(women_file, ".csv", sep = "")),
                        encoding = "UTF-8", header = TRUE)

WPP_POP_FEMALE_melt <- melt(WPP_POP_FEMALE, id.vars = c("Variant", "Country_code"),
                            variable.name = "timePointYears",
                            value.name = "Value")


setnames(WPP_POP_FEMALE_melt, "Country_code", "geographicAreaM49")

WPP_POP_FEMALE_melt[, Value := gsub(" ", "", WPP_POP_FEMALE_melt[,Value])]

WPP_POP_FEMALE_melt[, Value := as.numeric(Value)]
WPP_POP_FEMALE_melt[, timePointYears := as.character(timePointYears)]
WPP_POP_FEMALE_melt[, timePointYears := as.numeric(timePointYears)]
##Projection
message('Reading female projection')
WPP_POP_MVF_FEMALE <- fread(file.path(R_SWS_SHARE_PATH, "population", "female", paste(women_file_proj, ".csv", sep = "")),
                            encoding = "UTF-8", header = TRUE)
##melt data
WPP_POP_MVF_FEMALE_melt <- melt(WPP_POP_MVF_FEMALE, id.vars = c("Variant", "Country_code"),
                                variable.name = "timePointYears",
                                value.name = "Value")


WPP_POP_MVF_FEMALE_melt <- setnames(WPP_POP_MVF_FEMALE_melt, "Country_code","geographicAreaM49")
## Remove some spaces that in the csv file identifie the thousand separator
WPP_POP_MVF_FEMALE_melt[, Value := gsub(" ", "", WPP_POP_MVF_FEMALE_melt[,Value])]
WPP_POP_MVF_FEMALE_melt[, Value := as.numeric(Value)]

WPP_POP_MVF_FEMALE_melt[, timePointYears := as.character(timePointYears)]
WPP_POP_MVF_FEMALE_melt[, timePointYears := as.numeric(timePointYears)]

##Check common_year
WPP_POP_FEMALE_melt_common_year <- WPP_POP_FEMALE_melt[timePointYears == common_year,]
WPP_POP_MVF_FEMALE_melt_common_year <- WPP_POP_MVF_FEMALE_melt[timePointYears == common_year,]
WPP_POP_FEMALE_melt_common_year[, Variant := NULL]
WPP_POP_MVF_FEMALE_melt_common_year[, Variant := NULL]
if(nrow(setdiff(WPP_POP_FEMALE_melt_common_year,WPP_POP_MVF_FEMALE_melt_common_year))>0){warning("Check common_year data for FEMALE population!!!")}


WPP_POP_MVF_FEMALE_melt <- WPP_POP_MVF_FEMALE_melt[timePointYears != common_year]
totPop_female <- rbind(WPP_POP_FEMALE_melt, WPP_POP_MVF_FEMALE_melt)

totPop_female[, item := "female"]
################################################
#####           Male Population            #####
################################################
##Estimation
message('Reading male data')
WPP_POP_MALE <- fread(file.path(R_SWS_SHARE_PATH, "population", "male", paste(men_file, ".csv", sep = "")),
                      encoding = "UTF-8", header = TRUE)

WPP_POP_MALE_melt <- melt(WPP_POP_MALE, id.vars = c("Variant", "Country_code"),
                          variable.name = "timePointYears",
                          value.name = "Value")

setnames(WPP_POP_MALE_melt, "Country_code", "geographicAreaM49")

WPP_POP_MALE_melt[, Value := gsub(" ", "", WPP_POP_MALE_melt[,Value])]
WPP_POP_MALE_melt[, Value := as.numeric(Value)]
WPP_POP_MALE_melt[, timePointYears := as.character(timePointYears)]
WPP_POP_MALE_melt[, timePointYears := as.numeric(timePointYears)]


##Projection
message('Reading male projection')
WPP_POP_MVF_MALE <- fread(file.path(R_SWS_SHARE_PATH, "population", "male", paste(men_file_proj, ".csv", sep = "")),
                          encoding = "UTF-8", header = TRUE)
##melt data
WPP_POP_MVF_MALE_melt <- melt(WPP_POP_MVF_MALE, id.vars = c("Variant", "Country_code"),
                              variable.name = "timePointYears",
                              value.name = "Value")


WPP_POP_MVF_MALE_melt <- setnames(WPP_POP_MVF_MALE_melt, "Country_code","geographicAreaM49")
## Remove some spaces that in the csv file identifie the thousand separator
WPP_POP_MVF_MALE_melt[, Value := gsub(" ", "", WPP_POP_MVF_MALE_melt[,Value])]
WPP_POP_MVF_MALE_melt[, Value := as.numeric(Value)]

WPP_POP_MVF_MALE_melt[, timePointYears := as.character(timePointYears)]
WPP_POP_MVF_MALE_melt[, timePointYears := as.numeric(timePointYears)]

##Check common_year

WPP_POP_MALE_melt_common_year <- WPP_POP_MALE_melt[timePointYears == common_year,]
WPP_POP_MVF_MALE_melt_common_year <- WPP_POP_MVF_MALE_melt[timePointYears == common_year,]

WPP_POP_MALE_melt_common_year[, Variant := NULL]
WPP_POP_MVF_MALE_melt_common_year[, Variant := NULL]
if(nrow(setdiff(WPP_POP_MALE_melt_common_year,WPP_POP_MVF_MALE_melt_common_year))>0){warning("Check common_year data for MALE population!!!")}


WPP_POP_MVF_MALE_melt <- WPP_POP_MVF_MALE_melt[timePointYears != common_year]
totPop_male <- rbind(WPP_POP_MALE_melt,WPP_POP_MVF_MALE_melt)

totPop_male[, item := "male"]

################################################
#####           Urban Population           #####
################################################
##Estimation
message('Reading urban population data')
WPP_POP_URBAN <- fread(file.path(R_SWS_SHARE_PATH, "population", "urban", paste(urban_file, ".csv", sep = "")),
                       encoding = "UTF-8", header = TRUE)

WPP_POP_URBAN_melt <- melt(WPP_POP_URBAN, id.vars = c("Country_code"),
                           variable.name= "timePointYears",
                           value.name = "Value")
setnames(WPP_POP_URBAN_melt, "Country_code", "geographicAreaM49")

WPP_POP_URBAN_melt[, Value := gsub(" ", "", WPP_POP_URBAN_melt[,Value])]
WPP_POP_URBAN_melt[, Value := as.numeric(Value)]
WPP_POP_URBAN_melt[, timePointYears := as.character(timePointYears)]
WPP_POP_URBAN_melt[, timePointYears := as.numeric(timePointYears)]

totPop_urban <- WPP_POP_URBAN_melt
totPop_urban[, item := "urban"]

################################################
#####           Rural Population           #####
################################################
##Estimation
message('Reading rural population data')
WPP_POP_RURAL <- fread(file.path(R_SWS_SHARE_PATH, "population", "rural", paste(rural_file, ".csv", sep = "")),
                       encoding = "UTF-8", header = TRUE)

WPP_POP_RURAL_melt <- melt(WPP_POP_RURAL, id.vars = c("Country_code"),
                           variable.name = "timePointYears",
                           value.name = "Value")

setnames(WPP_POP_RURAL_melt, "Country_code", "geographicAreaM49")
WPP_POP_RURAL_melt[, Value := gsub(" ", "", WPP_POP_RURAL_melt[, Value])]
WPP_POP_RURAL_melt[, Value := as.numeric(Value)]
WPP_POP_RURAL_melt[, timePointYears := as.character(timePointYears)]
WPP_POP_RURAL_melt[, timePointYears := as.numeric(timePointYears)]

totPop_rural <- WPP_POP_RURAL_melt
totPop_rural[, item := "rural"]


################################################
#####   Build and Save the final Dataset   #####
################################################

totPop_urban[, Variant := NA]

totPop_rural[,Variant:=NA]

finalFile <- rbind(totPop, totPop_male, totPop_female, totPop_urban, totPop_rural)


#noMapped=rbind(noMappedTOT, noMapped_female, noMapped_male, noMapped_rural, noMapped_urban)
#
#noMappedCode=unique(noMapped[,geographicAreaM49])
#
#noMappedCode=noMappedCode[!noMappedCode %in% c("654", "344", "446",
#                                               "408", "275", "203",
#                                               "830", "807", "534",
#                                               "850", "583", "580", "234")]
#
#finalFileTest=finalFile[!(geographicAreaM49 %in% noMappedCode),  ]

# unique(finalFileTest[is.na(geographicAreaM49_description),geographicAreaM49])
# [1] "1503" "1517" "1502" "1501" "1500" "5500" "5501" "1505"
#finalFileTest=finalFileTest[!is.na(geographicAreaM49_description),]
#finalFileTest[,geographicAreaM49_description:=NULL]

## Assign the Element codes

finalFile[item == "tot", measuredElement := "511"]
finalFile[item == "male", measuredElement := "512"]
finalFile[item == "female", measuredElement := "513"]
finalFile[item == "urban", measuredElement := "561"]
finalFile[item == "rural", measuredElement := "551"]


finalFile[, item := NULL]

## Assign the Flags
finalFile[, flagObservationStatus := "X"]
finalFile[, flagMethod := "h"]
finalFile$geographicAreaM49 <- as.character(finalFile$geographicAreaM49)

# Get countries in geographicAreaM49
swsM49 <- GetCodeList('population', 'population_unpd', 'geographicAreaM49')
swsM49 <- swsM49[type == 'country' & selectionOnly==FALSE, .(code, description)]

finalSWS <- merge(finalFile, swsM49, by.x = 'geographicAreaM49', by.y = 'code', all.x = TRUE)
finalSWS <- finalSWS[!is.na(description) ]
finalSWS[ , description := NULL]

##ADD METADATA

fileMetadata <- copy(finalSWS)
fileMetadata[, flagObservationStatus := NULL]
fileMetadata[, flagMethod := NULL]
fileMetadata[, Value := NULL]



fileMetadata[measuredElement %in% c("511", "512", "513"), Metadata_Value := paste(WPPmetadata, Variant, sep="-")]
fileMetadata[measuredElement %in% c("561", "551"), Metadata_Value := paste(WUPmetadata, Variant, sep="-")]
fileMetadata[, Variant := NULL]

fileMetadata[, Metadata_Value := gsub("-NA", "", fileMetadata[, Metadata_Value])]


finalSWS[, Variant := NULL]


fileMetadata[, Metadata := "GENERAL"]
fileMetadata[, Metadata_Element := "COMMENT"]
fileMetadata[, Metadata_Language := "en"]
fileMetadata[, Metadata_Group := c(1:dim(fileMetadata)[1])]


setcolorder(fileMetadata, c("geographicAreaM49", "measuredElement",
                            "timePointYears", "Metadata",
                            "Metadata_Element", "Metadata_Language",
                            "Metadata_Group", "Metadata_Value"))


setcolorder(finalSWS, c("geographicAreaM49", "measuredElement",
                         "timePointYears", "Value", "flagObservationStatus",
                         "flagMethod"))

# setnames(finalSWS, "geographicAreaM49", "geographicAreaM49_unpd")
# setnames(fileMetadata, "geographicAreaM49", "geographicAreaM49_unpd")


finalSWSLabel <- nameData("population", "population_unpd", finalSWS)
unique(finalSWSLabel[is.na(geographicAreaM49_description), geographicAreaM49])

##GroupsOnly=finalFile[geographicAreaM49>900]
##setnames(GroupsOnly, "geographicAreaM49", "geographicAreaM49_unpd")
##GroupsOnly[,timePointYears:=as.character(timePointYears)]

finalSWS[, geographicAreaM49 := as.character(geographicAreaM49)]
fileMetadata[, geographicAreaM49 := as.character(geographicAreaM49)]
finalSWS[, timePointYears := as.character(timePointYears)]


# finalSWS[geographicAreaM49_unpd == "1517", geographicAreaM49_unpd := "1505"]
# fileMetadata[geographicAreaM49_unpd == "1517",  geographicAreaM49_unpd := "1505"]

startYear <- finalSWS[, min(timePointYears)]
endYear <- finalSWS[, max(timePointYears)]

timeWindow <- c(startYear:endYear)

for(i in seq_along(timeWindow)){
  currentYear <- timeWindow[i]
  finalSWSCurrentYear <- finalSWS[timePointYears == currentYear]
  finalSWSCurrentYear[, timePointYears := as.character(timePointYears)]
  finalSWSCurrentYear$Value <- ifelse(is.na(finalSWSCurrentYear$Value), 0, finalSWSCurrentYear$Value)
  finalSWSCurrentYear$flagObservationStatus <- ifelse(is.na(finalSWSCurrentYear$Value), 'O',
                                                       finalSWSCurrentYear$flagObservationStatus)

  fileMetadataCurrentYear <- fileMetadata[timePointYears == currentYear]
  fileMetadataCurrentYear[, timePointYears := as.character(timePointYears)]

  message(paste0("Save Population data for the year: ", currentYear))
  SaveData("population","population_unpd",finalSWSCurrentYear, fileMetadataCurrentYear)
}




## Initiate email
from <- "sws@fao.org"
to <- swsContext.userEmail
subject <- "UNPD population data"
body <- paste0("Data have been properly uploaded from the source dataset. Time range: ",startYear,"-", endYear, ". ")


sendmailR::sendmail(from = from, to = to, subject = subject, msg = body)

paste0("Upload completed successfully! ", "Email sent to ", swsContext.userEmail)




