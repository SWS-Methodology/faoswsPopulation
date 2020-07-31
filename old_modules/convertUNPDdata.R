WPPmetadata="WPP2017"
WUPmetadata="WUP2014"


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
  library(forecast)
  #library(tidyr)
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

## Data coming from UN Population Division contains "Estimates" up to the current year ena "Projections"
## up to a long time horizon (2010). That's why for each dataset we import two different tables : the first contains
## "estimates", the second contains "projections". This info is reported in the metadata.
##
################################################
#####           Total Population           #####
################################################

## Estimation

pop_tot_pop_WPP2017=fread(file.path(getwd(),"population/total/WPP2019_TOTAL_POPULATION.csv"),
                          header = TRUE)
pop_tot_pop_WPP[ , area := NULL]
pop_tot_pop_WPP2017_melt= melt(pop_tot_pop_WPP2017, id.vars = c("Variant","area","Notes","Country_code"),
                               variable.name= "timePointYears",
                               value.name = "Value")

pop_tot_pop_WPP2017_melt=setnames(pop_tot_pop_WPP2017_melt,"Country_code", "geographicAreaM49")

##Eliminate spaces in the Value columns
pop_tot_pop_WPP2017_melt[,Value:=gsub(" ", "", pop_tot_pop_WPP2017_melt[,Value])]

##Value and time must be numeric
pop_tot_pop_WPP2017_melt[,Value:=as.numeric(Value)]
pop_tot_pop_WPP2017_melt[,timePointYears:=as.character(timePointYears)]
pop_tot_pop_WPP2017_melt[,timePointYears:=as.numeric(timePointYears)]


##Projection
pop_tot_pop_WPP2017_MediumVariantFertility=fread(file.path(getwd(),"population/total/WPP2019_TOTAL_POPULATION_MVF.csv"),
                                                 header = TRUE)
##melt data
pop_tot_pop_WPP2017_MVF_melt= melt(pop_tot_pop_WPP2017_MediumVariantFertility, id.vars = c("Variant","area","Notes","Country_code"),
                                   variable.name= "timePointYears",
                                   value.name = "Value")


pop_tot_pop_WPP2017_MVF_melt=setnames(pop_tot_pop_WPP2017_MVF_melt, "Country_code","geographicAreaM49")

## Remove some spaces that in the csv file identifie the thousand separator
pop_tot_pop_WPP2017_MVF_melt[,Value:=gsub(" ", "", pop_tot_pop_WPP2017_MVF_melt[,Value])]

##Value and time must be numeric
pop_tot_pop_WPP2017_MVF_melt[,Value:=as.numeric(Value)]
pop_tot_pop_WPP2017_MVF_melt[,timePointYears:=as.character(timePointYears)]
pop_tot_pop_WPP2017_MVF_melt[,timePointYears:=as.numeric(timePointYears)]



##Check 2015: 2015 is in common between the two dataset used as input. Check if the results are exactly the same
pop_MVF_2015=pop_tot_pop_WPP2017_MVF_melt[timePointYears==2015,]
pop_est_2015=pop_tot_pop_WPP2017_melt[timePointYears==2015,]

pop_est_2015[,Variant:=NULL]
pop_MVF_2015[,Variant:=NULL]

if(nrow(setdiff(pop_MVF_2015,pop_est_2015))>0){warning("Check 2015 data for TOT population!!!")}


pop_tot_pop_WPP2017_MVF_melt=pop_tot_pop_WPP2017_MVF_melt[timePointYears!=2015]
totPop=rbind(pop_tot_pop_WPP2017_melt,pop_tot_pop_WPP2017_MVF_melt)
totPop[,item:="tot"]

################################################
#####         Female Population            #####
################################################
##Estimation

WPP2017_POP_FEMALE=fread(file.path(getwd(),"population/female/WPP2019_POP_FEMALE.csv"),
                         header = TRUE)

WPP2017_POP_FEMALE_melt= melt(WPP2017_POP_FEMALE, id.vars = c("Variant","area","Notes","Country_code"),
                              variable.name= "timePointYears",
                              value.name = "Value")


setnames(WPP2017_POP_FEMALE_melt, "Country_code", "geographicAreaM49")

WPP2017_POP_FEMALE_melt[,Value:=gsub(" ", "", WPP2017_POP_FEMALE_melt[,Value])]

WPP2017_POP_FEMALE_melt[,Value:=as.numeric(Value)]
WPP2017_POP_FEMALE_melt[,timePointYears:=as.character(timePointYears)]
WPP2017_POP_FEMALE_melt[,timePointYears:=as.numeric(timePointYears)]
##Projection
WPP2017_POP_MVF_FEMALE= fread(file.path(getwd(),"population/female/WPP2019_POP_FEMALE_MVF.csv"),
                             header = TRUE)
##melt data
WPP2017_POP_MVF_FEMALE_melt= melt(WPP2017_POP_MVF_FEMALE, id.vars = c("Variant","area","Notes","Country_code"),
                                  variable.name= "timePointYears",
                                  value.name = "Value")


WPP2017_POP_MVF_FEMALE_melt=setnames(WPP2017_POP_MVF_FEMALE_melt, "Country_code","geographicAreaM49")
## Remove some spaces that in the csv file identifie the thousand separator
WPP2017_POP_MVF_FEMALE_melt[,Value:=gsub(" ", "", WPP2017_POP_MVF_FEMALE_melt[,Value])]
WPP2017_POP_MVF_FEMALE_melt[,Value:=as.numeric(Value)]

WPP2017_POP_MVF_FEMALE_melt[,timePointYears:=as.character(timePointYears)]
WPP2017_POP_MVF_FEMALE_melt[,timePointYears:=as.numeric(timePointYears)]

##Check 2015
WPP2017_POP_FEMALE_melt_2015=WPP2017_POP_FEMALE_melt[timePointYears==2020,]
WPP2017_POP_MVF_FEMALE_melt_2015=WPP2017_POP_MVF_FEMALE_melt[timePointYears==2020,]
WPP2017_POP_FEMALE_melt_2015[,Variant:=NULL]
WPP2017_POP_MVF_FEMALE_melt_2015[,Variant:=NULL]
if(nrow(setdiff(WPP2017_POP_FEMALE_melt_2015,WPP2017_POP_MVF_FEMALE_melt_2015))>0){warning("Check 2015 data for FEMALE population!!!")}


WPP2017_POP_MVF_FEMALE_melt=WPP2017_POP_MVF_FEMALE_melt[timePointYears!=2020]
totPop_female=rbind(WPP2017_POP_FEMALE_melt,WPP2017_POP_MVF_FEMALE_melt)

totPop_female[,item:="female"]
################################################
#####           Male Population            #####
################################################
##Estimation

WPP2017_POP_MALE=fread(file.path(getwd(),"population/male/WPP2019_POP_MALE.csv"),
                       header = TRUE)

WPP2017_POP_MALE_melt= melt(WPP2017_POP_MALE, id.vars = c("Variant","area","Notes","Country_code"),
                            variable.name= "timePointYears",
                            value.name = "Value")

setnames(WPP2017_POP_MALE_melt, "Country_code", "geographicAreaM49")

WPP2017_POP_MALE_melt[,Value:=gsub(" ", "", WPP2017_POP_MALE_melt[,Value])]
WPP2017_POP_MALE_melt[,Value:=as.numeric(Value)]
WPP2017_POP_MALE_melt[,timePointYears:=as.character(timePointYears)]
WPP2017_POP_MALE_melt[,timePointYears:=as.numeric(timePointYears)]


##Projection
WPP2017_POP_MVF_MALE=fread(file.path(getwd(),"population/male/WPP2019_POP_MALE_MVF.csv"),
                           header = TRUE)
##melt data
WPP2017_POP_MVF_MALE_melt= melt(WPP2017_POP_MVF_MALE, id.vars = c("Variant","area","Notes","Country_code"),
                                variable.name= "timePointYears",
                                value.name = "Value")


WPP2017_POP_MVF_MALE_melt=setnames(WPP2017_POP_MVF_MALE_melt, "Country_code","geographicAreaM49")
## Remove some spaces that in the csv file identifie the thousand separator
WPP2017_POP_MVF_MALE_melt[,Value:=gsub(" ", "", WPP2017_POP_MVF_MALE_melt[,Value])]
WPP2017_POP_MVF_MALE_melt[,Value:=as.numeric(Value)]

WPP2017_POP_MVF_MALE_melt[,timePointYears:=as.character(timePointYears)]
WPP2017_POP_MVF_MALE_melt[,timePointYears:=as.numeric(timePointYears)]

##Check 2015

WPP2017_POP_MALE_melt_2015=WPP2017_POP_MALE_melt[timePointYears==2020,]
WPP2017_POP_MVF_MALE_melt_2015=WPP2017_POP_MVF_MALE_melt[timePointYears==2020,]

WPP2017_POP_MALE_melt_2015[,Variant:=NULL]
WPP2017_POP_MVF_MALE_melt_2015[,Variant:=NULL]
if(nrow(setdiff(WPP2017_POP_MALE_melt_2015,WPP2017_POP_MVF_MALE_melt_2015))>0){warning("Check 2015 data for MALE population!!!")}


WPP2017_POP_MVF_MALE_melt=WPP2017_POP_MVF_MALE_melt[timePointYears!=2020]
totPop_male=rbind(WPP2017_POP_MALE_melt,WPP2017_POP_MVF_MALE_melt)

totPop_male[,item:="male"]

################################################
#####           Urban Population           #####
################################################
##Estimation

WPP_POP_URBAN=fread(file.path(getwd(),"population/urban/WUP2018-Urban.csv"),
                    header = TRUE)

WPP_POP_URBAN_melt= melt(WPP_POP_URBAN, id.vars = c("area","Note","Country_Code"),
                         variable.name= "timePointYears",
                         value.name = "Value")
setnames(WPP_POP_URBAN_melt, "Country_Code", "geographicAreaM49")

WPP_POP_URBAN_melt[,Value:=gsub(" ", "", WPP_POP_URBAN_melt[,Value])]
WPP_POP_URBAN_melt[,Value:=as.numeric(Value)]
WPP_POP_URBAN_melt[,timePointYears:=as.character(timePointYears)]
WPP_POP_URBAN_melt[,timePointYears:=as.numeric(timePointYears)]

totPop_urban=WPP_POP_URBAN_melt
totPop_urban[,item:="urban"]

################################################
#####           Rural Population           #####
################################################
##Estimation

WPP_POP_RURAL=fread(file.path(getwd(),"population/rural/WUP2018-Rural.csv"),
                    header = TRUE)

WPP_POP_RURAL_melt= melt(WPP_POP_RURAL, id.vars = c("area","Note","Country_Code"),
                         variable.name= "timePointYears",
                         value.name = "Value")

setnames(WPP_POP_RURAL_melt, "Country_Code", "geographicAreaM49")
WPP_POP_RURAL_melt[,Value:=gsub(" ", "", WPP_POP_RURAL_melt[,Value])]
WPP_POP_RURAL_melt[,Value:=as.numeric(Value)]
WPP_POP_RURAL_melt[,timePointYears:=as.character(timePointYears)]
WPP_POP_RURAL_melt[,timePointYears:=as.numeric(timePointYears)]

totPop_rural=WPP_POP_RURAL_melt
totPop_rural[,item:="rural"]


################################################
#####   Build and Save the final Dataset   #####
################################################

totPop_urban[,Variant:=NA]
setnames(totPop_urban, "Note", "Notes")

totPop_rural[,Variant:=NA]
setnames(totPop_rural, "Note", "Notes")

finalFile=rbind(totPop,totPop_male,totPop_female,totPop_urban,totPop_rural)


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

finalFile[item=="tot", measuredElement:="511"]
finalFile[item=="male", measuredElement:="512"]
finalFile[item=="female", measuredElement:="513"]
finalFile[item=="urban", measuredElement:="561"]
finalFile[item=="rural", measuredElement:="551"]


finalFile[,item:=NULL]
finalFile[,area:=NULL]

## Assign the Flags
finalFile[, flagObservationStatus:="X"]
finalFile[, flagMethod:="h"]


##ADD METADATA

fileMetadata=copy(finalFile)
fileMetadata[,flagObservationStatus:=NULL]
fileMetadata[,flagMethod:=NULL]
fileMetadata[,Value:=NULL]



fileMetadata[measuredElement %in% c("511","512","513"), Metadata_Value:=paste(WPPmetadata, Variant, sep="-")]
fileMetadata[measuredElement %in% c("561","551"), Metadata_Value:=paste(WUPmetadata, Variant, sep="-")]
fileMetadata[,Variant:=NULL]

fileMetadata[,Metadata_Value:=gsub("-NA", "", fileMetadata[,Metadata_Value])]




finalFile[,Variant:=NULL]
finalFile[,Notes:=NULL]


fileMetadata[,Metadata:="GENERAL"]
fileMetadata[,Metadata_Element:="COMMENT"]
fileMetadata[,Metadata_Language:="en"]
fileMetadata[,Metadata_Group:=c(1:dim(fileMetadata)[1])]
fileMetadata[,Notes:=NULL]


setcolorder(fileMetadata,c( "geographicAreaM49","measuredElement",
                            "timePointYears",  "Metadata",
                            "Metadata_Element", "Metadata_Language",
                            "Metadata_Group","Metadata_Value"))


setcolorder(finalFile,c( "geographicAreaM49","measuredElement",
                         "timePointYears",  "Value", "flagObservationStatus",
                         "flagMethod"))

setnames(finalFile,"geographicAreaM49","geographicAreaM49_unpd")
setnames(fileMetadata,"geographicAreaM49","geographicAreaM49_unpd")


#finalFile=finalFile[timePointYears<=2018,]
#fileMetadata=fileMetadata[timePointYears<=2018,]

finalFileLabel=nameData("population","source_population_undp",finalFile)
unique(finalFileLabel[is.na(geographicAreaM49_unpd_description), geographicAreaM49_unpd])

##GroupsOnly=finalFile[geographicAreaM49>900]
##setnames(GroupsOnly, "geographicAreaM49", "geographicAreaM49_unpd")
##GroupsOnly[,timePointYears:=as.character(timePointYears)]

finalFile[,geographicAreaM49_unpd:=as.character(geographicAreaM49_unpd)]
fileMetadata[,geographicAreaM49_unpd:=as.character(geographicAreaM49_unpd)]
finalFile[,timePointYears:=as.character(timePointYears)]


finalFile[geographicAreaM49_unpd=="1517", geographicAreaM49_unpd:="1505"]
fileMetadata[geographicAreaM49_unpd=="1517",  geographicAreaM49_unpd:="1505"]



startYear=finalFile[, min(timePointYears)]
endYear=finalFile[, max(timePointYears)]

timeWindow=c(startYear:endYear)

for(i in seq_along(timeWindow)){
  currentYear=timeWindow[i]
  finalFileCurrentYear=finalFile[timePointYears==currentYear]
  finalFileCurrentYear[,timePointYears:=as.character(timePointYears)]

  fileMetadataCurrentYear=fileMetadata[timePointYears==currentYear]
  fileMetadataCurrentYear[,timePointYears:=as.character(timePointYears)]

  message(paste0("Save Population data for the year: ", currentYear))
  SaveData("population","source_population_undp",finalFileCurrentYear, fileMetadataCurrentYear)
}

message("UNPD data have been imported in the Source UNPD population dataset")

