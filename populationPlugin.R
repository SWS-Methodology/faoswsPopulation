

suppressMessages({
  library(data.table)
  library(faosws)
  library(faoswsFlag)
  library(faoswsUtil)
  library(faoswsImputation)
  library(faoswsProduction)
  library(faoswsProcessing)
  library(faoswsEnsure)
  library(magrittr)
  library(dplyr)
  library(sendmailR)
  library(faoswsStandardization)

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

## Take the Token from the population_unpd dataset

country=GetCodeList("population", "source_population_undp", "geographicAreaM49_unpd")[,code]
time=GetCodeList("population", "source_population_undp", "timePointYears")[,code]

endYear= max(time)
#startYear=finalData[, min(timePointYears)]
startYear="1950"
## All the population elements
elements=c("511", "512", "513", "551", "561")

#startYear=swsContext.computationParams$startYear
#endYear=swsContext.computationParams$endYear

#time=c(startYear:endYear)

key = DatasetKey(domain = "population", dataset = "source_population_undp", dimensions = list(
  geographicAreaM49_unpd = Dimension(name = "geographicAreaM49_unpd", keys = country),
  measuredElement= Dimension(name = "measuredElement", keys = elements),
  timePointYears = Dimension(name = "timePointYears", keys = time)
))



timeWindow=c(startYear:endYear)

for(i in seq_along(timeWindow)){

  message("Restricting the time key..")

  currentYear=as.character(timeWindow[i])
  key@dimensions$timePointYears@keys=currentYear

  message(paste0("Get the data and metadata for the year ", currentYear))
#Get data
sourceData = GetData(key)

#Get METAdata
sourceMetaData=GetMetadata(key)
sourceMetaDataWUP= sourceMetaData[grepl("WUP", Metadata_Value),]
sourceMetaDataWPP= sourceMetaData[grepl("WPP", Metadata_Value),]
sourceMetaData=rbind(sourceMetaDataWUP,sourceMetaDataWPP)


## Data manupulations
sourceData=nameData("population","source_population_undp",sourceData,except = c("measuredElement","timePointYears"))
setnames(sourceData, "geographicAreaM49_unpd", "geographicAreaM49")
sourceData=nameData("population","population_unpd",sourceData, except = c("measuredElement","timePointYears"))

checkM49=copy(sourceData)
checkM49[, label:=(geographicAreaM49_unpd_description==geographicAreaM49_description)]

## noMapped1 contains those countries that have a different label, it means that the same code is used to
## identify a different area
noMapped1=unique(checkM49[label==FALSE, .(geographicAreaM49, geographicAreaM49_unpd_description, geographicAreaM49_description)])

## Codes from UNPD that do not exist at all within the FAO classification (M49)
noMapped2=unique(checkM49[is.na(geographicAreaM49_description), .(geographicAreaM49, geographicAreaM49_unpd_description, geographicAreaM49_description)])

noMapped=rbind(noMapped1, noMapped2)

# The vector noMapped contains all the countries that have to be deleted, but I have manually mapped some codes that
# despite have different labels, are exactly the same and have to be kept in the operational dataset.
noMappedCode=noMapped[, geographicAreaM49]

noMappedCode=noMappedCode[!noMappedCode %in% c("654", "344", "446",
                                               "408", "275", "203",
                                               "830", "807", "534",
                                               "850", "583", "580", "234")]

finalData=sourceData[!(geographicAreaM49 %in% noMappedCode),  ]

## There additional check to be run:

## "China, main" issue.
## The M49 code used for China is 1248, this is not true for UNPD who identifies China, main with the code 156


finalData[geographicAreaM49=="156", geographicAreaM49:="1248"]
finalData[,geographicAreaM49_description:=NULL]
finalData[,geographicAreaM49_unpd_description:=NULL]

## Metadata manipulation
## filter metadata for M49 countries and transform China in 1248
setnames(sourceMetaData, "geographicAreaM49_unpd", "geographicAreaM49")
sourceMetaData[, geographicAreaM49:=as.character(geographicAreaM49)]
sourceMetaData[, measuredElement:=as.character(measuredElement)]
sourceMetaData[, timePointYears:=as.character(timePointYears)]
sourceMetaData[, Metadata_Value:=as.character(Metadata_Value)]
sourceMetaData[, Metadata_Language:=as.character(Metadata_Language)]

finalMetaData= sourceMetaData[!(geographicAreaM49 %in% noMappedCode),  ]
finalMetaData[geographicAreaM49=="156", geographicAreaM49:="1248"]
finalMetaData[,Metadata:="GENERAL"]
finalMetaData[, Metadata:=as.character(Metadata)]

finalMetaData[,Metadata_Element:="COMMENT"]
finalMetaData=as.data.table(left_join(finalData, finalMetaData, by=c("geographicAreaM49","measuredElement","timePointYears")))
finalMetaData[,Metadata_Group:=c(1:dim(finalMetaData)[1])]


finalMetaData=finalMetaData[,.( geographicAreaM49,measuredElement,
                  timePointYears,  Metadata,
                  Metadata_Element, Metadata_Language,
                  Metadata_Group,Metadata_Value)]

setcolorder(finalMetaData,c( "geographicAreaM49","measuredElement",
                             "timePointYears",  "Metadata",
                             "Metadata_Element", "Metadata_Language",
                             "Metadata_Group","Metadata_Value"))


## Checks

  finalDataCurrentYear=finalData[timePointYears==currentYear]
  finalDataCurrentYear[,timePointYears:=as.character(timePointYears)]


  finalMetaDataCurrentYear=finalMetaData[timePointYears==currentYear]
  finalMetaDataCurrentYear[,timePointYears:=as.character(timePointYears)]


  message(paste0("SavePopulation data for the year: ", currentYear))
  SaveData("population","population_unpd",finalDataCurrentYear, finalMetaDataCurrentYear)
}





## Initiate email
from = "sws@fao.org"
to = swsContext.userEmail
subject = "UNPD population data"
body = paste0("Data have been properly synchronized from the source dataset. Time range: ",startYear,"-", endYear )


sendmail(from = from, to = to, subject = subject, msg = body)



message("Data have been successfully imported from the Source Population dataset.")


