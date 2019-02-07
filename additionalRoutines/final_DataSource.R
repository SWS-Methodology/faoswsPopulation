

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



finalFile[item=="tot", measuredElement:="511"]
finalFile[item=="male", measuredElement:="512"]
finalFile[item=="female", measuredElement:="513"]
finalFile[item=="urban", measuredElement:="561"]
finalFile[item=="rural", measuredElement:="551"]


finalFile[,item:=NULL]
finalFile[,area:=NULL]
finalFile[, flagObservationStatus:="X"]
finalFile[, flagMethod:="h"]


##ADD METADATA

fileMetadata=copy(finalFile)
fileMetadata[,flagObservationStatus:=NULL]
fileMetadata[,flagMethod:=NULL]
fileMetadata[,Value:=NULL]



fileMetadata[measuredElement %in% c("511","512","513"), Metadata_Value:=paste("WPP2017", Variant, sep="-")]
fileMetadata[measuredElement %in% c("561","551"), Metadata_Value:=paste("WUP2014", Variant, sep="-")]
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


finalFile=finalFile[timePointYears<=2018,]
fileMetadata=fileMetadata[timePointYears<=2018,]

finalFileLabel=nameData("population","source_population_undp",finalFile)
unique(finalFileLabel[is.na(geographicAreaM49_unpd_description), geographicAreaM49_unpd])

##GroupsOnly=finalFile[geographicAreaM49>900]
##setnames(GroupsOnly, "geographicAreaM49", "geographicAreaM49_unpd")
##GroupsOnly[,timePointYears:=as.character(timePointYears)]

finalFile[geographicAreaM49_unpd=="1517", geographicAreaM49_unpd:="1505"]
fileMetadata[geographicAreaM49_unpd=="1517",  geographicAreaM49_unpd:="1505"]


finalFile[,geographicAreaM49_unpd:=as.character(geographicAreaM49_unpd)]
finalFile[,timePointYears:=as.character(timePointYears)]


SaveData("population", "source_population_undp",finalFile[1:10] , fileMetadata[1:10])



