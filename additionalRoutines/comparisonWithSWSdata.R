## Comparison with the already existing data (used by Cristina in the Standardization)




geo=GetCodeList("population", "population", "geographicAreaM49")[,code]
areaKeys=geo
elemKeys="21"
yearVals=as.character(c(2000:2016))

keyPOP = DatasetKey(domain = "population", dataset = "population", dimensions = list(
  geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = areaKeys),
  measuredElementSuaFbs = Dimension(name = "measuredElementPopulation", keys = elemKeys),
  timePointYears = Dimension(name = "timePointYears", keys = as.character(yearVals))
))

SWSpop=GetData(keyPOP)



## Get the population from the new DATASET population_unpd

elemKeys_unpd="511"
keyPOP_unpd = DatasetKey(domain = "population", dataset = "population_unpd", dimensions = list(
  geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = areaKeys),
  measuredElementSuaFbs = Dimension(name = "measuredElement", keys = elemKeys_unpd),
  timePointYears = Dimension(name = "timePointYears", keys = as.character(yearVals))
))

SWSpop_unpd=GetData(keyPOP_unpd)

merge(SWSpop_unpd,SWSpop, by=c("geographicAreaM49", "timePointYears"),suffixes =  c("_unpd", "_SWS"))


POP_compare=merge(SWSpop_unpd,SWSpop, by=c("geographicAreaM49", "timePointYears"),suffixes =  c("_unpd", "_SWS"))

POP_compare[,variation:=Value_unpd-Value_SWS]
POP_compare[,variation:=variation/Value_SWS]

POP_compare[,variation:=variation*100]

