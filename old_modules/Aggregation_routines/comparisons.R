
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

country=GetCodeList("population", "population_unpd", "geographicAreaM49")[,code]
time=GetCodeList("population", "population_unpd", "timePointYears")[,code]

endYear= max(time)
#startYear=finalData[, min(timePointYears)]
startYear="1950"
## All the population elements
elements=c("511", "512", "513", "551", "561")

#startYear=swsContext.computationParams$startYear
#endYear=swsContext.computationParams$endYear

#time=c(startYear:endYear)

key = DatasetKey(domain = "population", dataset = "population_unpd", dimensions = list(
  geographicAreaM49_unpd = Dimension(name = "geographicAreaM49", keys = country),
  measuredElement= Dimension(name = "measuredElement", keys = elements),
  timePointYears = Dimension(name = "timePointYears", keys = time)
))

sourceData <- GetData(key)
data2018 <- sourceData[timePointYears == '2018' & measuredElement == '511', ]


##-- From aggregation module

##' Obtain aggregation parameter. This parameter determines the type of aggregation
##' method.
type_aggregation = 'aggregate_area' #swsContext.computationParams$aggregation

# Name of parameters
domain_ <- swsContext.datasets[[1]]@domain
dataset_ <- swsContext.datasets[[1]]@dataset
country <- getVarName("geog")
element <- getVarName("element")
item <- getVarName("item")
years <- getVarName("years")


# Condition to stop
if(length(item) == 0){
  if(!(type_aggregation %in% c('ad_hoc_area', 'aggregate_area'))) {
    stop('Please, choose another kind of aggregate. This data set may not have item')
  }
}

## Data set to be used: from session or all

# Session
sessionKey <- swsContext.datasets[[1]]
countrySession <- sessionKey@dimensions[country][[1]]@keys
elementSession <- sessionKey@dimensions[element][[1]]@keys
yearsSession <- sessionKey@dimensions[years][[1]]@keys

# not all datasets contain item (ex: ess_eco_macroind_processing_test)
if(length(item) == 1){
  itemSession <- sessionKey@dimensions[item][[1]]@keys
}

additionalElements <- list()
for (i in 1:length(elementSession)) {
  # tab_vars <- ReadDatatable("aggregate_elements_func", columns = c("var", "intermed", "func_var"))
  # if(!is.na(tab_vars[var == elementSession[i], intermed])) {
  calculated_elements <- ReadDatatable('calculated_elements',
                                       columns = c('result_var_code', 'num_var_code', 'den_var_code'))
  calculated_elements <- calculated_elements[!den_var_code %in% c(5319) & !result_var_code == '664']
  calculated_elements[, intermed := paste0(num_var_code, ',', den_var_code)]

  if(length(calculated_elements[result_var_code == elementSession[i], intermed]) == 1){
    additionalElements[i] <- calculated_elements[result_var_code == elementSession[i], intermed]
  }
}

if(length(additionalElements) > 0) {
  elements2Add <- unlist(strsplit(unlist(additionalElements), ","))
  sessionKey@dimensions[element][[1]]@keys <- unique(c(sessionKey@dimensions[element][[1]]@keys, elements2Add))
}

# Computing element session again
elementSession <- sessionKey@dimensions[element][[1]]@keys

# Reading table of flag weight used in the function aggregateObservationFlag
flag_weight <- ReadDatatable("flag_weight_table")
setnames(flag_weight, c("flag_observation_status","flag_observation_weights"),
         c("flagObservationStatus", "flagObservationWeights"))

flag_weight[is.na(flagObservationStatus), flagObservationStatus := '']

# Get the key if ad hoc aggregate is not selected

key_ <- getKey()

################################################################################
master_domain_ <-  NULL
if(domain_ == 'trade') {
  master_domain_ = 'Trade'
} else if(domain_ == 'macro_stats') {
  master_domain_ = 'Macro-Statistics'
} else if(domain_ == 'agriculture') {
  master_domain_ = 'Production'
} else if(domain_ == 'suafbs') {
  if(dataset_ %in% c('fbs_standardized', 'sua_unbalanced', 'sua_balanced')) {
    master_domain_ = 'suafbs'
  } else {
    master_domain_ = 'Food Balance'
  }
} else if (domain_ == 'population') {
  master_domain_ = 'Population'
}

##' Selected the key based on the input parameter
selectedKey =
  switch(type_aggregation,
         'ad_hoc_area' = sessionKey,
         'ad_hoc_item' = sessionKey,
         'ad_hoc_area_item' = sessionKey,
         'aggregate_area' = key_$masterKeyArea,
         'aggregate_item' = key_$masterKeyItem,
         'aggregate_area_item' = key_$masterKeyAreaItem)

raw_data <- GetData(selectedKey, flags = TRUE)

#-- Compare ----

setdiff(raw_data, sourceData)
nrow(raw_data)
names(raw_data)
nrow(sourceData)
names(sourceData)

data2018 <- sourceData[timePointYears == '2018' & measuredElement == '511', ]
length(unique(data2018$geographicAreaM49))

countries_complete <- GetCodeList("population", "population_unpd", "geographicAreaM49")[type == 'country']
countries_complete <- countries_complete[ is.na(endDate), .(code, description)]

check <- merge(data2018[ , .(geographicAreaM49, Value)], countries_complete, by.x = 'geographicAreaM49', by.y = 'code', all = TRUE)
missingCountryes <- check[is.na(Value) ]
write.csv(check, 'check.csv', row.names = F)

missing <- countries_complete[!code %in% unique(data2018$geographicAreaM49) ]

area_years <- ReadDatatable('area_years')

validCountries <- merge(missing, area_years, by.x = 'code', by.y = 'm49', all.x = TRUE)

sum(data2018[geographicAreaM49 != '1248']$Value)

area_years[is.na(end_year)]$m49

currentCountries <- data2018[geographicAreaM49 %in% area_years[is.na(end_year)]$m49 ]

absentCountries <- data2018[!geographicAreaM49 %in% area_years[is.na(end_year)]$m49 ]

sum(currentCountries$Value) + sum(absentCountries[geographicAreaM49 != '156']$Value)

#-----
aggregate_area_groups <- ReadDatatable('aggregate_groups',
                                       where = paste0("var_type = 'area' ", "AND master_domain =", " '",
                                                      master_domain_, "'"))

aggregate_area_groups <- aggregate_area_groups[, mget(c('var_group_code', 'var_group_code_sws', 'var_code_sws'))]
aggregate_area_groups[, double := duplicated(aggregate_area_groups,
                                             by = c('var_group_code_sws', 'var_code_sws'))]

aggregate_area_groups <- aggregate_area_groups[double == FALSE]
aggregate_area_groups <- na.omit(aggregate_area_groups)

area_aggregates <- unique(aggregate_area_groups$var_group_code_sws)

FAOSTATgroups <- fread('Aggregation_routines/groups.csv', header = TRUE)
FAOSTATgroups$M49Code <- as.character(FAOSTATgroups$M49Code)
FAOSTATgroups$GroupCode <- as.character(FAOSTATgroups$GroupCode)
xx <- merge(FAOSTATgroups, aggregate_area_groups, by.x = c("GroupCode", "M49Code"), by.y = c("var_group_code", "var_code_sws"), all = T,
            allow.cartesian = T)
library(xslx)

original <- readxl::read_xlsx('R:/population/total/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES (1).xlsx', sheet = 1, range = "A17:BZ306",
                  col_names = TRUE)

original <- as.data.table(original)
original <- original[Type == 'Country', ]
names(original)
original <- original[ , c("Index", "Variant", "Notes", "Type", "Parent code") := NULL ]
originaldataset <- melt(original,  id.vars = c("Region, subregion, country or area *",
                            "Country code"), variable.name = "timePointYears",
     value.name = "Value")
setnames(originaldataset, c("Region, subregion, country or area *", "Country code"), c("descriptionUNPD", "geographicAreaM49"))
originaldataset[ , geographicAreaM49:= as.character(geographicAreaM49) ]
write.csv(originaldataset, 'UNPDdataOriginal.csv', row.names = FALSE)

unpd2018 <- originaldataset[ timePointYears == "2018",]

unpd2018[ , Value := round(as.numeric(Value),3)]

unpdCheck <- merge(check, unpd2018, by = c('geographicAreaM49'), all = TRUE)
unpdCheck[ , ValueCheck := ifelse(Value.x==Value.y, T,F)]

#-- AREAS ----
original <- readxl::read_xlsx('R:/population/total/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES (1).xlsx', sheet = 1, range = "A17:BZ306",
                              col_names = TRUE)

original <- as.data.table(original)

areas <- original[Type %in% c('SDG region', 'Subregion', 'Income Group', 'Region', 'SDG subregion', "World"), ]
areas <- areas[ , c("Index", "Variant", "Notes", "Type", "Parent code") := NULL ]
areasdata<- melt(areas,  id.vars = c("Region, subregion, country or area *",
                                                    "Country code"), variable.name = "timePointYears",
                             value.name = "Value")

setnames(areasdata, c("Region, subregion, country or area *", "Country code"), c("descriptionUNPD", "geographicAreaM49"))
areasdata[ , geographicAreaM49:= as.character(geographicAreaM49) ]
areasdata[, Value := round(as.numeric(Value),3)]

SWSm49 <- GetCodeList("population", "population_unpd", "geographicAreaM49")[ , .(code, description)]

SWSm495503 <- rbind(SWSm49, data.table(code = '5503', description = 'Micronesia'))
attempt <- merge(data2save, SWSm495503, by.x = "geographicAreaM49", by.y = 'code')
attempt <- attempt[geographicAreaM49 != '583' ]
unique(attempt$description)
unique(areasdata$descriptionUNPD)


areasdata[descriptionUNPD == "Western Africa", descriptionUNPD := "West Africa"]
areasdata[descriptionUNPD == "Eastern Africa", descriptionUNPD := "East Africa"]
areasdata[descriptionUNPD == "Central America", descriptionUNPD := "Central Amer"]
areasdata[descriptionUNPD == "South America", descriptionUNPD := "SouthAmerica"]
areasdata[descriptionUNPD == "Southern Asia", descriptionUNPD := "South Asia"]
areasdata[descriptionUNPD == "WORLD", descriptionUNPD := "World"]

commonareas <- merge(areasdata, attempt[measuredElement=='511'],
                     by.x = c('descriptionUNPD',"timePointYears"),
                     by.y = c('description', 'timePointYears'),
                     suffixes = c('_unpd', '_sws'))

commonareas[ , difference := Value_unpd - Value_sws]

zoomIn <- commonareas[ difference > 0.1]
zoomIn2 <- commonareas[ difference < - 0.1]

unique(zoomIn$descriptionUNPD)

correctAreas <- commonareas[difference < 0.1 ]

unique(correctAreas$descriptionUNPD)


asianDiff <- zoomIn[ descriptionUNPD == 'Asia']

test1992 <- merge(zoomIn[descriptionUNPD == 'Europe' & timePointYears %in% 1950:1991, .(descriptionUNPD, timePointYears, difference)],
      asianDiff[ , .(descriptionUNPD, timePointYears, difference)],
      by = 'timePointYears')

test1992
