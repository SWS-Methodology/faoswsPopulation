library(faosws)
library(faoswsUtil)
library(data.table)
library(tidyr)
library(dplyr)
library(faoswsFlag)

if(CheckDebug()){

  library(faoswsModules)
  sett <- ReadSettings("sws.yml")

  SetClientFiles(sett$certdir)
  GetTestEnvironment(sett$server, sett$token)
  files = dir("R", full.names = TRUE)
  invisible(sapply(files, source))
}

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

# Get the flag
flag_ <- grep('flag', colnames(raw_data), ignore.case = T, value = T)

#' ## Remove invalid years

area_years <- ReadDatatable('area_years')

filt_data <- raw_data[countryExists(countries = get(country),
                                    years = get(years),
                                    # countries = geographicAreaM49,
                                    # years = timePointYears,
                                    refcountries = area_years$m49,
                                    refstarts = area_years$start_year,
                                    refends = area_years$end_year),]

#' ## Remove existing aggregates
#'
#' Now that these invalid combinations have been removed, we need to remove any
#' existing aggregates that may be present in the data, both item and country
#' aggregates.
#'
#' ### Country aggregates

aggregate_area_groups <- ReadDatatable('aggregate_groups',
                                       where = paste0("var_type = 'area' ", "AND master_domain =", " '",
                                                      master_domain_, "'"))

aggregate_area_groups <- aggregate_area_groups[, mget(c('var_group_code_sws', 'var_code_sws'))]
aggregate_area_groups[, double := duplicated(aggregate_area_groups,
                                             by = c('var_group_code_sws', 'var_code_sws'))]

aggregate_area_groups <- aggregate_area_groups[double == FALSE]
aggregate_area_groups <- na.omit(aggregate_area_groups)

area_aggregates <- unique(aggregate_area_groups$var_group_code_sws)

aggregate_area_groups <- aggregate_area_groups[!(var_code_sws %in% area_aggregates)]

setnames(aggregate_area_groups, old=c('var_group_code_sws', 'var_code_sws'),
         new = c('group_code', 'm49'))

aggregate_area_groups[, double := NULL]

filt_data <- filt_data[!get(country) %chin% area_aggregates,]

#' ### Item aggregates

if(length(item) == 1){

  aggregate_item_groups <- ReadDatatable('aggregate_groups',
                                         where = paste0("var_type = 'item' ", "AND master_domain =", " '",
                                                        master_domain_, "'"))

  setnames(aggregate_item_groups, old=c('var_group_code_sws', 'var_code_sws'),
           new = c('item_group_code', 'cpc_code'))

  aggregate_item_groups <- aggregate_item_groups[, list(factor = max(factor)), by = c('item_group_code', 'cpc_code')]
  aggregate_groups <- unique(aggregate_item_groups$item_group_code)
  aggregate_groups <- aggregate_groups[!is.na(aggregate_groups)]

  filt_data <- filt_data[!(item %chin% aggregate_groups | item == 'AH01'), ]

}

#' ## Assign groups

#' ### Assign country groups

filt_area <- merge(filt_data, aggregate_area_groups,
                   by.x = country,
                   by.y = "m49",
                   all.x = TRUE,
                   allow.cartesian = TRUE)

filt_area <- na.omit(filt_area)

#' ### Assign item groups and factors

if(length(item) == 1){

factored_item <- merge(filt_data, aggregate_item_groups,
                   by.x = item,
                   by.y = "cpc_code",
                   all.x = TRUE,
                   allow.cartesian = TRUE)

#' ### Data with both groups

factored_both <- merge(filt_area, aggregate_item_groups,
                   by.x = item,
                   by.y = "cpc_code",
                   all.x = TRUE,
                   allow.cartesian = TRUE)

## All values with no factor have a factor of 1
factored_item[, factor := as.numeric(factor)]
factored_both[, factor := as.numeric(factor)]

factored_item[is.na(factor), factor := 1]
factored_both[is.na(factor), factor := 1]

# factored_item[, factorValue := ifelse(!element %in% c(5622, 5922), Value * factor, Value)]
# factored_both[, factorValue := ifelse(!element %in% c(5622, 5922), Value * factor, Value)]

factored_item[!get(element) %in% c(5622, 5922), factorValue := Value * factor]
factored_item[get(element) %in% c(5622, 5922), factorValue := Value]

factored_both[!get(element) %in% c(5622, 5922), factorValue := Value * factor]
factored_both[get(element) %in% c(5622, 5922), factorValue := Value]

factored_item[, c('Value', 'factor') := NULL]
factored_both[, c('Value', 'factor') := NULL]

setnames(factored_item, 'factorValue', 'Value')
setnames(factored_both, 'factorValue', 'Value')

}

# Clean up
rm(filt_data)

# Aggregate types

# There are items/countries that are not aggregate at all. Ex Casein for Trade

# area

if(domain_ %in% c('trade')) {
  domain_agg_type = 'Trade'
} else if(domain_ == 'agriculture') {
  domain_agg_type = 'Production'
} else if(domain_ == 'suafbs') {
  domain_agg_type = 'Food Balance'
} else if(domain_ == 'macro_stats') {
  domain_agg_type = 'Macro-Statistics'
} else if (domain_ == 'population') {
  domain_agg_type = 'Population'
}

if (domain_ %in% c('trade', 'agriculture', 'suafbs', 'macro_stats', 'population')) {
  aggregate_type_area = ReadDatatable('aggregate_type',
                                      where = paste0("var_type = 'area' AND master_domain = '",domain_agg_type, "'"))

  aggregate_type_area <- aggregate_type_area[!(domain_name %in%
                                                 c('Trade Flow', 'Trade Indices',
                                                   'Detailed trade matrix', 'Production Indices',
                                                   'Value of Agricultural Production'))]


  list_area <- aggregate_type_area[, mget(c('var_code_sws', 'domain_var_diss_flag_agg_int'))]
  list_area <- list_area[!(domain_var_diss_flag_agg_int == 0 | is.na(var_code_sws))]

  # item
  aggregate_type_item = ReadDatatable('aggregate_type',
                                      where = paste0("var_type = 'item' AND master_domain = '",domain_agg_type, "'"))

  aggregate_type_item <- aggregate_type_item[!(domain_name %in%
                                                 c('Trade Flow', 'Trade Indices',
                                                   'Detailed trade matrix', 'Production Indices',
                                                   'Value of Agricultural Production'))]

  list_item <- aggregate_type_item[, mget(c('var_code_sws', 'domain_var_diss_flag_agg_int'))]
  list_item <- list_item[!(domain_var_diss_flag_agg_int == 0 | is.na(var_code_sws))]
} else {
  list_area <- NULL
  list_item <- NULL
}

#' ## Perform aggregation

if(type_aggregation == 'ad_hoc_area') {

  filt_area <- filt_area[group_code == '953'] # only world

  if(length(item) == 1) {
  data2save <- plyr::ddply(filt_area,
                           .variables = c(item, years),
                           .fun = aggregateElement,
                           .progress = 'text')
  } else {
    data2save <- plyr::ddply(filt_area,
                             .variables = c(years),
                             .fun = aggregateElement,
                             .progress = 'text')

  }

  data2save <- data.table(data2save)

  if(length(item) == 1) {
    data2save <- melt.data.table(data2save, id.vars = c(item, years))
  } else {
    data2save <- melt.data.table(data2save, id.vars = c(years))
  }

  data2save[, variable := as.character(variable)]
  data2save <- data2save[variable %in% elementSession]
  data2save[, country := 'AH01']
  setnames(data2save, old = c('country', 'variable', 'value'),
           new = c(country, element, 'Value'))

  if(length(item) == 1) {
    if(length(flag_) == 2) {
  flagTab <- filt_area[, list(flagObservationStatus = aggregateObservationFlag(flagObservationStatus, flagTable = flag_weight),
                              flagMethod = 'x'),
                       by = c(element, item, years)]
  data2save <- merge(data2save, flagTab,
                     by.x = c(element, item, years),
                     by.y = c(element, item, years))
  } else {
    data2save[, flagFaostat := 'x']
  }
    } else {
      if(length(flag_) == 2) {

    flagTab <- filt_area[, list(flagObservationStatus = aggregateObservationFlag(flagObservationStatus, flagTable = flag_weight),
                                flagMethod = 'x'),
                         by = c(element, years)]

    data2save <- merge(data2save, flagTab,
                       by.x = c(element, years),
                       by.y = c(element, years))
      }
      else{
        data2save[, flagFaostat := 'x']
      }
  }

} else
  if(type_aggregation == 'ad_hoc_item') {
    # factored_item <- factored_item[item_group_code == 'F1882'] # only Agricult.Products,Total
    factored_item <- factored_item[, list(Value = unique(Value)),
                                   c(country, item,
                                     element, years,
                                     'flagObservationStatus', 'flagMethod')]
    data2save <- plyr::ddply(factored_item,
                             # .variables = c('item_group_code', years, country),
                             .variables = c(country, years),
                             .fun = aggregateElement,
                             .progress = 'text')
    data2save <- data.table(data2save)
    # data2save <- melt.data.table(data2save, id.vars = c(country, 'item_group_code', years))
    data2save <- melt.data.table(data2save, id.vars = c(country, years))
    data2save[, variable := as.character(variable)]
    data2save <- data2save[variable %in% elementSession]
    # data2save[, item_group_code := NULL]
    data2save[, item := 'AH01']
    setnames(data2save, old = c('item', 'variable', 'value'),
             new = c(item, element, 'Value'))

    flagTab <- factored_item[, list(flagObservationStatus = aggregateObservationFlag(flagObservationStatus, flagTable = flag_weight),
                                flagMethod = 'x'),
                         by = c(country, years, element)]
    data2save <- merge(data2save, flagTab,
                       by.x = c(country, years, element),
                       by.y = c(country, years, element))

  } else
    if(type_aggregation == 'ad_hoc_area_item') {
      factored_both <- factored_both[group_code == '953'] # only Agricult.Products,Total
      factored_both <- factored_both[, list(Value = unique(Value)),
                                     c(country, item,
                                       element, years,
                                       'flagObservationStatus', 'flagMethod')] # we are aggregating all items present together
      data2save <- plyr::ddply(factored_both,
                               # .variables = c('item_group_code', years, 'group_code'),
                               .variables = c(years),
                               .fun = aggregateElement,
                               .progress = 'text')
      data2save <- data.table(data2save)
      data2save <- melt.data.table(data2save, id.vars = c(years))
      data2save[, variable := as.character(variable)]
      data2save <- data2save[variable %in% elementSession]
      data2save[, country := 'AH01']
      data2save[, item := 'AH01']
      setnames(data2save, old = c('country', 'item', 'variable', 'value'),
               new = c(country, item, element, 'Value'))

      flagTab <- factored_both[, list(flagObservationStatus = aggregateObservationFlag(flagObservationStatus, flagTable = flag_weight),
                                      flagMethod = 'x'),
                               by = c(years, element)]
      data2save <- merge(data2save, flagTab,
                         by.x = c(years, element),
                         by.y = c(years, element))

    } else
      if(type_aggregation == 'aggregate_area') {

        if(nrow(list_area) > 0) {
          filt_area <- filt_area[filt_area[[country]] %in% list_area$var_code_sws]
        }

        if(nrow(list_item) > 0) {
          filt_area <- filt_area[filt_area[[item]] %in% list_item$var_code_sws]
        }

        if(length(item) == 1) {
        data2save <- plyr::ddply(filt_area,
                                 .variables = c('group_code', item, years),
                                 .fun = aggregateElement,
                                 .progress = 'text')
        } else {
          data2save <- plyr::ddply(filt_area,
                                   .variables = c('group_code', years),
                                   .fun = aggregateElement,
                                   .progress = 'text')
        }
        data2save <- data.table(data2save)

        if(length(item) == 1) {
        data2save <- melt.data.table(data2save, id.vars = c('group_code', item, years))
        } else {
          data2save <- melt.data.table(data2save, id.vars = c('group_code', item, years))
        }

        data2save[, variable := as.character(variable)]
        data2save <- data2save[variable %in% elementSession]

        setnames(data2save, old = c('variable', 'value'),
                 new = c(element, 'Value'))

        if(length(item) == 1) {
        flagTab <- filt_area[, list(flagObservationStatus = aggregateObservationFlag(flagObservationStatus, flagTable = flag_weight),
                                        flagMethod = 'x'),
                                 by = c(years, element, item, 'group_code')]
        data2save <- merge(data2save, flagTab,
                           by.x = c(years, element, item, 'group_code'),
                           by.y = c(years, element, item, 'group_code'))
        } else {

          flagTab <- filt_area[, list(flagObservationStatus = aggregateObservationFlag(flagObservationStatus, flagTable = flag_weight),
                                      flagMethod = 'x'),
                               by = c(years, element, 'group_code')]
          data2save <- merge(data2save, flagTab,
                             by.x = c(years, element, 'group_code'),
                             by.y = c(years, element, 'group_code'))

        }

        setnames(data2save, 'group_code', country)

      } else
        if(type_aggregation == 'aggregate_item') {

          if(nrow(list_area) > 0) {
            factored_item <- factored_item[factored_item[[country]] %in% list_area$var_code_sws]
          }

          if(nrow(list_item) > 0) {
            factored_item <- factored_item[factored_item[[item]] %in% list_item$var_code_sws]
          }

          data2save <- plyr::ddply(factored_item,
                                   .variables = c('item_group_code', years, country),
                                   .fun = aggregateElement,
                                   .progress = 'text')
          data2save <- data.table(data2save)
          data2save <- melt.data.table(data2save, id.vars = c(country, 'item_group_code', years))
          data2save[, variable := as.character(variable)]
          data2save <- data2save[variable %in% elementSession]
          setnames(data2save, old = c('variable', 'value'),
                   new = c(element, 'Value'))

          flagTab <- factored_item[, list(flagObservationStatus = aggregateObservationFlag(flagObservationStatus, flagTable = flag_weight),
                                      flagMethod = 'x'),
                               by = c(years, element, 'item_group_code', country)]

          data2save <- merge(data2save, flagTab,
                             by.x = c(country, 'item_group_code', years, element),
                             by.y = c(country, 'item_group_code', years, element))

          setnames(data2save, 'item_group_code', item)

        } else
          if(type_aggregation == 'aggregate_area_item') {

            if(nrow(list_area) > 0) {
              list_area_99 <- list_area[domain_var_diss_flag_agg_int == 99]
              factored_both <- factored_both[factored_both[[country]] %in% list_area_99$var_code_sws]
            }

            if(nrow(list_item) > 0) {
              list_item_99 <- list_item[domain_var_diss_flag_agg_int == 99]
              factored_both <- factored_both[factored_both[[item]] %in% list_item_99$var_code_sws]
            }

            data2save <- plyr::ddply(factored_both,
                                     .variables = c('item_group_code', years, 'group_code'),
                                     .fun = aggregateElement,
                                     .progress = 'text')
            data2save <- data.table(data2save)
            data2save <- melt.data.table(data2save, id.vars = c('group_code', 'item_group_code', years))
            data2save[, variable := as.character(variable)]
            data2save <- data2save[variable %in% elementSession]
            setnames(data2save, old = c('variable', 'value'), new = c(element, 'Value'))

            flagTab <- factored_both[, list(flagObservationStatus = aggregateObservationFlag(flagObservationStatus, flagTable = flag_weight),
                                            flagMethod = 'x'),
                                     by = c(years, element, 'item_group_code', 'group_code')]

            data2save <- merge(data2save, flagTab,
                               by.x = c('group_code', 'item_group_code', years, element),
                               by.y = c('group_code', 'item_group_code', years, element))

            setnames(data2save, old = c('group_code', 'item_group_code'), new = c(country, item))

          }

# Save
data2save <- data2save[!(is.na(Value) | Value == Inf)]
# data2save <- data2save[data2save[[country]] != 5503] # group code for Micronesia (it does not exist in SWS)
data2save <- data2save[data2save[[country]] != "NA"]

if(length(item) == 1) {
data2save <- data2save[data2save[[item]] != "NA"]
}

# Elements that are aggregated
elements_2_not_aggregate <- ReadDatatable('elements_to_not_aggregate')
elements_not_aggregate <- data2save[data2save[[element]] %in% elements_2_not_aggregate$element_code]
elements_not_aggregate <- unique(elements_not_aggregate[[element]])
not_aggregates <- store <- paste0('"', paste(elements_not_aggregate, collapse="\", \""), '"')

data2save <- data2save[!(data2save[[element]] %in% elements_not_aggregate)]
# stats <- SaveData(domain_, dataset_, data2save)
nowarn <- stats[!names(stats) %in% "warnings"]

if(length(elements_not_aggregate) > 0) {
  paste0(paste0(sprintf("%s: %s", names(nowarn), unlist(nowarn)), collapse = "\n"),
         paste0('Elements that are not aggregated: ', not_aggregates))

} else {
  paste0(sprintf("%s: %s", names(nowarn), unlist(nowarn)), collapse = "\n")
}
