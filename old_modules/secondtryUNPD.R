
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
  #library(igraph)
#  library(faoswsBalancing)
#  library(faoswsStandardization)
  library(dplyr)
  library(MASS)
  library(lattice)
  library(reshape2)
  library(sendmailR)
  library(openxlsx)
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

sheetNames <- getSheetNames('WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES (1).xlsx')

estimates <- sheetNames[grepl('ESTIM|estim|Estimat', sheetNames)]
startrow <- 17 # add parameter


setwd('C:/Users/Charlotte/OneDrive - Food and Agriculture Organization/Github/faoswsPopulation/population/total')
estimates <- read.xlsx('WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES (1).xlsx',
                 sheet = estimates, startRow = startrow)
estimates <- as.data.table(estimates)

variant <- names(estimates)[grepl('ariant|ARIANT', names(estimates))]
countrycode <-names(estimates)[grepl('ountr', names(estimates))]
countrycode <- countrycode[grepl('ode', countrycode)]

pop_code_conv <- ReadDatatable('pop_code_conversion_unpd_fao')
pop_code_conv <- pop_code_conv[!is.na(unpd_description)]


mediumvar <- sheetNames[grepl('EDIUM|edium', sheetNames)]
medium <- read.xlsx('WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES (1).xlsx',
                    sheet = mediumvar, startRow = startrow)
