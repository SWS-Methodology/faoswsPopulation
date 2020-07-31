library(data.table)
library(openxlsx)
faos <- read.csv('FAOSTAT_data_5-27-2020.csv')
faos <-as.data.table(faos)
test <- read.xlsx('test_aggregates.xlsx', sheet = 2)
test <- as.data.table(test)
names(test)
names(faos)
test <- test[Year %in% 1950:2018]
faos[ ,c( "Domain.Code", "Domain")] <- NULL
tot <- merge(faos, test, by.x = c("Area.Code", "Element.Code",
                                  "Item.Code", "Item", "Year"),
             by.y = c("AreaCode", "EleCode",
                      "ItemCode",  "ItemName", "Year"), all =T,
             suffixes = c('FS', 'SWS'))
tot[,check:='?']
tot[ValueFS==ValueSWS, check := 'ok']
tot[ValueFS!=ValueSWS, check := 'no']
tot[is.na(ValueFS) & !is.na(ValueSWS), check := 'sws']
tot[!is.na(ValueFS) & is.na(ValueSWS), check := 'fs']
nrow(tot[check=='ok'])
nrow(tot[check =='ok'])/nrow(tot)
View(tot[check=='sws'])
unique(tot[check=='sws']$AreaName)
View(tot[check=='no'])

tot[check=='no', diff := round(ValueSWS - ValueFS,6)]

write.csv(tot, 'PopulationComparison.csv', row.names = F)

agg<-ReadDatatable('geographic_hierarchy_ebx5')
aggCar <- agg[name_en_l3== 'Western Europe']
names(test)
car <- test[M49Code %in% aggCar$code_l5 ]
car <- car[Year == '2010']
carMF <- car[EleCode %in% c('512', '513')]
sum(carMF$Value)

cartot <- tot[M49Code %in% aggCar$code_l5 ]

aggCar[!code_l5 %in% unique(cartot$M49Code)]

sum(finalData[geographicAreaM49 %in% aggCar$code_l5 & timePointYears == '2010' &
           measuredElement %in% c('512', '513')]$Value) +
  sum(finalData[geographicAreaM49 == '155.01' & timePointYears == '2010' &
                  measuredElement %in% c('512', '513')]$Value)

sum(finalData[geographicAreaM49 %in% aggCar$code_l5 & timePointYears == '2010' &
              measuredElement %in% c('511')]$Value)


check <- finalData[geographicAreaM49 %in% countriesinterested & measuredElement %in% c('512', '513')]

check2 <- merge(check, adhocaggr, by.x = 'geographicAreaM49', by.y = 'code_l5', all =T)
check2[, Value:= sum(Value), by = c('code_l4', 'timePointYears')]
setkey(check2)
check2$measuredElement <-NULL
check2$geographicAreaM49 <- NULL
check2 <- unique(check2)
aaa<- check2[timePointYears == '2010']

toadd <- finalData[geographicAreaM49 %in% unspec_conv$sws_code]
toadd[timePointYears=='2010']
