
pop_tot_pop_WPP <- fread(file.path("population", "total",  paste('WPP2019_TOTAL_POPULATION', ".csv", sep = "")),
                         header = TRUE, encoding = "UTF-8")

pop_tot_pop_WPP_melt= melt(pop_tot_pop_WPP, id.vars = c("Variant","Country_code"),
                           variable.name= "timePointYears",
                           value.name = "Value")

pop_tot_pop_WPP_melt=setnames(pop_tot_pop_WPP_melt,"Country_code", "geographicAreaM49")


popT <- fread('PopCodeTable.csv', header = T, encoding = "UTF-8")
setnames(popT, 'ï»¿unpd_description', 'unpd_description')
popT$unpd_code<- as.character(popT$unpd_code)


m49 <- GetCodeList('population', 'population_unpd', 'geographicAreaM49')[ , .(code,description)]
m49$unpd_code <- m49$code

tot <- merge(m49, popT, by.x = 'code' , by.y = 'unpd_code', all = TRUE)


'unpd_fao_pop_code_conversion'

#ok <- tot[unpd_description == description]

descr <- tot[unpd_description != description]

codeunpdNo <- tot[is.na(unpd_description) & !is.na(description)]

codeNo <- tot[!is.na(unpd_description) & is.na(description)]

write.csv(descr, 'tocheck.csv', row.names = F)
write.csv(codeNo, 'tocheck1.csv', row.names = F)
write.csv(codeunpdNo, 'tocheck2.csv', row.names = F)

checked <- fread('tocheck.csv', header = T, encoding = "UTF-8")
checkedok <- checked[check=='ok']
codeokAdd <- checkedok$code
#tot[,compliant := NULL]
tot[, compliant := FALSE]

tot[unpd_description == description, compliant := TRUE]
tot[unpd_code %in% checkedok$unpd_code, compliant := TRUE]

tot[is.na(unpd_description) , unpd_code := NA]
tot[is.na(description) , unpd_code := code]
tot[is.na(description) , code := NA]
tot$unpd_description <- as.character(tot$unpd_description)
setnames(tot, 'description', 'sws_description')

tot[compliant == FALSE][sws_description %in% tot[compliant == FALSE]$unpd_description]

write.csv(tot, 'workingfile.csv')

aaa<- read.csv('pop_unspecified_conversion.csv')
aaa <- data.table(aaa)
aaa$sws_code <- as.character(aaa$sws_code)
aaa[8, sws_code:='011.01']
setnames(aaa, 'ï..area_name', 'unpd_area_name')
changeset <- Changeset('pop_unspecified_conversion')
AddInsertions(changeset, aaa)
Finalise(aaa)

bbb <- ReadDatatable('pop_unspecified_conversion')
names(aaa) <- names(bbb)
aaa$unpd_code <- as.character(aaa$unpd_code)
aaa
write.csv(aaa, 'pop_unspecified_conversion.csv')

setcolorder(tot, c("code",
                   "unpd_description",
                   "sws_description",
                   "compliant"))

aa <- ReadDatatable('pop_code_conversion_unpd_fao')
names(aa)

changeset <- Changeset('pop_code_conversion_unpd_fao')
AddInsertions(changeset, tot)
Finalise(changeset)

#
# missingcountry <- rbind(ok, checkedok[, names(ok), with = F])
#
# countries <- GetCodeList('population', 'population_unpd', 'geographicAreaM49')[type == 'country' ,]
#
# View(countries[!code %in% missingcountry$code])
#
# missingcountry[!code %in% countries$code]
#
