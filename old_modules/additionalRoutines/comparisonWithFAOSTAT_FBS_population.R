## Comparison with the FAOSTAT data


faostat_pop=fread("C:/Users/Rosa/Favorites/Github/sws_project/faoswsPopulation/data/FAOSTAT/FAOSTAT_data_3-21-2018.csv")

faostat_pop[,geographicAreaM49:=fs2m49(`Country Code`)]
##faostat_pop[is.na(geographicAreaM49), unique(Country)]
## Please note that the code fs= 351 is not mapped
## This code is China (but not mainland)
## China mainland is properly mapped on the M49 code 1248
## faostat_pop[geographicAreaM49=="1248"]

faostat_pop=faostat_pop[,.(geographicAreaM49,`Element Code`, Year, Value, Flag)]
faostat_pop=faostat_pop[!is.na(geographicAreaM49)]

setnames(faostat_pop,"Element Code","measuredElement")
setnames(faostat_pop,"Year","timePointYears")
setnames(faostat_pop,"geographicAreaM49","geographicAreaM49_unpd")
Syria=fread("C:/Users/Rosa/Favorites/Github/sws_project/faoswsPopulation/data/FAOSTAT/Syria.csv")
faostat_pop=rbind(faostat_pop, Syria)
finalFile[geographicAreaM49_unpd=="156", geographicAreaM49_unpd:="1248"]
## Merge with the UNPD Data
comparison=merge(finalFile,faostat_pop, by=c( "geographicAreaM49_unpd", "measuredElement", "timePointYears"), suffixes = c("_unpd","_faostat") )
comparison[,Value_faostat:=as.numeric(Value_faostat)]
## Difference

comparison[, change:=Value_unpd-Value_faostat]
comparison[, change:=change/Value_faostat]
comparison[abs(change)>0.20]

setnames(comparison,"geographicAreaM49_unpd", "geographicAreaM49")
comparison = nameData("population", "population_unpd", comparison)


## Plot the comparison:

suppressMessages({
  library(igraph)
  library(dplyr)
  library(MASS)
  library(lattice)
  library(reshape2)
  library(forecast)
  library(tidyr)
})

toPlot=melt(comparison, id.vars = c(1:3,8), measure.vars = c(4,7), value.name = "Population")
setnames(toPlot, "geographicAreaM49_unpd", "geographicAreaM49")
toPlot=nameData("population","population_unpd",toPlot)
toPlot[,timePointYears:=as.numeric(timePointYears)]

pdf(file.path(paste0(getwd(), "/Population.pdf")),
    paper = "a4",width=9, height=15)

geo=sort(unique(toPlot[, geographicAreaM49_description]))
chart_per_pags = 12


position_geo <- seq(1,length(geo),chart_per_pags)

if(length(geo)/chart_per_pags==length(geo)%/%chart_per_pags){
  lgp=length(geo)/chart_per_pags}else{
    lgp=length(geo)%/%chart_per_pags+1
  }

pnp=list()
for(i in 1:lgp){
  if(!is.na(position_geo[i+1])){



    pnp[[i]] <-  ggplot(toPlot[geographicAreaM49_description %in% geo[ position_geo[i]: (position_geo[i+1]-1)  ]], aes(x=timePointYears, y=Population)) +
      geom_line(aes(linetype=variable,color=variable,size=variable)) +
      scale_x_continuous(breaks=2000:2013) +
      scale_linetype_manual(values=c("solid","dotdash","dotdash","solid","solid")) +
      scale_colour_manual(values = c("red","blue")) +
      scale_size_manual(values=c(0.5,0.5)) +
      ##geom_point( aes(shape = PROTECTED))+
      theme(axis.title =element_text(size=5),
            axis.text.y = element_text(size=5),
            axis.text.x = element_text(size=4,angle = 50, hjust = 1),
            legend.text = element_text(size=6),
            strip.text.x = element_text(size = 7),
            legend.position = "top",
            panel.grid.major = element_line(colour = "grey80", linetype = "longdash",size= 0.2 ),
            panel.grid.minor = element_line(colour="white",size=0),
            panel.background = element_rect(fill="white")) +
      facet_wrap(~geographicAreaM49_description, ncol = 3,scales = "free")
    print(pnp[[i]])

  }else{
    if(!is.na(position_geo[i])){
      pnp[[i]] <-  ggplot(toPlot[geographicAreaM49_description %in% geo[position_geo[i]:length(geo)]], aes(x=timePointYears, y=Population)) +
        geom_line(aes(linetype=variable,color=variable,size=variable)) +
        scale_x_continuous(breaks=2000:2015) +
        scale_linetype_manual(values=c("solid","dotdash","dotdash","solid","solid")) +
        scale_colour_manual(values = c("red","blue")) +
        scale_size_manual(values=c(0.5,0.5)) +
        ##geom_point( aes(shape = PROTECTED))+
        theme(axis.title =element_text(size=5),
              axis.text.y = element_text(size=5),
              axis.text.x = element_text(size=4,angle = 50, hjust = 1),
              legend.text = element_text(size=6),
              strip.text.x = element_text(size = 7),
              legend.position = "top",
              panel.grid.major = element_line(colour = "grey80", linetype = "longdash",size= 0.2 ),
              panel.grid.minor = element_line(colour="white",size=0),
              panel.background = element_rect(fill="white")) +
        facet_wrap(~geographicAreaM49_description, ncol = 3,scales = "free")
      print(pnp[[i]])

    }}}



dev.off()



