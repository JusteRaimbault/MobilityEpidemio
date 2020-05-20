
setwd(paste0(Sys.getenv('CS_HOME'),'/MobilityEpidemio/Models/UrbanDensity'))

library(dplyr)

cases <- as.tbl(read.csv(paste0(Sys.getenv('CS_HOME'),'/MobilityEpidemio/Data/beoutbreakprepared/20200417/latestdata.csv')))


cases %>% group_by(geo_resolution) %>% summarize(count=n())

as.data.frame(cases %>% group_by(outcome) %>% summarize(count=n()))
# can not work worlwide on deaths

cases %>% group_by(country) %>% summarize(count=n())



# distrib of urban form within a region vs dynamics of epidemics?
# at the scale of urban areas, not enough localized data
# which indicators? time of peak, doubling time. when finished, total deaths
