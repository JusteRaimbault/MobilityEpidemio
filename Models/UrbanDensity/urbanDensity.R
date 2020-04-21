
setwd(paste0(Sys.getenv('CS_HOME'),'/MobilityEpidemio/Models/UrbanDensity'))

library(dplyr)

cases <- as.tbl(read.csv(paste0(Sys.getenv('CS_HOME'),'/MobilityEpidemio/Data/beoutbreakprepared/20200417/latestdata.csv')))


cases %>% group_by(geo_resolution) %>% summarize(count=n())

as.data.frame(cases %>% group_by(outcome) %>% summarize(count=n()))
# can not work worlwide on deaths

cases %>% group_by(country) %>% summarize(count=n())

