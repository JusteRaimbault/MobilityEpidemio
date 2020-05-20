
setwd(paste0(Sys.getenv('CS_HOME'),'/MobilityEpidemio/Models/DataConsolidation'))

library(dplyr)


# global

global <- as.tbl(read.csv(
  url('https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv')
))


# France 
france <- as.tbl(read.csv('../Data/France/donnees-hospitalieres-covid19-2020-04-21-19h00.csv'))


# Italy - province cases only
italy <- as.tbl(read.csv(url('https://github.com/pcm-dpc/COVID-19/raw/master/dati-regioni/dpc-covid19-ita-regioni-latest.csv')))

# US: county level
us <- as.tbl(read.csv(url('https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv')))




