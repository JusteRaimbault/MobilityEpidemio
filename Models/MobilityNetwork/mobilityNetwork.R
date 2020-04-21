
setwd(paste0(Sys.getenv('CS_HOME'),'/MobilityEpidemio/Models/MobilityNetwork'))

library(dplyr)
library(sf)
library(ggplot2)
library(igraph)
library(lubridate)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))


raw <- as.tbl(read.csv(paste0(Sys.getenv('CS_HOME'),'/MobilityEpidemio/Data/FB/France_mobilities_tiles_joined_4march_to_16march2020.csv')))
resdir = paste0(Sys.getenv('CS_HOME'),'/MobilityEpidemio/Results/MobilityNetwork/FR_4-16-03/');dir.create(resdir)
days <- c(paste0('202003',paste0('0',4:9)),paste0('202003',10:16))

# number of cells per region
g = ggplot(raw %>% group_by(Starting.Region.Name) %>% summarize(locs = length(unique(Starting.Location))),
           aes(x=locs))
g+geom_histogram()
g = ggplot(raw %>% group_by(Starting.Region.Name) %>% summarize(count=n()),aes(x=count))
g+geom_histogram()

# construct id of tiles from geometry
spraw = st_as_sf(raw,wkt = 'Geometry')
st_crs(spraw)<-4326
rawcoords = unlist(spraw[,"Geometry"])

spraw$oid = paste0(rawcoords[seq(1,length(rawcoords),4)],rawcoords[seq(3,length(rawcoords),4)])
spraw$did = paste0(rawcoords[seq(2,length(rawcoords),4)],rawcoords[seq(4,length(rawcoords),4)])

# add distance
coords = as.data.frame(st_coordinates(spraw))
wktpoints = paste0("POINT(",coords$X," ",coords$Y,")")
opoints = st_as_sf(data.frame(geometry=wktpoints[seq(1,length(wktpoints),2)]),wkt='geometry')
st_crs(opoints)<-4326
dpoints = st_as_sf(data.frame(geometry=wktpoints[seq(2,length(wktpoints),2)]),wkt='geometry')
st_crs(dpoints)<-4326
dist = st_distance(opoints,dpoints,by_element = T,tolerance=100)

spraw$distance = as.numeric(dist)/1000


# add regions

regions <- st_transform(st_read(paste0(Sys.getenv('CS_HOME'),'/Data/Countries/France/nouvelles_regions'),'nouvelles_regions'),st_crs(spraw))
spraw$oregion = as.character(st_join(opoints,y = regions, join=st_within)[['NOM_REGION']])
spraw$dregion = as.character(st_join(dpoints,y = regions, join=st_within)[['NOM_REGION']])
# length(which(is.na(spraw$oregion))) # = 18202 -> ?
# length(which(is.na(spraw$dregion))) # = 18205
# plot(opoints$geometry) # -> not only France, but square window with neighbor countries -> filter NA
# some international commute: Plymouth in opoints



# construct mobility networks

# mobility networks on first and last day
spraw$initflow = spraw$X20200304_0000 + spraw$X20200304_0800 + spraw$X20200304_1600
spraw$finalflow = spraw$X20200316_0000 + spraw$X20200316_0800 + spraw$X20200316_1600

ginit = graph_from_data_frame(spraw[,c("oid","did","initflow")])
E(ginit)$weight = E(ginit)$initflow
gfinal = graph_from_data_frame(spraw[,c("oid","did","finalflow")])
E(gfinal)$weight = E(gfinal)$finalflow

alldays = list()
for(currentday in days){
  spraw$currentflow = spraw[[paste0('X',currentday,'_0000')]] + spraw[[paste0('X',currentday,'_0800')]] + spraw[[paste0('X',currentday,'_1600')]]
  g = graph_from_data_frame(spraw[,c("oid","did","currentflow")])
  E(g)$weight = E(g)$currentflow
  E(g)$region = ifelse(spraw$oregion==spraw$dregion,spraw$oregion,'A-Interregion')
  E(g)$region[is.na(E(g)$region)]='Z-International'
  alldays[[currentday]]=g
}

## total flow
sum(E(ginit)$weight)
sum(E(gfinal)$weight)
# for FR: 2916430 ; 1870282

ggplot(data.frame(
        totalflow = sapply(alldays,function(g){sum(E(g)$weight)}),
        day = ymd(days)
      ),
      aes(x=day,y=totalflow)
)+geom_point()+geom_line()+xlab('Day')+ylab('Total flow')+stdtheme
ggsave(file=paste0(resdir,'allflows.png'),width=15,height=10,units='cm')

regflows = lapply(alldays,function(g){
  as.tbl(data.frame(flow = E(g)$weight, region=as.character(E(g)$region)))%>% group_by(region)%>%summarize(flow=sum(flow))
})

ggplot(data.frame(
  totalflow = unlist(lapply(regflows,function(d){d$flow})),
  region =  unlist(lapply(regflows,function(d){d$region})),
  day = unlist(lapply(names(regflows),function(d){rep(ymd(d),nrow(regflows[[1]]))}))
),
aes(x=day,y=totalflow,color=region,group=region)
)+geom_point()+geom_line()+xlab('Day')+ylab('Total flow')+stdtheme
ggsave(file=paste0(resdir,'allflows_regions.png'),width=25,height=20,units='cm')


# by region




### weighted degree distribution

ggplot(data.frame(strength = strength(ginit)),aes(x=strength))+geom_histogram()+scale_x_log10()+xlab('Weighted degree')+ylab('Count')+stdtheme
ggsave(filename = paste0(resdir,'strength_initnw.png'),width=15,height=10,units='cm')

ggplot(data.frame(strength = strength(gfinal)),aes(x=strength))+geom_histogram()+scale_x_log10()+xlab('Weighted degree')+ylab('Count')+stdtheme
ggsave(filename = paste0(resdir,'strength_finalnw.png'),width=15,height=10,units='cm')


### distribution of distances travelled
sum(spraw$distance*spraw$initflow/sum(spraw$initflow))
sum(spraw$distance*spraw$finalflow/sum(spraw$finalflow))
# FR: 11.24827, 10.78906












