
setwd(paste0(Sys.getenv('CS_HOME'),'/MobilityEpidemio/Models/MobilityNetwork'))

library(dplyr)
library(sf)
library(ggplot2)
library(igraph)
library(lubridate)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))

#area = 'France'
area='Delhi'

# France
if(area=='France'){
  raw <- as.tbl(read.csv(paste0(Sys.getenv('CS_HOME'),'/MobilityEpidemio/Data/FB/France_mobilities_tiles_joined_4march_to_16march2020.csv')))
  resdir = paste0(Sys.getenv('CS_HOME'),'/MobilityEpidemio/Results/MobilityNetwork/FR_4-16-03/');dir.create(resdir)
  days <- c(paste0('202003',paste0('0',4:9)),paste0('202003',10:16))
  hours <- c('_0000','_0800','_1600')
}
  
#Dehli
if(area=='Delhi'){
  raw <- as.tbl(read.csv(paste0(Sys.getenv('CS_HOME'),'/MobilityEpidemio/Data/FB/Delhi_Tiles_till_2603.csv'),sep=";"))
  resdir = paste0(Sys.getenv('CS_HOME'),'/MobilityEpidemio/Results/MobilityNetwork/Dehli_0225-0326/');dir.create(resdir)
  days <- paste0('2020.',c(paste0('02.',25:29),paste0('03.0',1:9),paste0('03.',10:26)))
  hours <- c('.0530','.1330','.2130')
}

# number of cells per region
#g = ggplot(raw %>% group_by(Starting.Region.Name) %>% summarize(locs = length(unique(Starting.Location))),
#           aes(x=locs))
#g+geom_histogram()
#g = ggplot(raw %>% group_by(Starting.Region.Name) %>% summarize(count=n()),aes(x=count))
#g+geom_histogram()

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

if(area=='France'){
  regions <- st_transform(st_read(paste0(Sys.getenv('CS_HOME'),'/Data/Countries/France/nouvelles_regions'),'nouvelles_regions'),st_crs(spraw))
  names(regions)[1]<-'name'
}
if(area=='Delhi'){
  regions <- st_transform(st_read('data/Delhi/','Districts'),st_crs(spraw))
  names(regions)[2]<-'name'
}

spraw$oregion = as.character(st_join(opoints,y = regions, join=st_within)[['name']])
spraw$dregion = as.character(st_join(dpoints,y = regions, join=st_within)[['name']])
# length(which(is.na(spraw$oregion))) # = 18202 -> ?
# length(which(is.na(spraw$dregion))) # = 18205
# plot(opoints$geometry) # -> not only France, but square window with neighbor countries -> filter NA
# some international commute: Plymouth in opoints

spraw = spraw[!is.na(spraw$oregion)&!is.na(spraw$dregion),]


# construct mobility networks

# mobility networks on first and last day
spraw$initflow = spraw[[paste0('X',days[1],hours[1])]] + spraw[[paste0('X',days[1],hours[2])]] + spraw[[paste0('X',days[1],hours[3])]]
spraw$finalflow = spraw[[paste0('X',days[length(days)],hours[1])]] + spraw[[paste0('X',days[length(days)],hours[2])]] + spraw[[paste0('X',days[length(days)],hours[3])]]

ginit = graph_from_data_frame(spraw[,c("oid","did","initflow")])
E(ginit)$weight = E(ginit)$initflow
gfinal = graph_from_data_frame(spraw[,c("oid","did","finalflow")])
E(gfinal)$weight = E(gfinal)$finalflow

alldays = list()
for(currentday in days){
  spraw$currentflow = spraw[[paste0('X',currentday,hours[1])]] + spraw[[paste0('X',currentday,hours[2])]] + spraw[[paste0('X',currentday,hours[3])]]
  g = graph_from_data_frame(spraw[,c("oid","did","currentflow","distance")])
  E(g)$weight = E(g)$currentflow
  E(g)$region = ifelse(spraw$oregion==spraw$dregion,spraw$oregion,'Interregion')
  #E(g)$region[is.na(E(g)$region)]='Z-International'
  alldays[[currentday]]=g
}

## total flow
sum(E(ginit)$weight)
sum(E(gfinal)$weight)
# for FR: 1767691 ; 1080709
# for Delhi: 764377 ; 101424

ggplot(data.frame(
        totalflow = sapply(alldays,function(g){sum(E(g)$weight)}),
        day = ymd(days)
      ),
      aes(x=day,y=totalflow)
)+geom_point()+geom_line()+xlab('Day')+ylab('Total flow')+stdtheme
ggsave(file=paste0(resdir,'allflows.png'),width=15,height=10,units='cm')

regflows = lapply(alldays,function(g){
  as.tbl(data.frame(flow = E(g)$weight,
                    region=as.character(E(g)$region),
                    distance=E(g)$distance
                    ))%>%
    group_by(region)%>%summarize(avgdist=sum(flow*distance)/sum(flow),flow=sum(flow))
})

# by region
ggplot(data.frame(
  totalflow = unlist(lapply(regflows,function(d){d$flow})),
  region =  unlist(lapply(regflows,function(d){d$region})),
  day = ymd(unlist(lapply(names(regflows),function(d){rep(d,nrow(regflows[[1]]))})))
),
aes(x=day,y=totalflow,color=region,group=region)
)+geom_point()+geom_line()+xlab('Day')+ylab('Total flow')+stdtheme
ggsave(file=paste0(resdir,'allflows_regions.png'),width=35,height=20,units='cm')





### weighted degree distribution

ggplot(data.frame(strength = strength(ginit)),aes(x=strength))+geom_histogram()+scale_x_log10()+xlab('Weighted degree')+ylab('Count')+stdtheme
ggsave(filename = paste0(resdir,'strength_initnw.png'),width=15,height=10,units='cm')

ggplot(data.frame(strength = strength(gfinal)),aes(x=strength))+geom_histogram()+scale_x_log10()+xlab('Weighted degree')+ylab('Count')+stdtheme
ggsave(filename = paste0(resdir,'strength_finalnw.png'),width=15,height=10,units='cm')


### distribution of distances travelled
sum(spraw$distance*spraw$initflow/sum(spraw$initflow))
sum(spraw$distance*spraw$finalflow/sum(spraw$finalflow))
# FR: 11.24827, 10.78906
# Delhi: 5.423425, 2.504622

# average distance in time
ggplot(data.frame(avgdist = sapply(alldays,function(g){sum(E(g)$distance*E(g)$weight/sum(E(g)$weight))}),day = ymd(days)),
    aes(x=day,y=avgdist))+geom_point()+geom_line()+xlab('Day')+ylab('Average travelled distance')+stdtheme
ggsave(file=paste0(resdir,'avgdist.png'),width=20,height=15,units='cm')

# avg travelled dist by region
ddist = data.frame(
  avgdist = unlist(lapply(regflows,function(d){d$avgdist})),
  region =  unlist(lapply(regflows,function(d){d$region})),
  day = ymd(unlist(lapply(names(regflows),function(d){rep(d,nrow(regflows[[1]]))})))
)
ggplot(ddist,aes(x=day,y=avgdist,color=region,group=region))+geom_point()+geom_line()+xlab('Day')+ylab('Average travelled distance')+stdtheme
ggsave(file=paste0(resdir,'avgdist_regions.png'),width=35,height=20,units='cm')

ggplot(ddist[ddist$region!='Interregion',],aes(x=day,y=avgdist,color=region,group=region))+geom_point()+geom_line()+xlab('Day')+ylab('Average travelled distance')+stdtheme
ggsave(file=paste0(resdir,'avgdist_regions_without_inter.png'),width=35,height=20,units='cm')


###
# structure of graphs:
# - degree distrib
# - modularity ; modularity of FUAs?

# rough hierarchy of weighted degree distrib
strengthHierarchy <- function(g){
  x = strength(g)
  x = x[x>0]
  return(lm(data = data.frame(rank=log(1:length(x)),strength = sort(log(x),decreasing = T)))$coefficients[2])
}

ggplot(data.frame(hierarchy = sapply(alldays,strengthHierarchy),day = ymd(days)),aes(x=day,y=hierarchy))+
  geom_point()+geom_line()+xlab('Day')+ylab('Strength hierarchy')+stdtheme
ggsave(file=paste0(resdir,'strengthhierarchy.png'),width=15,height=10,units='cm')

# Modularity

clusterModularity <- function(g,clusterFunction){
  A = as_adjacency_matrix(g,attr = "weight")
  M = (A+t(A))/2
  gundir= graph_from_adjacency_matrix(M,mode = "undirected",weighted = "weight")
  clusters =clusterFunction(gundir)
  return(modularity(gundir, clusters$membership))
}
clusterModularity(ginit,cluster_fast_greedy)
clusterModularity(gfinal,cluster_fast_greedy)
# Q: "assortativity" as normalized modularity?

ggplot(data.frame(modularity = sapply(alldays,function(g){clusterModularity(g,cluster_fast_greedy)}),day = ymd(days)),aes(x=day,y=modularity))+
  geom_point()+geom_line()+xlab('Day')+ylab('Modularity')+stdtheme
ggsave(file=paste0(resdir,'modularity.png'),width=15,height=10,units='cm')


# fit power laws
library(poweRlaw)

fitDistrPowerLaw<-function(x,xlab='x',ylab='CDF',file='fitDistrPowerLaw.png'){
  degpowerlaw = conpl$new(x)
  est = estimate_xmin(degpowerlaw,xmax = max(x))
  degpowerlaw$setXmin(est)
  png(file,width=15,height=15,units='cm',res=300)
  plot(degpowerlaw,xlab=xlab,ylab=ylab);lines(degpowerlaw, col=2, lwd=2)
  degln = conlnorm$new(x)
  est = estimate_xmin(degln)
  degln$setXmin(est)
  lines(degln, col=3, lwd=2)
  text(x=min(x),y=0.007,adj=c(0,0),labels = paste0('Log-normal: mu=',round(degln$pars[1],digits=2),', sigma=',round(degln$pars[2],digits=2),', xmin=',round(degln$xmin,digits=2)),cex=0.6)
  text(x=min(x),y=0.005,adj=c(0,0),labels = paste0('Power law: alpha=',round(degpowerlaw$pars[1],digits=2),', xmin=',round(degpowerlaw$xmin,digits=2)),cex=0.6)
  dev.off()
  return(list(powerlaw=degpowerlaw,ln=degln))
}

fitdeginit = fitDistrPowerLaw(strength(ginit)[strength(ginit)>0],'Weighted degree',file=paste0(resdir,'degreeDistrib_ginit.png'))
fitdegfinal = fitDistrPowerLaw(strength(gfinal)[strength(gfinal)>0],'Weighted degree',file=paste0(resdir,'degreeDistrib_gfinal.png'))














