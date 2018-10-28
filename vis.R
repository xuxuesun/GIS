curaddr<-as.data.frame(read.csv('xxx'))
datlen<-length(curaddr$Address)
newform<-paste0(curaddr$Address,rep(', ',datlen),
                curaddr$City,rep(', ',datlen),curaddr$State,rep(' ',datlen),
                curaddr$Zip)
curaddr<-cbind(curaddr,newform)

write.csv(curaddr,'xxx',row.names = FALSE)

curaddr<-as.data.frame(read.csv('xx',stringsAsFactors = FALSE))
algeoc<-as.numeric(geocode(curaddr[1,]$newform,source = c('dsk'),force=TRUE,override_limit=TRUE))
failreq<-c(0,' ')
for(i in 2:length(curaddr[,1]))
{
  tmpgeo<-as.numeric(geocode(curaddr[i,]$newform,source = c('dsk'),force=TRUE,override_limit=TRUE))
  if(!is.na(tmpgeo[1])){
    algeoc<-rbind(algeoc,tmpgeo)
  }else{
    failreq<-rbind(failreq,cbind(curaddr[i,]$AHCANum,curaddr[i,]$newform))
  }
}

tmpgeoc<-matrix(0,nrow=length(curaddr[,1]),ncol=2)
tmpgeoc[!(curaddr$AHCANum %in% failreq[,1]),]<-algeoc
curaddr<-cbind(curaddr,tmpgeoc)
colnames(curaddr)<-c('xxx','Address','City','State','Zip','County','xxx','longitude','latitude')
write.csv(curaddr,'xx',row.names = FALSE)




################ visualize 
library(ggmap)
library(mapproj)
library(ggplot2)
library(ggsn)
dat1<-read.csv('xxx',stringsAsFactors = FALSE)
dat1<-dat1[-(which(dat1$latitude>30.6 | abs(dat1$longitude)>86)),]
evacdat<-read.csv('xx')
clastmp<-rep(1,length(dat1[,1]))
clastmp[dat1$AHCANum %in% evacdat[evacdat$count!=0,]$AHCA.Number]<-2
geodat<-cbind(dat1$longitude,dat1$latitude,clastmp)
geodat<-cbind(seq(length(dat1[,1])),geodat)
colnames(geodat)<-c('ID','lon','lat','class')
geodat<-data.frame(geodat)

write.csv(geodat,'NHevacplot.csv',row.names = FALSE)


hurpat_orig<-read.delim('xx',header = TRUE,sep='\t',stringsAsFactors = FALSE)
hurdatlen<-length(hurpat_orig[,1])
hurpath<-matrix(0,nrow=17,ncol=3)
hurpath<-as.data.frame(hurpath)
colnames(hurpath)<-c('lon','lat','stat')
for(i in 36:hurdatlen)
{
  tmpstr<-unlist(strsplit(hurpat_orig[i,],split=' +'))
  tmpstr<-tmpstr[tmpstr!='']
  hurpath[(i-35),1]<-as.numeric(tmpstr[3])
  hurpath[(i-35),2]<-as.numeric(tmpstr[2])
  hurpath[(i-35),3]<-as.numeric(substr(tmpstr[7],2,2))
}

library(sp)
library(rgdal)
library(maptools)
ogrListLayers('AL112017_036adv_TRACK.kml')
tmpkml<-readOGR('AL112017_036adv_TRACK.kml',layer='Forecast Track',require_geomType='wkbPoint')#Forecast Track
trackpath<-tmpkml@coords[,1:2]
plot(trackpath)
#tkml <- getKMLcoordinates(kmlfile="AL112017_036adv_TRACK.kml", ignoreAltitude=T)

trackpath<-as.data.frame(trackpath)
colnames(trackpath)<-c('lon','lat')

centgeo<-geocode("Lakeland",override_limit = TRUE,force=TRUE,source='dsk')
#localmap<-ggmap(get_googlemap(center=as.numeric(centgeo),zoom=7,maptype ='roadmap',color='color',force=TRUE), extent = "normal")
#hybrid,roadmap
curmap<-get_map(location=c(lon =-83,lat=27.6),zoom=7,source='google',force = TRUE,maptype ='terrain')
localmap<-ggmap(curmap,legend='right', extent = "normal")+
  theme(legend.position = c(0.2, 0.2),legend.key = element_rect(fill = "white",colour = "white"),legend.background = element_rect(fill = NA,colour = NA),
        legend.text = element_text(colour="black"),legend.title = element_text(colour="black"))
#print(localmap+geom_point(aes(x = lon, y = lat), data = geodat, alpha = .5)+scale_color_manual(values=c('1'='red','2'='blue')))

cols    <- c( "c1" = "magenta", "c2" = "yellow")
shapes  <- c("s1" = 16, "s2" = 1)
sizes <-c("z1"=3,"z2"=3)

tmpplot<- localmap + geom_point(aes(x=lon,y=lat, color = "c2", shape = "s2",size="z2"),alpha =1,data = geodat[geodat$class==1,])
tmpplot <-  tmpplot + geom_point(aes(x=lon,y=lat,  color = "c1", shape = "s1",size="z1"),alpha = 1,data = geodat[geodat$class==2,])
tmpplot <- tmpplot + labs( x = "Longitude", y = "Latitude" )
tmpplot <- tmpplot + scale_color_manual(name = "Evacuation Case", 
                              breaks = c("c1", "c2"), 
                              values = cols,
                              labels = c("Evacuate", "Non-Evacuate"))#+
tmpplot <- tmpplot + scale_shape_manual(name = "Evacuation Case", 
                              breaks = c("s1", "s2"),
                              values = shapes,
                              labels = c("Evacuate", "Non-Evacuate"))
tmpplot <- tmpplot + scale_size_manual(name = "Evacuation Case", 
                                        breaks = c("z1", "z2"),
                                        values = sizes,
                                        labels = c("Evacuate", "Non-Evacuate"))


source("https://bioconductor.org/biocLite.R")
biocLite("EBImage")
library(gridExtra)
               
               
bb <- attr(curmap, "bb")
bb2 <- data.frame(long = unlist(bb[c(2, 4)]), lat = unlist(bb[c(1,3)]))

ggplot()+north(bb2, symbol = 16, scale = 0.15) +scalebar(data=bb2, dd2km=TRUE,dist = 5,model  = "WGS84")


################### for ARCGIS
#################################### NH
dat2<-read.csv('xxx',stringsAsFactors = FALSE)
evacdat2<-read.csv('xxx')
clastmp<-rep(1,length(dat2[,1]))
clastmp[dat2$AHCANum %in% evacdat2[evacdat2$count1!=0,]$AHCA.Number]<-2

nevacind<-which(clastmp==1)
geodat<-cbind(dat2[nevacind,]$Easting,dat2[nevacind,]$Northing)
colnames(geodat)<-c('East','North')
geodat<-data.frame(geodat)
write.csv(geodat,'xxx',row.names = FALSE)

evacind<-which(clastmp==2)
geodat<-cbind(dat2[evacind,]$Easting,dat2[evacind,]$Northing)
colnames(geodat)<-c('East','North')
geodat<-data.frame(geodat)
write.csv(geodat,'xxx',row.names = FALSE)


clastmp<-rep(1,length(dat2[,1]))
clastmp[dat2$AHCANum %in% evacdat2[evacdat2$count2!=0,]$AHCA.Number]<-2

nevacind<-which(clastmp==1)
geodat<-cbind(dat2[nevacind,]$Easting,dat2[nevacind,]$Northing)
colnames(geodat)<-c('East','North')
geodat<-data.frame(geodat)
write.csv(geodat,'xxxx',row.names = FALSE)

evacind<-which(clastmp==2)
geodat<-cbind(dat2[evacind,]$Easting,dat2[evacind,]$Northing)
colnames(geodat)<-c('East','North')
geodat<-data.frame(geodat)
write.csv(geodat,'xxxxx',row.names = FALSE)


clastmp<-rep(1,length(dat2[,1]))
clastmp[dat2$AHCANum %in% evacdat2[evacdat2$count3!=0,]$AHCA.Number]<-2

nevacind<-which(clastmp==1)
geodat<-cbind(dat2[nevacind,]$Easting,dat2[nevacind,]$Northing)
colnames(geodat)<-c('East','North')
geodat<-data.frame(geodat)
write.csv(geodat,'xxxx',row.names = FALSE)

evacind<-which(clastmp==2)
geodat<-cbind(dat2[evacind,]$Easting,dat2[evacind,]$Northing)
colnames(geodat)<-c('East','North')
geodat<-data.frame(geodat)
write.csv(geodat,'xxxx',row.names = FALSE)


############################ AL
dat2<-read.csv('xxxx',stringsAsFactors = FALSE)
evacdat2<-read.csv('xxxxx')
clastmp<-rep(1,length(dat2[,1]))
clastmp[dat2$AHCANum %in% evacdat2[evacdat2$count1!=0,]$AHCA.Number]<-2

nevacind<-which(clastmp==1)
geodat<-cbind(dat2[nevacind,]$Easting,dat2[nevacind,]$Northing)
colnames(geodat)<-c('East','North')
geodat<-data.frame(geodat)
write.csv(geodat,'xxxx',row.names = FALSE)

evacind<-which(clastmp==2)
geodat<-cbind(dat2[evacind,]$Easting,dat2[evacind,]$Northing)
colnames(geodat)<-c('East','North')
geodat<-data.frame(geodat)
write.csv(geodat,'xxx',row.names = FALSE)


clastmp<-rep(1,length(dat2[,1]))
clastmp[dat2$AHCANum %in% evacdat2[evacdat2$count2!=0,]$AHCA.Number]<-2

nevacind<-which(clastmp==1)
geodat<-cbind(dat2[nevacind,]$Easting,dat2[nevacind,]$Northing)
colnames(geodat)<-c('East','North')
geodat<-data.frame(geodat)
write.csv(geodat,'xxx',row.names = FALSE)

evacind<-which(clastmp==2)
geodat<-cbind(dat2[evacind,]$Easting,dat2[evacind,]$Northing)
colnames(geodat)<-c('East','North')
geodat<-data.frame(geodat)
write.csv(geodat,'xxx',row.names = FALSE)


clastmp<-rep(1,length(dat2[,1]))
clastmp[dat2$AHCANum %in% evacdat2[evacdat2$count3!=0,]$AHCA.Number]<-2

nevacind<-which(clastmp==1)
geodat<-cbind(dat2[nevacind,]$Easting,dat2[nevacind,]$Northing)
colnames(geodat)<-c('East','North')
geodat<-data.frame(geodat)
write.csv(geodat,'xxx',row.names = FALSE)

evacind<-which(clastmp==2)
geodat<-cbind(dat2[evacind,]$Easting,dat2[evacind,]$Northing)
colnames(geodat)<-c('East','North')
geodat<-data.frame(geodat)
write.csv(geodat,'xxx',row.names = FALSE)
