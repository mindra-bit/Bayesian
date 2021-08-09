#Buku Bayesian
setwd("/Users/mindra/BUKU BAYES")

#package require 
library(maptools)
library(spdep)
library(INLA)
library(sp)
library(RColorBrewer)
library(lattice)
library(gstat)
library(raster)
require(splancs)
library(ggplot2)
library(dplyr)
library(tidyr)
library(brinla)
library(sarima)
library(extrafont) 
library(SpatialEpi)
library(rgdal)
library(maps)
library(maptools)
library(ggplot2)
library(grid)
library(rgdal)
library(colourvalues)
library(viridis) 
library(ggsn)
library(sf)
library(CARBayesdata)

#Chapter 6
  
# Call data and Map
 
#Gambar 6.1(a) Berdasarkan data simulasi

Indonesia2<-getData('GADM', country='IDN', level=2)  
Bandung2<-Indonesia2[Indonesia2$NAME_2 == "Kota Bandung",]

#Transformaxg vector data to UTM

GridCell<- 1000  # size of squares, in units of the CRS (i.e. meters for BandungUTM 1000 M)
UTM <- CRS("+proj=robin +datum=WGS84") #Transform to meters
Bandung2UTM <- spTransform(Bandung2, UTM)
Bandung2UTMsf<-st_as_sf(Bandung2UTM)

GridCell<- 1000  # size of squares, in units of the CRS (i.e. meters for BandungUTM 1000 M)

GridPolygon2UTM <- st_make_grid(Bandung2UTMsf, square = F, cellsize = c(GridCell, GridCell))%>% st_sf()  
dim(GridPolygon2UTM)
GridPolygon2UTM$Kasus<-rpois(204,50)

library(classInt)
breaks_qt <- classIntervals(c(min(GridPolygon2UTM$Kasus) - .00001, GridPolygon2UTM$Kasus), n = 7, style = "quantile")
 
GridPolygon2UTM1 <- mutate(GridPolygon2UTM, Kasus_DB = cut(Kasus, breaks_qt$brks)) 

ggplot(GridPolygon2UTM1) + 
    geom_sf(aes(fill=Kasus_DB)) +
    scale_fill_brewer(palette = "OrRd") +
    theme_bw()+ ylab("Latitude")+xlab("Longitude")+
    labs(fill = "Kasus DBD")  
    
#Gambar 6.1(b)

Indonesia3<-getData('GADM', country='IDN', level=3)  
Bandung3<-Indonesia3[Indonesia3$NAME_2 == "Kota Bandung",]

#Transformaxg vector data to UTM

UTM <- CRS("+proj=robin +datum=WGS84") #Transform to meters
Bandung3UTM <- spTransform(Bandung3, UTM)
Bandung3UTMsf<-st_as_sf(Bandung3UTM)
Bandung3UTMsf$Kasus<-rpois(30,50)
 
library(classInt)
breaks_qt <- classIntervals(c(min(Bandung3UTMsf$Kasus) - .00001, Bandung3UTMsf$Kasus), n = 7, style = "quantile")
 
Bandung3UTMsf1<- mutate(Bandung3UTMsf, Kasus_DB = cut(Kasus, breaks_qt$brks)) 
Bandung3UTMsf2<-fortify(Bandung3UTMsf1)
ggplot(Bandung3UTMsf1) + 
    geom_sf(aes(fill=Kasus_DB)) +
    scale_fill_brewer(palette = "OrRd") +
    theme_bw()+ ylab("Latitude")+xlab("Longitude")+
    labs(fill = "Kasus DBD")  
     
####

#Gambar 6.1(c) #Simulation
Bandung2UTMsf<-st_as_sf(Bandung2UTM)
Site<-as.data.frame(coordinates(Bandung3UTM))
colnames(Site)<-c("x","y")
Site$Kasus<-rpois(30,50)    
    
 ggplot(Bandung2UTMsf) + 
    geom_sf() +    geom_point(data = Site, aes(x = x, y = y, size=Kasus),  
        shape = 21, fill = "red") + 
    theme_bw()+ ylab("Latitude")+xlab("Longitude")+
    labs(fill = "Kasus DBD")  
    
     












  
  
  
  
  
###Find GridPoint
GridPolygon<-as(GridPolygonUTM, 'Spatial')
CoordUTM<-coordinates(GridPolygon)
Coord<- coordinates(spTransform(GridPolygon, CRS("+proj=longlat +datum=WGS84")))

###Plot
plot(GridPolygonUTM, col = 'white', axes=T)
plot(st_geometry(BandungUTMSf), add = T)  
points(CoordUTM,pch=19,col="gray50",cex=0.5)
  
####
GridPolygon1<-spTransform(GridPolygon, CRS("+proj=longlat +datum=WGS84"))
plot(GridPolygon1, col = 'white', axes=T)
plot(Bandung, add=T)
points(Coord,pch=19,col="gray50",cex=0.5)














 
 
 
 
 
 
 
 
 
ID<-c(1:30) 
Bdg$ID<-c(1:30) #Memberi id pada map kota bandung 1 - 30


Wq <- poly2nb(Bdg, row.names=ID, queen=TRUE) #Mendapatkan W 
WqM <- nb2mat(Wq, style='B', zero.policy = TRUE) #menyajikan dalam bentuk matrix biner "B"
W<-as.matrix(WqM) 
Wls<-nb2listw(Wq)
Ws <- as(as_dgRMatrix_listw(Wls), "CsparseMatrix")
 
 
