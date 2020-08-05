#____________________________________#
# Code written by Felipe Suarez    #
# Version :  01-04-2019            #
#_____________________________________#

#clear workspace
rm(list=ls())

library(SpaDES)
library(raster)
library(spatial.tools)


#set working directory
setwd("~/human_footprint_carbon")

#here I am starting with layers that have been prepared and are in the same projection. 


#load carbon map
carbon <- raster("./Data/AGC_2010_1k.tif")
# load human footprint layer
human.footprint<-raster("./Data/hf_2013_proj.tif")


#Divide each raster in multiple tiles so I could loop and speed up the process
#this my take a few minutes.
#R will crash if you do not split the rasters

#split carbon raster
system.time(splitcarbon.project<-splitRaster(carbon,10,10))
#split human footprint raster
splithf<-splitRaster(human.footprint,10,10)

#get carbon values
points.sample<-list()
pb <- txtProgressBar(min = 0, max = length(splitcarbon.project), style = 3) #To check the progress
for (i in 1:length(splitcarbon.project))
{
  setTxtProgressBar(pb, i)  
  if (length(which(!is.na(getValues(splitcarbon.project[[i]]))))>0) #exclude tiles with only NA values (e.g. tiles in the middle of the ocean)
  {
    #myobject<-splitcarbon.project[[i]]
    df_raster<-splitcarbon.project[[i]]
    xy = xyFromCell(df_raster,1:prod(dim(df_raster)))#get coordinates
    rastervalues<- getValues(df_raster)#get carbon values
    rastercarbon<-data.frame(xy,rastervalues)#create data frame containing coordinates and carbon values
    rasterna<-subset(rastercarbon,!is.na(rastervalues))# exclude cells with NAs
    points.sample[[i]]<-subset(rastercarbon,!is.na(rastervalues))#for working with the whole dataset
    #points.sample[[i]]<-rasterna[sample(nrow(rasterna), round(nrow(rasterna)*0.3)), ]#activate this if you only want a random sample
  }else{
    points.sample[[i]]<-data.frame(x=as.numeric(),y=as.numeric(),rastervalues=as.numeric())#for tiles that only have NA values
  }
}

perc_overlap<-list()
#extract values from human foot print following the same process.
timestamp()
varhf<-list()
for (i in 1:length(splithf))
{
  setTxtProgressBar(pb, i) 
  if (length(which(!is.na(getValues(splitcarbon.project[[i]]))))>0)
  {
    hfvalues<-raster::extract(splithf[[i]],points.sample[[i]][c("x","y")])
    varhf[[i]]<-data.frame(points.sample[[i]],hfvalues) #merge carbon values and human footprint values
    #varhf[[i]]$variable<-names(splitraster[[i]])
    colnames(varhf[[i]])[4]<-names(splithf[[i]])
  }else{
    varhf[[i]]<-data.frame(x=as.numeric(),y=as.numeric(),rastervalues=as.numeric(),hfvalues=as.numeric())
  }
  mydf<-varhf[[i]]
  mydfPAS<-subset(mydf,mydf$hf_2013_proj>4)
  myspPAS<-subset(mydfPAS,mydfPAS$rastervalues>1)
  perc_overlap[[i]]<-nrow(myspPAS)/nrow(mydfPAS)
}
timestamp()