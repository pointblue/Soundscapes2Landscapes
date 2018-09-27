library(raster)


ws<-"C:/Soundscapes2Landscapes-master/sdmTool/data/BCM/1000M"

resolution<-"_1000m.tif"


bcm_vars<-c(
  "aet",
  "cwd",
  "pet",
  "ppt",
  "tmn",
  "tmx"
)


quarters<-c(
  "q1_OctNovDec",
  "q2_JanFebMar",
  "q3_AprMayJun",
  "q4_JulAugSep"
)

for (bcm in bcm_vars) {
  for (quarter in quarters) {
    setwd(paste(ws,"/",bcm,sep=""))
    r1<-raster(paste(bcm,"_wy2013_",quarter,resolution,sep=""))
    r2<-raster(paste(bcm,"_wy2014_",quarter,resolution,sep=""))
    r3<-raster(paste(bcm,"_wy2015_",quarter,resolution,sep=""))
    rstack<-stack(r1,r2,r3)
    ra <- mean(rstack, na.rm=TRUE)
    output<-paste(bcm,"_wy2013-2015_",quarter,resolution,sep="")
    rf <- writeRaster(ra, filename=output, datatype='FLT4S', format="GTiff", overwrite=TRUE)
  }
}