#!/usr/bin/env Rscript 

# Plot all the parts of the Front detection process

library(GSDF.WeatherMap)
library(GSDF.Front)
library(GSDF.ERAI)
library(getopt)

opt = getopt(matrix(c(
  'year',   'y', 2, "integer",
  'month',  'm', 2, "integer",
  'day',    'd', 2, "integer",
  'hour',   'h', 2, "numeric"
), byrow=TRUE, ncol=4))
if ( is.null(opt$year) )   { stop("Year not specified") }
if ( is.null(opt$month) )  { stop("Month not specified") }
if ( is.null(opt$day) )    { stop("Day not specified") }
if ( is.null(opt$hour) )   { stop("Hour not specified") }

Imagedir<-"."
if(!file.exists(Imagedir)) dir.create(Imagedir,recursive=TRUE)

scale<-55
Options<-WeatherMap.set.option(NULL)
Options<-WeatherMap.set.option(Options,'land.colour',rgb(100,100,100,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'sea.colour',rgb(150,150,150,255,
                                                       maxColorValue=255))
Options<-WeatherMap.set.option(Options,'ice.colour',Options$land.colour)
Options<-WeatherMap.set.option(Options,'lat.min',scale*-1)
Options<-WeatherMap.set.option(Options,'lat.max',scale)
Options<-WeatherMap.set.option(Options,'lon.min',scale*-1*16/9)
Options<-WeatherMap.set.option(Options,'lon.max',scale*16/9)
Options$vp.lon.min<-Options$lon.min
Options$vp.lon.max<-Options$lon.max
Options<-WeatherMap.set.option(Options,'pole.lon',110)

Options<-WeatherMap.set.option(Options,'pole.lat',179)
Options$mslp.range=50000                    # Anomaly for max contour
Options$mslp.step=500                       # Smaller -> more contours
Options$mslp.tpscale=500                    # Smaller -> contours less transparent
Options$mslp.lwd=1
 
Draw.pressure<-function(mslp,Options,colour=c(0,0,0)) {

  M<-GSDF.WeatherMap:::WeatherMap.rotate.pole(mslp,Options)
  M<-GSDF:::GSDF.pad.longitude(M) # Extras for periodic boundary conditions
  lats<-M$dimensions[[GSDF.find.dimension(M,'lat')]]$values
  longs<-M$dimensions[[GSDF.find.dimension(M,'lon')]]$values
    # Need particular data format for contourLines
  maxl<-Options$vp.lon.max+2
  if(lats[2]<lats[1] || longs[2]<longs[1] || max(longs) > maxl ) {
    if(lats[2]<lats[1]) lats<-rev(lats)
    if(longs[2]<longs[1]) longs<-rev(longs)
    longs[longs>maxl]<-longs[longs>maxl]-(maxl*2)
    longs<-sort(longs)
    M2<-M
    M2$dimensions[[GSDF.find.dimension(M,'lat')]]$values<-lats
    M2$dimensions[[GSDF.find.dimension(M,'lon')]]$values<-longs
    M<-GSDF.regrid.2d(M,M2)
  }
  lats<-M$dimensions[[GSDF.find.dimension(M,'lat')]]$values
  longs<-M$dimensions[[GSDF.find.dimension(M,'lon')]]$values
  z<-matrix(data=M$data,nrow=length(longs),ncol=length(lats))
  contour.levels<-seq(Options$mslp.base-Options$mslp.range,
                      Options$mslp.base+Options$mslp.range,
                      Options$mslp.step)
  lines<-contourLines(longs,lats,z,
                       levels=contour.levels)
  if(!is.na(lines) && length(lines)>0) {
     for(i in seq(1,length(lines))) {
         tp<-min(1,(abs(lines[[i]]$level-Options$mslp.base)/
                    Options$mslp.tpscale))
         lt<-2
         lwd<-1
         if(lines[[i]]$level<=Options$mslp.base) {
             lt<-1
             lwd<-1
         }
         gp<-gpar(col=rgb(colour[1],colour[2],colour[3],tp),
                             lwd=Options$mslp.lwd*lwd,lty=lt)
         res<-tryCatch({
             grid.xspline(x=unit(lines[[i]]$x,'native'),
                        y=unit(lines[[i]]$y,'native'),
                        shape=1,
                        gp=gp)
             }, warning = function(w) {
                 print(w)
             }, error = function(e) {
                print(e)
             }, finally = {
                # Do nothing
             })
     }
  }
}

grid.col<-rgb(1,1,0,0.5)
Draw.grid<-function(grid,Options) {

  for(lon in seq_along(grid$dimensions[[1]]$values)) {
    for(lat in seq_along(grid$dimensions[[2]]$values)) {
      if(is.na(grid$data[lon,lat,1])) next
      col<-grid.col
      gp<-gpar(col=rgb(1,1,1,0),fill=col)
      x<-grid$dimensions[[1]]$values[lon]
      dx<-(grid$dimensions[[1]]$values[2]-grid$dimensions[[1]]$values[1])*1.0
      if(x<Options$vp.lon.min-dx/2) x<-x+360
      if(x>Options$vp.lon.max+dx/2) x<-x-360
      y<-grid$dimensions[[2]]$values[lat]
      dy<-(grid$dimensions[[2]]$values[2]-grid$dimensions[[2]]$values[1])*1.0
      p.x<-c(x-dx/2,x+dx/2,x+dx/2,x-dx/2)
      p.y<-c(y-dy/2,y-dy/2,y+dy/2,y+dy/2)
      p.r<-GSDF.ll.to.rg(p.y,p.x,Options$pole.lat,Options$pole.lon,polygon=TRUE)
      if(max(p.r$lon)<Options$vp.lon.min) p.r$lon<-p.r$lon+360
      if(min(p.r$lon)>Options$vp.lon.max) p.r$lon<-p.r$lon-360
      grid.polygon(x=unit(p.r$lon,'native'),
                   y=unit(p.r$lat,'native'),
                   gp=gp)
      if(min(p.r$lon)<Options$vp.lon.min) {
        p.r$lon<-p.r$lon+360
        grid.polygon(x=unit(p.r$lon,'native'),
                     y=unit(p.r$lat,'native'),
                     gp=gp)
      }
      if(max(p.r$lon)>Options$vp.lon.max) {
         p.r$lon<-p.r$lon-360
         grid.polygon(x=unit(p.r$lon,'native'),
                      y=unit(p.r$lat,'native'),
                      gp=gp)
    
      }
    }
  }

}

Front.plot<-function(front,gp,Options) {
  frll.rotated<-GSDF.ll.to.rg(front$lat,front$lon,Options$pole.lat,Options$pole.lon)
  w<-which(frll.rotated$lon>Options$lon.max)
  if(length(w)>0) frll.rotated$lon<-frll.rotated$lon-360
  w<-which(frll.rotated$lon<Options$lon.min)
  if(length(w)>0) frll.rotated$lon<-frll.rotated$lon+360
  w<-which(abs(diff(frll.rotated$lon))>180)
  if(length(w)>0) is.na(frll.rotated$lon[w])<-TRUE
  grid.lines(x=unit(frll.rotated$lon,'native'),
               y=unit(frll.rotated$lat,'native'),
               gp=gp)
}


land<-WeatherMap.get.land(Options)
    
psm<-ERAI.get.slice.at.hour('prmsl',opt$year,opt$month,opt$day,opt$hour)
u<-ERAI.get.slice.at.hour('uwnd.10m',opt$year,opt$month,opt$day,opt$hour)
v<-ERAI.get.slice.at.hour('vwnd.10m',opt$year,opt$month,opt$day,opt$hour)

ct<-lubridate::ymd_hms(sprintf("%04d-%02d-%02d:%02d:%02d:00",
                               opt$year,opt$month,opt$day,
                               as.integer(opt$hour),
                               as.integer((opt$hour%%1)*60)))-lubridate::hours(6)
u.old<-ERAI.get.slice.at.hour('uwnd.10m',lubridate::year(ct),
                                         lubridate::month(ct),
                                         lubridate::day(ct),
                                         lubridate::hour(ct)+
                                         lubridate::minute(ct)/60)
v.old<-ERAI.get.slice.at.hour('vwnd.10m',lubridate::year(ct),
                                         lubridate::month(ct),
                                         lubridate::day(ct),
                                         lubridate::hour(ct)+
                                         lubridate::minute(ct)/60)


wcp<-Front.find.wind.change.points(u,v,u.old,v.old)
cls<-Front.cluster(wcp)
raw.fronts<-Front.cluster.to.linear(cls)
smoothed.fronts<-Front.smooth(raw.fronts)


image.name<-sprintf("%04d-%02d-%02d:%02d.%02d.png",opt$year,opt$month,opt$day,as.integer(opt$hour),
                        as.integer((opt$hour%%1)*100))

    png(image.name,
             width=1080*16/9,
             height=1080,
             bg=Options$sea.colour,
             pointsize=24,
             type='cairo')
    base.gp<-gpar(family='Helvetica',font=1,col='black')
  lon.min<-Options$lon.min
  if(!is.null(Options$vp.lon.min)) lon.min<-Options$vp.lon.min
  lon.max<-Options$lon.max
  if(!is.null(Options$vp.lon.max)) lon.max<-Options$vp.lon.max
  lat.min<-Options$lat.min
  if(!is.null(Options$vp.lat.min)) lat.min<-Options$vp.lat.min
  lat.max<-Options$lat.max
  if(!is.null(Options$vp.lat.max)) lat.max<-Options$vp.lat.max
  pushViewport(dataViewport(c(lon.min,lon.max),c(lat.min,lat.max),
		            extension=0,clip='on'))
    
      WeatherMap.draw.land(land,Options)
     Draw.grid(wcp,Options)
     Draw.pressure(psm,Options,colour=c(0,0,0))
     rfgp<-gpar(col=rgb(1,0,0,1),fill=rgb(1,0,0,1),lwd=4)
     for(i in seq_along(raw.fronts)) {
        Front.plot(raw.fronts[[i]],rfgp,Options)
     }
     sfgp<-gpar(col=rgb(0,0,1,1),fill=rgb(0,0,1,1),lwd=4)
     for(i in seq_along(smoothed.fronts)) {
        if(max(diff(smoothed.fronts[[i]]$lat),na.rm=TRUE)>10) next
        if(max(diff(smoothed.fronts[[i]]$lon),na.rm=TRUE)>10) next
        Front.plot(smoothed.fronts[[i]],sfgp,Options)
     }
  popViewport()
    

