library(GSDF.TWCR)
library(GSDF.WeatherMap)
library(grid)

source('streamlines.R')

Options<-WeatherMap.set.option()
Options$wind.vector.density<-1
Options$wind.vector.points<-3
Options$wind.vector.padding<-3
Options$wind.vector.max.number<-10000
Options$wind.vector.seed.width<-10
Options$wind.vector.seed.decimate<-10
Options$wind.vector.scale<-2

u<-TWCR.get.slice.at.hour('uwnd.10m',1879,3,12,6,version='3.5.1')
v<-TWCR.get.slice.at.hour('vwnd.10m',1879,3,12,6,version='3.5.1')

s<-WeatherMap.allocate.streamlines(u,v,Options)


s.draw<-function(s) {
  png(filename='tst.png',width=1000,height=500)
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
		            extension=0,gp=base.gp))
  for(i in seq_along(s$status)) {     
      gp<-gpar(col=rgb(.2,.2,.2,1),fill=rgb(.2,.2,.2,1),
               lwd=Options$wind.vector.lwd)
          grid.xspline(x=unit(na.omit(s[['x']][i,]),'native'),
                       y=unit(na.omit(s[['y']][i,]),'native'),
                       shape=1,
                       arrow=Options$wind.vector.arrow,
                       gp=gp)
    }
  upViewport()
  dev.off()
}
