context("RotateLatLon") 

test_that("Rotation works", {

    locus<-list(lat<-numeric(0),lon<-numeric(0),pole=numeric(0))
    for(pole.lat in seq(-90,90,10)) {
      ll<-GSDF::RotateLatLon(45,0,pole.lat,180)
      locus$lat<-c(locus$lat,ll$lat)
      locus$lon<-c(locus$lon,ll$lon)
      locus$pole<-c(locus$pole,pole.lat)  
    }
    expect_equal(locus$lat,c(seq(-45,-85,-10),seq(-85,45,10)))
    expect_equal(locus$lon,rep(0,19))
          
    locus<-list(lat<-numeric(0),lon<-numeric(0),pole=numeric(0))
    for(pole.lon in seq(-180,180,60)) {
      ll<-GSDF::RotateLatLon(45,0,90,pole.lon)
      locus$lat<-c(locus$lat,ll$lat)
      locus$lon<-c(locus$lon,ll$lon)
      locus$pole<-c(locus$pole,pole.lon)
    }
    expect_equal(locus$lat,rep(45,7))
    expect_equal(locus$lon,c(0,-60,-120,-180,120,60,0))
  }
)

# Test script for plotting winds
plot.wind<-function(lat,lon,u,v) {
  plot(NULL, type = "n", xlim=c(min(lon),max(lon)),
                         ylim=c(min(lat),max(lat)))
  arrows(lon, lat, lon+u, lat+v, length=0)
}

plot.rotated.wind<-function(pole.lat,pole.lon) {
  lon<-RollDimensions(circle,'lon','lat')
  lat<-RollDimensions(circle,'lat','lon')
  u<-rep(0,length(lon))
  v<-rep(3,length(lat))
  ll<-GSDF::RotateLatLon(lat,lon,pole.lat,pole.lon)
  print(summary(ll$lon))
  wl<-WindToPoleInternal(u,v,lat,lon,ll$lat,ll$lon,pole.lat,pole.lon)
  plot.wind(ll$lat,ll$lon,wl$u,wl$v)
}
