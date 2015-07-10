#' Rotate lats and lons
#'
#' From standard pole to same positions in a rotated pole.
#' 
#' Convert latitudes and longitudes to the equivalents
#'  with a rotated pole. (Formulae from UMDP S1).
#'
#' @export
#' @param lat vector of latitudes in standard pole (degrees).
#' @param lon vector of longitudes in standard pole (degrees).
#' @param pole.lat latitude of pole to rotate to (degrees).
#' @param pole.lon longitude of pole to rotate to (degrees).
#' @param polygon are lats and longs sets of polygon vertices 
#'  (probably from maps)?
#' @return list with components 'lat' and 'lon' - vectors of
#'   rotated lat and lon (in degrees).
RotateLatLon<-function(lat,lon,pole.lat,pole.lon,polygon=FALSE) {

   if(pole.lat==90 && pole.lon==180) {
     return(list(lat=lat,lon=lon))
   }
   while(pole.lon>180) pole.lon<-pole.lon-360
   l0<-pole.lon+180
   lon<-lon-l0
   w<-which(!is.na(lon) & lon>=180)
   lon[w]<-lon[w]-360
   w<-which(!is.na(lon) & lon< -180)
   lon[w]<-lon[w]+360

   dtr<-pi/180
   sin.pole.lat<-sin(pole.lat*dtr)
   cos.pole.lat<-cos(pole.lat*dtr)
   if(pole.lat<0) {
      sin.pole.lat<- -sin.pole.lat
      cos.pole.lat<- -cos.pole.lat
   }
   
   lat.rotated<-asin(pmax(-1,pmin(1,-cos.pole.lat*
                                   cos(lon*dtr)*
                                   cos(lat*dtr)+
                                   sin.pole.lat*
                                   sin(lat*dtr))))
                         
   lon.rotated<-lon*0
   w<-which(cos(lat.rotated)>1.0e-6)
   cos.rotated<-lon*0
   cos.rotated[w]<-pmax(-1,pmin(1,(cos.pole.lat*
                                     sin(lat[w]*dtr)+
                                     sin.pole.lat*
                                     cos(lon[w]*dtr)*
                                     cos(lat[w]*dtr))/
                                     cos(lat.rotated[w])))
   lon.rotated[w]<-acos(cos.rotated[w])
   lon.rotated<-lon.rotated*sign(lon)
   lat.rotated<-lat.rotated/dtr
   lon.rotated<-lon.rotated/dtr
   w<-which(!is.na(lon.rotated) & lon.rotated>180)
   lon.rotated[w]<-lon.rotated[w]-360
   w<-which(!is.na(lon.rotated) & lon.rotated< -180)
   lon.rotated[w]<-lon.rotated[w]+360
   if(polygon) { # Data are polygon vertices, shift them as whole polygons
     idx<-1+cumsum(is.na(lon.rotated)) # polygon indices
     d<-diff(lon.rotated)
     w<-which(abs(d)>180)
     if(length(w)>0) { # fixes needed
       ptf<-unique(idx[w+1])
       for(p in ptf) {
          w<-which(idx==p & !is.na(lon.rotated))
          d<-lon.rotated[w]-lon.rotated[w[1]] # distance from 1st point
          wp<-which(d>180)
          lon.rotated[w[wp]]<-lon.rotated[w[wp]]-360
          wp<-which(d< -180)
          lon.rotated[w[wp]]<-lon.rotated[w[wp]]+360
       }
     }
   }
   return(list(lat=lat.rotated,lon=lon.rotated))
 }

#' Reverse rotate lats and lons
#'
#' From rotated pole to same positions in a standard pole.
#' 
#' Convert latitudes and longitudes to the equivalents
#'  with a un-rotated pole. (Formulae from UMDP S1).
#'
#' @export
#' @param lat vector of latitudes in rotated pole (degrees).
#' @param lon vector of longitudes in rotated pole (degrees).
#' @param pole.lat latitude of pole to rotate from (degrees).
#' @param pole.lon longitude of pole to rotate from (degrees).
#' @return list with components 'lat' and 'lon' - vectors of
#'   un-rotated lat and lon (in degrees).
UnrotateLatLon<-function(lat,lon,pole.lat,pole.lon) {

   if(pole.lat==90 && pole.lon==180) {
     return(list(lat=lat,lon=lon))
   }
   l0<-pole.lon+180
   dtr<-pi/180
   sin.pole.lat<-sin(pole.lat*dtr)
   cos.pole.lat<-cos(pole.lat*dtr)
   if(pole.lat<0) {
      sin.pole.lat<- -sin.pole.lat
      cos.pole.lat<- -cos.pole.lat
   }
   w<-which(!is.na(lon) & lon>180)
   lon[w]<-lon[w]-360
   w<-which(!is.na(lon) & lon< -180)
   lon[w]<-lon[w]+360
   w<-which(!is.na(lon) & lon==0) # Why discontinuity here
   lon[w]<-0.001
   
   lat.rotated<-asin(pmax(-1,pmin(1,cos.pole.lat*
                                  cos(lon*dtr)*
                                  cos(lat*dtr)+
                                  sin.pole.lat*
                                  sin(lat*dtr))))

   lon.rotated<-lon*0
   w<-which(cos(lat.rotated)>1.0e-6)
   lon.rotated[w]<-acos(pmax(-1,pmin(1,(-cos.pole.lat*
                                     sin(lat[w]*dtr)+
                                     sin.pole.lat*
                                     cos(lon[w]*dtr)*
                                     cos(lat[w]*dtr))/
                                     cos(lat.rotated[w]))))
   
   lon.rotated<-lon.rotated*sign(lon)
   lon.rotated<-lon.rotated+l0*dtr
   lat.rotated<-lat.rotated/dtr
   lon.rotated<-lon.rotated/dtr
   w<-which(!is.na(lon.rotated) & lon.rotated>180)
   lon.rotated[w]<-lon.rotated[w]-360
   w<-which(!is.na(lon.rotated) & lon.rotated< -180)
   lon.rotated[w]<-lon.rotated[w]+360
   return(list(lat=lat.rotated,lon=lon.rotated))
 }

#' Rotate winds - internal detail
#'
#' From u & v one pole to same in a different pole.
#' 
#' (Formulae from UMDP S1). Works for any vector field, not just winds.
#'
#' @param u vector of zonal wind speeds in source pole.
#' @param v vector of meridional wind speeds in source pole.
#' @param lat.orig vector of latitudes of wind vectors in source pole (degrees).
#' @param lon.orig vector of longitudes of wind vectors in source pole (degrees).
#' @param lat.new vector of latitudes of wind vectors in target pole (degrees).
#' @param lon.new vector of longitudes of wind vectors in target pole (degrees).
#' @param pole.lat latitude of pole to rotate to (degrees).
#' @param pole.lon longitude of pole to rotate to (degrees).
#' @return list with components 'u' and 'v' - vectors of
#'   rotated zonal and meridional wind speeds.
#' @seealso \code{\link{GSDF.ll.to.rg}} and \code{\link{GSDF.rg.to.ll}}.
WindToPoleInternal <-function(u,v,lat.orig,lon.orig,
                               lat.new,lon.new,
                               pole.lat,pole.lon=180) {

   if(pole.lat==90 && pole.lon==180) {
     return(list(u=u,v=v))
   }
   l0<-pole.lon+180
   w<-which(!is.na(l0) && l0>180)	
   l0[w]<-l0[w]-360
   w<-which(!is.na(l0) && l0< -180)
   l0[w]<-l0[w]+360

   dtr<-pi/180
   c1<- sin((lon.orig-l0)*dtr)*sin(lon.new*dtr)*sin(pole.lat*dtr)+
           cos((lon.orig-l0)*dtr)*cos(lon.new*dtr)
   c2<-sqrt(1-c1*c1)
   w<-which(sin(lon.new*dtr)*sin(pole.lat*dtr)<0)
   c2[w]<-c2[w]*-1
   if(pole.lat>180) pole.lat<-pole.lat-360
   return(list(u=c1*u-c2*v,v=c1*v+c2*u))
}

#' Rotate winds 
#'
#' From u & v one pole to same in a different pole.
#' 
#' (Formulae from UMDP S1). Works for any vector field, not just winds.
#'
#' @export
#' @param u field of zonal wind speeds in source pole.
#' @param v field of meridional wind speeds in source pole.
#' @param pole.lat latitude of pole to rotate to (degrees).
#' @param pole.lon longitude of pole to rotate to (degrees).
#' @return list with components 'u' and 'v' - fields of
#'   rotated zonal and meridional wind speeds.
RotateWindToPole <-function(u,v,pole.lat,pole.lon=180) {
      u2<-GSDF::RotateFieldToPole(u,pole.lat,pole.lon)
      v2<-GSDF::RotateFieldToPole(v,pole.lat,pole.lon)
      lat.new<-GSDF::RollDimensions(u2,GSDF.find.dimension(u2,'lat'),
                                       GSDF.find.dimension(u2,'lon'))
      lon.new<-GSDF::RollDimensions(u2,GSDF.find.dimension(u2,'lon'),
                                       GSDF.find.dimension(u2,'lat'))
      ll.orig<-GSDF::RotateLatLon(lat.new,lon.new,pole.lat,pole.lon)
      r.u.v<-WindToPoleInternal(u2$data,v2$data,ll.orig$lat,
                                        ll.orig$lon,lat.new,lon.new,
                                        pole.lat,pole.lon)
      u2$data[]<-r.u.v$u
      v2$data[]<-r.u.v$v
      return(list(u=u2,v=v2))
}     

#' Rotate a field
#'
#' Keeps the same grid but moves the North pole to a different place.
#'
#' Effectively allows you to move the centre of the lat-lon grid (where
#'  distortion is low) to any position.
#'
#' @export
#' @param g field to be rotated
#' @param pole.lat latitude of pole to rotate to (degrees).
#' @param pole.lon longitude of pole to rotate to (degrees).
#' @param greedy boolean. How to deal with missing data: If FALSE (default)
#'   missing regions will stay missing (and grow). If TRUE, missing
#'   regions will shrink.
#' @return input field but with the rotated pole.
#' @seealso \code{\link{GSDF::RotateLatLon}} and \code{\link{GSDF::UnrotateLatLon}}.
FieldToPole<-function(g,pole.lat,pole.lon,greedy=FALSE) {
  if(is.null(g$meta)) g$meta<-list()
  if(is.null(g$meta$pole.lat)) g$meta$pole.lat<-90
  if(is.null(g$meta$pole.lon)) g$meta$pole.lon<-180
  if(g$meta$pole.lat==pole.lat &&
     g$meta$pole.lon==pole.lon) return(g)
  result<-g
  if(is.null(result$meta)) result$meta<-list()
  result$meta$pole.lat<-pole.lat
  result$meta$pole.lon<-pole.lon
  result<-GSDF::Regrid2d(g,result,greedy=greedy)
  return(result)
}

