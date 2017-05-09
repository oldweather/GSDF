#' Data structure describing a weather front
#' 
#' Fundamentally just a list with particular components:
#' \describe{
#'  \item{$lat} {numeric vector of latitudes}
#'  \item{$lon} {numeric vector of longitudes}
#'  \item{$type} {'cold','warm','stationary', or 'occluded'.
#'               NULL if unknown}
#'  \item{$moving} {'east' or 'west', NULL if unknown}
#'  \item{$meta} {any other info (list - key-value pairs)} Notably:
#'       pole.lat - latitude of the pole used (assume 90 if unspecified)
#'       pole.lon - longitude of the pole used (assume 180 if unspecified)
#' }
#'
#' @export
#' @return A list as described (all components empty)
GSDF.Front<-function() {
   result<-list()
   result$lat<-numeric(0)
   result$lon<-numeric(0)
   result$meta<-list()
   return(result)
}

#' Find fronts from 6-hr changes in the wind field
#' 
#' Uses the method of Simmonds et al. 'Identification and Climatology of
#'   Southern Hemisphere Mobile Fronts in a Modern Reanalysis'
#'   JCLI, 2011.
#'
#' Basically identifies regions where wind changes direction and speed
#'  over 6 hours, and fits a smooth line to them.
#'
#' @export
#' @param uwnd - Current U wind field.
#' @param uwnd.old - U wind field from 6-hours ago
#' @param vwnd - Current V wind field.
#' @param vwnd.old - V wind field from 6-hours ago
#' @return A list of GSDF.Front structures
Front.find<-function(uwnd,vwnd,uwnd.old,vwnd.old) {
}

#' Find regions with suitable 6-hr changes in the wind field
#' 
#' Want NH regions where wind moves from SW quadrant to NW quadrant
#'  and meridional speeds change by at least 2m/s
#' and SH regions where wind moves from NW quadrant to SW quadrant
#'  and meridional speeds change by at least 2m/s
#'
#' @export
#' @param uwnd - Current U wind field.
#' @param uwnd.old - U wind field from 6-hours ago
#' @param vwnd - Current V wind field.
#' @param vwnd.old - V wind field from 6-hours ago
#' @return A list of GSDF.Front structures
Front.find.wind.change.points<-function(uwnd,vwnd,uwnd.old,vwnd.old) {
  result<-uwnd
  result$data[]<-rep(NA,length(result$data))
  dim.lat<-GSDF.find.dimension(uwnd,'lat')
  dim.lon<-GSDF.find.dimension(uwnd,'lon')
  lon.coords<-GSDF.roll.dimensions(uwnd,dim.lat,dim.lon)
  w<-which(lon.coords>0) # Northern hemisphere points
  vwnd$data[w]<-vwnd$data[w]*-1
  vwnd.old$data[w]<-vwnd.old$data[w]*-1
  w<-which(uwnd$data>0 & vwnd$data>0 &
           uwnd.old$data>0 & vwnd.old$data<0 &
           abs(vwnd$data-vwnd.old$data)>2)
  if(length(w)>0) result$data[w]<-0
  return(result)
}

#' Group selected frontal points into 8-connected regions
#' 
#' Each connected region is a single front.
#'
#' @export
#' @param points - GSDF field marking frontal grid points (non NA=frontal)
#' @return A similar field with each point numbered according to its cluster
Front.cluster<-function(points) {
  w<-which(!is.na(points$data))
  if(length(w)==0) return(points) # No regions
  points$data[w]<-0
  front.count<-1
  unclassified<-which(points$data==0)
  dims<-GSDF.get.extended.dimensions(points)
  if(length(dims)!=2) {
      stop('Must have exactly 2 extended dimensions')
  }
  d1<-length(points$dimensions[[dims[1]]]$values)
  d2<-length(points$dimensions[[dims[2]]]$values)
  # increments to give neighbours
  n.idx<-c(0,1,-1,d1,-d1,d1+1,d1-1,-d1-1,-d1+1)
  front.count<-1
  while(length(unclassified)>0) {
    seed<-unclassified[1]
    while(TRUE) {
        # add neighbour increments to each current point
        seed<-as.vector(outer(seed,n.idx,'+'))
        seed<-unique(seed)
        w<-which(seed>0 & seed<=length(points$data))
        seed<-seed[w]
        w<-which(!is.na(points$data[seed]))
        seed<-seed[w]
        w<-which(points$data[seed]==0)
        if(length(w)==0) break
        points$data[seed]<-front.count
   }
   unclassified<-which(points$data==0)
   front.count<-front.count+1
  }
  return(points)    
}

#' Convert frontal clusters into linear features
#' 
#' Takes the point of max longitude at each latitude
#'
#' Clusters that span a single grid-cell of latitude are deleted.
#'
#' @export
#' @param clusters - GSDF of clusters - output from \code{\link{Front.cluster}}
#' @return A list of GSDF.front
Front.cluster.to.linear<-function(clusters) {
   dims<-GSDF.get.extended.dimensions(clusters)
   if(length(dims)!=2) {
       stop('Must have exactly 2 extended dimensions')
   }
   dim.lat<-GSDF.find.dimension(clusters,'lat')
   dim.lon<-GSDF.find.dimension(clusters,'lon')
   d1<-length(clusters$dimensions[[dims[1]]]$values)
   d2<-length(clusters$dimensions[[dims[2]]]$values)
   no.of.clusters<-max(clusters$data,na.rm=TRUE)
   cluster.idx<-1
   result<-list()
   for(i in seq(1,no.of.clusters)) {
     w<-which(!is.na(clusters$data) & clusters$data==i)
     lats.w<-as.integer((w-1)/d1)+1
     longs.w<-w-(lats.w-1)*d1
     if(dim.lat<dim.lon) {
        longs.w<-as.integer((w-1)/d1)+1
        lats.w<-w-(longs.w-1)*d1
     }
     if(length(unique(lats.w))<2) next
     front<-GSDF.Front()
     for(lat in sort(unique(lats.w))) {
         lon<-max(longs.w[which(lats.w==lat)])
         # skip regions where easterly extent is limited by end-of-data
         if(lon==length(clusters$dimensions[[dim.lon]]$values)) next
        front$lat<-c(front$lat,clusters$dimensions[[dim.lat]]$values[lat])
        front$lon<-c(front$lon,clusters$dimensions[[dim.lon]]$values[lon])
     }
     result[[cluster.idx]]<-front
     cluster.idx<-cluster.idx+1
   }
   return(result)
}
     
     
#' Smooth the raw front longitudes
#' 
#' Uses the 5353H smoother, twice
#'
#' @export
#' @param fronts - list of front structures - output from \code{\link{Front.cluster.to.linear}}
#' @return Same front structures, smoothed.
Front.smooth<-function(fronts) {
  for(i in seq_along(fronts)) {
    fronts[[i]]$lon<-Front.smooth.longitudes(fronts[[i]]$lon)
  }
  return(fronts)
}

#' Smooth a series of longitudes
#' 
#' Uses the 5353H smoother, twice
#'
#' @export
#' @param lons - vector of longitudes (at least 2)
#' @return Same longitudes, smoothed.
Front.smooth.longitudes<-function(lons) {
  ll<-length(lons)
  lons<-c(lons[2],lons[1],lons,lons[ll],lons[ll-1])
  # median filters of length 5,3,5,3
  fp<-runmed(lons,5,endrule='keep')
  fp<-runmed(fp,3,endrule='keep')
  fp<-runmed(fp,5,endrule='keep')
  fp<-runmed(fp,3,endrule='keep')
  fp<-filter(fp,c(0.25,0.5,0.25),sides=2)
  fp[1]<-fp[2]
  fp[ll+4]<-fp[ll+3]
  # Same with the residuals
  resid<-lons-fp
  resid<-runmed(resid,5,endrule='keep')
  resid<-runmed(resid,3,endrule='keep')
  resid<-runmed(resid,5,endrule='keep')
  resid<-runmed(resid,3,endrule='keep')
  resid<-filter(resid,c(0.25,0.5,0.25),sides=2)
  # Return the sum - trimed back to the original length
  return((fp+resid)[3:(ll+2)])
}
