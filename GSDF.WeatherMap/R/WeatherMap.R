# Tools for making weather maps

# Load the package data - land mask & ice shelves
data(WeatherMap.land.mask, envir=environment())
data(WeatherMap.ice.shelves, envir=environment())

# Plot is controlled by this set of options
Defaults<-list(
   cores=1,                             # Not currently used
   wrap.spherical=FALSE,                # For plotting on a sphere?
   region='global',
   pole.lat=90,pole.lon=180,            # Position of north pole
   lon.min=-180,lon.max=180,
   lat.min=-90,lat.max=90,              # Map range (around centre)
   vp.lon.min=NULL,vp.lon.max=NULL,
   vp.lat.min=NULL,vp.lat.max=NULL,     # Range to actually draw (around centre)
   show.wind=TRUE,
   show.precipitation=TRUE,
   show.mslp=TRUE,
   show.temperature=TRUE,
   show.ice=FALSE,
   show.fog=FALSE,
   show.obs=FALSE,
   show.ice.shelves=TRUE,
   precip.points=25000,                 # Bigger -> higher res precip
   precip.threshold=0.0025,             # Only show where more than this
   precip.range=0.03,                   # Precip rate for max intensity
   precip.resolution=0.25,              # Grid resolution in degrees
   precip.colour=c(0,0,0),              # 0-1, RGB for intense precip
   precip.min.transparency=0.95,        # 0-1, density of intense precip
   precip.max.opacity=1,
   wind.vector.density=1.5,             # Decrease for closer-packed streamlines
   bridson.max.attempt=5,               # Decrease=faster but less good streamline arrangement
   wind.vector.arrow=NULL,              # See ?arrow
   wind.vector.points=3,                # Bigger -> smoother curves and slower
   wind.vector.scale=0.25,              # Bigger -> longer vectors
   wind.vector.move.scale=1,            # Bigger -> faster moving vectors
   wind.vector.lwd=4,                   # Line width
   jitter=TRUE,                         # Jitter vector seed points?
   wind.palette=diverge_hcl(70, c = 50,
                    l = 25, power = 1), # Interpolated blue red
   wind.palette.bias=1,                 # ?colorRamp
   wind.palette.opacity=1,              # 
   temperature.range=7,                 # T2m anomaly for max. colour
   mslp.base=101325,                    # Base value for anomalies
   mslp.range=10000,                    # Anomaly for max contour
   mslp.step=750,                       # Smaller -> more contours
   mslp.tpscale=2000,                   # Smaller -> contours less transparent
   mslp.lwd=1, 
   background.resolution='low',         # 'low' for polygons, 'high' for grid
   sea.colour=rgb(80*1.5,95*1.5,107*1.5,255,
                  maxColorValue=255),   # For background
   ice.colour=rgb(150*1.2,165*1.2,177*1.2,255,
                maxColorValue=255),
   ice.points=10000,                    # Bigger - higher res ice
   land.colour=rgb(123,121,117,255,
                    maxColorValue=255),
   fog.colour=c(0.65,0.65,0.65),        # 0-1, bigger -> lighter fog
   fog.min.transparency=0.85,           # 0-1, bigger -> thicker fog
   fog.resolution=1,                    # Grid resolution in degrees
   obs.size=0.5,                        # In degrees
   obs.colour=rgb(255,215,0,255,
                   maxColorValue=255),  # For observations
   label='',                            # Label - the date is a good choice
   label.xp=0.99,label.yp=0.01,         # Location, npc units
   label.bg.colour=rgb(123,121,117,255, # Background colour for label
                    maxColorValue=255)  #  defaults to land colour.
)

#' WeatherMap.option
#'
#' Set or query the options controling the plot.
#'
#' The rendering of a map is controlled by a large
#'  number of options contained in a list.
#' See source for values and defaults.
#'
#' @export
#' @param Options list of options - if NULL, use defaults
#' @param option name of option to set
#' @param value value to set selected option to
#' @return new list of options
WeatherMap.set.option<-function(Options=NULL,option=NULL,value=NULL) {
  if(is.null(Options)) Options<-Defaults
  if(!is.null(option)) {
     if(is.null(Options[[option]])) {
       warning(paste("No option",option))
       return(FALSE)
     }
     if(!is.null(value)) Options[[option]]<-value
   }
  return(Options)
}

#' Aspect ratio
#'
#' Appropriate aspect ratio - for device allocation
#'
#' Get an aspect ratio (width/height) for the region
#' set in the options.
#'
#' @export
#' @param Options list of options - see \code{WeatherMap.set.option}
#' @return ratio 
WeatherMap.aspect<-function(Options) {
   Ratio<-(Options$lon.max-Options$lon.min)/
          (Options$lat.max-Options$lat.min)
   return(Ratio)  
}

# Generate n random points at a distance between r.min and 2(r.min)
#  of a specified point - we'll be doing this often so generate
# a normalised sample of length about 1000 and then sa,mple from it
bridson.annual.sample.cache<-list()
    x.s<-(runif(2000)-0.5)*4
    y.s<-(runif(2000)-0.5)*4
    d.s<-sqrt(x.s**2+y.s**2)
    w<-which(d.s<1 | d.s>2)
    x.s<-x.s[-w]
    y.s<-y.s[-w]
bridson.annual.sample.cache$x<-x.s
bridson.annual.sample.cache$y<-y.s

bridson.annular.sample<-function(n=NULL,x=NULL,y=NULL,r=NULL) {
   order<-sample.int(length(bridson.annual.sample.cache$x),size=n)
   return(list(x=bridson.annual.sample.cache$x[order]*r+x,
               y=bridson.annual.sample.cache$y[order]*r+y))
}

# Find the subset of points not too far from a selected point
#  using their grid indices
bridson.close.points<-function(index,n.x,n.y) {
  idx.y<-as.integer((index-1)/n.x)+1
  idx.x<-index-(idx.y-1)*n.x
  x.range<-seq.int(max(1,idx.x-3),min(n.x,idx.x+3))
  y.range<-seq.int(max(1,idx.y-3),min(n.y,idx.y+3))
  result=(rep(y.range,length(x.range))-1)*n.x+
         sort(rep(x.range,length(y.range)))
  return(result)
}

# Calculate the minimum scaled distance, for each of n
#  new points, from a set of m existing points.
bridson.min.distance<-function(x.new,y.new,x.old,y.old,scale.x,scale.y) {
  d<-rep(0,length(x.new))
  for(i in seq_along(x.new)) {
    d[i]<-min(((x.new[i]-x.old)/scale.x)**2+
              ((y.new[i]-y.old)/scale.y)**2)
  }
  return(sqrt(d))
}

#' Allocate points using poisson-disc coverage
#'
#' Uses Bridson's algorithm.
#'
#' Requires a characteristic length and a max attempt
#'  number - both set in options
#'
#' @export
#' @param Options list of options - see \code{WeatherMap.set.option}
#' @param previous list with elements 'lat' and lon' - set of points to
#'  start from. Defaults to NULL - start from scratch.
#' @param scale.x GSDF field with horizontal wind speeds - used to adjust
#'  horizontal distance to allow for streamlines extending with the wind.
#'  Defaults to NULL - no adjustment made.
#' @param scale.y GSDF field with vertical wind speeds - used to adjust
#'  vertical distance to allow for streamlines extending with the wind.
#'  Defaults to NULL - no adjustment made.
#' @return list with elements 'lats' and lons'
WeatherMap.bridson<-function(Options,
                             previous=NULL,
                             scale.x=NULL,scale.y=NULL) {

    x.range<-c(Options$lon.min,Options$lon.max)
    y.range<-c(Options$lat.min,Options$lat.max)
    view.scale<-max(diff(x.range)/360,diff(y.range)/180)
    r.min<-Options$wind.vector.density*view.scale
    max.attempt<-Options$bridson.max.attempt

    # For spherical layout scale down x locations according to latitude
    # use as normal on scaled locations, then scale up again and throw out
    # any positions outside the lon.range.
    # Inefficient, but simple.
    if(Options$wrap.spherical && !is.null(previous)) {
      previous$lon<-previous$lon*cos(previous$lat*pi/180)
    }
     
    # Choose background grid spacing close to r/sqrt(2)
    #  and which gives an integer number of points
    n.x<-as.integer(diff(x.range)/(r.min/sqrt(2)))
    r.x<-diff(x.range)/n.x
    n.y<-as.integer(diff(y.range)/(r.min/sqrt(2)))
    r.y<-diff(y.range)/n.y

    # Positions of point at each grid location
    #  NA if nothing there
    x<-rep(NA,n.x*n.y)
    y<-rep(NA,n.x*n.y)

    # Scale factors at each grid location
    scalef.x<-rep(1,n.x*n.y)
    scalef.y<-rep(1,n.x*n.y)
    if(!is.null(scale.x) && !is.null(scale.y)) {
        gp.x<-(seq(1,n.x)-0.5)*r.x+x.range[1]
        gp.y<-(seq(1,n.y)-0.5)*r.y+y.range[1]
        gp.x.full<-as.vector(matrix(data=rep(gp.x,n.y),ncol=n.x,byrow=F))
        gp.y.full<-as.vector(matrix(data=rep(gp.y,n.x),ncol=n.y,byrow=T))
        scalef.x<-abs(GSDF.interpolate.ll(scale.x,gp.y.full,gp.x.full))
        scalef.y<-abs(GSDF.interpolate.ll(scale.y,gp.y.full,gp.x.full))
       # There's something wrong here - the *0.1 below should not be there (should be 1.0)
        v.scale<-view.scale*Options$wind.vector.scale*0.1
        scalef.x<-(scalef.x*v.scale+r.min)/r.min
        scalef.y<-(scalef.y*v.scale+r.min)/r.min
    }

    # set of active points
    active<-integer(0)

    # Order of addition
    order.added<-integer(0)
    
    # Generate start point at random
    #x.c<-runif(1)*diff(x.range)+min(x.range)
    #y.c<-runif(1)*diff(y.range)+min(y.range)
    # start at top left
    x.c<-min(x.range) + r.min/2
    y.c<-max(y.range) - r.min/2
    index.c<-as.integer((y.c-min(y.range))/r.y)*n.x+
             as.integer((x.c-min(x.range))/r.x)+1
    active<-index.c
    x[index.c]<-x.c
    y[index.c]<-y.c
    order.added<-c(order.added,index.c)

    # If starting from a pre-existing set of points, load them
    # in random order, culling any too close to one already loaded.
    if(!is.null(previous)) {
        w<-which(previous$lat<min(y.range) |
                 previous$lat>max(y.range) |
                 previous$lon<min(x.range) |
                 previous$lon>max(x.range))
        if(length(w)>0) {
            previous$lat<-previous$lat[-w]
            previous$lon<-previous$lon[-w]
        }
    order<-sample.int(length(previous$lon))
    for(i in seq_along(order)) {
        index.i<-as.integer((previous$lat[i]-min(y.range))/r.y)*n.x+
                 as.integer((previous$lon[i]-min(x.range))/r.x)+1
        if(!is.na(x[index.i])) next
        cp<-bridson.close.points(index.i,n.x,n.y)
        cp<-cp[!is.na(x[cp])]
        if(length(cp)>0) {
            d.s<-(((previous$lon[i]-x[cp])/scalef.x[cp])**2+
                  ((previous$lat[i]-y[cp])/scalef.y[cp])**2)
            if(min(d.s,na.rm=TRUE)<r.min**2) next
        }
        x[index.i]<-previous$lon[i]
        y[index.i]<-previous$lat[i]
        order.added<-c(order.added,index.i)
        # Ideally we'd set all points to active, but try
        #  only a subset - faster
        if(index.i%%7==0) active<-c(active,index.i)
      }
  }
    
    # Allocate more points to fill gaps
    while(length(active)>0) {
      c<-active[sample.int(length(active),1)] # Choose random active point
      cp<-bridson.close.points(c,n.x,n.y)
      cp<-cp[!is.na(x[cp])]
         ns<-bridson.annular.sample(n=max.attempt,x=x[c],y=y[c],r=r.min)
         w<-which(ns$y<y.range[1] | ns$y>y.range[2] |
            ns$x<x.range[1] | ns$x>x.range[2])
         if(length(w)>0) {
           ns$x<-ns$x[-w]
           ns$y<-ns$y[-w]
         }
         index.s<-as.integer((ns$y-y.range[1])/r.y)*n.x+
                     as.integer((ns$x-x.range[1])/r.x)+1
         w<-which(is.na(ns$x[index.s]))
         if(length(w)>0) { # At least one sample not in occupied box
           index.s<-index.s[w]
           ns$x<-ns$x[w]
           ns$y<-ns$y[w]
           if(length(cp)>0) { # At least one close point, so test necessary     
              d<-bridson.min.distance(ns$x,ns$y,x[cp],y[cp],scalef.x[cp],scalef.y[cp])
              w<-which(d>r.min)
           } else w<-1 # no test - just take first point
           if(length(w)>0) { # new point
               x[index.s[w[1]]]<-ns$x[w[1]]
               y[index.s[w[1]]]<-ns$y[w[1]]
               active<-c(active,index.s[w[1]])
               order.added<-c(order.added,index.s[w[1]])
               next
           }
         }
         # All failed - remove current point from active list
         w<-which(active==c)
         active<-active[-w]
   }

    #w<-which(is.na(x))
    #x<-x[-w]
    #y<-y[-w]
    x<-x[order.added]
    y<-y[order.added]

    if(Options$wrap.spherical) {
      x<-x/cos(y*pi/180)
      w<-which(x<Options$lon.max & x>=Options$lon.min)
      x<-x[w]
      y<-y[w]
     }

    return(list(lon=x,lat=y))
  }

#' Rectpoints
#'
#' Create a set of uniformly distributed points on which
#'  to plot.
#'
#' Generate a set of (approximately) N lat,long points that are
#'  uniformly distributed in latitude and longitude.
#'
#' @export
#' @param n (approximate) number of points.
#' @param Options list of options - see \code{WeatherMap.set.option}
#' @return list with components 'lat' and 'lon'.
WeatherMap.rectpoints<-function(n,Options) {
   Ratio<-(Options$lat.max-Options$lat.min)/
          (Options$lon.max-Options$lon.min)
   Mx<-as.integer(sqrt(n/Ratio))
   My<-as.integer(Ratio*Mx)
   lats<-vector(mode='numeric')
   lons<-vector(mode='numeric')
   m<-seq(1,My)
   Lat<-(m-0.5)*(Options$lat.max-Options$lat.min)/My + Options$lat.min
   lats<-rep(lats,length(lons))
   m<-seq(1,Mx)
   Lon<-(m-0.5)*(Options$lon.max-Options$lon.min)/Mx + Options$lon.min
   lats<-rep(Lat,length(Lon))
   lons<-matrix(data=rep(Lon,length(Lat)),
                 nrow=length(Lat),ncol=length(Lon),byrow=T)
   longs<-as.vector(lons)
   lats<-pmax(Options$lat.min+0.01,lats)
   lats<-pmin(Options$lat.max-0.01,lats)
   lons<-pmax(Options$lon.min+0.01,lons)
   lons<-pmin(Options$lon.max-0.01,lons)
   return(list(lat=lats,lon=lons))
}

#' Propagate streamlines
#'
#' Propagate out streamlines from start positions
#'
#' A streamline is the locus of a point moving with the wind. This
#' function moves a set of points out with the wind.
#' For details of the streamline data structure, see \code{WeatherMap.make.streamlines}
#' @param lat vector of latitudes
#' @param lon vector of longitudes
#' @param status vector of status (see \code{WeatherMap.make.streamlines})
#' @param u GSDF field of zonal wind (m/s)
#' @param v GSDF field of meridional wind (m/s)
#' @param t GSDF field of air temperature (K)
#' @param t.c GSDF field of air temperature normal (K)
#' @param Options list of options - see \code{WeatherMap.set.option}
#' @return streamlines data structure (see \code{WeatherMap.make.streamlines})
WeatherMap.propagate.streamlines<-function(lat,lon,status,u,v,t,t.c,Options) {

    streamlets<-list()
    streamlets[['x']]<-array(dim=c(length(lon),Options$wind.vector.points))
    streamlets[['x']][,1]<-lon
    streamlets[['y']]<-array(dim=c(length(lat),Options$wind.vector.points))
    streamlets[['y']][,1]<-lat
    streamlets[['shape']]<-array(dim=c(length(lat),Options$wind.vector.points))
    streamlets[['shape']][,1]<-0
    if(!is.null(t) && !is.null(t.c)) {
    # Get temperature anomalies
        tp<-GSDF.interpolate.ll(t,lat,lon)
        cp<-GSDF.interpolate.ll(t.c,lat,lon)
	streamlets[['t_anom']]<-tp-cp
    }
    streamlets[['magnitude']]<-0
    view.scale<-max((Options$lon.max-Options$lon.min)/360,(Options$lat.max-Options$lat.min)/180)
    for(i in seq(1,Options$wind.vector.points)) {
		up<-GSDF.interpolate.ll(u,lat,lon)
                if(Options$wrap.spherical) up<-up/cos(lat*pi/180)
		vp<-GSDF.interpolate.ll(v,lat,lon)
		m<-sqrt(up**2+vp**2)
		direction<-atan2(vp,up)
		streamlets[['x']][,i]<-lon
		streamlets[['y']][,i]<-lat        
		streamlets[['shape']][i]<-0
		streamlets[['magnitude']]<-streamlets[['magnitude']] +
		(m)*view.scale*Options$wind.vector.scale/Options$wind.vector.points
		lon<-lon+cos(direction)*(m)*
                view.scale*
		Options$wind.vector.scale/Options$wind.vector.points
		lat<-lat+sin(direction)*(m)*
                view.scale*
		Options$wind.vector.scale/Options$wind.vector.points
    }
    streamlets[['magnitude']][is.null(streamlets[['magnitude']])]<-0
    streamlets[['magnitude']]<-pmax(0,pmin(1,streamlets[['magnitude']]))
    streamlets[['status']]<-status
    return(streamlets)
}
   
#' Streamline getGC
#'
#' Generate a graphics context for a streamline
#'
#' Set the colour, width and transparency to draw a streamline
#'
#' @param value Range 0-1 - set the colour of the streamline
#' @param transparency Range 0-1 - set the transparency of a streamline
#' @param status Range 0-1 - set the line width of a streamline
#' @param Options list of options - see \code{WeatherMap.set.option}
#' @return a graphics context for drawing
WeatherMap.streamline.getGC<-function(value,transparency=NA,status=1,Options) {
   if(is.na(value)) { # No data - fully transparent
      colour<-rgb(255,255,255,0,maxColorValue = 255)
      return(gpar(col=colour,fill=colour,lwd=Options$wind.vector.lwd))
   }
   value<-max(0.001,min(0.999,value))
   #rgb<-colorRamp(Options$wind.palette,bias=Options$wind.palette.bias)(value)
   rgb<-col2rgb(Options$wind.palette[ceiling(value*length(Options$wind.palette))])
   if(is.na(transparency)) alpha<-255
   else {
	 transparency<-max(0,min(1,transparency))
         alpha<-c(255,192,128,54,0)[min(as.integer(transparency*5)+1,5)]
     }
   if(any(is.na(c(rgb,alpha)))) {
      colour<-rgb(255,255,255,0,maxColorValue = 255)
      return(gpar(col=colour,fill=colour,lwd=Options$wind.vector.lwd))
   }
   colour<-rgb(rgb[1],rgb[2],rgb[3],alpha,maxColorValue = 255)
   if(Options$wrap.spherical) status<-0.01 # polygon fill in this case
   return(gpar(col=colour,fill=colour,lwd=Options$wind.vector.lwd*status))
}


#' Make streamlines
#'
#' A streamline is the locus of a point moving with the wind. We need to
#'  seed them, propagate them out with the wind, keep track of the local
#'  temperature (for colour when drawing), and thin them. 
#'
#' We keep streamline data in a list. If there are 'i' streamlines, each of length 'j',
#'  components are
#'   \itemize{
#'      \item{x}{longitudes, array[i,j]}
#'      \item{y}{latitudes, array[i,j]}
#'      \item{status}{vector[i]. For a newly created streamline, status=0. It increases
#' by 1/Options$wind.vector.fade.steps each timestep to a max of 1. When chosen for thinning,
#' status is set to -1 and increases similarly to 0, when the streamline is deleted. As line width
#' is proportional to status, this allows streamlines to fade gradually in and out.}
#'      \item{shape}{vector[i]. See \code{grid::xspline}, currently always 0.}
#'      \item{t_anom}{Temperature anomaly at origin (controls plot colour). vector[i]}
#'   }
#' This function will move the streamlines on one timestep, seeding new ones as necessary,
#'  propagating, thinning and updating the status of existing streamlines.
#'
#' @seealso \code{WeatherMap.draw.streamlines}
#' @export
#' @param s streamlines from previous timestep (or NULL)
#' @param u GSDF field of zonal wind (m/s)
#' @param v GSDF field of meridional wind (m/s)
#' @param t GSDF field of air temperature (K)
#' @param t.c GSDF field of air temperature normal (K)
#' @param Options list of options - see \code{WeatherMap.set.option}
#' @return streamlines data structure.
WeatherMap.make.streamlines<-function(s,u,v,t,t.c,Options) {
   t<-WeatherMap.rotate.pole(t,Options)
   t.c<-WeatherMap.rotate.pole(t.c,Options)
   if(Options$pole.lon!=180 || Options$pole.lat!=90) {
      r.u.v<-GSDF.wind.to.pole(u,v,Options$pole.lat,Options$pole.lon)
	  u<-r.u.v$u
	  v<-r.u.v$v
    }
   view.scale<-max((Options$lon.max-Options$lon.min)/360,(Options$lat.max-Options$lat.min)/180)
   lats<-numeric(0)
   longs<-numeric(0)
   status<-numeric(0)
   initial=FALSE # starting from scratch
   if(is.null(s)) initial=TRUE
   if(!is.null(s)) {
      # Move the vectors along at the speed of the wind (*wind.vector.move.scale)
      # Assumes frames are hourly (0.033 converts m/s to degrees/hr)
      move.scale<-0.033*Options$wind.vector.points/Options$wind.vector.scale
      move.scale<-move.scale*Options$wind.vector.move.scale*view.scale
      lats<-s[['y']][,1]<-s[['y']][,1]+(s[['y']][,2]-s[['y']][,1])*move.scale
      longs<-s[['x']][,1]<-s[['x']][,1]+(s[['x']][,2]-s[['x']][,1])*move.scale
      status<-s[['status']]
      w<-which(is.na(lats) | is.na(longs))
      if(length(w)>0) {
        lats<-lats[-w]
        longs<-longs[-w]
        status<-status[-w]
      }
      # Update plot status and remove expired ones or those outside the frame
         w<-which(is.na(lats) | is.na(longs))
         if(length(w)>0) {
           lats<-lats[-w]
           longs<-longs[-w]
           status<-status[-w]
       }
  }
   # Update positions and Roll-out the streamlines
   if(!Options$jitter) set.seed(27)
   p<-WeatherMap.bridson(Options,previous=list(lat=lats,lon=longs),
                          scale.x=u,scale.y=v)
   s<-WeatherMap.propagate.streamlines(p$lat,p$lon,rep(1,length(p$lat)),u,v,t,t.c,Options)
   # Need periodic boundary conditions?
   if(Options$lon.max-Options$lon.min>360) {
      w<-which(s[['x']][,1]< Options$lon.max-360)
      for(var in c('status','t_anom','magnitude')) {
         s[[var]]<-s[[var]][-w]
      }
      for(var in c('x','y','shape')) {
         s[[var]]<-s[[var]][-w,]
      }
      w<-which(s[['x']][,1]> Options$lon.min+360)
      for(var in c('status','t_anom','magnitude')) {
        s[[var]]<-c(s[[var]],s[[var]][w])
      }
      for(var in c('x','y','shape')) {
        s2<-s[[var]][w,]
        if(var=='x') s2[]<-s2-360
        s[[var]]<-rbind(s[[var]],s2)
      }
  }
   return(s)
}

#' Draw streamlines
#'
#' Draw a set of streamlines (previously made by \code{make.streamlines}).
#'
#' For each streamline, makes the appropriate graphics context and draws
#'  a spline.
#'
#' @seealso \code{WeatherMap.draw.streamlines}
#' @export
#' @param s list of streamlines (previously made by \code{make.streamlines}).
#' @param Options list of options - see \code{WeatherMap.set.option}
#' @return nothing - side effect only.
WeatherMap.draw.streamlines<-function(s,Options) {

   for(i in seq_along(s$status)) {     
      if(any(is.na(s[['x']][i,]))) next()
      gp<-gpar(col=rgb(.2,.2,.2,1),fill=rgb(.2,.2,.2,1),lwd=Options$wind.vector.lwd)
      if(!is.null(s[['t_anom']][i])) {
          level<-max(min((na.omit(s[['t_anom']][i]) +
                           Options$temperature.range)/
                           (Options$temperature.range*2),1),0)
          tp<-min(1-min(1,abs(level-0.5)*2),0.75)
          tp<-1.0-Options$wind.palette.opacity
          #tp<-tp*abs(s[['status']])
          gp<-WeatherMap.streamline.getGC(level,transparency=tp,
                                          status=min(abs(s[['status']][i]),1),Options)
      }
      if(Options$wrap.spherical) {
          theta<-atan2(diff(na.omit(s[['y']][i,])),diff(na.omit(s[['x']][i,])))
          dx<-Options$wind.vector.lwd*sin(theta)*0.1
          dx<-c(dx[1],dx)
          dx<-dx/cos(na.omit(s[['y']][i,])*pi/180)
          dy<-Options$wind.vector.lwd*cos(theta)*0.1
          dy<-c(dy[1],dy)
          grid.polygon(x=unit(c(na.omit(s[['x']][i,])-dx,rev(na.omit(s[['x']][i,])+dx))
                         ,'native'),
                       y=unit(c(na.omit(s[['y']][i,])+dy,rev(na.omit(s[['y']][i,])-dy))
                         ,'native'),
                       gp=gp)
      } else {
          grid.xspline(x=unit(na.omit(s[['x']][i,]),'native'),
                       y=unit(na.omit(s[['y']][i,]),'native'),
                       shape=na.omit(s[['shape']][i]),
                       arrow=Options$wind.vector.arrow,
                       gp=gp)
      }
   }
 }


# Deal with cases where land polygons are split by the anti-meridian
# The polygons (from MapData) are designed for use with the standard pole,
#  they don't always cope well with rotation.
# Land polygons are from WeatherMap.get.land.
WeatherMap.land.fix.am<-function(land) {
     w<-c(1,which(is.na(land$x)),length(land$x))
     for(p in seq(2,length(w))) {
       s<-land$x[w[p-1]:w[p]]
       d<-diff(s)
       if(max(d,na.rm=T)>355 || min(d,na.rm=T)< -355) {
         q<-which(s>0)
         s2<-s
         s2[q]<-s2[q]-360
         land$x[w[p-1]:w[p]]<-s2
         q<-which(s<0)
         s[q]<-s[q]+360
         land$x<-c(land$x,NA,s)
         land$y<-c(land$y,NA,land$y[w[p-1]:w[p]])
       }
    }
    return(land)	
}

#' Land-mask
#'
#' Make the land-mask polygons on the selected pole.
#'
#' Mostly can use medium-resolution polygons from the MapData package
#' Alternatively have a high-resolution land-mask gridded field, but
#'  that's a lot of polygons - so slow.
#'
#' @export
#' @param Options list of options - see \code{WeatherMap.set.option}
#'  background.resolution is the relevant one, 'low' or 'high'.
#' @return a set of land-mask polygons for \code{WeatherMap.draw.land}.
WeatherMap.get.land<-function(Options) {
	
  if(Options$background.resolution=='high') {
     if(Options$pole.lon!=180 || Options$pole.lat!=90) {
        land<-GSDF.field.to.pole(WeatherMap.land.mask,Options$pole.lat,Options$pole.lon)
      } else land<-WeatherMap.land.mask
     return(land)
   }
  else {
      land<-map('worldHires',plot=F,fill=T)
       # Split Antarctica polygon to render when pole inside continent
       startend<-c(1529532,1573853)
       if(!is.na(land$y[startend[2]+1])) startend[2]<-1573851 # Version dependent
       land$y[startend]<- -89.99 # First and last points
       #is.na(land$x[c(1529531,1573854)])<-TRUE
       #is.na(land$y[c(1529531,1573854)])<-TRUE
       split<-c(1541505,1551505,1561505,1571505) # Split rest into chunks
       is.na(land$x[split])<-TRUE
       is.na(land$y[split])<-TRUE
       land$y[split+1]<- -89.99
       land$y[split-1]<- -89.99
       if(Options$pole.lon!=0 || Options$pole.lat!=90) {
           l2<-GSDF.ll.to.rg(land$y,land$x,Options$pole.lat,Options$pole.lon)
           land$x<-l2$lon
           land$y<-l2$lat
       }
       lon.range<-Options$lon.max-Options$lon.min
       if(lon.range>350) {
          land<-WeatherMap.land.fix.am(land)
       } else {
         # Break all the polygons now wrapped across the longitude break
         w<-which(abs(diff(land$x))>150)
         ml<-length(land$x)
         for(p in seq_along(w)) {
            land$x<-c(land$x[1:(w[p]+p-1)],NA,land$x[(w[p]+p):(ml+p-1)])
            land$y<-c(land$y[1:(w[p]+p-1)],NA,land$y[(w[p]+p):(ml+p-1)])
         }
         # Needs to be done twice? why?
         w<-which(abs(diff(land$x))>150)
         ml<-length(land$x)
         for(p in seq_along(w)) {
            land$x<-c(land$x[1:(w[p]+p-1)],NA,land$x[(w[p]+p):(ml+p-1)])
            land$y<-c(land$y[1:(w[p]+p-1)],NA,land$y[(w[p]+p):(ml+p-1)])
         }
       }
       lakes<-map('worldHires',plot=F,fill=T,exact=F,region=c('.*Lake|.*Sea'))
       if(Options$pole.lon!=0 || Options$pole.lat!=90) {
           l2<-GSDF.ll.to.rg(lakes$y,lakes$x,Options$pole.lat,Options$pole.lon)
           lakes$x<-l2$lon
           lakes$y<-l2$lat
        }
        lakes<-WeatherMap.land.fix.am(lakes)
        land$lakes<-lakes
        return(land)
    }
}

#' Draw land mask
#'
#' Draw the land (probably onto a pre-existing background of sea)
#' Also draws Antarctic ice shelves.
#'
#' Uses maps/mapdata land polygons if none provided
#' See \code{WeatherMap.get.land}
#'
#' @export
#' @param land list with components 'x' and 'y' - the coordinates
#' of a set of polygons bounding land regions. Suitable for drawing with
#' \code{grid.polygon}. If NULL, will be obtained from \code{WeatherMap.get.land}.
#' @param Options list of options - see \code{WeatherMap.set.option}.
#' @return nothing - side effect only.
WeatherMap.draw.land<-function(land,Options,height=NULL) {

  if(is.null(land)) {
	land<-WeatherMap.get.land(Options)
  }
  if(Options$background.resolution=='high') {
    # land will be a GSDF land mask - plot it as an image
      plot.colours<-rep(rgb(0,0,0,0),length(land$data))
      lons<-land$dimensions[[GSDF.find.dimension(land,'lon')]]$values
      w<-which(land$data>0)
      plot.colours[w]<-Options$land.colour
      m<-matrix(plot.colours, ncol=length(lons), byrow=T)
      # flip the data order up<->down to be right for an image
      m<-apply(m,2,rev)
      grid.raster(m,
                   x=unit(0,'native'),
                   y=unit(0,'native'),
                   width=unit(360,'native'),
                   height=unit(180,'native'))
   } else {
    # land will be a list of polygons from MapData
      if(Options$show.ice.shelves) {
          gp.ice<-gpar(col=Options$ice.colour,
                       fill=Options$ice.colour)
         if(Options$pole.lon!=180 || Options$pole.lat!=90) {
            l2<-GSDF.ll.to.rg(WeatherMap.ice.shelves$y,
                            WeatherMap.ice.shelves$x,
                            Options$pole.lat,Options$pole.lon)
          # Trim all the edge points which might wrap
            w<-which(l2$lon< Options$lon.min+2 | l2$lon>Options$lon.max-2)
            is.na(l2$lon[w])<-T
            grid.polygon(x=unit(l2$lon,'native'),
                         y=unit(l2$lat,'native'),
                         gp=gp.ice)
          }
         else{
          grid.polygon(x=unit(WeatherMap.ice.shelves$x,'native'),
                       y=unit(WeatherMap.ice.shelves$y,'native'),
                       gp=gp.ice)
        }
      } 

        gp.land<-gpar(col=Options$land.colour,
                      fill=Options$land.colour)
        if(Options$background.resolution=='high') {
          grid.polygon(x=unit(land$x,'native'),
                       y=unit(land$y,'native'),
                       id.lengths=rep(4,length(land$x)/4),
                       gp=gp.land)
      } else {
        grid.polygon(x=unit(land$x,'native'),
                     y=unit(land$y,'native'),
                     gp=gp.land)
        gp.sea<-gpar(col=Options$sea.colour,
                      fill=Options$sea.colour)
        grid.polygon(x=unit(land$lakes$x,'native'),
                     y=unit(land$lakes$y,'native'),
                     gp=gp.sea)
      }
    }
    if(!is.null(height)) WeatherMap.draw.haze(height,Options)

}

#' Draw sea-ice
#'
#' Draw sea-ice at a set of points.
#'
#' Ice is drawn as a set of polygons (details controled by Options).
#'  Deliberately de-coupled from native resolution of sea-ice field.
#'
#' @export
#' @param lat vector of latitutes
#' @param lon vector of longitudes
#' @param icec GSDF field of ice coverage (0-1)
#' @param Options list of options - see \code{WeatherMap.set.option}
#' @return nothing - side effect only.
WeatherMap.draw.ice<-function(lat,lon,icec,Options) {
  
   length<-sqrt((Options$lon.max-Options$lon.min)*
                                    (Options$lat.max-Options$lat.min)/
                                                   Options$ice.points)
   icec<-GSDF.grow(icec)
   icec<-WeatherMap.rotate.pole(icec,Options)
   icec<-GSDF.grow(icec)
   ip<-GSDF.interpolate.ll(icec,lat,lon)
   # discard missing and zero points
   w<-which(is.finite(ip) & ip>0.05)
   if(length(w)<1) return() # Might be no ice in field of view
   ip<-ip[w]
   lat<-lat[w]
   lon<-lon[w]
   if(!Options$jitter) set.seed(27) # seed is abitrary - must be same each time
   vscale<-runif(length(lon))*0.05+1
   length<-length*vscale # Cosmetic, make plotted squares irregular
   ice.transparency<-col2rgb(Options$ice.colour,alpha=TRUE)[4,1]
   for(i in seq_along(lat)) {

     ipt<-ip[i]*ice.transparency/255
     col<-(col2rgb(Options$ice.colour)*ipt+col2rgb(Options$sea.colour)*(1-ipt))[,1]
     col<-rgb(col[1],col[2],col[3],maxColorValue=255)
     gp<-gpar(col=col,fill=col)
              sx<-c(lon[i]-length[i]/2,lon[i]+length[i]/2,
                    lon[i]+length[i]/2,lon[i]-length[i]/2)
              sy<-c(lat[i]-length[i]/2,lat[i]-length[i]/2,
                    lat[i]+length[i]/2,lat[i]+length[i]/2)
        grid.polygon(x=unit(sx,'native'),
                     y=unit(sy,'native'),gp=gp)
   }
 }

#' Draw Pressure Contours
#'
#' Draw a pressure field with semi-transparent monochrome contours
#'
#' The idea is to mark high and low pressure regions, but not to
#'  generate much visual clutter. So this is a standard monochrome
#'  contour plot, except that contours close to the base value are transparent
#'
#' @export
#' @param mslp GSDF field of pressure (typically in Pa)
#' @param Options list of options - see \code{WeatherMap.set.option}
#' @return nothing - side effect only.
WeatherMap.draw.pressure<-function(mslp,Options) {

  M<-WeatherMap.rotate.pole(mslp,Options)
  lats<-M$dimensions[[GSDF.find.dimension(M,'lat')]]$values
  longs<-M$dimensions[[GSDF.find.dimension(M,'lon')]]$values
    # Need particular data format for contourLines
  if(lats[2]<lats[1] || longs[2]<longs[1] || max(longs) > 180 ) {
    if(lats[2]<lats[1]) lats<-rev(lats)
    if(longs[2]<longs[1]) longs<-rev(longs)
    longs[longs>180]<-longs[longs>180]-360
    longs<-sort(longs)
    M2<-M
    M2$dimensions[[GSDF.find.dimension(M,'lat')]]$values<-lats
    M2$dimensions[[GSDF.find.dimension(M,'lon')]]$values<-longs
    M<-GSDF.regrid.2d(M,M2)
  }
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
         gp<-gpar(col=rgb(0,0,0,tp),lwd=Options$mslp.lwd*lwd,lty=lt)
         grid.xspline(x=unit(lines[[i]]$x,'native'),
                    y=unit(lines[[i]]$y,'native'),
                    shape=1, 
                    gp=gp)
     }
  }
}
#' Fog of ignorance
#'
#' Draw the fog - semi-transparent grey
#'
#' As it's a semi-transparent area field we need to draw in
#'  pixel coordinates to rule out gaps or overlaps, so
#'   use an image.
#'
#' @export
#' @param fog GSDF field of fog thickness (0-1)
#' @param Options list of options - see \code{WeatherMap.set.option}
#' @return nothing - side effect only.
WeatherMap.draw.fog<-function(fog,Options) {

  fog<-WeatherMap.rotate.pole(fog,Options)
  # 100 colours for fog - fully transparent to mostly opaque
  n.colours<-100
  fog.colours<-rep(rgb(0,0,0,1),n.colours+1)
  fog.colours<-rgb(Options$fog.colour[1],Options$fog.colour[2],Options$fog.colour[3],
                   Options$fog.min.transparency*(seq(1,n.colours)-1)/(n.colours-1))
  fog.colours[n.colours+1]<-fog.colours[n.colours]

  plot.colours<-rep(fog.colours[[1]],length(fog$data))
  # fog$data should be on the range 0-1
  fog$data[]<-pmax(0,pmin(1,fog$data))
  lats<-rev(seq(Options$lat.min,Options$lat.max,Options$fog.resolution))
  longs<-seq(Options$lon.min,Options$lon.max,Options$fog.resolution)
  full.lats<-matrix(data=rep(lats,length(longs)),ncol=length(longs),byrow=F)
  full.longs<-matrix(data=rep(longs,length(lats)),ncol=length(longs),byrow=T)
  plot.colours<-GSDF.interpolate.ll(fog,as.vector(full.lats),as.vector(full.longs))
  plot.colours<-fog.colours[as.integer(plot.colours*n.colours)+1]
  dl<-longs[2]-longs[1]
    grid.raster(matrix(plot.colours, ncol=length(longs), byrow=F),
                x=unit((Options$lon.min+Options$lon.max)/2,'native'),
                y=unit((Options$lat.min+Options$lat.max)/2,'native'),
                width=unit(Options$lon.max-Options$lon.min,'native'),
                height=unit(Options$lat.max-Options$lat.min,'native'))
}

#' Haze to indicate altitude.
#' 
#' Draw the haze - semi transparent blue
#'
#' As it's a semi-transparent area field we need to draw in
#'  pixel coordinates to rule out gaps or overlaps, so
#'   use an image.
#'
#' @export
#' @param height height to simulate (hpa)
#' @param Options list of options - see \code{WeatherMap.set.option}
#' @return nothing - side effect only.
#'
#' 
WeatherMap.draw.haze<-function(height,Options) {

  hdx<-max(0,log(height)/log(1000))
  hdx<-pmax(0.1,pmin(1,hdx*(1+(runif(10000)-0.5)*0.2)))
  haze.colour<-rgb(0.8,0.8,1,1-hdx)
  grid.raster(matrix(haze.colour, ncol=100, byrow=F),
                x=unit((Options$lon.min+Options$lon.max)/2,'native'),
                y=unit((Options$lat.min+Options$lat.max)/2,'native'),
                width=unit(Options$lon.max-Options$lon.min,'native'),
                height=unit(Options$lat.max-Options$lat.min,'native'))

}

#' Precipitation
#'
#' Draw the precipitation rate - semi-transparent black
#'
#' As it's a semi-transparent area field we need to draw in
#'  pixel coordinates to rule out gaps or overlaps, so
#'   use an image.
#'
#' @export
#' @param fog GSDF field of fog thickness (0-1)
#' @param Options list of options - see \code{WeatherMap.set.option}
#' @return nothing - side effect only.
WeatherMap.draw.precipitation<-function(precip,Options) {

  precip<-WeatherMap.rotate.pole(precip,Options)
  # 100 colours  - fully transparent to mostly opaque
  n.colours<-100
  precip.colours<-rep(rgb(0,0,0,1),n.colours+1)
  precip.colours<-rgb(Options$precip.colour[1],Options$precip.colour[2],Options$precip.colour[3],
                  Options$precip.min.transparency*(seq(1,n.colours)-1)/(n.colours-1))
  precip.colours[n.colours+1]<-precip.colours[n.colours]

  # Remove precip below threshold
  precip$data[]<-pmax(precip$data,0) # some models still have negative precip
  w<-which(sqrt(precip$data)<Options$precip.threshold)
  precip$data[w]<-0
  # Scale data to range 0-1 
  precip$data[]<-pmax(0,pmin(1,(sqrt(precip$data)/Options$precip.range)))
  lats<-rev(seq(Options$lat.min,Options$lat.max,Options$precip.resolution))
  longs<-seq(Options$lon.min,Options$lon.max,Options$precip.resolution)
  full.lats<-matrix(data=rep(lats,length(longs)),ncol=length(longs),byrow=F)
  full.longs<-matrix(data=rep(longs,length(lats)),ncol=length(longs),byrow=T)
  plot.colours<-GSDF.interpolate.ll(precip,as.vector(full.lats),as.vector(full.longs))
  plot.colours<-precip.colours[as.integer(plot.colours*n.colours)+1]
  dl<-longs[2]-longs[1]
    grid.raster(matrix(plot.colours, ncol=length(longs), byrow=F),
                x=unit((Options$lon.min+Options$lon.max)/2,'native'),
                y=unit((Options$lat.min+Options$lat.max)/2,'native'),
                width=unit(Options$lon.max-Options$lon.min,'native'),
                height=unit(Options$lat.max-Options$lat.min,'native'))
}

#' Draw observations
#' 
#' Draw a set of observations - as points
#'
#' Only expected to be used with 20CR observations, so take observations
#'  in the prepbufr format generated by \code{GSDF.TWCR::TWCR.get.obs}.
#'  That is a data frame - we're only interested in columns V4 (lon) and
#'  V5 (lat).
#'
#' @export
#' @param obs data frame from \code{GSDF.TWCR::TWCR.get.obs} or equivalent.
#' @param Options list of options - see \code{WeatherMap.set.option}
#' @return nothing - side effect only.
WeatherMap.draw.obs<-function(obs,Options) {

  if(Options$pole.lon!=0 || Options$pole.lat!=90) {
	   l2<-GSDF.ll.to.rg(obs$Latitude,obs$Longitude,Options$pole.lat,Options$pole.lon)
	   obs$Longitude<-l2$lon
	   obs$Latitude<-l2$lat
  }
  if(length(obs$Latitude)<1) return()	
  gp<-gpar(col=Options$obs.colour,fill=Options$obs.colour)
  grid.points(x=unit(obs$Longitude,'native'),
              y=unit(obs$Latitude,'native'),
              size=unit(Options$obs.size,'native'),
              pch=20,gp=gp)
  
}

#' Draw label
#'
#' Add a label - usually the date
#'
#' Position and content set in options.
#'
#' @export
#' @param Options list of options - see \code{WeatherMap.set.option}
#' @return nothing - side effect only.
WeatherMap.draw.label<-function(Options) {
   label.gp<-gpar(family='Helvetica',font=1,col='black')
   tg<-textGrob(Options$label,x=unit(Options$label.xp,'npc'),
                              y=unit(Options$label.yp,'npc'),
                              hjust=1,vjust=0,
                              gp=label.gp)
   bg.gp<-gpar(col=Options$land.colour,fill=Options$land.colour)
   h<-heightDetails(tg)
   w<-widthDetails(tg)
   xp<-unit(Options$label.xp,'npc')
   yp<-unit(Options$label.yp,'npc')
   b<-unit(0.2,'char') # border
   grid.polygon(x=unit.c(xp+b,xp-w-b,xp-w-b,xp+b),
                y=unit.c(yp+h+b,yp+h+b,yp-b,yp-b),
                gp=bg.gp)
   grid.draw(tg)
}

# Rotate the pole of a scalar field if necessary
WeatherMap.rotate.pole<-function(field,Options) {
  
  W<-GSDF.field.to.pole(field,Options$pole.lat,Options$pole.lon)
  return(W)
}

#' Draw the map
#'
#' Draw everything as specified in the options
#'
#' Sets up a viewport and then calls all the drawing functions
#'  in sequence. Main user function for actual drawing.
#' The basic idea is that you set the options, load the data,
#'  and then call this function.
#'
#' @export
#' @param Options list of options - see \code{WeatherMap.set.option}
#' @param t.actual GSDF field of 2m temperature
#' @param t.normal GSDF field of temperature normals
#' @param icec GSDF field of sea-ice concentration
#' @param mslp GSDF field of mean-sea-level-pressure
#' @param precip GSDF field of precipitation rate
#' @param obs data frame from \code{GSDF.TWCR::TWCR.get.obs} or equivalent.
#' @param uwnd GSDF field of zonal wind
#' @param vwnd GSDF field of meridional wind
#' @param land list with components 'x' and 'y' - the coordinates
#' of a set of polygons bounding land regions. Suitable for drawing with
#' \code{grid.polygon}. If NULL, will be obtained from \code{WeatherMap.get.land}.
#' @param streamlines list of streamline data - see \code{WeatherMap.make.streamlines}
#' @param fog GSDF field of fog of ignorance.
#' @return nothing - side effect only.
WeatherMap.draw<-function(Options=NULL,t.actual=NULL,
                          t.normal=NULL,icec=NULL,
                          mslp=NULL,precip=NULL,obs=NULL,
                          uwnd=NULL,vwnd=NULL,land=NULL,
                          streamlines=NULL,fog=NULL,height=NULL) {

  if(is.null(Options)) Options<-WeatherMap.get.options()
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
  p<-WeatherMap.rectpoints(Options$precip.points,Options)
  ip<-WeatherMap.rectpoints(Options$ice.points,Options)
  if(Options$show.ice) {
    if(is.null(icec)) stop("No icec provided")
	WeatherMap.draw.ice(ip$lat,ip$lon,icec,Options)
  }
  WeatherMap.draw.land(land,Options,height=height)
  if(Options$show.obs) {
    if(is.null(obs)) stop("No obs. provided")
    WeatherMap.draw.obs(obs,Options)
  }
  if(Options$show.mslp) {
    if(is.null(mslp)) stop("No mslp provided")
    WeatherMap.draw.pressure(mslp,Options)
  }
  if(Options$show.precip) {
    if(is.null(precip)) stop("No precip provided")
    WeatherMap.draw.precipitation(precip,Options)
  }
  if(Options$show.wind) {
     if(!Options$show.temperature) {
       t.actual<-NULL
       t.normal<-NULL
     }
     if(is.null(streamlines)) {
       if(is.null(uwnd)) stop("No uwnd provided")
       if(is.null(vwnd)) stop("No vwnd provided")
       streamlines<-WeatherMap.make.streamlines(NULL,uwnd,vwnd,
                                                    t.actual,t.normal,
                                                    Options)
     }
     WeatherMap.draw.streamlines(streamlines,Options)
  } else if(Options$show.temperature) {
    stop("Can't show temperature but not wind")
  }
  if(Options$show.fog) {
     if(is.null(fog)) stop("No fog provided")
    WeatherMap.draw.fog(fog,Options)
  }
  
  if(Options$label != '') {
	WeatherMap.draw.label(Options)
  }
  upViewport()
}
