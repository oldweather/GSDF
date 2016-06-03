#' Allocate streamlines uniformly over the plot area
#'
#' Loosely based on Bridson's algorithm.
#'
#' Requires a characteristic length and a max attempt
#'  number - both set in options
#'
#' @export
#' @param u - x wind field (GSDF structure)
#' @param v - y wind field (GSDF structure)
#' @param Options list of options - see \code{WeatherMap.set.option}
#' @param previous streamlines data structure (see \code{WeatherMap.make.streamlines})
#'  from previous calculation. Defaults to NULL - start from scratch.
#' @return streamlines data structure (see \code{WeatherMap.make.streamlines})
WeatherMap.allocate.streamlines<-function(u,v,Options,previous=NULL) {

    x.range<-c(Options$lon.min,Options$lon.max)
    y.range<-c(Options$lat.min,Options$lat.max)
    view.scale<-max(diff(x.range)/360,diff(y.range)/180)
    r.min<-view.scale/Options$wind.vector.density
    max.attempt<-Options$bridson.max.attempt
     
    # Choose grid spacing close to r.min
    #  and which gives an integer number of points
    n.x<-as.integer(diff(x.range)/r.min)
    r.x<-diff(x.range)/n.x
    n.y<-as.integer(diff(y.range)/r.min)
    r.y<-diff(y.range)/n.y

    # Positions of point at each grid location
    columns.x<-seq(Options$lon.min+r.x/2,Options$lon.max,r.x)
    rows.y<-seq(Options$lat.min+r.y/2,Options$lat.max,r.y)
    grid.x<-rep(columns.x,n.y)
    grid.y<-as.vector(matrix(
                data=rep(rows.y,n.x),
		nrow=n.x,ncol=n.y,byrow=TRUE))
		
    # Which grid cells are unavailable - too close to an existing streamline
    filled<-integer(0)
    
    # set of seed points
    seed<-integer(0)

    # Order of addition
    order.added<-rep(NA,Options$wind.vector.max.number)
    
    # Allocate streamlines - start at top left
    x.c<-min(x.range) + 2*r.x
    y.c<-max(y.range) - 2*r.y

    seed<-as.integer(((y.c-Options$lat.min)/r.y)*n.x+(x.c-Options$lon.min)/r.x)

    streamlines<-list(x=array(dim=c(Options$wind.vector.max.number,Options$wind.vector.points)),
                      y=array(dim=c(Options$wind.vector.max.number,Options$wind.vector.points)),
                      status=rep(NA,Options$wind.vector.max.number))

    streamlines.count<-0                           
    while(length(seed)>0) {

      new.s<-WeatherMap.add.streamline(grid.x[seed[1]],grid.y[seed[1]],u,v,r.x,r.y,n.x,n.y,Options)
      if(length(intersect(filled,new.s$filled))==0) { # No overlaps, accept
        streamlines.count<-streamlines.count+1
        if(streamlines.count>Options$wind.vector.max.number) {
          stop(sprintf("Exceeded max. number of streamlines %d",Options$wind.vector.max.number))
        }
        streamlines$x[streamlines.count,]<-new.s$s$x[1,]
        streamlines$y[streamlines.count,]<-new.s$s$y[1,]
        streamlines$status[streamlines.count]<-1
        filled<-unique(c(filled,new.s$filled))
        seed<-unique(c(seed,new.s$seed))
        w<-which(seed %in% filled)
        seed<-seed[-w]
      }
      else {
        seed<-seed[-1] # Remove seed point
      }
      #if(streamlines.count>1000) break
    }

    streamlines$x<-streamlines$x[1:streamlines.count,] 
    streamlines$y<-streamlines$y[1:streamlines.count,] 
    streamlines$status<-streamlines$status[1:streamlines.count] 

    return(streamlines)
}

#' Try and add another streamline 
#'
#' Propagates a streamline from a given point, works out
#'  which allocation grid points it passes through, accepts
#'  the streamline if all those grid points are still empty.
#'
#' If the streamline is accepted, also work out what grid points
#'  are now filled, and what grid points should be activated as
#'  new seed points.
#'
#' @export
#' @param x - x location of potential new streamline
#' @param y - y location of potential new streamline
#' @param u - x wind field (GSDF structure)
#' @param v - y wind field (GSDF structure)
#' @param Options list of options - see \code{WeatherMap.set.option}
#' @return list containing s - new streamline, filled - vector of
#'   activation grid indices now filled & seed - vector of activation
#'   grid indices to make seed points.
WeatherMap.add.streamline<-function(x,y,u,v,r.x,r.y,n.x,n.y,Options) {
  new.s<-WeatherMap.propagate.streamlines(y,x,u,v,Options)
  
  filled<-numeric(0)
  for(i in seq(1,Options$wind.vector.points-1)) {
    x0<-as.integer((new.s[['x']][1,i]-Options$lon.min)/r.x)
    x1<-as.integer((new.s[['x']][1,i+1]-Options$lon.min)/r.x)
    y0<-as.integer((new.s[['y']][1,i]-Options$lat.min)/r.y)
    y1<-as.integer((new.s[['y']][1,i+1]-Options$lat.min)/r.y)
    filled<-unique(c(filled,WeatherMap.raytrace(x0,y0,x1,y1,n.x,n.y)))
  }
  # Add some padding around the lines
  for(i in seq(1,Options$wind.vector.padding)) {
    filled<-unique(c(filled,filled+1,filled-1,
                     filled+n.x,filled-n.x))
  }
  w<-which(filled>0 & filled<=n.x*n.y)
  filled<-filled[w]
  # Seed points go around the padding
  seed<-filled
  for(i in seq(1,Options$wind.vector.seed.width)) {
    seed<-unique(c(seed,seed+1,seed--1,
                     seed+n.x,seed-n.x))
  }
  w<-which(seed>0 & seed<=n.x*n.y)
  seed<-seed[w]
  w<-which(seed %in% filled)
  seed<-seed[-w]
  # Decimate for speed
  w<-seq(1,length(seed),Options$wind.vector.seed.decimate)
  seed<-seed[w]
  return(list(s=new.s,filled=filled,seed=seed))

}
  
#' Identify grid points touched by a line segment
#'
#' Uses a ray-tracing algorithm
#'
#' Takes inputs in integer grid index coordinates
#'
#' @export
#' @param x0 - x start location
#' @param y0 - y start location
#' @param x1 - x end location
#' @param y1 - y end location
#' @param nx - number of grid cells in x direction
#' @param ny - number of grid cells in y direction
#' @return filled - vector of grid indices touched
#'  by the line segment.
WeatherMap.raytrace<-function(x0,y0,x1,y1,nx,ny) {
    dx <- abs(x1 - x0)
    dy <- abs(y1 - y0)
    x <- x0
    y <- y0
    n <- 1 + dx + dy
    x_inc <- -1
    if(x1 > x0) x_inc <- 1
    y_inc <- -1
    if(y1 > y0) y_inc <- 1
    error <- dx - dy
    dx <- dx*2
    dy <- dy*2

    filled<-integer(length(n))

    for (i in seq(n,1))
    {
        if(y>0 && y<=ny &&
           x>0 && x<=nx) filled[i]<-y*nx+x

        if (error > 0)
        {
            x<-x+x_inc
            error <- error-dy
        }
        else
        {
            y<-y+y_inc
            error <- error+ dx;
        }
    }
    return(filled)
}


#' The bridson point allocation is often the rate-limiting step
#'
#' Run it on n cores - this means dividing the window into 2n
#' vertical slices, doing the n odd slices in parallel and then
#' the n even slices in parallel. Avoids doing two adjacent regions
#' at the same time, which would over-allocate points.
#'
#' Each vertical slice is expanded a bit horizontally to avoid
#' boundary artefacts when doing the calculation, but only points in
#' the slice are kept from the results.
#'
#' @export
#' @param Options list of options - see \code{WeatherMap.set.option}
#' @param previous list with elements 'lat' and lon' - set of points to
#'  start from. Defaults to NULL - start from scratch.
#' @param downstream list with elements 'lat' and lon' - downstream points from
#'  seeds in previous. Defaults to NULL - start from scratch.
#' @return list with elements 'lats' and lons'
WeatherMap.bridson.parallel<-function(Options,
                                      previous=NULL,
				      downstream=NULL) {
  
    # For spherical layout scale down x locations according to latitude
    # use as normal on scaled locations, then scale up again and throw out
    # any positions outside the lon.range.
    # Inefficient, but simple.
    if(Options$wrap.spherical && !is.null(previous)) {
      previous$lon<-previous$lon*cos(previous$lat*pi/180)
      if(!is.null(downstream)) downstream$lon<-downstream$lon*cos(downstream$lat*pi/180)
    }

  if(Options$cores==1) {
    return(WeatherMap.bridson(Options,previous,downstream))
  }
  # Allocate the ranges for each core
  slices<-list()
  slices$idx<-seq(1,Options$cores*2)
  slices$width<-(Options$lon.max-Options$lon.min)/length(slices$idx)
  slices$x.min<-Options$lon.min+(slices$idx-1)*slices$width
  slices$x.max<-Options$lon.min+slices$idx*slices$width
  slices$x.pad<-slices$width/4
  # Set up a function to be called in parallel for each slice
  bpf<-function(idx) {
    Options.slice<-Options
    Options.slice$lon.min<-max(Options$lon.min,slices$x.min[idx]-slices$x.pad)
    Options.slice$lon.max<-min(Options$lon.max,slices$x.max[idx]+slices$x.pad)
    previous.slice<-previous
    downstream.slice<-downstream
    if(!is.null(previous.slice)) {
      w<-which(previous$lon>=Options.slice$lon.min &
               previous$lon<Options.slice$lon.max)
      previous.slice$lon<-previous.slice$lon[w]
      downstream.slice$lon<-downstream.slice$lon[w,]
      previous.slice$lat<-previous.slice$lat[w]
      downstream.slice$lat<-downstream.slice$lat[w,]
      previous.slice$status<-previous.slice$status[w]
    }
    res<-WeatherMap.bridson(Options.slice,previous.slice,downstream.slice)
    w<-which(res$lon>=slices$x.min[idx] & res$lon<slices$x.max[idx])
    res$lon<-res$lon[w]
    res$lat<-res$lat[w]
    res$status<-res$status[w]
    return(res)
  }
  # Run for all the odd slices
  res.odd<-mclapply(seq(1,length(slices$idx),2),bpf,mc.cores=Options$cores)
  # Update the previous positions with the new ones
  if(is.null(previous)) {
    previous<-list(lat=numeric(0),lon=numeric(0),status=numeric(0))
  }
  s.count<-1
  for(slice in seq(1,length(slices$idx),2)) {
    w<-which(previous$lon>=slices$x.min[slice] &
             previous$lon<slices$x.max[slice])
    if(length(w)>0) {
      previous$lon<-previous$lon[-w]
      previous$lat<-previous$lat[-w]
      previous$status<-previous$status[-w]
    }
    previous$lon<-c(previous$lon,res.odd[[s.count]]$lon)
    previous$lat<-c(previous$lat,res.odd[[s.count]]$lat)
    previous$status<-c(previous$status,res.odd[[s.count]]$status)
    s.count<-s.count+1
  }
  # Run for all the even slices
  res.even<-mclapply(seq(2,length(slices$idx),2),bpf,mc.cores=Options$cores)
  # Update the previous positions with the new ones
  s.count<-1
  for(slice in seq(2,length(slices$idx),2)) {
    w<-which(previous$lon>=slices$x.min[slice] &
             previous$lon<slices$x.max[slice])
    if(length(w)>0) {
      previous$lon<-previous$lon[-w]
      previous$lat<-previous$lat[-w]
      previous$status<-previous$status[-w]
    }
    previous$lon<-c(previous$lon,res.even[[s.count]]$lon)
    previous$lat<-c(previous$lat,res.even[[s.count]]$lat)
    previous$status<-c(previous$status,res.even[[s.count]]$status)
    s.count<-s.count+1
  }
  # Scale-out for spherical case
    if(Options$wrap.spherical) {
      previous$lon<-previous$lon/cos(previous$lat*pi/180)
      w<-which(previous$lon<Options$vp.lon.max & previous$lon>=Options$vp.lon.min)
      previous$lon<-previous$lon[w]
      previous$lat<-previous$lat[w]
      previous$status<-previous$status[w]
     }

  return(previous)
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
#' @param u GSDF field of zonal wind (m/s)
#' @param v GSDF field of meridional wind (m/s)
#' @param Options list of options - see \code{WeatherMap.set.option}
#' @return streamlines data structure (see \code{WeatherMap.make.streamlines})
WeatherMap.propagate.streamlines<-function(lat,lon,u,v,Options) {

    streamlets<-list()
    streamlets[['x']]<-array(dim=c(length(lon),Options$wind.vector.points))
    streamlets[['x']][,1]<-lon
    streamlets[['y']]<-array(dim=c(length(lat),Options$wind.vector.points))
    streamlets[['y']][,1]<-lat
    view.scale<-max((Options$lon.max-Options$lon.min)/360,(Options$lat.max-Options$lat.min)/180)
    for(i in seq(1,Options$wind.vector.points)) {
		up<-GSDF.interpolate.ll(u,lat,lon,greedy=Options$greedy)
                if(Options$wrap.spherical) up<-up/cos(lat*pi/180)
		vp<-GSDF.interpolate.ll(v,lat,lon,greedy=Options$greedy)
		m<-sqrt(up**2+vp**2)
		direction<-atan2(vp,up)
		streamlets[['x']][,i]<-lon
		streamlets[['y']][,i]<-lat        
		lon<-lon+cos(direction)*(m)*
                view.scale*
		Options$wind.vector.scale/Options$wind.vector.points
		lat<-lat+sin(direction)*(m)*
                view.scale*
		Options$wind.vector.scale/Options$wind.vector.points
    }
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
   alpha<-c(10,50,150,255)[min(status,4)]
   colour<-rgb(rgb[1],rgb[2],rgb[3],alpha,maxColorValue = 255)
   colourb<-rgb(rgb[1],rgb[2],rgb[3],0,maxColorValue = 255)
   #if(Options$wrap.spherical) status<-0.01 # polygon fill in this case
   if(Options$wrap.spherical) {
      return(gpar(col=colour,fill=colour,lwd=0))
    } else {
      return(gpar(col=colour,fill=colour,lwd=Options$wind.vector.lwd))
    }
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
   ds.lat<-numeric(0)
   ds.lon<-numeric(0)
   initial=FALSE # starting from scratch
   if(is.null(s)) initial=TRUE
   if(!is.null(s)) {
      # Move the vectors along at the speed of the wind (*wind.vector.move.scale)
      # Assumes frames are hourly (0.033 converts m/s to degrees/hr)
      move.scale<-0.033*Options$wind.vector.points/Options$wind.vector.scale
      move.scale<-move.scale*Options$wind.vector.move.scale*view.scale
      lats<-s[['y']][,1]+(s[['y']][,2]-s[['y']][,1])*move.scale
      ds.lat<-(s[['y']]+(s[['y']][,2]-s[['y']][,1])*move.scale)[,2:Options$wind.vector.points]
      longs<-s[['x']][,1]+(s[['x']][,2]-s[['x']][,1])*move.scale
      ds.lon<-(s[['x']]+(s[['x']][,2]-s[['x']][,1])*move.scale)[,2:Options$wind.vector.points]
      if(Options$wrap.spherical) {
         w<-which(longs>Options$vp.lon.max)
	 if(length(w)>0) longs[w]<-longs[w]-360
      }
      status<-s[['status']]+1
      # Update plot status and remove expired ones or those outside the frame
         w<-which(is.na(lats) | is.na(longs))
         if(length(w)>0) {
           lats<-lats[-w]
           longs<-longs[-w]
           status<-status[-w]
           ds.lat<-ds.lat[-w,]
           ds.lon<-ds.lon[-w,]
         }
         w<-which(ds.lat>Options$lat.max | ds.lat<Options$lat.min |
                  ds.lon>Options$lon.max | ds.lon<Options$lon.min)
         if(length(w)>0) {
            is.na(ds.lat[w])<-TRUE
            is.na(ds.lon[w])<-TRUE
         }
  }
   # Update positions and Roll-out the streamlines
   if(!Options$jitter) set.seed(27)
   s<-WeatherMap.propagate.streamlines(lats,longs,status,u,v,t,t.c,Options)
   ds.lat<-s$y[,2:Options$wind.vector.points]
   ds.lon<-s$x[,2:Options$wind.vector.points]
         w<-which(ds.lat>Options$lat.max | ds.lat<Options$lat.min |
                  ds.lon>Options$lon.max | ds.lon<Options$lon.min)
         if(length(w)>0) {
            is.na(ds.lat[w])<-TRUE
            is.na(ds.lon[w])<-TRUE
         }   
   p<-WeatherMap.bridson.parallel(Options,previous=list(lat=s$y[,1],lon=s$x[,1],status=s$status),
                                          downstream=list(lat=ds.lat,lon=ds.lon))
   s<-WeatherMap.propagate.streamlines(p$lat,p$lon,p$status,u,v,t,t.c,Options)
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
          tp
          gp<-WeatherMap.streamline.getGC(level,transparency=tp,
                                          status=s[['status']][i],Options)
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


