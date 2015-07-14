# Plot a GSDF record

#' Map a field with only 2 dimensions
#' 
#' Make an imagemap or contourplot of a 2-dimensional GSDF field
#'
#' Plots a 2-dimensional slice - if it's a long::lat plot also
#' show continent outlines by default.
#' Requires the field to have exactly two dimensions with length
#'  greater than 1. (So a 10x20 array is fine, as is 20x1x200x1,
#'  but not 10x20x5).
#'
#' @export
#' @param g GSDF (with 2 extended dimensions)
#' @param dimensions 2-element vector; indexes of the 2 extended dimensions
#'  first will be the x axis, second the y.
#' @param palette 'diverging', 'sequential', or 'greyscale'
#' @param ncols Number of colours to use (overridden by levels)
#' @param levels vector giving colour boundaries - seq(-10,10,1) will
#'  paint everything between -10 and -9 one colour, -9 and -8 the next and so on.
#' @param draw If true, draw the map; if false return a grob (to be 'print'ed later).
#' @param continents Draw 'high' or 'low' resolution continental outlines. If NULL (default)
#'  draw low resolution, but only if plot is long::lat. If 'none' don't draw any.
#' @param y.range Range to show on y axis - units of relevant dimension. If NULL 
#' (default) use range of data.
#' @param x.range Range to show on x axis - units of relevant dimension. If NULL 
#' (default) use range of data.
#' @param y.scale Tic positions on y axis - units of relevant dimension. If NULL 
#' (default) generate automatically.
#' @param x.scale Tic positions on x axis - units of relevant dimension. If NULL 
#' (default) generate automatically.
#' @param y.label Text label for y axis. If NULL (default) generate automatically.
#' @param x.label Text label for x axis. If NULL (default) generate automatically.
#' @param contour Draw contour plot.
#' @param region Draw image map.
#' @param aspect Control aspect ratio - see ?contourplot. If NULL (default) set
#' to 'iso' for lat::lon and lon::lat and to 'fill' otherwise.
#' @return Grob containing map if draw=FALSE, nothing otherwise.
Plot2d<-function(g,dimensions=NULL,
                      palette="diverging",ncols=17,levels=NULL,
                      draw=TRUE,continents=NULL,
                      y.range=NULL,y.scale=NULL,y.label=NULL,
                      x.range=NULL,x.scale=NULL,x.label=NULL,
                      contour=FALSE,region=TRUE,pretty=FALSE,
                      aspect=NULL) {
   # Must have only 2 extended dimensions
   dims<-numeric(0)
   for(d in seq_along(g$dimensions)) {
      if(length(g$dimensions[[d]]$values)>1) dims<-c(dims,d)
   }
   if(length(dims)!=2) {
      stop('Must have exactly 2 extended dimensions')
   }
   x.coords<-numeric(0)
   y.coords<-numeric(0)
   if(!is.null(dimensions)) {
     if(length(dimensions)!=2) {
        stop('Must specify exactly 2 dimensions')
     }
     for(d in c(1,2)) {
        if(dimensions[d]!=dims[1]&&dimensions[d]!=dims[2]) {
           stop(sprintf("Invalid dimension %d",dimensions[d]))
        }
     }
   } else {
     if(g$dimensions[[dims[2]]]$type=='lon') {
        dimensions<-c(dims[2],dims[1])
     } else {
        dimensions<-c(dims[1],dims[2])
     }
   }
   x.coords<-GSDF::RollDimensions(g,dimensions[1],dimensions[2])
   if(g$dimensions[[dimensions[1]]]$type=='time') x.coords=chron(x.coords)
   y.coords<-GSDF::RollDimensions(g,dimensions[2],dimensions[1])
   if(g$dimensions[[dimensions[2]]]$type=='time') y.coords=chron(y.coords)
   if(is.null(continents) && 
         g$dimensions[[dimensions[1]]]$type=='lon' &&
         g$dimensions[[dimensions[2]]]$type=='lat') {
      continents='low'
   }
   if(is.null(x.label)) {
      if(g$dimensions[[dimensions[1]]]$type=='lon') x.label='Longitude'
      if(g$dimensions[[dimensions[1]]]$type=='lat') x.label='Latitude'
      if(g$dimensions[[dimensions[1]]]$type=='height') x.label='Height (hPa)'
   }
   if(is.null(x.label)) x.label=''
   if(is.null(y.label)) {
      if(g$dimensions[[dimensions[2]]]$type=='lon') y.label='Longitude'
      if(g$dimensions[[dimensions[2]]]$type=='lat') y.label='Latitude'
      if(g$dimensions[[dimensions[2]]]$type=='height') y.label='Height (hPa)'
   }
   if(is.null(y.label)) y.label=''
   if(is.null(aspect)) {
      if((g$dimensions[[dimensions[1]]]$type=='lat' ||
          g$dimensions[[dimensions[1]]]$type=='lon') &&
         (g$dimensions[[dimensions[2]]]$type=='lat' ||
          g$dimensions[[dimensions[2]]]$type=='lon')) aspect='iso'
      else aspect='fill'
   }
   pole.lat<-NULL
   if(!is.null(g$meta$pole.lat)) pole.lat<-g$meta$pole.lat
   pole.lon<-NULL
   if(!is.null(g$meta$pole.lon)) pole.lon<-g$meta$pole.lon
    
   PlotMap(as.vector(g$data),x.coords,y.coords,
                  palette=palette,ncols=ncols,levels=levels,
                  draw=draw,continents=continents,
                  y.range=y.range,y.scale=y.scale,y.label=y.label,
                  x.range=x.range,x.scale=x.scale,x.label=x.label,
                  pole.lat=pole.lat,pole.lon=pole.lon,
                  contour=contour,region=region,pretty=pretty,
                  aspect=aspect)
}

PlotMap <-function(data,x.coords,y.coords,
                  palette="diverging",ncols=17,levels=NULL,
                  draw=TRUE,continents=NULL,
                  y.range=NULL,y.scale=NULL,y.label='',
                  x.range=NULL,x.scale=NULL,x.label='',
                  pole.lat=NULL,pole.lon=NULL,
                  contour=FALSE,region=TRUE,pretty=FALSE,
                  aspect='fill') {

    if(is.null(y.scale)) y.scale<-pretty(y.coords)
    if(is.null(x.scale)) x.scale<-pretty(x.coords)
  	    
    # Get continental outline data
    continent.outlines <- maps::map('world',interior=FALSE,plot=FALSE)
    is.na(continent.outlines$x[8836])=T  # Remove Antarctic bug
    if(!is.null(continents) && tolower(continents)=='high') {
       continent.outlines <- maps::map('worldHires',interior=FALSE,plot=FALSE)
    }  
    # If w are using a rotated pole, rotate the continent outlines to match.
    if(!is.null(pole.lat) && !is.null(pole.lon)) {
          nl<-GSDF::RotateLatLon(continent.outlines$y,
                                     continent.outlines$x,
                                     pole.lat,pole.lon)
          continent.outlines$y<-nl$lat
          continent.outlines$x<-nl$lon
        # Remove any polygons which are distorted by the change in pole
          w<-which(abs(diff(continent.outlines$x))>50)
          is.na(continent.outlines$x[w+1])<-T
    }
    continent.outlines$x<-c(continent.outlines$x,NA,
                            continent.outlines$x+360)
    continent.outlines$y<-c(continent.outlines$y,NA,
                            continent.outlines$y)
        
    # Set default ranges
    if(is.null(x.range)) x.range<-c(min(x.coords,na.rm=T)-0.01,
                                  max(x.coords,na.rm=T)+0.01)
    if(is.null(y.range)) y.range<-c(min(y.coords,na.rm=T)-0.01,
                                  max(y.coords,na.rm=T)+0.01)


    mappanel <- function(x,y,...) {
        lattice::panel.contourplot(x,y,...)
        if(!is.null(continents) &&
             (tolower(continents)=='high' || tolower(continents)=='low')) {
            lattice::llines(continent.outlines$x,
                   continent.outlines$y,col="black")
        }
    }

  c<-0
  if(length(levels)>1) {
      ncols<-length(levels)
      c<-lattice::contourplot(data ~ x.coords * y.coords,
         ylab=y.label,xlab=x.label,
         xlim=x.range,
         ylim=y.range,
         scales=list(x=list(at=x.scale),
                     y=list(at=y.scale)
                    ),
         panel=mappanel,
         aspect=aspect,
         region=region,
         contour=contour,
         pretty=pretty,
         cuts=ncols-1,
         at=levels,
         col.regions=switch(palette,
             diverging=GetPaletteDiverging(ncols),
             sequential=GetPaletteSequential(ncols),
             greyscale=GetPaletteGreyscale(ncols))
      )
  }
  else {
      c<-lattice::contourplot(data ~ x.coords * y.coords,
         ylab=y.label,xlab=x.label,
         xlim=x.range,
         ylim=y.range,
         scales=list(x=list(at=x.scale),
                     y=list(at=y.scale)
                    ),
         panel=mappanel,
         aspect=aspect,
         region=region,
         contour=contour,
         pretty=pretty,
         cuts=ncols-1,
         col.regions=switch(palette,
             diverging=GetPaletteDiverging(ncols),
             sequential=GetPaletteSequential(ncols),
             greyscale=GetPaletteGreyscale(ncols))
      )
  }
  if(draw) { print(c) }
  else {return(c)}

}
    

# Use the Light and Bartlein sequential and diverging colour schemes
GetPaletteDiverging<-colorRampPalette(
       c(
        rgb( 36,  0,   216 ,maxColorValue = 255 ),
        rgb( 24,  28,  247 ,maxColorValue = 255 ),
        rgb( 40,  87,  255 ,maxColorValue = 255 ),
        rgb( 61,  135, 255 ,maxColorValue = 255 ),
        rgb( 86,  176, 255 ,maxColorValue = 255 ),
        rgb( 117, 211, 255 ,maxColorValue = 255 ),
        rgb( 153, 234, 255 ,maxColorValue = 255 ),
        rgb( 188, 249, 255 ,maxColorValue = 255 ),
        rgb( 234, 255, 255, maxColorValue = 255 ),
        rgb( 255, 255, 234, maxColorValue = 255 ),
        rgb( 255, 241, 188 ,maxColorValue = 255 ),
        rgb( 255, 214, 153 ,maxColorValue = 255 ),
        rgb( 255, 172, 117 ,maxColorValue = 255 ),
        rgb( 255, 120,  86 ,maxColorValue = 255 ),
        rgb( 255,  61,  61 ,maxColorValue = 255 ),
        rgb( 247,  39,  53 ,maxColorValue = 255 ),
        rgb( 216,  21,  47 ,maxColorValue = 255 ),
        rgb( 165,   0,  33 ,maxColorValue = 255 )
       )
   )

GetPaletteSequential<-colorRampPalette(
       c(
         rgb( 229, 255, 255 ,maxColorValue = 255 ),
         rgb( 204, 250, 255 ,maxColorValue = 255 ),
         rgb( 178, 242, 255 ,maxColorValue = 255 ),
         rgb( 153, 229, 255 ,maxColorValue = 255 ),
         rgb( 127, 212, 255 ,maxColorValue = 255 ),
         rgb( 101, 191, 255 ,maxColorValue = 255 ),
         rgb( 76,  165, 255 ,maxColorValue = 255 ),
         rgb( 50,  136, 255 ,maxColorValue = 255 ),
         rgb( 25,  101, 255 ,maxColorValue = 255 ),
         rgb( 0,   63,  255 ,maxColorValue = 255 )
        )
   )
   
GetPaletteGreyscale<-colorRampPalette(
       c(
         rgb( 192, 192, 192 ,maxColorValue = 255 ),
         rgb( 160, 160, 160 ,maxColorValue = 255 ),
         rgb( 128, 128, 128 ,maxColorValue = 255 ),
         rgb(  96,  96,  96 ,maxColorValue = 255 ),
         rgb(  64,  64,  64 ,maxColorValue = 255 ),
         rgb(  32,  32,  32 ,maxColorValue = 255 ),
         rgb(   0,   0,   0 ,maxColorValue = 255 )
        )
   )

#' Pair of 2d maps, one above the other
#' 
#' Two maps from plot.2d, one above the other
#'
#' Exactly the same as GSDF.plot.2d, except that it takes 2 fields
#' and plots them both. Same arguments as the simgle plotter, except
#' that 'draw' is ignored - only plots, can't return a grob.
#'
#' @export
#' @param g1 GSDF (with 2 extended dimensions)
#' @param g2 GSDF (with 2 extended dimensions)
#' @param ... see GSDF.plot.2d
PPlot2d<-function(g1,g2,...) {
  grid::grid.newpage()
    grid::pushViewport(viewport(x=0,y=0.5,width=1,height=0.5,just=c('left','bottom')))
     m1<-GSDF::Plot2d(g1,...,draw=F)
     print(m1,newpage=F)
  grid::popViewport()
  grid::pushViewport(viewport(x=0,y=0,width=1,height=0.5,just=c('left','bottom')))
     m2<-GSDF::Plot2d(g2,...,draw=F)
     print(m2,newpage=F)
  grid::popViewport()
}
