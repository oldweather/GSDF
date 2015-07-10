#' Interpolate points in a 2d field
#'
#' 2d bilinear interpolation.
#'
#' Interpolate values at a set of x,y points in a
#' 2d field. Note that x is the first dimension in the field
#' and y the second dimension whatever the names of those dimensions.
#' The field must have exactly 2 extended dimensions.
#'
#' @export
#' @param g GSDF data structure
#' @param x values to interpolate to in 1st dimension
#' @param y values to interpolate to in 2nd dimension
#' @param greedy boolean. If FALSE (default) NA values in the input field
#'      propagate to generate NA values in all output values adjacent to them.
#'      If true, output values are NA only if all adjacent grid points are NA.
#' @return numeric vector of interpolated values
#' @seealso \code{\link{Regrid2d}} regrid 1 field to match another
Interpolate2d<-function(g,x,y,greedy=FALSE) {
   if(length(x) != length(y)) stop("Mismatch in interpolation points")
   # Must have exactly 2 extended dimensions
   dims<-GSDF::ExtendedDimensions(g)
   if(length(dims)!=2) {
      stop('Must have exactly 2 extended dimensions')
   }
   grid<-array(data=g$data,dim=c(length(g$dimensions[[dims[1]]]$values),
                                 length(g$dimensions[[dims[2]]]$values)))
   interp<-GSDF::InterpSurface(list(x=g$dimensions[[dims[1]]]$values,
                                    y=g$dimensions[[dims[2]]]$values,z=grid),
                                    cbind(x,y),greedy=greedy)
   return(interp)
}

#' Interpolate lat:lon points in a 2d field
#'
#' 2d bilinear interpolation
#'
#' Basically the same as \code{\link{Interpolate2d}} except
#' that we check that the field has lat and lon as dimensions
#' and arrange them in the right order.
#'
#' @export
#' @param g GSDF data structure (must have lat and lon as dimensions)
#' @param lat latitudes to interpolate to
#' @param lon longitude values to interpolate to
#' @param full - TRUE => apply appropriate boundary conditions for full
#'   global lat:lon field (wrap longitudes and extrapolate near poles if required,
#'   FALSE => just interpolate (removes data outside range of g),
#'   NULL (default) => Guess which to do (is field ll and covering most of globe?).
#' @param greedy boolean. If FALSE (default) NA values in the input field
#'      propagate to generate NA values in all output values adjacent to them.
#'      If true, output values are NA only if all adjacent grid points are NA.
#' @return numeric vector of interpolated values
InterpolateLatLon<-function(g,lat,lon,full=NULL,greedy=FALSE) {
   dims<-GSDF::ExtendedDimensions(g)
   if(length(dims)!=2) {
      stop('Must have exactly 2 extended dimensions')
   }
   if(is.null(full)) full<-GSDF::IsFull(g)
   if(g$dimensions[[dims[1]]]$type=='lat') {
     if(g$dimensions[[dims[2]]]$type=='lon') {
       if(full) {
        lat<-GSDF::Regrid2dBoundaryConditions(g$dimensions[[dims[1]]]$values,
                                                  lat,g$dimensions[[dims[1]]]$type)
        lon<-GSDF::Regrid2dBoundaryConditions(g$dimensions[[dims[2]]]$values,
                                                  lon,g$dimensions[[dims[2]]]$type)
        g<-GSDF::PadLongitude(g)
      }
       return(GSDF::Interpolate2d(g,lat,lon,greedy=greedy))
     } else stop("Field has no longitudes")
   }
   if(g$dimensions[[dims[1]]]$type=='lon') {
     if(g$dimensions[[dims[2]]]$type=='lat') {
       if(full) {
        lat<-GSDF::Regrid2dBoundaryConditions(g$dimensions[[dims[2]]]$values,
                                                  lat,g$dimensions[[dims[2]]]$type)
        lon<-GSDF::Regrid2dBoundaryConditions(g$dimensions[[dims[1]]]$values,
                                                  lon,g$dimensions[[dims[1]]]$type)
        g<-GSDF::PadLongitude(g)
      }
       return(GSDF::Interpolate2d(g,lon,lat,greedy=greedy))
     } else stop("Field has no latitudes")
   }
   stop("Field is not lat:lon")
 }

#' 2d bilinear interpolation
#'
#' Identical to interp.surface from the fields package, except
#'  that it optionally ignores NA values in the source field.
#'
#' Useful for regridding sea-ice and SST fields, which otherwise
#'  end up as missing along coastlines.
#'
#' @export
#' @param obj A list with components x,y, and z in the same style as used
#'            by contour, persp, image etc. x and y are the X and Y grid
#'            values and z is a matrix with the corresponding values of the
#'            surface.
#' @param loc A matrix of (irregular) locations to interpolate. First
#'            column of loc is the X coordinates and second is the Y's.
#' @param greedy Boolean - if TRUE (default) NA are returned for regions of
#'          the obj\$z where any neighbour points are NA (same as version in fields).
#'          If FALSE, only return NA where all neighbouring points are NA.
#' @return An vector of interpolated values. 
InterpSurface<-function(obj,loc,greedy=FALSE) {
    x <- obj$x
    y <- obj$y
    z <- obj$z
    nx <- length(x)
    ny <- length(y)
    lx <- approx(x, 1:nx, loc[, 1])$y
    ly <- approx(y, 1:ny, loc[, 2])$y
    if(greedy) {
        lx <- approx(x, 1:nx, loc[, 1],rule=2)$y
        ly <- approx(y, 1:ny, loc[, 2],rule=2)$y
    }  
    lx1 <- floor(lx)
    ly1 <- floor(ly)
    ex <- lx - lx1
    ey <- ly - ly1
    ex[lx1 == nx] <- 1
    ey[ly1 == ny] <- 1
    lx1[lx1 == nx] <- nx - 1
    ly1[ly1 == ny] <- ny - 1
    if(!greedy) {
        return(z[cbind(lx1, ly1)] * (1 - ex) * (1 - ey) + z[cbind(lx1 + 
            1, ly1)] * ex * (1 - ey) + z[cbind(lx1, ly1 + 1)] * (1 - 
            ex) * ey + z[cbind(lx1 + 1, ly1 + 1)] * ex * ey)
    } else {
      result<-rep(0,length(lx))
      weight<-rep(0,length(lx))
      w<-which(!is.na(z[cbind(lx1, ly1)]))
      if(length(w)>0) {
        weight[w]<-weight[w]+(1 - ex[w]) * (1 - ey[w])
        result[w]<-result[w]+z[cbind(lx1, ly1)][w]* (1 - ex[w]) * (1 - ey[w])
      } 
      w<-which(!is.na(z[cbind(lx1+1, ly1)]))
      if(length(w)>0) {
        weight[w]<-weight[w]+ ex[w] * (1 - ey[w])
        result[w]<-result[w]+z[cbind(lx1+1, ly1)][w]* ex[w] * (1 - ey[w])
      }
      w<-which(!is.na(z[cbind(lx1, ly1+1)]))
      if(length(w)>0) {
        weight[w]<-weight[w]+(1 - ex[w]) * ey[w]
        result[w]<-result[w]+z[cbind(lx1, ly1+1)][w]* (1 - ex[w]) * ey[w]
      }
      w<-which(!is.na(z[cbind(lx1+1, ly1+1)]))
      if(length(w)>0) {
        weight[w]<-weight[w]+ ex[w] * ey[w]
        result[w]<-result[w]+z[cbind(lx1+1, ly1+1)][w]* ex[w] * ey[w]
      }
      w<-which(weight==0)
      if(length(w)>0) {
         is.na(result[w])<-TRUE
         result[-w]<-result[-w]/weight[-w]
      } else {
         result<-result/weight
      }
      return(result)
    }
}
