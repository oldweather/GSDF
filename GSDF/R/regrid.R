#' Regrid a 2d field to match another 2d field
#'
#' Makes the first field  have the same grid as the second
#'
#' Both fields must have exactly 2 extended dimensions, and those dimensions
#' must have the same labels (e.g. both lat,long, or both lat,height).
#'
#' @export
#' @param g 2d GSDF field
#' @param g.grid 2d GSDF field with same set of dimensions
#' @param full - TRUE => apply appropriate boundary conditions for full
#'   global lat:lon field (wrap longitudes and extrapolate near poles if required,
#'   FALSE => just interpolate (removes data outside range of g.grid),
#'   NULL (default) => Guess which to do (is field ll and covering most of globe?).
#' @param greedy boolean. If FALSE (default) NA values in the input field
#'      propagate to generate NA values in all output values adjacent to them.
#'      If true, output values are NA only if all adjacent grid points are NA.
#' @return GSDF field with data from g on grid of g.grid
Regrid2d<-function(g,g.grid,full=NULL,greedy=FALSE) {
   dims<-GSDF::ExtendedDimensions(g)
   if(length(dims)!=2) {
      stop('Field must have exactly 2 extended dimensions')
   }
   dims.grid<-GSDF::ExtendedDimensions(g.grid)
   if(length(dims.grid)!=2) {
      stop('Grid field must have exactly 2 extended dimensions')
   }
   if(is.null(full)) full<-GSDF::IsFull(g.grid)
   g.pole.lat<-90
   if(!is.null(g$meta$pole.lat)) g.pole.lat<-g$meta$pole.lat
   g.pole.lon<-180
   if(!is.null(g$meta$pole.lon)) g.pole.lon<-g$meta$pole.lon
   g.grid.pole.lat<-90
   if(!is.null(g.grid$meta$pole.lat)) g.grid.pole.lat<-g.grid$meta$pole.lat
   g.grid.pole.lon<-180
   if(!is.null(g.grid$meta$pole.lon)) g.grid.pole.lon<-g.grid$meta$pole.lon
   result<-g
   result$dimensions<-g.grid$dimensions
   if(g$dimensions[[dims[1]]]$type == g.grid$dimensions[[dims.grid[1]]]$type &&
      g$dimensions[[dims[2]]]$type == g.grid$dimensions[[dims.grid[2]]]$type) {
      new.x<-GSDF::RollDimensions(g.grid,dims[1],dims[2])
      new.y<-GSDF::RollDimensions(g.grid,dims[2],dims[1])
      if(g.pole.lat != g.grid.pole.lat || g.pole.lon != g.grid.pole.lon) {
        result$meta$pole.lat<-g.grid.pole.lat
        result$meta$pole.lon<-g.grid.pole.lon
        if(g$dimensions[[dims[1]]]$type=='lat' &&
           g$dimensions[[dims[2]]]$type=='lon') {
           unrotated<-GSDF::RotateLatLon(new.x,new.y,
                                    g.grid.pole.lat,g.grid.pole.lon)
           rerotated<-GSDF::UnrotateLatLon(unrotated$lat,unrotated$lon,
                                    g.pole.lat,g.pole.lon)
           new.x<-rerotated$lat
           new.y<-rerotated$lon
        }
        if(g$dimensions[[dims[1]]]$type=='lon' &&
           g$dimensions[[dims[2]]]$type=='lat') {
           unrotated<-GSDF::RotateLatLon(new.y,new.x,
                                    g.grid.pole.lat,g.grid.pole.lon)
           rerotated<-GSDF::UnrotateLatLon(unrotated$lat,unrotated$lon,
                                    g.pole.lat,g.pole.lon)
           new.x<-rerotated$lon
           new.y<-rerotated$lat
        }
        if(g$dimensions[[dims[1]]]$type!='lon' && g$dimensions[[dims[1]]]$type!='lat') {
          stop('Regriding non-lat-lon grids with different poles is not supported')
        }
      }   
      if(full) {
        new.x<-Regrid2dBoundaryConditions(g$dimensions[[dims[1]]]$values,
                                                  new.x,g$dimensions[[dims[1]]]$type)
        new.y<-Regrid2dBoundaryConditions(g$dimensions[[dims[2]]]$values,
                                                  new.y,g$dimensions[[dims[2]]]$type)
        g<-GSDF::PadLongitude(g)
      }  
      result$data<-array(data=GSDF::Interpolate2d(g,new.x,new.y,greedy=greedy),
                         dim=dim(g.grid$data))
      return(result)
   }
   if(g$dimensions[[dims[1]]]$type == g.grid$dimensions[[dims.grid[2]]]$type &&
      g$dimensions[[dims[2]]]$type == g.grid$dimensions[[dims.grid[1]]]$type) {
      new.x<-GSDF::RollDimensions(g.grid,dims[2],dims[1])
      new.y<-GSDF::RollDimensions(g.grid,dims[1],dims[2])
      if(g.pole.lat != g.grid.pole.lat || g.pole.lon != g.grid.pole.lon) {
        result$meta$pole.lat<-g.grid.pole.lat
        result$meta$pole.lon<-g.grid.pole.lon
        if(g$dimensions[[dims[1]]]$type=='lon' &&
           g$dimensions[[dims[2]]]$type=='lat') {
           unrotated<-GSDF::RotateLatLon(new.y,new.x,
                                    g.grid.pole.lat,g.grid.pole.lon)
           rerotated<-GSDF::UnrotateLatLon(unrotated$lat,unrotated$lon,
                                    g.pole.lat,g.pole.lon)
           new.x<-rerotated$lat
           new.y<-rerotated$lon
        }
        if(g$dimensions[[dims[1]]]$type=='lat' &&
           g$dimensions[[dims[2]]]$type=='lon') {
           unrotated<-GSDF::RotateLatLon(new.x,new.y,
                                    g.grid.pole.lat,g.grid.pole.lon)
           rerotated<-GSDF::UnrotateLatLon(unrotated$lat,unrotated$lon,
                                    g.pole.lat,g.pole.lon)
           new.x<-rerotated$lon
           new.y<-rerotated$lat
        }
        if(g$dimensions[[dims[1]]]$type!='lon' && g$dimensions[[dims[1]]]$type!='lat') {
          stop('Regriding non-lat-lon grids with duifferent poles is not supported')
        }
      }
      if(full) {
        new.x<-Regrid2dBoundaryConditions(g$dimensions[[dims[2]]]$values,
                                                  new.x,g$dimensions[[dims[1]]]$type)
        new.y<-Regrid2dBoundaryConditions(g$dimensions[[dims[1]]]$values,
                                                  new.y,g$dimensions[[dims[2]]]$type)
        g<-GSDF::PadLongitude(g)
      }  
      result$data<-array(data=GSDF::Interpolate2d(g,new.x,new.y,greedy=greedy),
                        dim=dim(g.grid$data))
      return(result)
   }
   stop("Incompatable fields - different dimensions")
}

#' Apply boundary conditions to a field for interpolation
#'
#' Makes regrid.2d work correctly for globaly complete fields
#'
#' When the 'to' grid has higher resolution than the 'from'
#' grid, some 'from' points will be outside the span of the 'to'
#' points. Tweak the 'to' points to put them inside - effectively
#' this does constant extrapolation outside the bounds.
#' Also fixes the -180-180 or 0-360 inconsistency in longitude
#'  if 'old' and 'new' are different in this respect.
#'
#' @param old - 'from' points in 1-dimension
#' @param new - 'to' points
#' @param type - 'lat', 'lon' or other, apply periodic bc if 'lon'
#' @return revised 'new' points
Regrid2dBoundaryConditions<-function(old,new,type) {
   if(type=='lon') {
      w<-which(new > max(old) & new-360 > min(old))
      if(length(w)>0) new[w]<-new[w]-360
      w<-which(new < min(old) & new+360 < max(old))
      if(length(w)>0) new[w]<-new[w]+360
    } else {
       w<-which(new > max(old))
       if(length(w)>0) new[w]<-max(old)
       w<-which(new < min(old))
       if(length(w)>0) new[w]<-min(old)
     }
    return(new)
}

