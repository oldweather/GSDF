#' Print a GSDF structure
#' 
#' \code{GSDF.print} prints a condensed version of the structure
#'  provided as its first argument.
#'
#' @export
#' @param g GSDF structure to print
print.GSDF<-function(g) {
   for(i in seq_along(g$dimensions)) {
      print(sprintf("%s %d",g$dimensions[[i]]$type,
                        length(g$dimensions[[i]]$values)))
      print(g$dimensions[[i]]$values)
      if(length(g$dimensions[[i]]$meta)>0) {
          print(g$dimensions[[i]]$meta)
      }
   }
   if(length(g$meta)>0) print(g$meta)
}

#' GSDF is full field
#'
#' Is field lat:lon and global in scope?
#'
#' Interpolation can shrink a field round the edges,
#' this is not OK for global fields - we want to preserve global coverage
#' so identify those and treat then specially.
#' Also this deals with wrap-arounds in longitude (0:360 v -180:180).
#'
#' @param g.grid Field to test
#' @return logical: TRUE if full, FALSE otherwise
IsFull<-function(g.grid) {
   dims.grid<-GSDF::ExtendedDimensions(g.grid)
   if(length(dims.grid)!=2) return(FALSE)
   if( (g.grid$dimensions[[dims.grid[1]]]$type=='lat' &&
          g.grid$dimensions[[dims.grid[2]]]$type=='lon' &&
          (max(g.grid$dimensions[[dims.grid[1]]]$values)-
          min(g.grid$dimensions[[dims.grid[1]]]$values)>160) &&
          (max(g.grid$dimensions[[dims.grid[2]]]$values)-
          min(g.grid$dimensions[[dims.grid[2]]]$values)>340)) ||
          (g.grid$dimensions[[dims.grid[1]]]$type=='lon' &&
          g.grid$dimensions[[dims.grid[2]]]$type=='lat' &&
          (max(g.grid$dimensions[[dims.grid[1]]]$values)-
          min(g.grid$dimensions[[dims.grid[1]]]$values)>340) &&
          (max(g.grid$dimensions[[dims.grid[2]]]$values)-
          min(g.grid$dimensions[[dims.grid[2]]]$values)>160))) return(TRUE)
      else return(FALSE)
 }   


# Expand a field in longitude - copying the first column to the end
#  and the last column to the beginning (or rows, if apropriate)
# Allows correct interpolation of points beyond the last row or before
#  the first row.
# Internal function, needed by InterpolateLatLon
# Field to be expanded must have only 2 extended dimensions.
# Assumes longitudes are in ascending order.
PadLongitude<-function(g) {
  result<-g
  d<-GSDF::ExtendedDimensions(g)
  if(length(d)!=2) stop('Wrong dimensions in pad.longitude')
  a<-array(data<-as.vector(g$data),dim=c(length(g$dimensions[[d[1]]]$values),
                                         length(g$dimensions[[d[2]]]$values)))
  if(g$dimensions[[d[1]]]$type=='lon') {
    l<-length(g$dimensions[[d[1]]]$values)
    a<-rbind(a[l,],a,a[1,])
    nd<-dim(g$data)
    nd[d]<-nd[d]+2
    result$data<-array(data<-as.vector(a),dim=nd)
    result$dimensions[[d[1]]]$values<-c(g$dimensions[[d[1]]]$values[l]-360,
                                        g$dimensions[[d[1]]]$values,
                                        g$dimensions[[d[1]]]$values[1]+360)
    return(result)
  }
  if(g$dimensions[[d[2]]]$type=='lon') {
    l<-length(g$dimensions[[d[2]]]$values)
    a<-cbind(a[,l],a,a[,1])
    nd<-dim(g$data)
    nd[d]<-nd[d]+2
    result$data<-array(data<-as.vector(a),dim=nd)
    result$dimensions[[d[2]]]$values<-c(g$dimensions[[d[2]]]$values[l]-360,
                                        g$dimensions[[d[2]]]$values,
                                        g$dimensions[[d[2]]]$values[1]+360)
    return(result)
  }
  stop('Field has no longitudes to pad')
}

