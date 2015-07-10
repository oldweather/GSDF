#' Find the index of a named dimension
#'
#' Identify the dimension that contains 'lat', 'time' etc.
#' 
#' If the first dimension in g is 'lat', the second 'time', and 
#' the third 'lon', \code{FindDimensions(g,'lon')} will
#' return 3.
#'
#' @export
#' @param g GSDF structure
#' @param label text string, 'lat','lon', 'ens' or 'time'
#' @return Integer number of requested dimension, or NULL if not present.
FindDimension<-function(g,label) {
  for(d in seq_along(g$dimensions)) {
     if(g$dimensions[[d]]$type==label) return(d)
  }
  return(NULL)
}  

#' Roll-out a pair of dimensions
#'
#' Get (for example) lat for each point in a 2d slice
#'
#' If g has 2 dimensions, 'lat' and 'lon', each of length 10, 
#' \code{RollDimensions(g,1,2)} will return the latitude
#' for each of the 100 points in the slice; and
#' \code{RollDimensions(g,2,1)} will return the longitude
#' similarly.
#'
#' @export
#' @param g GSDF data structure
#' @param d1 name or integer index of first dimension
#' @param d2 name or integer index of second dimension
#' @return numeric vector of first dimension values 
RollDimensions<-function(g,d1,d2) {
  results<-list()
  if(!is.numeric(d1)) {
    dn<-GSDF::FindDimension(g,d1)
    if(is.null(dn)) stop(sprintf("Field has no dimension %s",d1))
    d1<-dn
  } 
  if(!is.numeric(d2)) {
    dn<-GSDF::FindDimension(g,d2)
    if(is.null(dn)) stop(sprintf("Field has no dimension %s",d2))
    d2<-dn
  } 
  if(d1<=d2) {
     return(rep(g$dimensions[[d1]]$values,
                  length(g$dimensions[[d2]]$values)))
  } else {
     return(as.vector(matrix(data=rep(g$dimensions[[d1]]$values,
                                   length(g$dimensions[[d2]]$values)),
                            nrow=length(g$dimensions[[d2]]$values),
                            ncol=length(g$dimensions[[d1]]$values),
                            byrow=T)))
  }
}
#' GSDF extended dimensions
#'
#' Get the extended dimensions (those with length>1) from a field
#'
#' @export
#' @param g Field to extract dimensions from
#' @return Integer vector with indices of extended dimensions
ExtendedDimensions<-function(g) {
   dims<-integer(0)
   for(d in seq_along(g$dimensions)) {
      if(length(g$dimensions[[d]]$values)>1) dims<-c(dims,d)
   }
   return(dims)
 }

#' Remove a dimension from a field
#'
#' Convert a n-dimensional field to an (n-1) dimensional field
#' by applying a function over the selected dimension.
#'
#' If you have a 3-d field (say) lat*lon*ensemble member, convert it
#'  to a two d (lat*lon) field with the ensemble mean by running this with
#'  function 'mean' and dimension 'ensemble'. But you can use any function
#'  (that has a vector input and scalar output) over any dimension.
#'
#' @export
#' @param d GSDF field
#' @param dimn name (e.g. 'ensemble') or number (3) of dimension to reduce
#' @param fn function to do the reduction (often mean or sd).
#' @param ... additional arguments to fn (e.g. na.rm=T)
#' @return reduced field.
ReduceDimension<-function(d,dimn,fn,...) {
  idx.d<-NULL
  if(is.numeric(dimn)) {
    idx.d<-dimn
  } else idx.d<-GSDF::FindDimension(d,dimn)
  if(is.null(idx.d)) stop(sprintf("Field has no dimension %s",dimn))
  result<-d
  old.d<-seq_along(dim(d$data))
  result$data<-apply(d$data,old.d[-idx.d],fn,...)
  if(length(old.d)>idx.d) {
     for(i in seq(idx.d,length(old.d)-1)) {
       result$dimensions[[i]]<-d$dimensions[[i+1]]
     }
  }
  result$dimensions[[length(old.d)]]<-NULL
  return(result)
}

#' Merge two fields along one dimension.
#'
#' Takes two n-dimensional fields where n-1 of the dimensions
#'  are identical and concatnate the fields along the remaining
#'  dimension.
#'
#' If you have a 3-d field (say) lat*lon*time1, and a second field
#'  lat*lon*time2 on the same lat lon grid - combine them to
#'  make 1 output field lat*lon*(time1+time2). Can concatenate
#'  along any dimension if all the other dimensions are the same.
#'
#' @export
#' @param d1 GSDF field
#' @param d2 GSDF field
#' @param dimn name (e.g. 'ensemble') or number (3) of dimension to concatenate along
#' @return combined field.
ConcatenateFields<-function(d1,d2,dimn) {
  idx.d<-NULL
  if(is.numeric(dimn)) {
    idx.d<-dimn
  } else {
    idx.d<-GSDF::FindDimension(d1,dimn)
    if(is.null(idx.d)) stop(sprintf("Field has no dimension %s",dimn))
    if(GSDF::FindDimension(d2,dimn)!=idx.d) {
       stop("Dimension orders don't match")
    }
  }
  if(is.null(d1$dimensions[[idx.d]]$type) ||
     is.null(d2$dimensions[[idx.d]]$type) ||
     d1$dimensions[[idx.d]]$type != d2$dimensions[[idx.d]]$type) {
    stop("Concatenation dimensions don't match")
  }
  if(length(d1$dimensions)!=length(d2$dimensions)) {
    stop("Field dimension counts don't match")
  }
  for(i in seq_along(d1$dimensions)) {
    if(i==idx.d) next
    if(d1$dimensions[[i]]$type != d2$dimensions[[i]]$type ||
       !isTRUE(all.equal(d1$dimensions[[i]]$values,
                          d2$dimensions[[i]]$values))) {
       stop(sprintf("Field dimensions %d don't match",i))
    }
  }
  result<-d1
  result$data<-abind::abind(d1$data,d2$data,along=idx.d)
  result$dimensions[[idx.d]]$values<-c(
                      d1$dimensions[[idx.d]]$values,
                      d2$dimensions[[idx.d]]$values)
  return(result)
}

