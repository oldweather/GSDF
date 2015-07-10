#' Take out data for February 29th.
#'
#' Removes all data for leap-days from a GSDF field.
#'  Has no effect if input field has no data from Feb 29th.
#'
#' Often want to compare data from two years, one of which was a leap year
#'  simplest approach is to strip Feb 29th data before comparison.
#' Most often occurs when making anomalies.
#'
#' @export
#' @param d GSDF field
#' @return field with Feb 29th removed
Unleap<-function(d) {
   t.d<-GSDF::FindDimension(d,'time')
   if(is.null(t.d)) {
      warning('Field has no time dimension')
      return(d)
   }
   w<-which(as.integer(months(d$dimensions[[t.d]]$values))==2 &
            days(d$dimensions[[t.d]]$values)==29)
   if(length(w)==0) return(d) # No leap day data
   call.l<-list()
   call.l[[1]]<-d$data
   for(i in seq(1,length(d$dimensions))) {
      if(i==t.d) call.l[[i+1]]<- -w
      else call.l[[i+1]]<-TRUE
   }
   d$data<-do.call("[",call.l)
   d$dimensions[[t.d]]$values<-d$dimensions[[t.d]]$values[-w]
   return(d)
}

#' Add fake data for February 29th.
#'
#' Makes a fake Feb 29th in a data slab by copying Feb 28th.
#'  Throws error if run on a slab with Feb 29th data or without Feb 28th.
#'
#' Often want to compare data from two years, one of which was a leap year
#'  one approach is to make a fake Feb 29th in the other.
#' Most often occurs when making anomalies.
#' Note that the time dimension values are just copied, so the data block
#'  will be sized as if there was a Feb 29th, but selecting data for Feb 29 will
#'  fail, andelecting data for Feb 28th will select the doubled data.
#'
#' @export
#' @param d GSDF field
#' @return field with Feb 29th removed
AddLeap<-function(d) {
   t.d<-GSDF::FindDimension(d,'time')
   if(is.null(t.d)) {
      warning('Field has no time dimension')
      return(d)
   }
   w<-which(as.integer(months(d$dimensions[[t.d]]$values))==2 &
            days(d$dimensions[[t.d]]$values)==29)
   if(length(w)!=0) stop("Field already has leap day data")
   w<-which(as.integer(months(d$dimensions[[t.d]]$values))==2 &
            days(d$dimensions[[t.d]]$values)==28)
   if(length(w)==0) stop("Field has no Feb 28th data")
   # Split source data into three blocks (<= Feb 28th,== Feb 28th,
   #    & > Feb 28th), and then combine them into one output
   w1<-seq(1,max(w))
   call.l<-list()
   call.l[[1]]<-d$data
   for(i in seq(1,length(d$dimensions))) {
      if(i==t.d) call.l[[i+1]]<- w1
      else call.l[[i+1]]<-TRUE
   }
   d1<-do.call("[",call.l)
   dim1<-d$dimensions[[t.d]]$values[w1]
   w2<-w
   call.l<-list()
   call.l[[1]]<-d$data
   for(i in seq(1,length(d$dimensions))) {
      if(i==t.d) call.l[[i+1]]<- w2
      else call.l[[i+1]]<-TRUE
   }
   d1<-abind::abind(d1,do.call("[",call.l),along=t.d)
   dim1<-c(dim1,d$dimensions[[t.d]]$values[w2])
   
   w3<-seq(max(w)+1,length(d$dimensions[[t.d]]$values))
   if(length(w3)>0) {
      call.l<-list()
      call.l[[1]]<-d$data
      for(i in seq(1,length(d$dimensions))) {
         if(i==t.d) call.l[[i+1]]<- w3
         else call.l[[i+1]]<-TRUE
      }
      d1<-abind::abind(d1,do.call("[",call.l),along=t.d)
      dim1<-c(dim1,d$dimensions[[t.d]]$values[w3])
   }
   d$data<-d1
   d$dimensions[[t.d]]$values<-dim1
   return(d)
}

