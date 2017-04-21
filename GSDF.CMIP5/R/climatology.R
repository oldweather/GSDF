# Functions to make (and access) climatologies from the CMIP5 data

#' Make (and store) a climatological average 
#'
#' Data are stored as .nc files in a parallel directory to the original
#'  data - same as original data exept only for 1 year.
#' Loads a whole year of data at once - high resolution 3D fields
#'  may run into memory problems.
#'
#' Chose to use 1981 as the storage year in the netcdf file.
#'
#' @export
#' @param model  - 'HadGEM2-A','CCSM4'  - or any model name
#' @param variable  - 'tas', 'pr', 'uas', 'vas' - or any short name
#' @param experiment - 'historical', 'rcp85', '1pctCO2' etc..
#' @param ensemble -  r<N>i<M>p<L> - defaults to 'r1i1p1'.
#' @param realm - 'atmos' (default), 'ocean', 'land', etc.
#' @param table - see http://cmip-pcmdi.llnl.gov/cmip5/docs/standard_output.pdf, defaults to '3hr'.
#' @param frequency '3hr' (default), 'mon', 'yr', 'fx', etc.
#' @param variable short variable name 'tas', 'pr', uas, 'ps' etc.
#' @param first.year first year of climatological period
#' @param last.year last year of climatological period
#' @param calendar - calendar used by the data 'gregorian', '360_day', or '365_day'
#' @return Nothing - a climatological field will be created as a side effect.
CMIP5.make.climatology<-function(
                         model=NULL,
                         experiment=NULL,
                         variable=NULL,
                         ensemble='r1i1p1',
                         realm='atmos',
                         table='3hr',
                         frequency='3hr',
                         height.range=NULL,
                         lat.range=c(-90,90),
                         lon.range=c(-180,360),
                         calendar='gregorian',
                         first.year,
                         last.year) {

  # Load the data 1 year at a time.
  result<-NULL
  for(year in seq(first.year,last.year)) {
      date.range<-sprintf("%04d-%02d-%02d:%02d:%02d",c(year,year),
                                 c(1,12),c(1,31),c(0,23),c(0,59))
      if(calendar=='360_day') {
         date.range<-sprintf("%04d-%02d-%02d:%02d:%02d",c(year,year),
                                    c(1,12),c(1,30),c(0,23),c(0,59))
      }  
      slab<-CMIP5.get.slab(model=model,
                       experiment=experiment,
                       variable=variable,
                       ensemble=ensemble,
                       realm=realm,
                       table=table,
                       frequency=frequency,
                       date.range=date.range,
                       calendar=calendar,
                       height.range=height.range,
                       lat.range=lat.range,
                       lon.range=lon.range)
     if(is.null(slab)) {
       stop("Data for climatology is not on disc.")
     }
     if(is.null(result)) {
       result<-slab
     } else {
       result$data[]<-result$data+slab$data
     }
     gc(verbose=FALSE )
    }
  result$data[]<-result$data/(last.year-first.year+1)

  # set the year to 1981
  t.i<-GSDF.find.dimension(result,'time')
  m<-stringr::str_match(result$dimensions[[t.i]]$values,
               "(\\d\\d\\d\\d)\\D(\\d\\d)\\D(\\d\\d)\\D(\\d\\d)\\D(\\d\\d)")
  
  result$dimensions[[t.i]]$values<-sprintf("1981-%s-%s:%s:%s",
                                           m[,3],m[,4],m[,5],m[,6])

  # Write the result to a disc location paralleling the data
  fn<-CMIP5.get.climatology.file.name(model=model,
                       experiment=experiment,
                       variable=variable,
                       ensemble=ensemble,
                       realm=realm,
                       table=table,
                       frequency=frequency)


  GSDF.ncdf.write(result,fn,name=variable)
}  

#' CMIP5 get file name (hourly climatologies)
#'
#' Get the file name for selected climatological data.
#'
#' Called internally by \code{CMIP5.make.climatology.at.hour} but also useful
#'  called directly - you can then access the data with another tool.
#' The vocbulary for the options is given in
#'  http://cmip-pcmdi.llnl.gov/cmip5/docs/cmip5_data_reference_syntax.pdf
#'
#' @export
#' @param model  - 'HadGEM2-A','CCSM4'  - or any model name
#' @param variable  - 'tas', 'pr', 'uas', 'vas' - or any short name
#' @param experiment - 'historical', 'rcp85', '1pctCO2' etc..
#' @param ensemble -  r<N>i<M>p<L> - defaults to 'r1i1p1'.
#' @param realm - 'atmos' (default), 'ocean', 'land', etc.
#' @param table - see http://cmip-pcmdi.llnl.gov/cmip5/docs/standard_output.pdf, defaults to '3hr'.
#' @param frequency '3hr' (default), 'mon', 'yr', 'fx', etc.
#' @return File names for the climatology data
CMIP5.get.climatology.file.name<-function(model=NULL,
                                     experiment=NULL,
                                     variable=NULL,
                                     ensemble='r1i1p1',
                                     realm='atmos',
                                     table='3hr',
                                     frequency='3hr') {
 
   base.dir<-sprintf("%s/managecmip/cmip5",Sys.getenv('SCRATCH'))
   institute<-CMIP5.get.institute.from.model(model)
   clim.dir<-sprintf("%s/climatology/%s/%s/%s/%s/%s/%s/%s",base.dir,
                   institute,model,experiment,table,realm,frequency,ensemble)
    if(!file.exists(clim.dir)) {
       dir.create(clim.dir,recursive=TRUE)
    }
    return(sprintf("%s/%s.nc",clim.dir,variable))
 }
