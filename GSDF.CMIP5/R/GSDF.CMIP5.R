# Functions for getting 3-hourly data from the CMIP5

# Which institute goes with each model?
CMIP5.institutes<-c(
   "ACCESS1-0"      = "CSIRO-BOM",
   "CCSM4"          = "NCAR",
   "CESM1-BGC"      = "NSF-DOE-NCAR",
   "CESM1-CAM5"     = "NSF-DOE-NCAR",
   "CNRM-CM5"       = "CNRM-CERFACS",
   "GFDL-CM3"       = "NOAA-GFDL",
   "GFDL-ESM2G"     = "NOAA-GFDL",
   "GISS-E2-R"      = "NASA-GISS",
   "HadGEM2-A"      = "MOHC",
   "HadGEM2-AO"     = "NIMR-KMA",
   "HadGEM2-CC"     = "MOHC",
   "HadGEM2-ES"     = "MOHC",
   "IPSL-CM5A-LR"   = "IPSL",
   "IPSL-CM5A-MR"   = "IPSL",
   "MIROC-ESM-CHEM" = "MIROC",
   "MIROC-ESM"      = "MIROC",
   "MIROC5"         = "MIROC",
   "MPI-ESM-LR"     = "MPI-M",
   "MRI-CGCM3"      = "MRI",
   "NorESM1-M"      = "NCC",
   "BCC-CSM1-1"     = "BCC",
   "INM-CM4"        = "INM")

#' CMIP5 get Institute from model
#'
#' It's redundant to specify the institude, just the model is sufficient.
#' But we need the institute to find the data - get it from the model.
#' 
#' Requires a table relating institutes and models but this doesn't
#'  change often.
#'
#' @export
#' @param model CMIP5 model name
#' @return Name of institute behind model.
CMIP5.get.institute.from.model<-function(model) {
  if(model %in% names(CMIP5.institutes)) return(CMIP5.institutes[[model]])
  stop(sprintf("Unknown model %s. Should be one of: %s",
               model,paste(names(CMIP5.institutes),collapse=" ")))
}

#' CMIP5 get data directory
#'
#' Find directories that may have data in, there may be several
#' 
#' Data may be in any of several different directory trees.
#' This function returns a list of possible base directories.
#'
#' @export
#' @return Base directory name vector - NULL for no possibilities.
CMIP5.get.data.dir<-function(model=NULL,
                            experiment=NULL,
                            variable=NULL,
                            ensemble=NULL,
                            realm=NULL,
                            table=NULL,
                            frequency=NULL,
                            year=NULL,
                            month=NULL,
                            day=NULL,
                            hour=NULL) {
  # The arguments are only there for consistency (at the moment)
  #  all the data is in the same places

  result<-character(0)

  # First look in $SCRATCH
    base.dir<-sprintf("%s/managecmip/cmip5",Sys.getenv('SCRATCH'))
    if(file.exists(base.dir)) {
           result<-c(result,base.dir )
    }
  
  # Then the central ManageCMIP repository
    base.dir<-'/project/champ/data/cmip5/'
    if(file.exists(base.dir)) {
           result<-c(result,base.dir )
    }
  
    if(length(result)==0) return(NULL) # No directories

    return(result)
}

#' CMIP5 get file name (hourly)
#'
#' Get the file name for selected data
#'
#' Called internally by \code{CMIP5.get.slice.at.hour} but also useful
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
#' @param start character string giving period start time 'YYYY-MM-DD:HH:MM'
#' @param end character string giving period end time 'YYYY-MM-DD:HH:MM'
#' @return vector of File names containing the requested data 
CMIP5.get.file.name<-function(model=NULL,
                                     experiment=NULL,
                                     variable=NULL,
                                     ensemble='r1i1p1',
                                     realm='atmos',
                                     table='3hr',
                                     frequency='3hr',
                                     start=NULL,
                                     end=NULL) {
  
  base.dirs<-CMIP5.get.data.dir(model=model,
                                experiment=experiment,
                                variable=variable,
                                ensemble=ensemble,
                                realm=realm,
                                table=table,
                                frequency=frequency,
                                year=year,
                                month=month,
                                day=day,
                                hour=hour)
  if(is.null(base.dirs)) return(NULL)

  institute<-CMIP5.get.institute.from.model(model)
  sub.dir<-sprintf("output1/%s/%s/%s/%s/%s/%s/%s",
                   institute,model,experiment,table,realm,frequency,ensemble)

  # There may be several matching data files (versions) use them all
  data.file<-character(0)
  for(base in base.dirs) {
     version.dirs<-list.dirs(path=sprintf("%s/%s",base,sub.dir),
                             recursive=FALSE,full.names<-TRUE)
     if(length(version.dirs)==0) next
     for(version in version.dirs) {
       candidates<-list.files(path=sprintf("%s/%s",version,variable),
                              full.names=TRUE)
       if(length(candidates)==0) next
       w<-which(CMIP5.files.for.timeslice(candidates,start,end))
       if(length(w)>0) data.file<-c(data.file,candidates[w])
     }
   }

  if(length(data.file)==0) return(NULL) # No data
  if(length(data.file)==1) return(data.file) # Only choice
  # Where we have different versions of the same file, take only the newest
  b.names<-basename(data.file)
  if(anyDuplicated(b.names)) {
     mtime<-file.info(data.file)$mtime
     data.file<-data.file[order(mtime)]
     b.names<-basename(data.file)
     w<-which(duplicated(b.names))
     data.file<-data.file[w]
  }
  # Sort into increasing (date) order
  b.names<-basename(data.file)
  data.file<-data.file[order(b.names)]
  return(data.file)
}
#' Test file names against desired date range
#'
#' Variables are often split into files by date, with the file name
#'  marking the date range.
#' 
#' This function tests file names against a specified date range, to find
#'  those that contain data covering those dates.
#'
#' @export
#' @param candidates vector of character file names with embedded date strings
#' @param start character string giving period start time 'YYYY-MM-DD:HH:MM'
#' @param end character string giving period end time 'YYYY-MM-DD:HH:MM'
#' @return vector of TRUE/False - is target time in file.
CMIP5.files.for.timeslice<-function(candidates,start,end) {
    target.start<-ymd_hms(sprintf("%s:00",start))
    target.end<-ymd_hms(sprintf("%s:59",end))
    c.dates<-stringr::str_match(candidates,'(\\d+)-(\\d+)')
    f.start<-c.dates[,2]
    w<-which(length(f.start)==4) # Year only
    if(length(w)>0) {
       f.start[w]<-sprintf("%s01010000",f.start[w])
    }
    w<-which(length(f.start)==6) # Year and month
    if(length(w)>0) {
       f.start[w]<-sprintf("%s010000",f.start[w])
    }
    w<-which(length(f.start)==8) # Year, month, day
    if(length(w)>0) {
       f.start[w]<-sprintf("%s0000",f.start[w])
    }
    w<-which(length(f.start)==10) # Year, month, day, hour
    if(length(w)>0) {
       f.start[w]<-sprintf("%s00",f.start[w])
    }
    l.start<-ymd_hms(sprintf("%04d-%02d-%02d:%02d:%02d:00",
                              as.integer(substr(f.start,1,4)),
                              as.integer(substr(f.start,5,6)),
                              as.integer(substr(f.start,7,8)),
                              as.integer(substr(f.start,9,10)),
                              as.integer(substr(f.start,11,12))))
  
    f.end<-c.dates[,3]
    w<-which(length(f.end)==4) # Year only
    if(length(w)>0) {
       f.end[w]<-sprintf("%s12312359",f.end[w])
    }
    w<-which(length(f.end)==6) # Year and month
    if(length(w)>0) {
       f.end[w]<-sprintf("%s312359",f.end[w])
    }
    w<-which(length(f.end)==8) # Year, month, day
    if(length(w)>0) {
       f.end[w]<-sprintf("%s2359",f.end[w])
    }
    w<-which(length(f.end)==10) # Year, month, day, hour
    if(length(w)>0) {
       f.end[w]<-sprintf("%s59",f.end[w])
    }
    l.end<-ymd_hms(sprintf("%04d-%02d-%02d:%02d:%02d:59",
                              as.integer(substr(f.end,1,4)),
                              as.integer(substr(f.end,5,6)),
                              as.integer(substr(f.end,7,8)),
                              as.integer(substr(f.end,9,10)),
                              as.integer(substr(f.end,11,12))))
    
    return(l.start<=target.end & l.end>=target.start)
}


#' Extract a hyperslab of data.
#'
#' Up to 4d (lat, lon, height, time).
#'
#' 
#' @export
#' @param model  - 'HadGEM2-A','CCSM4'  - or any model name
#' @param variable  - 'tas', 'pr', 'uas', 'vas' - or any short name
#' @param experiment - 'historical', 'rcp85', '1pctCO2' etc..
#' @param ensemble -  r<N>i<M>p<L> - defaults to 'r1i1p1'.
#' @param realm - 'atmos' (default), 'ocean', 'land', etc.
#' @param table - see http://cmip-pcmdi.llnl.gov/cmip5/docs/standard_output.pdf, defaults to '3hr'.
#' @param frequency '3hr' (default), 'mon', 'yr', 'fx', etc.
#' @param variable 20CR variable name, only 2d variables will work.
#' @param date.range A pair of date strings, e.g. c('1981-02-05:12','1981-03-27:18') - inclusive.
#' @param calendar Calendar used by data - 'gregorian' (default), '360_day' or '365_day'.
#' @param height.range Bottom and top heights in hPa - leave NULL for monolevel
#' @param lat.range Min and max latitude in degrees - defaults to c(-90,90)
#' @param lon.range Min and max longitude in degrees - defaults to c(0,360)
#' @param type 'mean' or NULL (default) - get the data, 'normal' gets a climatology.
#' @return A GSDF field with the selected multidimensional data
CMIP5.get.slab<-function(model=NULL,
                         experiment=NULL,
                         variable=NULL,
                         ensemble='r1i1p1',
                         realm='atmos',
                         table='3hr',
                         frequency='3hr',
                         date.range,
                         calendar='gregorian',
                         height.range=NULL,
                         lat.range=c(-90,90),
                         lon.range=c(-180,360),
                         type=NULL) {

  if(!is.null(type) && type=='normal') {
     result<-CMIP5.get.slab.climatology(
                         model=model,
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
     if(is.null(result)) {
       stop(sprintf("No climatology on disc for %s",paste(c(
           model,experiment,variable,ensemble,realm,table,
           frequency,date.range[1],date.range[2]),collapse=" ")))
     }
     return(result)
  }
       
  # Not a climatology request - get the original data
  source.files<-CMIP5.get.file.name(model=model,
                                    experiment=experiment,
                                    variable=variable,
                                    ensemble=ensemble,
                                    realm=realm,
                                    table=table,
                                    frequency=frequency,
                                    start=date.range[1],
                                    end=date.range[2])
  if(is.null(source.files)) {
    stop(sprintf("No data on disc for %s",paste(c(
           model,experiment,variable,ensemble,realm,table,
           frequency,date.range[1],date.range[2]),collapse=" ")))
  }
  
  slab<-NULL
  for(file.name in source.files) {
      v<-GSDF.ncdf.load2(file.name,variable,lat.range=lat.range,lon.range=lon.range,
			 height.range=height.range,time.range=GSDF.time(date.range,calendar),
                         default.calendar=calendar)
      if(!is.null(slab)) slab<-GSDF.concatenate(slab,v,'time')
      else slab<-v
   }
  return(slab)
}
 
#' Extract a hyperslab of climatology
#'
#' Up to 4d (lat, lon, height, time).
#'
#' Don't call this directly - called from CMIP5.get.slab
#' 
#' @param model  - 'HadGEM2-A','CCSM4'  - or any model name
#' @param variable  - 'tas', 'pr', 'uas', 'vas' - or any short name
#' @param experiment - 'historical', 'rcp85', '1pctCO2' etc..
#' @param ensemble -  r<N>i<M>p<L> - defaults to 'r1i1p1'.
#' @param realm - 'atmos' (default), 'ocean', 'land', etc.
#' @param table - see http://cmip-pcmdi.llnl.gov/cmip5/docs/standard_output.pdf, defaults to '3hr'.
#' @param frequency '3hr' (default), 'mon', 'yr', 'fx', etc.
#' @param variable 20CR variable name, only 2d variables will work.
#' @param date.range A pair of date strings, e.g. c('1981-02-05:12','1981-03-27:18') - inclusive.
#' @param calendar Calendar used by data - 'gregorian' (default), '360_day' or '365_day'.
#' @param height.range Bottom and top heights in hPa - leave NULL for monolevel
#' @param lat.range Min and max latitude in degrees - defaults to c(-90,90)
#' @param lon.range Min and max longitude in degrees - defaults to c(0,360)
#' @return A GSDF field with the selected multidimensional data
CMIP5.get.slab.climatology<-function(
                         model=NULL,
                         experiment=NULL,
                         variable=NULL,
                         ensemble='r1i1p1',
                         realm='atmos',
                         table='3hr',
                         frequency='3hr',
                         date.range,
                         calendar='gregorian',
                         height.range=NULL,
                         lat.range=c(-90,90),
                         lon.range=c(-180,360)) {

  file.name<-CMIP5.get.climatology.file.name(
                                    model=model,
                                    experiment=experiment,
                                    variable=variable,
                                    ensemble=ensemble,
                                    realm=realm,
                                    table=table,
                                    frequency=frequency)
  if(!file.exists(file.name)) {
    return(NULL)
  }
  
  # The complication is that if the slab covers a year boundary
  #  we need to patch together multiple calls
  # Also need to reset the year on the returned data
  m<-stringr::str_match(date.range,
               "(\\d\\d\\d\\d)\\D(\\d\\d)\\D(\\d\\d)\\D(\\d\\d)\\D(\\d\\d)")
  years<-as.integer(m[,2])
  slab<-NULL
  if(years[1]==years[2]) {
    start.date<-sprintf("1981-%s-%s:%s:%s",m[1,3],m[1,4],m[1,5],m[1,6])
    end.date<-sprintf("1981-%s-%s:%s:%s",m[2,3],m[2,4],m[2,5],m[2,6])
    slab<-GSDF.ncdf.load2(file.name,variable,lat.range=lat.range,lon.range=lon.range,
			 height.range=height.range,time.range=GSDF.time(
                                              c(start.date,end.date),calendar),
                         default.calendar=calendar)
    t.i<-GSDF.find.dimension(slab,'time')
    m2<-stringr::str_match(slab$dimensions[[t.i]]$values,
               "(\\d\\d\\d\\d)\\D(\\d\\d)\\D(\\d\\d)\\D(\\d\\d)\\D(\\d\\d)")
  
    slab$dimensions[[t.i]]$values<-sprintf("%04d-%s-%s:%s:%s",years[1],
                                           m2[,3],m2[,4],m2[,5],m2[,6])
  } else {  
      for(year in years) {
        if(year>years[1]) {
          start.date<-"1981-01-01:00:00"
        } else {
          start.date<-sprintf("1981-%s-%s:%s:%s",m[1,3],m[1,4],m[1,5],m[1,6])
        }
        if(year<years[2]) {
          end.date<-"1981-12-31:23:59"
          if(calendar=='360_day') end.date<-"1981-12-30:23:59"
        } else {
          end.date<-sprintf("1981-%s-%s:%s:%s",m[2,3],m[2,4],m[2,5],m[2,6])
        }
        v<-GSDF.ncdf.load2(file.name,variable,lat.range=lat.range,lon.range=lon.range,
                             height.range=height.range,time.range=GSDF.time(
                                       c(start.date,end.date),calendar),
                             default.calendar=calendar)
        if(is.null(v)) next # No data
        # set the year 
        t.i<-GSDF.find.dimension(v,'time')
        m2<-stringr::str_match(v$dimensions[[t.i]]$values,
                   "(\\d\\d\\d\\d)\\D(\\d\\d)\\D(\\d\\d)\\D(\\d\\d)\\D(\\d\\d)")

        v$dimensions[[t.i]]$values<-sprintf("%04d-%s-%s:%s:%s",year,
                                               m2[,3],m2[,4],m2[,5],m2[,6])
        if(!is.null(slab)) slab<-GSDF.concatenate(slab,v,'time')
        else slab<-v
      }
    }
  return(slab)
}
  

#' Extract a hyperslab of data interplolated to a given time
#'
#' Up to 3d (lat, lon, height).
#'
#' Extracts a time range containing points before and after the selected time
#'  and interpolates linearly.
#'
#' 
#' @export
#' @param model  - 'HadGEM2-A','CCSM4'  - or any model name
#' @param variable  - 'tas', 'pr', 'uas', 'vas' - or any short name
#' @param experiment - 'historical', 'rcp85', '1pctCO2' etc..
#' @param ensemble -  r<N>i<M>p<L> - defaults to 'r1i1p1'.
#' @param realm - 'atmos' (default), 'ocean', 'land', etc.
#' @param table - see http://cmip-pcmdi.llnl.gov/cmip5/docs/standard_output.pdf, defaults to '3hr'.
#' @param frequency '3hr' (default), 'mon', 'yr', 'fx', etc.
#' @param variable  short variable name 'tas', 'pr', uas, 'ps' etc.
#' @param date Target date as a string. In format '1981-02-05:12'
#' @param calendar Calendar used by data - 'gregorian', '365_day', or '360_day' 
#' @param height.range Bottom and top heights in hPa - leave NULL for monolevel
#' @param lat.range Min and max latitude in degrees - defaults to c(-90,90)
#' @param lon.range Min and max longitude in degrees - defaults to c(0,360)
#' @param type 'mean' or NULL (default) - get the data, 'normal' gets a climatology.
#' @return A GSDF field with the selected multidimensional data
CMIP5.get.slice.at.hour<-function(model=NULL,
                         experiment=NULL,
                         variable=NULL,
                         ensemble='r1i1p1',
                         realm='atmos',
                         table='3hr',
                         frequency='3hr',
                         date,
                         calendar='360_day',
                         height.range=NULL,
                         lat.range=c(-90,90),
                         lon.range=c(-180,360),
                         type=NULL) {

  # Get a slab of data for the period around the selected one, so we can interpolate
  #  if the selected date does not exactly match a timestep.
  if(frequency=='3hr') {
    date.range<-GSDF.time.increment(GSDF.time(date,calendar),c(-3,3),'hours')
  } else {
    stop("Only 3-hourly data is currently supported")
  }
  slab<-CMIP5.get.slab(model=model,
                       experiment=experiment,
                       variable=variable,
                       ensemble=ensemble,
                       realm=realm,
                       table=table,
                       frequency=frequency,
                       date.range=date.range$date,
                       calendar=date.range$calendar,
                       height.range=height.range,
                       lat.range=lat.range,
                       lon.range=lon.range,
                       type=type)
  if(is.null(slab)) return(NULL)  # No data on disc
  
  # Is there data for the time point selected in the slab?
  # If so, extract and return it.
  t.i<-GSDF.find.dimension(slab,'time')
  slab.times<-GSDF.time(slab$dimensions[[t.i]]$values,slab$meta$calendar)
  w<-which(slab.times$date==date)
  if(length(w)==1) {
    slab<-GSDF.select.from.1d(slab,t.i,w)
    return(slab)
  }

  # No exact match, so interpolate linearly - find the before and after dates in the slab
  d.times<-GSDF.time.difference(slab.times,GSDF.time(date,slab.times$calendar))
  before<-which(d.times<0)
  if(length(before)==0) {
    stop("SNH error - inconsistent set of times in interpolation slab")
  }
  before=max(before)
  after<-which(d.times>0)
  if(length(after)==0) {
    stop("SNH error - inconsistent set of times in interpolation slab")
  }
  after=min(after)
  slice.b<-GSDF.select.from.1d(slab,t.i,before)
  slice.a<-GSDF.select.from.1d(slab,t.i,after)
  result<-slab
  b.weight<-d.times[after]/(d.times[after]-d.times[before])
  result$data[]<-slice.b$data*b.weight+slice.a$data*(1-b.weight)
  t.i<-GSDF.find.dimension(result,'time')
  result$dimensions[[t.i]]$values<-date
  return(result)
 
}
   
