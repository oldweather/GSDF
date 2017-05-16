# Functions for getting data from the GFS forecast ensemble
# Opendap info http://nomads.ncep.noaa.gov:9090/dods/gens/gens20140118/gec00_00z.info
# http://nomads.ncep.noaa.gov/

# names and classes of variables
GFS.latname<-'lat'
GFS.lonname<-'lon'
GFS.hgtname<-'lev'
GFS.tname<-'time'
GFS.ensname<-'ens'
# Everything is 6-Hourly

# Monolevel
GFS.monolevel<-c('no4lftxsfc','no5wava500mb','no5wavh500mb','acpcpsfc','albdosfc','apcpsfc',
                 'capesfc','cape180_0mb','cfrzrsfc','cicepsfc','cinsfc','cin180_0mb','cpratsfc',
                 'crainsfc','csnowsfc','cwatclm','cworkclm','dlwrfsfc','dpt2m','dswrfsfc',
                 'gfluxsfc','gpa1000mb','gpa500mb','hgtsfc','hgt2pv','hgtneg2pv','hgttop0c',
                 'hgttop0c','hgt0c','hgtmwl','hgttrop','hpblsfc','icecsfc','landsfc','lftxsfc',
                 'lhtflsfc','pevprsfc','potsig995','pratesfc','preslclb','preslclt','presmclb',
                 'presmclt','preshclb','preshclt','pressfc','pres2pv','presneg2pv','prescclb',
                 'prescclt','presmwl','prestrop','prmslmsl','pvort320k','pwatclm','rh2m',
                 'rhsg330_1000','rhsg440_1000','rhsg720_940','rhsg440_720','rhsig995','rh30_0mb',
                 'rhclm','rhtop0c','rh0c','shtflsfc','snodsfc','soilw0_10cm','soilw10_40cm',
                 'soilw40_100cm','soilw100_200cm','spfh2m','spfh30_0mb','sunsdsfc','tcdcclm',
                 'tcdcblcll','tcdclcll','tcdcmcll','tcdchcll','tcdcccll','tmax2m','tmin2m',
                 'tmplclt','tmpmclt','tmphclt','tmpsfc','tmp_1829m','tmp_2743m','tmp_3658m',
                 'tmp2m','tmpsig995','tmp0_10cm','tmp10_40cm','tmp40_100cm','tmp100_200cm',
                 'tmp30_0mb','tmp2pv','tmpneg2pv','tmpmwl','tmptrop','tozneclm','ugwdsfc',
                 'uflxsfc','ugrd_1829m','ugrd_2743m','ugrd_3658m','ugrd10m','ugrdsig995',
                 'ugrd30_0mb','ugrd2pv','ugrdneg2pv','ugrdmwl','ugrdtrop','ulwrfsfc','ulwrftoa',
                 'uswrfsfc','uswrftoa','vgwdsfc','vflxsfc','vgrd_1829m','vgrd_2743m',
                 'vgrd_3658m','vgrd10m','vgrdsig995','vgrd30_0mb','vgrd2pv','vgrdneg2pv',
                 'vgrdmwl','vgrdtrop','vvelsig995','vwsh2pv','vwshneg2pv','vwshtrop',
                 'watrsfc','weasdsfc')

# 4 different sets of heights
GFS.heights<-list()
                 
GFS.heights[['all']]<-c(1000,975,950,925,900,850,800,750,700,650,600,550,500,450, 
                         400,350,300,250,200,150,100,70,50,30,20,10)
GFS.h.all<-c('absvprs','hgtprs','tmpprs','ugrdprs','vgrdprs')

GFS.heights[['trop']]<-c(1000,975,950,925,900,850,800,750,700,650,600,550,500,450, 
                         400,350,300,250,200,150,100)
GFS.h.trop<-c('clwmrprs','rhprs','vvelprs')

GFS.heights[['trop2']]<-c(1000,925,850,700,500,300,250,200)
GFS.h.trop2<-c('spfhprs')

GFS.heights[['strat']]<-c(100,70,50,30,20,10)
GFS.h.strat<-c('o3mrprs')


#' GFS show variables
#' 
#' List all the variables available in the GFS ensemble.
#'
#' Two sorts of variables:
#' \describe{
#' \item{Monolevel} {Lat x Lon x Time}
#' \item{Pressure level} {Lat x Lon x Height x Time}
#' }
#' This function has no parmeters and returns no value
#' it prints a list of available variables.
#' @export
GFS.show.variables<-function() {
  print('Monolevel')
  print(GFS.monolevel)
  print('Pressure level')
  print(GFS.h.all)
  print(GFS.h.trop)
  print(GFS.h.trop2)
  print(GFS.h.strat)
}

# Get group of variable.
#  Distinguish between diferent sets of vertical heights 
GFS.get.variable.group<-function(variable) {
  variable<-tolower(variable)
  if(length(which(GFS.monolevel==variable))>0) return('monolevel')
  if(length(which(GFS.h.all==variable))>0) return('all')
  if(length(which(GFS.h.trop==variable))>0) return('trop')
  if(length(which(GFS.h.trop2==variable))>0) return('trop2')
  if(length(which(GFS.h.strat==variable))>0) return('strat')
 stop(sprintf("Unrecognised variable: %s",variable))
}

#' GFS hourly get file name
#'
#' Get file name (URI) for GFS variable
#' 
#' GFS URIs depend on the time of the run, this function
#'  calculates them from the variable name and the run date.
#'
#' @export
#' @param variable 'tmp2m', 'prmslmsl', 'ugrd10m', 'tmpprs' - or any GFS variable
#' @param type must be 'member' (everthing else derived from members).
#' @param member The member to use' - integer in the range 0-20.
#' @param opendap Must be TRUE - no local option currently supported.
#' @return A URI containing the requested GFS data
GFS.hourly.get.file.name<-function(variable,year,month,day,hour,opendap=TRUE,type='member',member=0) {
    if(opendap) {
        base.dir<-'http://nomads.ncep.noaa.gov:9090/dods/gens/gens'
        if(type=='member') {
           if(member==0) return(sprintf("%s%04d%02d%02d/gec00_%02dz",base.dir,year,month,day,hour))
           if(member<=20) return(sprintf("%s%04d%02d%02d/gep%02d_%02dz",base.dir,year,month,day,member,hour))
           stop("member must be in 0..20")
         }
        if(type=='block') {
          return(sprintf("%s%04d%02d%02d/gep_all_%02dz",base.dir,year,month,day,hour))
        }
        stop('Type for file name must be "member" or "block"')
    }     
    else {
        stop(sprintf("GFS only available via openDAP"))
   }
}

GFS.is.in.file<-function(hour,lead) {
      if(hour%%6==0 && lead%%6==0) return(TRUE)
      return(FALSE)
}

# GFS has an analysis every 6 hours, and each analysis has 64 forecast steps at 6-hour intervals
#  so in general need to interpolate between forecast steps and between analysis times
GFS.get.interpolation.times<-function(variable,year,month,day,hour,lead=0) {
   interpolation.points<-list()
   if(hour%%6==0) { # At an analysis time - no need to interpolate that
       interpolation.points[[1]]<-list(
          year=year,
          month=month,
          day=day,
          hour=hour)     
     if(lead%%6==0) { # Also at a forecast step - no need to interpolate at all
       interpolation.points[[1]]$lead=lead
       interpolation.points[[1]]$weight=1
       return(interpolation.points)
     } else {
        interpolation.points[[2]]<-interpolation.points[[1]]
        interpolation.points[[1]]$lead<-lead-lead%%6
        interpolation.points[[1]]$weight<-(6-lead%%6)/6
        interpolation.points[[2]]$lead<-interpolation.points[[1]]$lead+6
        interpolation.points[[2]]$weight<-1-interpolation.points[[1]]$weight
        return(interpolation.points)
      }
   } else { # Interpolate the analysis time
	ct<-chron(dates=sprintf("%04d/%02d/%02d",year,month,day),
	          times=sprintf("%02d:00:00",hour),
	          format=c(dates='y/m/d',times='h:m:s'))
        cp<-ct-(hour%%6)/24
        cn<-cp+0.25
        interpolation.points[[1]]<-list(
             year=as.numeric(as.character(years(cp))),
             month=months(cp),
             day=days(cp),
             hour=hours(cp))
        interpolation.points[[2]]<-list(
             year=as.numeric(as.character(years(cn))),
             month=months(cn),
             day=days(cn),
             hour=hours(cn))
        if(lead%%6==0) { # At a forecast step
           interpolation.points[[1]]$lead<-lead
           interpolation.points[[1]]$weight<-(6-hour%%6)/6
           interpolation.points[[2]]$lead<-lead
           interpolation.points[[2]]$weight<-1-interpolation.points[[1]]$weight
           return(interpolation.points)
        } else { # Interpolate the forecast step as well
           interpolation.points[[3]]<-interpolation.points[[1]]
           interpolation.points[[4]]<-interpolation.points[[2]]
           interpolation.points[[1]]$lead<-lead-lead%%6
           interpolation.points[[2]]$lead<-lead-lead%%6
           interpolation.points[[1]]$weight<-((6-hour%%6)/6)*((6-lead%%6)/6)
           interpolation.points[[2]]$weight<-(1-(6-hour%%6)/6)*((6-lead%%6)/6)
           interpolation.points[[3]]$lead<-interpolation.points[[1]]$lead+6
           interpolation.points[[4]]$lead<-interpolation.points[[2]]$lead+6
           interpolation.points[[3]]$weight<-((6-hour%%6)/6)*(1-(6-lead%%6)/6)
           interpolation.points[[4]]$weight<-(1-(6-hour%%6)/6)*(1-(6-lead%%6)/6)
           return(interpolation.points)
        }
     }
 }
        
# This is the function users will call.
#' Get slice at hour.
#'
#' Get a 2D horizontal slice of a selected variable (as a GSDF field) for a given hour.
#'
#' Interpolates to the selected hour when the data available are less than 6-hourly.
#' Interpolates to the selected height when the selected height is not that of a GFS level.
#'
#' @export
#' @param variable 'tmp2m', 'prmslmsl', 'ugrd10m', 'tmpprs' - or any GFS variable
#' @param (year,month,day,hour) Time at which the forecast was made.
#' @param lead number of hours ahead of forecast time (0-259, default=0)
#' @param type must be 'member', 'mean', or 'spread' (no normals or sds from GFS). (Can also be
#'  'block' which returns a 3d field with all the ensemble members).
#' @param member The member to use' - integer in the range 0-20 (default=0, mean and sd use all members)
#' @param height Height in hPa - leave NULL for monolevel
#' @param opendap Must be TRUE - no local option currently supported.
#' @return A GSDF field with lat and long as extended dimensions
GFS.get.slice.at.hour<-function(variable,year,month,day,hour,height=NULL,opendap=TRUE,
                                type='member',member=1,lead=0) {
   if(type=='member') return(GFS.get.member.slice.at.hour(variable,year,month,day,hour,
                                                          height=height,opendap=opendap,
                                                          member=member,lead=lead))
   if(type=='mean') {
     v<-GFS.get.block.slice.at.hour(variable,year,month,day,hour,
                                    height=height,opendap=opendap,
                                                          lead=lead)
     v<-GSDF.reduce.1d(v,'ensemble',mean,na.rm=T)
     return(v)
   }  
   if(type=='spread') {
     v<-GFS.get.block.slice.at.hour(variable,year,month,day,hour,
                                    height=height,opendap=opendap,
                                                          lead=lead)
     v<-GSDF.reduce.1d(v,'ensemble',sd,na.rm=T)
     return(v)
   }  
   if(type=='block') return(GFS.get.block.slice.at.hour(variable,year,month,day,hour,
                                                          height=height,opendap=opendap,
                                                          lead=lead))
   stop('Type must be "member", "mean", "spread", or "block"')
 }

# Get slice for a single member
GFS.get.member.slice.at.hour<-function(variable,year,month,day,hour,height=NULL,opendap=TRUE,
                                       member=1,lead=0) {
   group<-GFS.get.variable.group(variable)
   if(group=='monolevel') {
    if(!is.null(height)) warning("Ignoring height specification for monolevel variable")
    return(GFS.get.member.slice.at.level.at.hour(variable,year,month,day,hour,opendap=opendap,
                                          member=member,lead=lead))
  }
  # Find levels above and below selected height, and interpolate between them
  if(is.null(height)) stop(sprintf("No height specified for pressure variable %s",variable))
  if(height>max(GFS.heights[[group]]) || height<min(GFS.heights[[group]])) {
    stop("Height outside range for %s: %f to %f",variable,max(GFS.heights[[group]]),
                                                          min(GFS.heights[[group]]))
  }
  level.below<-max(which(GFS.heights[[group]]>=height))
  if(height==GFS.heights[[group]][level.below]) {
    return(GFS.get.member.slice.at.level.at.hour(variable,year,month,day,hour,
                                                 height=GFS.heights[[group]][level.below],
                                                 opendap=opendap,member=member,lead=lead))
  }
  below<-GFS.get.member.slice.at.level.at.hour(variable,year,month,day,hour,
                                               height=GFS.heights[[group]][level.below],
                                               opendap=opendap,member=member,lead=lead)
  above<-GFS.get.member.slice.at.level.at.hour(variable,year,month,day,hour,
                                               height=GFS.heights[[group]][level.below+1],
                                               opendap=opendap,member=member,lead=lead)
  above.weight<-(GFS.heights[[group]][level.below]-height)/(GFS.heights[[group]][level.below]-
                             GFS.heights[[group]][level.below+1])
  below$data[]<-below$data*(1-above.weight)+above$data*above.weight
  idx.h<-GSDF.find.dimension(below,'height')
  below$dimensions[[idx.h]]$value<-height
  return(below)
}

GFS.get.member.slice.at.level.at.hour<-function(variable,year,month,day,hour,
                                                   height=NULL,opendap=TRUE,member=1,lead=0) {
    # Is it from an analysis+forecast time (no need to interpolate)?
    if(GFS.is.in.file(hour,lead)) {
    file.name<-GFS.hourly.get.file.name(variable,year,month,day,hour,opendap=opendap,type='member')
       t<-chron(sprintf("%04d/%02d/%02d",year,month,day),sprintf("%02d:00:00",hour),
                format=c(dates='y/m/d',times='h:m:s'))
       t<-t+2 # GFS timestamps seem to be two days out - why?
       v<-GSDF.ncdf.load(file.name,variable,lat.range=c(-90,90),lon.range=c(0,360),
                         height.range=rep(height,2),time.range=c(t+lead/24,t+lead/24+1/24-0.001),
                         ens.range=c(1,1))
       return(v)
    }
    # Interpolate from the surrounding forecast times
    interpolation.times<-GFS.get.interpolation.times(variable,year,month,day,hour,lead=lead)
    if(!GFS.is.in.file(interpolation.times[[1]]$hour,interpolation.times[[1]]$lead)) {
      stop('Error in GFS.get.interpolation.times')
    }
    v<-GFS.get.member.slice.at.level.at.hour(variable,interpolation.times[[1]]$year,
                                              interpolation.times[[1]]$month,
                                              interpolation.times[[1]]$day,
                                              interpolation.times[[1]]$hour,
                                              opendap=opendap,height=height,
                                              member=member,
                                              lead=interpolation.times[[1]]$lead)
    v$data[]<-v$data*interpolation.times[[1]]$weight
    for(i in seq(2,length(interpolation.times))) {
       if(!GFS.is.in.file(interpolation.times[[i]]$hour,interpolation.times[[i]]$lead)) {
         stop('Error in GFS.get.interpolation.times')
       }
       v2<-GFS.get.member.slice.at.level.at.hour(variable,interpolation.times[[i]]$year,
                                                 interpolation.times[[i]]$month,
                                                 interpolation.times[[i]]$day,
                                                 interpolation.times[[i]]$hour,
                                                 opendap=opendap,height=height,
                                                 member=member,
                                                 lead=interpolation.times[[i]]$lead)
      v$data[]<-v$data+v2$data*interpolation.times[[i]]$weight
    }      
    c1<-chron(dates=sprintf("%04d/%02d/%02d",year,month,day),
              times=sprintf("%02d:00:00",hour),
              format=c(dates='y/m/d',times='h:m:s'))
    idx.t<-GSDF.find.dimension(v,'time')
    v$dimensions[[idx.t]]$value<-c1+lead
    return(v)
}

# Get slice for all members
GFS.get.block.slice.at.hour<-function(variable,year,month,day,hour,height=NULL,opendap=TRUE,
                                      lead=0) {
   group<-GFS.get.variable.group(variable)
   if(group=='monolevel') {
    if(!is.null(height)) warning("Ignoring height specification for monolevel variable")
    return(GFS.get.block.slice.at.level.at.hour(variable,year,month,day,hour,opendap=opendap,
                                          lead=lead))
  }
  # Find levels above and below selected height, and interpolate between them
  if(is.null(height)) stop(sprintf("No height specified for pressure variable %s",variable))
  if(height>max(GFS.heights[[group]]) || height<min(GFS.heights[[group]])) {
    stop("Height outside range for %s: %f to %f",variable,max(GFS.heights[[group]]),
                                                          min(GFS.heights[[group]]))
  }
  level.below<-max(which(GFS.heights[[group]]>=height))
  if(height==GFS.heights[[group]][level.below]) {
    return(GFS.get.block.slice.at.level.at.hour(variable,year,month,day,hour,
                                                 height=GFS.heights[[group]][level.below],
                                                 opendap=opendap,lead=lead))
  }
  below<-GFS.get.block.slice.at.level.at.hour(variable,year,month,day,hour,
                                               height=GFS.heights[[group]][level.below],
                                               opendap=opendap,lead=lead)
  above<-GFS.get.block.slice.at.level.at.hour(variable,year,month,day,hour,
                                               height=GFS.heights[[group]][level.below+1],
                                               opendap=opendap,lead=lead)
  above.weight<-(GFS.heights[[group]][level.below]-height)/(GFS.heights[[group]][level.below]-
                             GFS.heights[[group]][level.below+1])
  below$data[]<-below$data*(1-above.weight)+above$data*above.weight
  idx.h<-GSDF.find.dimension(below,'height')
  below$dimensions[[idx.h]]$value<-height
  return(below)
}

GFS.get.block.slice.at.level.at.hour<-function(variable,year,month,day,hour,
                                                   height=NULL,opendap=TRUE,member=1,lead=0) {
    # Is it from an analysis+forecast time (no need to interpolate)?
    if(GFS.is.in.file(hour,lead)) {
    file.name<-GFS.hourly.get.file.name(variable,year,month,day,hour,opendap=opendap,type='block')
       t<-chron(sprintf("%04d/%02d/%02d",year,month,day),sprintf("%02d:00:00",hour),
                format=c(dates='y/m/d',times='h:m:s'))
       t<-t+2 # GFS timestamps seem to be two days out - why?
       v<-GSDF.ncdf.load(file.name,variable,lat.range=c(-90,90),lon.range=c(0,360),
                         height.range=rep(height,2),time.range=c(t+lead/24,t+lead/24+1/24-0.001),
                         ens.range=c(1,21))
       return(v)
    }
    # Interpolate from the surrounding forecast times
    interpolation.times<-GFS.get.interpolation.times(variable,year,month,day,hour,lead=lead)
    if(!GFS.is.in.file(interpolation.times[[1]]$hour,interpolation.times[[1]]$lead)) {
      stop('Error in GFS.get.interpolation.times')
    }
    v<-GFS.get.block.slice.at.level.at.hour(variable,interpolation.times[[1]]$year,
                                              interpolation.times[[1]]$month,
                                              interpolation.times[[1]]$day,
                                              interpolation.times[[1]]$hour,
                                              opendap=opendap,height=height,
                                              lead=interpolation.times[[1]]$lead)
    v$data[]<-v$data*interpolation.times[[1]]$weight
    for(i in seq(2,length(interpolation.times))) {
       if(!GFS.is.in.file(interpolation.times[[i]]$hour,interpolation.times[[i]]$lead)) {
         stop('Error in GFS.get.interpolation.times')
       }
       v2<-GFS.get.block.slice.at.level.at.hour(variable,interpolation.times[[i]]$year,
                                                 interpolation.times[[i]]$month,
                                                 interpolation.times[[i]]$day,
                                                 interpolation.times[[i]]$hour,
                                                 opendap=opendap,height=height,
                                                 lead=interpolation.times[[i]]$lead)
      v$data[]<-v$data+v2$data*interpolation.times[[i]]$weight
    }      
    c1<-chron(dates=sprintf("%04d/%02d/%02d",year,month,day),
              times=sprintf("%02d:00:00",hour),
              format=c(dates='y/m/d',times='h:m:s'))
    idx.t<-GSDF.find.dimension(v,'time')
    v$dimensions[[idx.t]]$value<-c1+lead
    return(v)
}
