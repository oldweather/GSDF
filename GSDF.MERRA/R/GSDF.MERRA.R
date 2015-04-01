# Functions for getting data from the MERRA Reanalysis
# MERRA data are distributed in daily files, so it's not
# practical to get data from more than one time in one call.

# names and classes of variables
MERRA.latname<-'YDim'
MERRA.lonname<-'XDim'
MERRA.hgtname<-'Height'
MERRA.tname<-'TIME'
# Hourly monolevel
MERRA.MAT1NXSLV<-c('SLP','PS','U850','U500','U250','U850','V500','vV50','V850','V500','V250',
                  'Q850','Q500','Q250','H1000','H850','H500','H250','OMEGA500','U10M','U2M',
                  'U50M','V10M','V2M','V50M','T10M','T2M','QV10M','QV2M','TS','DISPH',
                  'TROPPV','TROPPT','TROPPB','TROPT','TROPQ','CLDPRS','CLDTMP')
MERRA.MAI1NXINT<-c('TQV','TQI','TQL','TOX','MASS','KE','CPT','THV') 
MERRA.MAT1NXFLX<-c('EFLUX','EVAP','HFLUX','TAUX','TAUY','TAUGWX','TAUGWY','PBLH','DISPH',
                   'BSTAR','USTAR','TSTAR','QSTAR','RI','Z0H','Z0M','HLML','TLML','QLML',
                   'ULML','VLML','RHOA','SPEED','CDH','CDQ','CDM','CN','TSH','QSH','FRSEAICE',
                   'PRECANV','PRECCON','PRECLSC','PRECSNO','PRECTOT','PGENTOT')
MERRA.MAT1NXINT<-c('DMDT_DYN','DMDT_ANA','DQVDT_DYN','DQVDT_PHY','DQVDT_ANA','DQVDT_MST',
                   'DQVDT_TRB','DQVDT_CHM','DQLDT_DYN','DQLDT_PHY','DQLDT_ANA','DQLDT_MST',
                   'DQIDT_DYN','DQIDT_PHY','DQIDT_ANA','DQIDT_MST','DOXDT_DYN','DOXDT_PHY',
                   'DOXDT_ANA','DOXDT_CHM','DKDT_DYN','DKDT_PHY','DKDT_ANA','DKDT_PHYPHY',
                   'DHDT_DYN','DHDT_PHY','DHDT_ANA','DHDT_RES','DPDT_DYN','DPDT_PHY','DPDT_ANA',
                   'UFLXPHI','VFLXPHI','UFLXKE','VFLXKE','UFLXQE','VFLXQE','UFLXQL','VFLXQL',
                   'UFLXQI','VFLXQI','TEFIXER','CONVCPT','CONVPHI','CONVKE','CONVTHV','DKDT_PG',
                   'DKDT_REMAP','DKDT_INRES','DKDT_GEN','DKDT_PGRES','DHDT_REMAP','DPDT_REMAP',
                   'DKDT_GWD','DKDT_RAY','DKDT_BKG','DKDT_ORO','DKDT_GWDRES','BKGERR','DHDT_GWD',
                   'DHDT_RAY','DHDT_BKG','DHDT_ORO','DKDT_SRB','DKDT_INT','DKDT_TOP','DKDT_MST',
                   'DHDT_TRB','DHDT_MST','DHDT_FRI','DHDT_RAD','DHDT_CUF','QTFILL','DQVDT_FIL',
                   'DQLDT_FIL','DOXDT_FIL','HFLUX','EVAP','PRECCU','PRECLS','PRECSN','DTHDT_ANA',
                   'DTHDT_PHY','DTHDT_DYN','DTHDT_REMAP','DTHDT_CONSV','DTHDT_FIL','LWTNET',
                   'LWGNET','SWNETTOA','SWNETSURF','LSCNVCL','LSCNVRN','CUCNVCL','CUCNVCI',
                   'EVPCL','EVPRN','SUBCI','SUBSN','AUTCNVRN','SDMCI','COLCNVRN','COLCNVSN',
                   'FRZCL','FRZRN')
MERRA.MAT1NXLND<-c('GRN','LAI','GWETROOT','GWETTOP','TPSNOW','TUNST','TSAT','TWLT','PRECSNO',
                   'PRECTOT','SNOMAS','SNODP','EVPSOIL','EVPTRNS','EVPINTR','EVPSBLN','RUNOFF',
                   'BASEFLOW','SMLAND','FRUNST','FRSAT','FRSNO','FRWLT','PARDF','PARDR',
                   'SHLAND','LHLAND','EVLAND','LWLAND','SWLAND','GHLAND','TWLAND','TELAND',
                   'WCHANGE','ECHANGE','SPLAND','SPWATR','SPSNOW')
MERRA.MAT1NXRAD<-c('EMIS','TS','ALBEDO','ALBNIRDF','ALBNIRDR','ALBVISDF','ALBVISDR','LWGEM',
                   'LWGAB','LWGABCLR','LWGABCLRCLN','LWGNT','LWGNTCLR','LWGNTCLRCLN','LWTUP',
                   'LWTUPCLR','LWTUPCLRCLN','SWTDN','SWGDN','SWGDNCLR','SWGNT','SWGNTCLR',
                   'SWGNTCLN','SWGNTCLRCLN','SWTNT','SWTNTCLR','SWTNTCLN','SWTNTCLRCLN','TAUHGH',
                   'TAULOW','TAUMID','TAUTOT','CLDHGH','CLDLOW','CLDMID','CLDTOT')
# 3-hourly pressure level
MERRA.MAI3CPASM<-c('H','O3','QV','QL','QI','RH','T','U','V','EPV','OMEGA')
MERRA.MAT3CPCLD<-c('RH','QLLS','QILS','QLAN','QIAN','QCCU','CFLS','CFAN','CFCU')
MERRA.MAT3CPMST<-c('CMFMC','DQRCU','DQRLSAN','PFLCU','PFICU','PFLLSAN','PFILSAN','REEVAPCN',
                   'REEVAPLSAN')
MERRA.MAT3CPODT<-c('DOXDTMST','DOXDTTRB','DOXDTCHM','DOXDTDYN','DOXDTANA')
MERRA.MAT3CPQDT<-c('DQVDTMST','DQVDTTRB','DQVDTCHM','DQVDTDYN','DQVDTANA','DQIDTMST',
                   'DQIDTTRB','DQIDTDYN','DQLDTMST','DQLDTTRB','DQLDTDY')
MERRA.MAT3CPRAD<-c('CLOUD','DTDTLWR','DTDTLWRCLR','DTDTSWR','DTDTSWRCLR')
MERRA.MAT3CPTDT<-c('DTDTRAD','DTDTMST','DTDTTRB','DTDTFRI','DTDTGWD','DTDTTOT','DTDTDYN',
                   'DTDTANA')
MERRA.MAT3CPTRB<-c('KM','KMLS','KMLK','KH','KHLS','KHLK','KHRAD','KHSFC','RI')
MERRA.MAT3CPUDT<-c('DUDTMST','DUDTTRB','DUDTGWD','DUDTDYN','DUDTANA','DVDTMST','DVDTTRB',
                   'DVDTGWD','DVDTDYN','DVDTANA')

# Height of each pressure level in hPa
MERRA.heights<-c(1000,975,950,925,900,875,850,825,800,775,750,725,700,650,600,550,500,450,
                 400,350,300,250,200,150,100,70,50,40,30,20,10,7,5,4,3,2,1,0.7,0.5,0.4,0.3,0.1)

#' Merra show variables
#' 
#' List all the variables available in the MERRA reanalyis.
#'
#' Two sorts of variables:
#' \describe{
#' \item{Monolevel} {Lat x Lon x Time}
#' \item{Pressure level} {Lat x Lon x Height x Time}
#' }
#' This function has no parmeters and returns no value
#' it prints a list of available variables.
#' @export
MERRA.show.variables<-function() {
  print('Monolevel')
  print(MERRA.MAT1NXSLV)
  print(MERRA.MAI1NXINT)
  print(MERRA.MAT1NXFLX)
  print(MERRA.MAT1NXINT)
  print(MERRA.MAT1NXLND)
  print(MERRA.MAT1NXRAD)
  print('Pressure level')
  print(MERRA.MAI3CPASM)
  print(MERRA.MAT3CPCLD)
  print(MERRA.MAT3CPMST)
  print(MERRA.MAT3CPODT)
  print(MERRA.MAT3CPQDT)
  print(MERRA.MAT3CPRAD)
  print(MERRA.MAT3CPTDT)
  print(MERRA.MAT3CPTRB)
  print(MERRA.MAT3CPUDT)
}

# Get group of variable.
#  They have different locations on the openDAP server.
MERRA.get.variable.group<-function(variable) {
  variable<-toupper(variable)
  if(length(which(MERRA.MAT1NXSLV==variable))>0) return('MAT1NXSLV')
  if(length(which(MERRA.MAI1NXINT==variable))>0) return('MAI1NXINT')
  if(length(which(MERRA.MAT1NXFLX==variable))>0) return('MAT1NXFLX')
  if(length(which(MERRA.MAT1NXINT==variable))>0) return('MAT1NXINT')
  if(length(which(MERRA.MAT1NXLND==variable))>0) return('MAT1NXLND')
  if(length(which(MERRA.MAT1NXRAD==variable))>0) return('MAT1NXRAD')
  if(length(which(MERRA.MAI3CPASM==variable))>0) return('MAI3CPASM')
  if(length(which(MERRA.MAT3CPCLD==variable))>0) return('MAT3CPCLD')
  if(length(which(MERRA.MAT3CPMST==variable))>0) return('MAT3CPMST')
  if(length(which(MERRA.MAT3CPODT==variable))>0) return('MAT3CPODT')
  if(length(which(MERRA.MAT3CPQDT==variable))>0) return('MAT3CPQDT')
  if(length(which(MERRA.MAT3CPRAD==variable))>0) return('MAT3CPRAD')
  if(length(which(MERRA.MAT3CPTDT==variable))>0) return('MAT3CPTDT')
  if(length(which(MERRA.MAT3CPTRB==variable))>0) return('MAT3CPTRB')
  if(length(which(MERRA.MAT3CPUDT==variable))>0) return('MAT3CPUDT')
  stop(sprintf("Unrecognised variable: %s",variable))
}

#' MERRA hourly get file name
#'
#' Get file name (URI) for MERRA variable
#' 
#' MERRA URIs depend on the variable group and the day, this function
#'  calculates them from the variable name and the date.
#'
#' @export
#' @param variable 'T2M', 'SLP', 'U10M', 'H500' - or any MERRA variable
#' @param type must be 'mean', (no spreads, normals etc available from MERRA)
#' @param opendap Must be TRUE - no local option currently supported.
#' @return A file or URI containing the requested MERRA data
MERRA.hourly.get.file.name<-function(variable,year,month,day,hour,opendap=TRUE,type='mean') {
    if(opendap) {
        runId<-100
        if(year>=1993 && year<2001) runId<-200
        if(year>=2001) runId<-300
        if(year==2010 & (month>5 & month < 9)) runId<-301
        if(type=='mean') {
            base.dir<-'http://goldsmr2.sci.gsfc.nasa.gov:80/opendap/MERRA'
            if(MERRA.get.variable.group(variable)=='MAT1NXSLV') {
              return(sprintf("%s/MAT1NXSLV.5.2.0/%04d/%02d/MERRA%03d.prod.assim.tavg1_2d_slv_Nx.%04d%02d%02d.hdf",
                                base.dir,year,month,runId,year,month,day))
            }
            if(MERRA.get.variable.group(variable)=='MAI1NXINT') {
              return(sprintf("%s/MAI1NXINT.5.2.0/%04d/%02d/MERRA%03d.prod.assim.inst1_2d_int_Nx.%04d%02d%02d.hdf",
                                base.dir,year,month,runId,year,month,day))
            }
            if(MERRA.get.variable.group(variable)=='MAT1NXFLX') {
              return(sprintf("%s/MAT1NXFLX.5.2.0/%04d/%02d/MERRA%03d.prod.assim.tavg1_2d_flx_Nx.%04d%02d%02d.hdf",
                                base.dir,year,month,runId,year,month,day))
            }
            if(MERRA.get.variable.group(variable)=='MAT1NXINT') {
              return(sprintf("%s/MAT1NXINT.5.2.0/%04d/%02d/MERRA%03d.prod.assim.tavg1_2d_int_Nx.%04d%02d%02d.hdf",
                                base.dir,year,month,runId,year,month,day))
            }
            if(MERRA.get.variable.group(variable)=='MAT1NXLND') {
              return(sprintf("%s/MAT1NXLND.5.2.0/%04d/%02d/MERRA%03d.prod.assim.tavg1_2d_lnd_Nx.%04d%02d%02d.hdf",
                                base.dir,year,month,runId,year,month,day))
            }
            if(MERRA.get.variable.group(variable)=='MAT1NXRAD') {
              runId<-runId+1
              return(sprintf("%s/MAT1NXRAD.5.2.0/%04d/%02d/MERRA%03d.prod.assim.tavg1_2d_rad_Nx.%04d%02d%02d.hdf",
                                base.dir,year,month,runId,year,month,day))
            }
            base.dir<-'http://goldsmr3.sci.gsfc.nasa.gov:80/opendap/MERRA'
            if(MERRA.get.variable.group(variable)=='MAI3CPASM') {
              return(sprintf("%s/MAI3CPASM.5.2.0/%04d/%02d/MERRA%03d.prod.assim.inst3_3d_asm_Cp.%04d%02d%02d.hdf",
                                base.dir,year,month,runId,year,month,day))
            }
            if(MERRA.get.variable.group(variable)=='MAT3CPCLD') {
              return(sprintf("%s/MAT3CPCLD.5.2.0/%04d/%02d/MERRA%03d.prod.assim.tavg3_3d_cld_Cp.%04d%02d%02d.hdf",
                                base.dir,year,month,runId,year,month,day))
            }
            if(MERRA.get.variable.group(variable)=='MAT3CPMST') {
              return(sprintf("%s/MAT3CPMST.5.2.0/%04d/%02d/MERRA%03d.prod.assim.tavg3_3d_mst_Cp.%04d%02d%02d.hdf",
                                base.dir,year,month,runId,year,month,day))
            }
            if(MERRA.get.variable.group(variable)=='MAT3CPODT') {
              return(sprintf("%s/MAT3CPODT.5.2.0/%04d/%02d/MERRA%03d.prod.assim.tavg3_3d_odt_Cp.%04d%02d%02d.hdf",
                                base.dir,year,month,runId,year,month,day))
            }
            if(MERRA.get.variable.group(variable)=='MAT3CPQDT') {
              return(sprintf("%s/MAT3CPQDT.5.2.0/%04d/%02d/MERRA%03d.prod.assim.tavg3_3d_qdt_Cp.%04d%02d%02d.hdf",
                                base.dir,year,month,runId,year,month,day))
            }
            if(MERRA.get.variable.group(variable)=='MAT3CPRAD') {
              return(sprintf("%s/MAT3CPRAD.5.2.0/%04d/%02d/MERRA%03d.prod.assim.tavg3_3d_rad_Cp.%04d%02d%02d.hdf",
                                base.dir,year,month,runId,year,month,day))
            }
            if(MERRA.get.variable.group(variable)=='MAT3CPTDT') {
              return(sprintf("%s/MAT3CPTDT.5.2.0/%04d/%02d/MERRA%03d.prod.assim.tavg3_3d_tdt_Cp.%04d%02d%02d.hdf",
                                base.dir,year,month,runId,year,month,day))
            }
            if(MERRA.get.variable.group(variable)=='MAT3CPTRB') {
              return(sprintf("%s/MAT3CPTRB.5.2.0/%04d/%02d/MERRA%03d.prod.assim.tavg3_3d_trb_Cp.%04d%02d%02d.hdf",
                                base.dir,year,month,runId,year,month,day))
            }
            if(MERRA.get.variable.group(variable)=='MAT3CPUDT') {
              return(sprintf("%s/MAT3CPUDT.5.2.0/%04d/%02d/MERRA%03d.prod.assim.tavg3_3d_udt_Cp.%04d%02d%02d.hdf",
                                base.dir,year,month,runId,year,month,day))
            }
          }
        if(type=='normal') {
          fn<-sprintf("/project/projectdirs/m958/netCDF.data/MERRA/hourly/normals/%s/%02d-%02d.nc",
	              variable,month,day)
          if(file.exists(fn) && file.info(fn)$size>0) return(fn)
          fn<-sprintf("/data/cr2/hadpb//MERRA/hourly/normals/%s/%02d-%02d.nc",
	              variable,month,day)
          if(file.exists(fn) && file.info(fn)$size>0) return(fn)
          stop("Selected normal not available on this system")
        }
        if(type=='standard.deviation') {
          fn<-sprintf("/project/projectdirs/m958/netCDF.data/MERRA/hourly/standard.deviations/%s/%02d-%02d.nc",
	              variable,month,day)
          if(file.exists(fn) && file.info(fn)$size>0) return(fn)
          fn<-sprintf("/data/cr2/hadpb//MERRA/hourly/standard.deviations/%s/%02d-%02d.nc",
	              variable,month,day)
          if(file.exists(fn) && file.info(fn)$size>0) return(fn)
          stop("Selected standard deviation not available on this system")
        }
        stop(sprintf("Only means, normals, and SDs available from MERRA"))      
    }     
    else {
        stop(sprintf("MERRA only available via openDAP"))
   }
}


MERRA.is.in.file<-function(variable,year,month,day,hour,type='mean') {
   group<-MERRA.get.variable.group(variable)
   if(group=='MAT1NXSLV' || group=='MAI1NXINT' || group=='MAT1NXFLX' ||
      group=='MAT1NXINT' || group=='MAT1NXLND' || group=='MAT1NXRAD') {
      if(hour%%1==0) return(TRUE)
      return(FALSE)
   }
   if(group=='MAI3CPASM' || group=='MAT3CPCLD' || group=='MAT3CPMST' ||
      group=='MAT3CPODT' || group=='MAT3CPQDT' || group=='MAT3CPRAD' ||
      group=='MAT3CPTDT' || group=='MAT3CPTRB' || group=='MAT3CPUDT') {
      if(hour%%3==0) return(TRUE)
      return(FALSE)
   }
   stop("Internal error - unknown variable group")
}

# Go backward and forward in hours to find previous and subsequent
#  hours at an analysis time.
# Could do this directly, but it's vital to keep get.interpolation.times
# and is.in.file consistent.
MERRA.get.interpolation.times<-function(variable,year,month,day,hour,type='mean') {
	if(MERRA.is.in.file(variable,year,month,day,hour,type=type)) {
		stop("Internal interpolation failure")
	}
	ct<-chron(dates=sprintf("%04d/%02d/%02d",year,month,day),
	          times=sprintf("%02d:00:00",hour),
	          format=c(dates='y/m/d',times='h:m:s'))
	t.previous<-list()
        back.hours=1
	while(back.hours<24) {
		p.hour<-hour-back.hours
                p.year<-year
                p.month<-month
                p.day<-day
                if(p.hour<0) {
                  p.year<-as.numeric(as.character(years(ct-1)))
                  p.month<-months(ct-1)
                  p.day<-days(ct-1)
                  p.hour<-p.hour+24
                }
		if(MERRA.is.in.file(variable,p.year,p.month,p.day,p.hour,type=type)) {
			t.previous$year<-p.year
			t.previous$month<-p.month
			t.previous$day<-p.day
			t.previous$hour<-p.hour
			break
		}
                back.hours<-back.hours+1
	}
	if(length(t.previous)<4) {
		stop("Interpolation failure, too far back")
	}
	t.next<-list()
	forward.hours<-1
	while(forward.hours<24) {
		n.hour<-hour+forward.hours
                n.year<-year
                n.month<-month
                n.day<-day
                if(n.hour>23) {
                  n.year<-as.numeric(as.character(years(ct+1)))
                  n.month<-months(ct+1)
                  n.day<-days(ct+1)
                  n.hour<-n.hour-24
                }
		if(MERRA.is.in.file(variable,n.year,n.month,n.day,n.hour,type=type)) {
			t.next$year<-n.year
			t.next$month<-n.month
			t.next$day<-n.day
			t.next$hour<-n.hour
			break
		}
                forward.hours<-forward.hours+1
	}
	if(length(t.next)<4) {
		stop("Interpolation failure, too far forward")
	}
	return(list(t.previous,t.next))
}

# This is the function users will call.
#' Get slice at hour.
#'
#' Get a 2D horizontal slice of a selected variable (as a GSDF field) for a given hour.
#'
#' Interpolates to the selected hour when the data available are less than hourly.
#' Interpolates to the selected height when the selected height is not that of a MERRA level.
#'
#' @export
#' @param variable 'T2M', 'SLP', 'U10M', 'H500' - or any MERRA variable
#' @param type 'mean' (default), 'normal', or 'standard.deviation' (no spreads available from MERRA)
#' @param height Height in hPa - leave NULL for monolevel
#' @param opendap Must be TRUE - no local option currently supported.
#' @return A GSDF field with lat and long as extended dimensions
MERRA.get.slice.at.hour<-function(variable,year,month,day,hour,height=NULL,opendap=TRUE,type='mean') {
  if(type=='normal') year<-1981 # MERRA normals have this year
  if(type=='standard.deviation') year<-1981 # So do SDs
  group<-MERRA.get.variable.group(variable)
  if(group=='MAT1NXSLV' || group=='MAI1NXINT' || group=='MAT1NXFLX' ||
      group=='MAT1NXINT' || group=='MAT1NXLND' || group=='MAT1NXRAD') {
    if(!is.null(height)) warning("Ignoring height specification for monolevel variable")
    return(MERRA.get.slice.at.level.at.hour(variable,year,month,day,hour,opendap=opendap,type=type))
  }
  # Find levels above and below selected height, and interpolate between them
  if(is.null(height)) stop(sprintf("No height specified for pressure variable %s",variable))
  if(height>1000 || height<0.1) stop("Height must be between 0.1 and 1000 hPa")
  level.below<-max(which(MERRA.heights>=height))
  if(height==MERRA.heights[level.below]) {
    return(MERRA.get.slice.at.level.at.hour(variable,year,month,day,hour,height=MERRA.heights[level.below],
                                          opendap=opendap,type=type))
  }
  below<-MERRA.get.slice.at.level.at.hour(variable,year,month,day,hour,height=MERRA.heights[level.below],
                                         opendap=opendap,type=type)
  above<-MERRA.get.slice.at.level.at.hour(variable,year,month,day,hour,height=MERRA.heights[level.below+1],
                                         opendap=opendap,type=type)
  above.weight<-(MERRA.heights[level.below]-height)/(MERRA.heights[level.below]-
                             MERRA.heights[level.below+1])
  below$data[]<-below$data*(1-above.weight)+above$data*above.weight
  idx.h<-GSDF.find.dimension(below,'height')
  below$dimensions[[idx.h]]$value<-height
  return(below)
}

MERRA.get.slice.at.level.at.hour<-function(variable,year,month,day,hour,
                                                   height=NULL,opendap=TRUE,type='mean') {
	dstring<-sprintf("%04d-%02d-%02d:%02d",year,month,day,hour)
	# Is it from an analysis time (no need to interpolate)?
	if(MERRA.is.in.file(variable,year,month,day,hour,type=type)) {
        file.name<-MERRA.hourly.get.file.name(variable,year,month,day,hour,opendap=opendap,type=type)
           t<-chron(sprintf("%04d/%02d/%02d",year,month,day),sprintf("%02d:00:00",hour),
                    format=c(dates='y/m/d',times='h:m:s'))
        offset<-3 # gap between dumps
        group<-MERRA.get.variable.group(variable)
        if(group=='MAT1NXSLV' || group=='MAI1NXINT' || group=='MAT1NXFLX' ||
           group=='MAT1NXINT' || group=='MAT1NXLND' || group=='MAT1NXRAD') offset=1
        
           v<-GSDF.ncdf.load(file.name,variable,lat.range=c(-90,90),lon.range=c(-180,180),
                             height.range=c(height,height+0.05),time.range=c(t,t+offset/24-0.001))
	   return(v)
	}
	# Interpolate from the previous and subsequent analysis times
	interpolation.times<-MERRA.get.interpolation.times(variable,year,month,day,hour,type=type)
	v1<-MERRA.get.slice.at.level.at.hour(variable,interpolation.times[[1]]$year,
                                                  interpolation.times[[1]]$month,
		                                  interpolation.times[[1]]$day,
                                                  interpolation.times[[1]]$hour,
                                                  opendap=opendap,type=type,height=height)
	v2<-MERRA.get.slice.at.level.at.hour(variable,interpolation.times[[2]]$year,
                                                  interpolation.times[[2]]$month,
		                                  interpolation.times[[2]]$day,
                                                  interpolation.times[[2]]$hour,
                                                  opendap=opendap,type=type,height=height)
	c1<-chron(dates=sprintf("%04d/%02d/%02d",interpolation.times[[1]]$year,
	                                         interpolation.times[[1]]$month,
	                                         interpolation.times[[1]]$day),
	          times=sprintf("%02d:00:00",interpolation.times[[1]]$hour),
	          format=c(dates='y/m/d',times='h:m:s'))
	c2<-chron(dates=sprintf("%04d/%02d/%02d",interpolation.times[[2]]$year,
	                                         interpolation.times[[2]]$month,
	                                         interpolation.times[[2]]$day),
	          times=sprintf("%02d:00:00",interpolation.times[[2]]$hour),
	          format=c(dates='y/m/d',times='h:m:s'))
	c3<-chron(dates=sprintf("%04d/%02d/%02d",year,month,day),
	          times=sprintf("%02d:00:00",hour),
	          format=c(dates='y/m/d',times='h:m:s'))
    if(c2==c1) stop("Zero interval in time interpolation")
    weight<-as.numeric((c2-c3)/(c2-c1))
    v<-v1
    idx.t<-GSDF.find.dimension(v,'time')
    v$dimensions[[idx.t]]$value<-v1$dimensions[[idx.t]]$value+
                                 as.numeric(v2$dimensions[[idx.t]]$value-v1$dimensions[[idx.t]]$value)*(1-weight)
    v$data[]<-v1$data*weight+v2$data*(1-weight)
    return(v)
}

#' Extract a hyperslab of data.
#'
#' Up to 4d (lat, lon, height, time).
#'
#' MERRA data is in daily files - time range must be contained in 1 day
#'
#' @export
#' @param variable MERRA variable name, only 2d variables will work.
#' @param date.range A pair of text date strings, e.g. c('1981-02-05:12','1981-03-27:18') - inclusive.
#' @param type - 'mean' (default), 'spread', 'normal', or 'standard.deviation'. 
#'  Note that standard deviations are not available over opendap.
#' @param height.range Bottom and top heights in hPa - leave NULL for monolevel
#' @param lat.range Min and max latitude in degrees - defaults to c(-90,90)
#' @param lon.range Min and max longitude in degrees - defaults to c(-180,180)
#' @param opendap must be TRUE - no local option
#' @return A GSDF field with the selected multidimensional data
MERRA.get.slab.from.hourly<-function(variable,date.range,
                                             height.range=NULL,
                                             lat.range=c(-90,90),
                                             lon.range=c(-180,180),
                                    opendap=TRUE,type='mean') {
    # Get the start and end dates
     start.d<-list()
     start.d$year<-as.integer(substr(date.range[1],1,4))
     start.d$month<-as.integer(substr(date.range[1],6,7))
     start.d$day<-as.integer(substr(date.range[1],9,10))
     start.d$hour<-as.integer(substr(date.range[1],12,13))
     start.d$chron<-chron(sprintf("%04d/%02d/%02d",start.d$year,start.d$month,start.d$day),
                          sprintf("%02d:00:00",start.d$hour),
                          format=c(dates='y/m/d',times='h:m:s'))
     end.d<-list()
     end.d$year<-as.integer(substr(date.range[2],1,4))
     end.d$month<-as.integer(substr(date.range[2],6,7))
     end.d$day<-as.integer(substr(date.range[2],9,10))
     end.d$hour<-as.integer(substr(date.range[2],12,13))
     end.d$chron<-chron(sprintf("%04d/%02d/%02d",end.d$year,end.d$month,end.d$day),
                          sprintf("%02d:00:00",end.d$hour),
                          format=c(dates='y/m/d',times='h:m:s'))
     if(start.d$chron>end.d$chron) stop("End date must be after start date")
     if(start.d$year != end.d$year | start.d$month != end.d$month | start.d$day != end.d$day ) {
       stop('Extraction not contained in one day')
     }
     file.name<-MERRA.hourly.get.file.name(variable,start.d$year,start.d$month,start.d$day,start.d$hour,
					    opendap=opendap,type=type)
     v<-GSDF.ncdf.load(file.name,variable,lat.range=lat.range,lon.range=lon.range,
			height.range=height.range,time.range=c(start.d$chron,end.d$chron))
     return(v)
}
