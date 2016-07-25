# Define an R class encapsulating a PP field
# And a set of methods for reading PP data

# Define the class
setClass("PP",
 representation(
  lbyr    = "integer",
  lbmon   = "integer",
  lbdat   = "integer",
  lbhr    = "integer",
  lbmin   = "integer",
  lbday   = "integer",
  lbyrd   = "integer",
  lbmond  = "integer",
  lbdatd  = "integer",
  lbhrd   = "integer",
  lbmind  = "integer",
  lbdayd  = "integer",
  lbtim   = "integer",
  lbft    = "integer",
  lblrec  = "integer",
  lbcode  = "integer",
  lbhem   = "integer",
  lbrow   = "integer",
  lbnpt   = "integer",
  lbext   = "integer",
  lbpack  = "integer",
  lbrel   = "integer",
  lbfc    = "integer",
  lbcfc   = "integer",
  lbproc  = "integer",
  lbvc    = "integer",
  lbrvc   = "integer",
  lbexp   = "integer",
  lbegin  = "integer",
  lbnrec  = "integer",
  lbproj  = "integer",
  lbtyp   = "integer",
  lblev   = "integer",
  lbrsvd  = "integer",
  lbsrce  = "integer",
  lbuser  = "integer",
  bacc    = "numeric",
  brsvd   = "numeric",
  bdatum  = "numeric",
  blev    = "numeric",
  brlev   = "numeric",
  bhlev   = "numeric",
  bhrlev  = "numeric",
  bplat   = "numeric",
  bplon   = "numeric",
  bgor    = "numeric",
  bzy     = "numeric",
  bdy     = "numeric",
  bzx     = "numeric",
  bdx     = "numeric",
  bmdi    = "numeric",
  bmks    = "numeric",
  data    = "numeric",
 # Remaining variables are from the extra data (and so optional)
  extra            = "integer",
  x                = "numeric",
  y                = "numeric",
  x_lower_domain   = "numeric",
  y_lower_domain   = "numeric",
  z_lower_domain   = "numeric",
  x_upper_domain   = "numeric",
  y_upper_domain   = "numeric",
  z_upper_domain   = "numeric",
  x_lower_boundary = "numeric",
  y_lower_boundary = "numeric",
  x_upper_boundary = "numeric",
  y_upper_boundary = "numeric",
  field_title      = "character",
  domain_title     = "character"
 ),
 prototype(
  lbyr    = as.integer(0),
  lbmon   = as.integer(0),
  lbdat   = as.integer(0),
  lbhr    = as.integer(0),
  lbmin   = as.integer(0),
  lbday   = as.integer(0),
  lbyrd   = as.integer(0),
  lbmond  = as.integer(0),
  lbdatd  = as.integer(0),
  lbhrd   = as.integer(0),
  lbmind  = as.integer(0),
  lbdayd  = as.integer(0),
  lbtim   = as.integer(0),
  lbft    = as.integer(0),
  lblrec  = as.integer(0),
  lbcode  = as.integer(0),
  lbhem   = as.integer(0),
  lbrow   = as.integer(0),
  lbnpt   = as.integer(0),
  lbext   = as.integer(0),
  lbpack  = as.integer(0),
  lbrel   = as.integer(0),
  lbfc    = as.integer(0),
  lbcfc   = as.integer(0),
  lbproc  = as.integer(0),
  lbvc    = as.integer(0),
  lbrvc   = as.integer(0),
  lbexp   = as.integer(0),
  lbegin  = as.integer(0),
  lbnrec  = as.integer(0),
  lbproj  = as.integer(0),
  lbtyp   = as.integer(0),
  lblev   = as.integer(0),
  lbrsvd  = rep(as.integer(0),4),
  lbsrce  = as.integer(0),
  lbuser  = rep(as.integer(0),7),
  bacc    = 0.0,
  brsvd   = rep(0.0,4),
  bdatum  = 0.0,
  blev    = 0.0,
  brlev   = 0.0,
  bhlev   = 0.0,
  bhrlev  = 0.0,
  bplat   = 90.0,
  bplon   = 0.0,
  bgor    = 0.0,
  bzy     = 0.0,
  bdy     = 0.0,
  bzx     = 0.0,
  bdx     = 0.0,
  bmdi    = 0.0,
  bmks    = 0.0,
  data    = rep(0.0,7007),
  extra            = as.integer(0),
  x                = 0.0,
  y                = 0.0,
  x_lower_domain   = 0.0,
  y_lower_domain   = 0.0,
  z_lower_domain   = 0.0,
  x_upper_domain   = 0.0,
  y_upper_domain   = 0.0,
  z_upper_domain   = 0.0,
  x_lower_boundary = 0.0,
  y_lower_boundary = 0.0,
  x_upper_boundary = 0.0,
  y_upper_boundary = 0.0,
  field_title      = "Undef",
  domain_title     = "Undef"
  )
)
setMethod("show","PP",function(object) pp.print(object))

# Define a method to print the PP field
pp.print <- function(pp, ...) {
 print(sprintf("Starts: %04d/%02d/%02d:%02d:%02d",
                pp@lbyr,pp@lbmon,pp@lbdat,pp@lbhr,pp@lbmin))
 print(sprintf("Ends:   %04d/%02d/%02d:%02d:%02d",
                pp@lbyrd,pp@lbmond,pp@lbdatd,pp@lbhrd,pp@lbmind))
 print(sprintf("lbday =%9d lbdayd=%9d lbtim =%9d",pp@lbday,pp@lbdayd,pp@lbtim))
 print(sprintf("lbft  =%9d lblrec=%9d lbcode=%9d lbhem  =%9d",pp@lbft,pp@lblrec,pp@lbcode,pp@lbhem))
 print(sprintf("lbrow =%9d lbnpt =%9d lbext =%9d lbpack =%9d",pp@lbrow,pp@lbnpt,pp@lbext,pp@lbpack))
 print(sprintf("lbrel =%9d lbfc  =%9d lbcfc =%9d lbproc =%9d",pp@lbrel,pp@lbfc,pp@lbcfc,pp@lbproc))
 print(sprintf("lbvc  =%9d lbrvc =%9d lbexp =%9d lbegin =%9d",pp@lbvc,pp@lbrvc,pp@lbexp,pp@lbegin))
 print(sprintf("lbnrec=%9d lbproj=%9d lbtyp =%9d lblev  =%9d",pp@lbnrec,pp@lbproj,pp@lbtyp,pp@lblev))
 print(sprintf("lbsrce=%9d",pp@lbsrce))
 print(sprintf("lbrsvd=%8d,%8d,%8d,%8d",pp@lbrsvd[1],pp@lbrsvd[2],pp@lbrsvd[3],pp@lbrsvd[4]))
 print(sprintf("lbuser=%8d,%8d,%8d,%8d,%8d,%8d,%8d",pp@lbuser[1],pp@lbuser[2],pp@lbuser[3],pp@lbuser[4],
                                                                 pp@lbuser[5],pp@lbuser[6],pp@lbuser[7]))
 print(sprintf("bacc  =%9g brsvd =%9g,%9g,%9g,%9g",pp@bacc,pp@brsvd[1],pp@brsvd[2],pp@brsvd[3],pp@brsvd[4]))
 print(sprintf("bdatum=%9g blev  =%9g brlev =%9g bhlev =%9g",pp@bdatum,pp@blev,pp@brlev,pp@bhlev))
 print(sprintf("bhrlev=%9g bplat =%9g bplon =%9g bgor  =%9g",pp@bhrlev,pp@bplat,pp@bplon,pp@bgor))
 print(sprintf("bzy   =%9g bdy   =%9g bzx   =%9g bdx   =%9g",pp@bzy,pp@bdy,pp@bzx,pp@bdx))
 print(sprintf("bmdi  =%9g bmds  =%9g",pp@bmdi,pp@bmks))
 print(sprintf("data: Array of length %9d",length(pp@data)))
 if(pp@lbext>0) {
    print("Extra data:")
 }
 if(length(pp@x)>1 || !is.na(pp@x)) {
    print(sprintf(" x: Array of length %9d",length(pp@x)))
 }
 if(length(pp@y)>1 || !is.na(pp@y)) {
    print(sprintf(" y: Array of length %9d",length(pp@y)))
 }
 if(length(pp@x_lower_domain)>1 || !is.na(pp@x_lower_domain)) {
    print(sprintf(" x_lower_domain: Array of length %9d",length(pp@x_lower_domain)))
 }
 if(length(pp@y_lower_domain)>1 || !is.na(pp@y_lower_domain)) {
    print(sprintf(" y_lower_domain: Array of length %9d",length(pp@y_lower_domain)))
 }
 if(length(pp@z_lower_domain)>1 || !is.na(pp@z_lower_domain)) {
    print(sprintf(" z_lower_domain: Array of length %9d",length(pp@z_lower_domain)))
 }
 if(length(pp@x_upper_domain)>1 || !is.na(pp@x_upper_domain)) {
    print(sprintf(" x_upper_domain: Array of length %9d",length(pp@x_upper_domain)))
 }
 if(length(pp@y_upper_domain)>1 || !is.na(pp@y_upper_domain)) {
    print(sprintf(" y_upper_domain: Array of length %9d",length(pp@y_upper_domain)))
 }
 if(length(pp@z_upper_domain)>1 || !is.na(pp@z_upper_domain)) {
    print(sprintf(" z_upper_domain: Array of length %9d",length(pp@z_upper_domain)))
 }
 if(length(pp@x_lower_boundary)>1 || !is.na(pp@x_lower_boundary)) {
    print(sprintf(" x_lower_boundary: Array of length %9d",length(pp@x_lower_boundary)))
 }
 if(length(pp@y_lower_boundary)>1 || !is.na(pp@y_lower_boundary)) {
    print(sprintf(" y_lower_boundary: Array of length %9d",length(pp@y_lower_boundary)))
 }
 invisible(pp)
}

# R function to close a file for PP access
pp.close.file <- function(pp.fn) {
    close(pp.fn)
}

# Function to open a file so we can read or write PP data to it
# Use modes 'rb' (read only), 'wb' (write - overwriting previous data)
#  and 'ab' (write - appending to previous data)
pp.open.file <- function(pp.file.name,mode="rb") {
# pp.file.name is a string naming a file
 fn<-file(pp.file.name,open=mode)
 return(fn)
}

# Function to read in a PP field from an (already opened) file
#  returns a PP structure.
pp.read <- function(pp.file.id) {
# pp.file.id is an integer returned by pp_open_file

    pp<-new('PP')

  # Read in the header
    padding<-readBin(pp.file.id,'integer',endian ='big')
    if(length(padding)==0) return(NULL) # No more fields
    pp@lbyr  <-readBin(pp.file.id,'integer',endian ='big')
    pp@lbmon <-readBin(pp.file.id,'integer',endian ='big')
    pp@lbdat <-readBin(pp.file.id,'integer',endian ='big')
    pp@lbhr  <-readBin(pp.file.id,'integer',endian ='big')
    pp@lbmin <-readBin(pp.file.id,'integer',endian ='big')
    pp@lbday <-readBin(pp.file.id,'integer',endian ='big')
    pp@lbyrd <-readBin(pp.file.id,'integer',endian ='big')
    pp@lbmond<-readBin(pp.file.id,'integer',endian ='big')
    pp@lbdatd<-readBin(pp.file.id,'integer',endian ='big')
    pp@lbhrd <-readBin(pp.file.id,'integer',endian ='big')
    pp@lbmind<-readBin(pp.file.id,'integer',endian ='big')
    pp@lbdayd<-readBin(pp.file.id,'integer',endian ='big')
    pp@lbtim <-readBin(pp.file.id,'integer',endian ='big')
    pp@lbft  <-readBin(pp.file.id,'integer',endian ='big')
    pp@lblrec<-readBin(pp.file.id,'integer',endian ='big')
    pp@lbcode<-readBin(pp.file.id,'integer',endian ='big')
    pp@lbhem <-readBin(pp.file.id,'integer',endian ='big')
    pp@lbrow <-readBin(pp.file.id,'integer',endian ='big')
    pp@lbnpt <-readBin(pp.file.id,'integer',endian ='big')
    pp@lbext <-readBin(pp.file.id,'integer',endian ='big')
    pp@lbpack<-readBin(pp.file.id,'integer',endian ='big')
    pp@lbrel <-readBin(pp.file.id,'integer',endian ='big')
    pp@lbfc  <-readBin(pp.file.id,'integer',endian ='big')
    pp@lbcfc <-readBin(pp.file.id,'integer',endian ='big')
    pp@lbproc<-readBin(pp.file.id,'integer',endian ='big')
    pp@lbvc  <-readBin(pp.file.id,'integer',endian ='big')
    pp@lbrvc <-readBin(pp.file.id,'integer',endian ='big')
    pp@lbexp <-readBin(pp.file.id,'integer',endian ='big')
    pp@lbegin<-readBin(pp.file.id,'integer',endian ='big')
    pp@lbnrec<-readBin(pp.file.id,'integer',endian ='big')
    pp@lbproj<-readBin(pp.file.id,'integer',endian ='big')
    pp@lbtyp <-readBin(pp.file.id,'integer',endian ='big')
    pp@lblev <-readBin(pp.file.id,'integer',endian ='big')
    pp@lbrsvd<-readBin(pp.file.id,'integer',endian ='big',n=4)
    pp@lbsrce<-readBin(pp.file.id,'integer',endian ='big')
    pp@lbuser<-readBin(pp.file.id,'integer',endian ='big',n=7)
    
    pp@brsvd <-readBin(pp.file.id,'numeric',size=4,endian ='big',n=4)
    pp@bdatum<-readBin(pp.file.id,'numeric',size=4,endian ='big')
    pp@bacc  <-readBin(pp.file.id,'numeric',size=4,endian ='big')
    pp@blev  <-readBin(pp.file.id,'numeric',size=4,endian ='big')
    pp@brlev <-readBin(pp.file.id,'numeric',size=4,endian ='big')
    pp@bhlev <-readBin(pp.file.id,'numeric',size=4,endian ='big')
    pp@bhrlev<-readBin(pp.file.id,'numeric',size=4,endian ='big')
    pp@bplat <-readBin(pp.file.id,'numeric',size=4,endian ='big')
    pp@bplon <-readBin(pp.file.id,'numeric',size=4,endian ='big')
    pp@bgor  <-readBin(pp.file.id,'numeric',size=4,endian ='big')
    pp@bzy   <-readBin(pp.file.id,'numeric',size=4,endian ='big')
    pp@bdy   <-readBin(pp.file.id,'numeric',size=4,endian ='big')
    pp@bzx   <-readBin(pp.file.id,'numeric',size=4,endian ='big')
    pp@bdx   <-readBin(pp.file.id,'numeric',size=4,endian ='big')
    pp@bmdi  <-readBin(pp.file.id,'numeric',size=4,endian ='big')
    pp@bmks  <-readBin(pp.file.id,'numeric',size=4,endian ='big')
    padding<-readBin(pp.file.id,'integer',endian ='big')
                                                                   
  # Allocate space for the data array
    if(pp@lbpack==1) {
      stop("Reading packed data not supported")
      pp@data<-rep(pp@bmdi,pp@lbrow*pp@lbnpt)
    } else{
      pp@data<-rep(pp@bmdi,pp@lblrec-pp@lbext)
    }
  # Read in the data array
    padding<-readBin(pp.file.id,'integer',endian ='big')
    if(length(padding)==0) return(NULL) # No more data
    pp@data<-readBin(pp.file.id,'numeric',size=4,endian ='big',n=length(pp@data))
    padding<-readBin(pp.file.id,'integer',endian ='big')
  # Set the extra data fields to missing (can this be done in the prototype?)
    is.na(pp@x)<-T
    is.na(pp@y)<-T
    is.na(pp@x_lower_domain)<-T
    is.na(pp@y_lower_domain)<-T
    is.na(pp@z_lower_domain)<-T
    is.na(pp@x_upper_domain)<-T
    is.na(pp@y_upper_domain)<-T
    is.na(pp@z_upper_domain)<-T
    is.na(pp@x_lower_boundary)<-T
    is.na(pp@y_lower_boundary)<-T
    is.na(pp@x_upper_boundary)<-T
    is.na(pp@y_upper_boundary)<-T
    is.na(pp@field_title)<-T
    is.na(pp@domain_title)<-T
  # If the field contains extra data, read and parse that as well
    if(pp@lbext!=0) {
      length.read<-0
      padding<-readBin(pp.file.id,'integer',endian ='big')
      while(pp@lbext > length.read) {
        eflag<-readBin(pp.file.id,'integer',endian ='big')
        elength<-as.integer(eflag/1000)
        etype<-eflag%%1000
        if(etype<1 || etype>15 || etype==9) {
          stop("Bad PP extra data: unknown type")
        }
         if(etype==1) {
          pp@x<-readBin(pp.file.id,'numeric',size=4,endian ='big',n=elength)
        }
         if(etype==2) {
          pp@y<-readBin(pp.file.id,'numeric',size=4,endian ='big',n=elength)
        }
         if(etype==3) {
          pp@y_lower_domain<-readBin(pp.file.id,'numeric',size=4,endian ='big',n=elength)
        }
         if(etype==4) {
          pp@x_lower_domain<-readBin(pp.file.id,'numeric',size=4,endian ='big',n=elength)
        }
         if(etype==5) {
          pp@y_upper_domain<-readBin(pp.file.id,'numeric',size=4,endian ='big',n=elength)
        }
         if(etype==6) {
          pp@x_upper_domain<-readBin(pp.file.id,'numeric',size=4,endian ='big',n=elength)
        }
         if(etype==7) {
          pp@z_lower_domain<-readBin(pp.file.id,'numeric',size=4,endian ='big',n=elength)
        }
         if(etype==8) {
          pp@z_upper_domain<-readBin(pp.file.id,'numeric',size=4,endian ='big',n=elength)
        }
        if(etype==10) {
          pp@field_title<-readBin(pp.file.id,'character',size=1,endian ='big',n=elength*4)
        }
        if(etype==11) {
          pp@domain_title<-readBin(pp.file.id,'character',size=1,endian ='big',n=elength*4)
        }
        if(etype==12) {
          pp@x_lower_boundary<-readBin(pp.file.id,'numeric',size=4,endian ='big',n=elength)
        }
         if(etype==13) {
          pp@x_upper_boundary<-readBin(pp.file.id,'numeric',size=4,endian ='big',n=elength)
        }
         if(etype==14) {
          pp@y_lower_boundary<-readBin(pp.file.id,'numeric',size=4,endian ='big',n=elength)
        }
         if(etype==15) {
          pp@y_upper_boundary<-readBin(pp.file.id,'numeric',size=4,endian ='big',n=elength)
        }
       length.read<-length.read+elength+1
     }
     padding<-readBin(pp.file.id,'integer',endian ='big')
   }
   
   return(pp);
    
}
# Function to write a (list of) PP field(s) to an (already opened) file
#  returns count of fields written.
pp.write <- function(pps,pp.file.id) {
# pp.file.id is an integer returned by pp_open_file (with mode='w' or 'a')
  if(!is.list(pps)) {
    pps<-list(pps)
  }
  written<-0
  for(pp in pps) {
      if(!inherits(pp,'PP')) {
       stop("non-PP-field passed to pp.write")
      }
      # Write out the header
    writeBin(256,      pp.file.id,'integer',endian ='big') # Fortran padding
    writeBin(pp@lbyr,  pp.file.id,'integer',endian ='big')
    writeBin(pp@lbmon, pp.file.id,'integer',endian ='big')
    writeBin(pp@lbdat, pp.file.id,'integer',endian ='big')
    writeBin(pp@lbhr,  pp.file.id,'integer',endian ='big')
    writeBin(pp@lbmi,  pp.file.id,'integer',endian ='big')
    writeBin(pp@lbday, pp.file.id,'integer',endian ='big')
    writeBin(pp@lbyrd, pp.file.id,'integer',endian ='big')
    writeBin(pp@lbmond,pp.file.id,'integer',endian ='big')
    writeBin(pp@lbdatd,pp.file.id,'integer',endian ='big')
    writeBin(pp@lbhrd, pp.file.id,'integer',endian ='big')
    writeBin(pp@lbmind,pp.file.id,'integer',endian ='big')
    writeBin(pp@lbdayd,pp.file.id,'integer',endian ='big')
    writeBin(pp@lbtim, pp.file.id,'integer',endian ='big')
    writeBin(pp@lbft,  pp.file.id,'integer',endian ='big')
    writeBin(pp@lblrec,pp.file.id,'integer',endian ='big')
    writeBin(pp@lbcode,pp.file.id,'integer',endian ='big')
    writeBin(pp@lbhem, pp.file.id,'integer',endian ='big')
    writeBin(pp@lbrow, pp.file.id,'integer',endian ='big')
    writeBin(pp@lbnpt, pp.file.id,'integer',endian ='big')
    writeBin(pp@lbext, pp.file.id,'integer',endian ='big')
    writeBin(pp@lbpack,pp.file.id,'integer',endian ='big')
    writeBin(pp@lbrel, pp.file.id,'integer',endian ='big')
    writeBin(pp@lbfc,  pp.file.id,'integer',endian ='big')
    writeBin(pp@lbcfc, pp.file.id,'integer',endian ='big')
    writeBin(pp@lbproc,pp.file.id,'integer',endian ='big')
    writeBin(pp@lbvc,  pp.file.id,'integer',endian ='big')
    writeBin(pp@lbrvc, pp.file.id,'integer',endian ='big')
    writeBin(pp@lbexp, pp.file.id,'integer',endian ='big')
    writeBin(pp@lbegin,pp.file.id,'integer',endian ='big')
    writeBin(pp@lbnrec,pp.file.id,'integer',endian ='big')
    writeBin(pp@lbproj,pp.file.id,'integer',endian ='big')
    writeBin(pp@lbtyp, pp.file.id,'integer',endian ='big')
    writeBin(pp@lblev, pp.file.id,'integer',endian ='big')
    writeBin(pp@lbrsvd,pp.file.id,'integer',endian ='big')# n=4
    writeBin(pp@lbsrce,pp.file.id,'integer',endian ='big')
    writeBin(pp@lbuser,pp.file.id,'integer',endian ='big')# n=7
      
    writeBin(pp@brsvd, pp.file.id,'numeric',size=4,endian ='big')# n=4
    writeBin(pp@bdatum,pp.file.id,'numeric',size=4,endian ='big')
    writeBin(pp@bacc,  pp.file.id,'numeric',size=4,endian ='big')
    writeBin(pp@blev,  pp.file.id,'numeric',size=4,endian ='big')
    writeBin(pp@brlev, pp.file.id,'numeric',size=4,endian ='big')
    writeBin(pp@bhlev, pp.file.id,'numeric',size=4,endian ='big')
    writeBin(pp@bhrlev,pp.file.id,'numeric',size=4,endian ='big')
    writeBin(pp@bplat, pp.file.id,'numeric',size=4,endian ='big')
    writeBin(pp@bplon, pp.file.id,'numeric',size=4,endian ='big')
    writeBin(pp@bgor,  pp.file.id,'numeric',size=4,endian ='big')
    writeBin(pp@bzy,   pp.file.id,'numeric',size=4,endian ='big')
    writeBin(pp@bdy,   pp.file.id,'numeric',size=4,endian ='big')
    writeBin(pp@bzx,   pp.file.id,'numeric',size=4,endian ='big')
    writeBin(pp@bdx,   pp.file.id,'numeric',size=4,endian ='big')
    writeBin(pp@bmdi,  pp.file.id,'numeric',size=4,endian ='big')
    writeBin(pp@bmks,  pp.file.id,'numeric',size=4,endian ='big')
    writeBin(256,      pp.file.id,'integer',endian ='big') # Fortran padding

    # Write the data array
    writeBin(256,    pp.file.id,'integer',endian ='big') # Fortran padding
    writeBin(pp@data,pp.file.id,'numeric',size=4,endian ='big')
    writeBin(256,    pp.file.id,'integer',endian ='big') # Fortran padding

    # Can't be bothered with Extra data at the moment
    if(pp@lbext>0) stop('Writing PP files with extra data not supported')
    written<-written+1
  }
  return(written)
}
# Get an array of PP fields - all those from a file where the header
#  matches a set of conditions
pp.ppa<-function(filename,where=NULL,max=NULL) {
  i<-pp.ppa.iter(filename,where=where,max=max)
  return(as.list(i))
}

# Get an iterator returning a set of PP fields - all those from a file
#  where the header matches a set of conditions
pp.ppa.iter<-function(filename,where=NULL,max=NULL) {
 # Convert the conditionals into a function testing them on a PP field
   check<-pp.internal.checkfn(where=where)
   f<-pp.open.file(filename)
   idx<-0
   cache<-NULL
    nextEl<-function() { # Get the next element
      pp<-NULL
      if(!is.null(max) && idx>=max) {
        pp.close.file(f)
        stop("StopIteration",call.=FALSE)
      }
      if(!is.null(cache)) {
         pp<-cache
         cache<<-NULL
      } else {
        repeat {
           pp<-pp.read(f)
           if(is.null(pp)) {
              pp.close.file(f)
              stop("StopIteration",call.=FALSE)
           }
           if(length(where)==0 || check(pp)==1) break
         }
         idx<<-idx+1 
      }
      return(pp)
     }
     hasNX<-function() {
       if(!is.null(cache)) return(TRUE)
       tryCatch({cache<<-nextEl()},
                error=function(e) {
                        if(!identical(conditionMessage(e),"StopIteration")) {
                             stop(e)
                           }
                })
       if(!is.null(cache)) return(TRUE)
       else return(FALSE)
     }
   obj<-list(nextElem = nextEl, hasNext = hasNX)
   class(obj) <- c("ippa","abstractiter","iter")
   return(obj)
}

# Output a set of PP fields - all those where the header
#  matches a set of conditions. Returns the number of fields output.
pp.ppw<-function(ppl,filename,where=NULL,max=NULL, mode='w') {
 # ppl is a (list of) pp fields, filename the name of the file to write to
 # (overwritten by default - set mode to 'a' to append).

 # if ppl is a single field, convert it to a list
    if(!is.list(ppl)) ppl<-list(ppl)
 
 # Convert the conditionals into a function testing them on a PP field
    check<-pp.internal.checkfn(where=where)
 # For each field in the list, storing those that pass the conditional test
   f<-pp.open.file(filename,mode=mode)
   n_written=0
   for (pp in ppl) {
      if(check(pp)==1) { 
        pp.write(pp,f)
        n_written<-n_written+1
      }
      if(!is.null(max) && n_written>=max) { break }
   }
   pp.close.file(f)
   return(n_written)
}

# Utility function for ppa and ppw - convert a list of conditionals into a function
#  testing them on a PP field.
# Take a list like c('lbyr==1987','lbmon<3','bdx>7.2') and returns a function.
#  Calling that function with a PP field as an argument will return 1 if all the
#  conditions are satisfied, and 0 otherwise.
pp.internal.checkfn<-function(where=NULL) {
    if(length(where)<1) { # No conditionals - always true
       check_fn<-"check<-function(pp) {return(1)}"
    }
    else {
    check_fn<-paste("check<-function(pp) {\n",
                    "if(")
            if(length(where)>1) {
               for(i in 1:{length(where)-1}) {
                   check_fn<-paste(check_fn,sprintf("pp@%s &&\n",where[i]))
               }
            }
            check_fn<-paste(check_fn,sprintf("pp@%s) {\n",where[length(where)]),
                    "return(1) }\n",
                    "else { return (0) }\n",
                    "}\n")
    }
    check<-eval(parse(text=check_fn))
    return(check)
}

# Try and strip extra data from PP fields - only possible
#  if the x and y arrays are evenly spaced.
pp.strip.extra<-function(fields) {
#  field is a (list of) PP field(s)

  a.isList<-TRUE
  if(!is.list(fields)) {
    fields<-list(fields)
    a.isList<-FALSE
  }

  results<-list()
  count<-0
  for(field in fields) {
      if(!inherits(field,'PP')) {
       stop("non-PP-field passed to pp.strip.extra")
      }
      
    count<-count+1 
    if(length(field@x)>1) {
        diffs=rep(0,length(field@x)-1)
        for(i in 2:length(field@x)) {
            diffs[i-1]=field@x[i]-field@x[i-1]
        }
        if((max(diffs)-min(diffs))<min(diffs)/1000) {
            field@bdx=field@x[2]-field@x[1]
            field@bzx = (3*field@x[1]-field@x[2])/2
        }
        else {
            warning("Can't strip extra data - irregular x spacings")
        }
    }
    if(length(field@y)>1) {
        diffs=rep(0,length(field@y)-1)
        for(i in 2:length(field@y)) {
            diffs[i-1]=field@y[i]-field@y[i-1]
        }
        if((max(diffs)-min(diffs))<min(diffs)/1000) {
            field@bdy=field@y[2]-field@y[1]
            field@bzy = (3*field@y[1]-field@y[2])/2
        }
        else {
            warning("Can't strip extra data - irregular y spacings")
        }
    }
    field@lblrec=field@lblrec-field@lbext
    field@lbext=as.integer(0)
    results[[count]]<-field
    }
  names(results)<-names(fields)
  if(a.isList) return(results)
  return(results[[1]])
}
        
# Undo run_length encoding (should use the unpack library instead).
pp.nrle<-function(pps) {
  
  a.isList<-TRUE
  if(!is.list(pps)) {
    pps<-list(pps)
    a.isList<-FALSE
  }

  results<-list()
  count<-0
  for(pp in pps) {
      if(!inherits(pp,'PP')) {
       stop("non-PP-field passed to pp.nrle")
      }
      count<-count+1
      pp2<-pp
      if(pp2@lbpack !=4) { return(pp2) }
      d2<-rep(1.0,0)
      i <- 1
      while(i <= length(pp@data)) {
        if(is.na(pp@data[i])) {
           d2<-c(d2,rep(NA,pp@data[i+1]))
           i<-i+1
        } else {
           d2<-c(d2,pp@data[i])
        }   
        i<-i+1
      }
      pp2@data<-d2
      results[[count]]<-pp2
   }
   names(results)<-names(pps)
   if(a.isList) return(results)
   return(results[[1]])
}


# Get the grid point longitudes and latitudes
#  .get.lats/longs - returns a value for each grid point (same length as the data field)
#  .get.lats/longs.short - returns just the set of lats/longs used (length of y/x dimensions of the data field)
pp.get.longs<-function(pps) {

  a.isList<-TRUE
  if(!is.list(pps)) {
    pps<-list(pps)
    a.isList<-FALSE
  }
  results<-list()
  count<-0
  for(pp in pps) {
      if(!inherits(pp,'PP')) {
       stop("non-PP-field passed to pp.get.longs")
      }
      count<-count+1
  # Allocate space for the longitudes
      longs<-rep(0.0,pp@lbrow*pp@lbnpt)
  # Calculate the longs
      o1<-.C("pp_data_get_long_c",pp@lbrow,pp@lbnpt,pp@bdx,pp@bzx,
              pp@x,longs,DUP=FALSE,NAOK=TRUE,PACKAGE="PP")
      results[[count]]<-longs
   }
   names(results)<-names(pps)
   if(a.isList) return(results)
   return(results[[1]])
}

# Get latitudes for each data point
pp.get.lats<-function(pps) {

  a.isList<-TRUE
  if(!is.list(pps)) {
    pps<-list(pps)
    a.isList<-FALSE
  }
  results<-list()
  count<-0
  for(pp in pps) {
      if(!inherits(pp,'PP')) {
       stop("non-PP-field passed to pp.get.lats")
      }
      count<-count+1
      # Allocate space for the latitudes
        lats<-rep(0.0,pp@lbrow*pp@lbnpt)
      # Calculate the lats
        o1<-.C("pp_data_get_lat_c",pp@lbrow,pp@lbnpt,pp@bdy,pp@bzy,
                pp@y,lats,DUP=FALSE,NAOK=TRUE,PACKAGE="PP")
       results[[count]]<-lats
   }
   if(a.isList) return(results)
   names(results)<-names(pps)
   return(results[[1]])
}
pp.get.longs.short<-function(pps) {
  
  a.isList<-TRUE
  if(!is.list(pps)) {
    pps<-list(pps)
    a.isList<-FALSE
  }

  results<-list()
  count<-0
  for(pp in pps) {
      if(!inherits(pp,'PP')) {
       stop("non-PP-field passed to pp.get.longs.short")
      }
      count<-count+1
      if(length(pp@x)>1) {
        results[[count]]<-pp@x
        next
      }
      results[[count]]<-seq(1,pp@lbnpt)*pp@bdx+pp@bzx
    }
   names(results)<-names(pps)
   if(a.isList) return(results)
   return(results[[1]])
}
pp.get.lats.short<-function(pps) {
  
  a.isList<-TRUE
  if(!is.list(pps)) {
    pps<-list(pps)
    a.isList<-FALSE
  }

  results<-list()
  count<-0
  for(pp in pps) {
      if(!inherits(pp,'PP')) {
       stop("non-PP-field passed to pp.get.lats.short")
      }
      count<-count+1
      if(length(pp@y)>1) {
        results[[count]]<-pp@y
        next
      }
      results[[count]]<-seq(1,pp@lbrow)*pp@bdy+pp@bzy
    }
   names(results)<-names(pps)
   if(a.isList) return(results)
   return(results[[1]])
}

# Get Boundary polygons for each data point
pp.get.poly<-function(pps) {

  a.isList<-TRUE
  if(!is.list(pps)) {
    pps<-list(pps)
    a.isList<-FALSE
  }

  results<-list()
  count<-0
  for(pp in pps) {
      if(!inherits(pp,'PP')) {
       stop("non-PP-field passed to pp.get.poly")
      }
      count<-count+1
      ll<-rep(0.0,pp@lbrow*pp@lbnpt*8)
      # Calculate the lats
      o1<-.C("pp_data_get_poly_c",pp@lbrow,pp@lbnpt,
                                  pp@bdx,pp@bzx,pp@x,
                                  pp@x_upper_boundary,pp@x_lower_boundary,
                                  pp@bdy,pp@bzy,pp@y,
                                  pp@y_upper_boundary,pp@y_lower_boundary,
                ll,DUP=FALSE,NAOK=TRUE,PACKAGE="PP")
      llpoly<- array(data=ll,dim=c(2,4,pp@lbnpt,pp@lbrow))
      results[[count]]<-llpoly
   }
   names(results)<-names(pps)
   if(a.isList) return(results)
   return(results[[1]])
 
}

# Specials for pp.regrid (in PP.utils library) - same as .get.lats/longs
# except they don't apply the 0.5 grid-box shift to get the
# grid box centre.
pp.get.longs.noshift<-function(pp) {
   if(length(pp@x)>1) { return(rep(pp@x,pp@lbrow)) }
   x<-seq(1,pp@lbnpt)*pp@bdx+pp@bzx
   return(rep(x,pp@lbrow))
}
pp.get.lats.noshift<-function(pp) {
   if(length(pp@y)>1) { y<-pp@y }
   else { y<-seq(1,pp@lbrow)*pp@bdy+pp@bzy }
   y2<-rep(1.0,0)
   for(i in seq(1,length(y))) {
      y2<-c(y2,rep(y[i],pp@lbnpt))
   }
   return(y2)
}
