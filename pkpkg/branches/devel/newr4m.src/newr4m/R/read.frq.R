read.frq <-
  function (frq.file, frq.title = "", ntop = 0, fishdefs = NA, sf=7)
##======================================================================
##    Revised, cleaned, simplified, PKleiber, jan11-2012
## Outputs separate matrices for catch/effort, length samples and
## weight samples. For now also makes combined matrix with weight
## samples occupying length sample area in rows w/o length samples, as
## in MFCL .frq files.
##======================================================================
{
    ## deal with pesky hidden character
  op <- options(warn = -1)
  ff <- readLines(frq.file)
  options(op)
  ff <- gsub("\032", "", ff)
    ## title of .frq file
  if (frq.title == "") 
    frq.title <- ff[1]
    ## additional text at top of file
  if (ntop > 0) 
    top <- ff[2:ntop]
  else top <- "#"
  
    ## get all numerical values in file
  tmpfile <- tempfile()
  writeLines(ff, tmpfile)
  a <- scan(tmpfile, comment.char = "#", quiet=TRUE)
  file.remove(tmpfile)

    ## structural stuff
  nreg <- a[1]
  nf <- a[2]
  gendiff <- a[3]
  ntg <- a[4]
  yr1 <- a[5]
  ta <- a[6]
  tb <- a[7]
  tc <- a[8]
  td <- a[9]
  te <- a[10]
    ## relative region size vector
  first <- 11
  last <- first + nreg - 1
  relreg <- a[first:last]
    ## fishery region vector
  first <- last + 1
  last <- first + nf - 1
  fishreg <- a[first:last]
    ## incidence matrix
  if (nreg > 1) {
    first <- last + 1
    last <- first + (nreg * (nreg - 1)/2) - 1
    incidence <- a[first:last]
  }
  else {
    incidence <- NA
  }
    ## data flags matrix
  dflags <- matrix(0, nrow = 5, ncol = nf)
  for (i in 1:5) {
    first <- last + 1
    last <- first + nf - 1
    dflags[i, ] <- a[first:last]
  }
    ## season-region flags
  if (te >= 6) {
    first <- last + 1
    last <- first + tc * nreg - 1
    seas_reg_flags <- matrix(a[first:last], nrow = tc)
  }
    ## movement weeks
  first <- last + 1
  mpy <- a[first]
  first <- first + 1
  last <- first + mpy -1
  mweeks <- a[first:last]
    ## catch/effort/sample data structure
  first <- last + 1
  last <- first + 8
  dl <- as.list(a[first:last])
  names(dl) = c("dsets", "lfint", "lffirst", "lfwidth", "lffactor", 
         "wfint", "wffirst", "wfwidth", "wffactor")
   if (te >= 6) {
    first <- last + 1
    last <- first + 1
    age_inds <- a[first:last]
  }

    ## define catch/effort and sample matrices 
  hh <- ifelse(te>=6,7,6)
  mat1 <- matrix(0, dl$dsets, hh)
  mat2 <- matrix(0,dl$dsets,dl$lfint)
  mat3 <- matrix(0,dl$dsets,dl$wfint) # has no columns if wfint==0
  if (te >= 6)  colnames(mat1) <- c("year", "qtr", "week", "fishery", 
                       "catch", "effort", "cv")
  else          colnames(mat1) <- c("year", "qtr", "week", "fishery", 
                       "catch", "effort")
    ## fill matrices
  for(row in 1:dl$dsets) {
    first <- last + 1
    last <- last + hh
    mat1[row,1:hh] <- a[first:last]
    
    first <- last + 1
    nn <- ifelse(a[first]<0,1,dl$lfint) #(assume for now that lfint aways >0)
    last <- last+nn
    mat2[row,1:nn] <- a[first:last]
    
    if(dl$wfint>0) {
      first <- last + 1
      nn <- ifelse(a[first]<0,1,dl$wfint)
      last <- last+nn
      mat3[row,1:nn] <- a[first:last]
    }
  }
  
    ## round to 7 significant figures
  mat1 <- signif(mat1,sf)
  
    ## combine into one big matrix as in the .frq file
  mat <- matrix(0,nrow=dl$dsets,ncol=hh+dl$lfint+dl$wfint)
  mat[,1:hh] <- mat1
  colnames(mat) <- c(colnames(mat1),rep("",dl$lfint+dl$wfint))
  mat[,(hh+1):(hh+ncol(mat2))] <- mat2
  if(dl$wfint>0) {
    cc1 <- ifelse(mat2[,1]<0, hh+2, hh+ncol(mat2)+1)
    cc2 <- cc1+ncol(mat3)-1
    for(row in 1:nrow(mat)) mat[row,cc1[row]:cc2[row]] <- mat3[row,]
  }
 
    ## sanity check on no. fisheries
  true_nf <- length(unique(mat1[,"fishery"]))
  if (nf != true_nf) 
    warning("nf = ", nf, " and there are ", true_nf, " fisheries")
  
    ## assemble fishery stuff
  if (is.na(fishdefs)) 
    fishdefs <- data.frame(cbind(fishery = sort(unique(mat1[,"fishery"])),
                                 gear = NA, nations = NA, areas = NA))
  fish <- data.frame(fishreg = fishreg, ctype = dflags[1,], fishdefs)

  reg <- list(relreg = relreg, incidence = incidence)
  if (te >= 6) reg$seas_reg_flags = seas_reg_flags
  
  struct <- list(nreg = nreg, nf = nf, gendiff = gendiff, 
                 ntg = ntg, yr1 = yr1, ta = ta, tb = tb, tc = tc,
                 td = td, te = te)
  if (te >= 6) struct$age_inds = age_inds
  struct$nf <- length(unique(mat1[,"fishery"]))
  
  list(frq.title = frq.title, top = top, fish = fish, 
              reg = reg, struct = struct, dflags = dflags, mpy = mpy, 
              mweeks = mweeks, dl = dl,
              mat = mat)
            ##mat = mat, mat1 = as.data.frame(mat1), mat2 = mat2, mat3 = mat3)
}
