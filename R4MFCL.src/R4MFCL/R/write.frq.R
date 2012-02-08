write.frq <- function (frqfile, frq.obj, sf=8) 
##======================================================================
##    Revised, cleaned, simplified, PKleiber, jan-2012
## Does a sanity checks on number of fisheries and on number of
## catch/effort data records. Makes appropriate fixes with warnings.
##======================================================================
{
  tt <- ungrp.mat(frq.obj)
  mat.ce <- tt$mat.ce
  mat.l <- tt$mat.l
  mat.w <- tt$mat.w
    ## sanity checks on frq.obj:
    ##   number of fisheries
  nf <- length(unique(frq.obj$mat[, "fishery"]))
  if(frq.obj$struct$nf != nf) {
    warning(paste("number of fisheries changed to",nf))
    frq.obj$struct$nf <- nf
  }
    ##   num data records
  nd <- nrow(frq.obj$mat)
  if(frq.obj$dl$dsets != nd) {
    warning(paste("number of data records changed to",nd))
    frq.obj$dl$dsets <- nd
  }

    ## deal with header info in .frq file
  ttl <- frq.obj$frq.title
  defs <- paste(c("# Definition of fisheries", "#",
                  "# Fishery   Gear   Nation          Region    Season, "), 
                 collapse = "\n")
  for (i in 1:dim(frq.obj$fish)[1]) defs <- rbind(defs, paste(c("#", 
                      frq.obj$fish[i, 3:6]), collapse = "       "))
  defs <- rbind(defs, "#")
  t1 <- rbind(c("# Number of   Number of   Use generic   Number of     Year1", 
                "#  Region     Fisheries    diffusion    tag groups"))
  t2 <- "# Relative Region Size"
  t3 <- rbind(c("#", "# Region in which each fishery is located"))
  t4a <- rbind(c("#", "# Incidence matrix"))
  nreg <- frq.obj$struct$nreg
  l_inc <- ""
  if (nreg > 1) {
    l_inc <- rep(NA, nreg - 1)
    first <- 1
    last <- nreg - 1
    for (i in 1:(nreg - 1)) {
      l_inc[i] <- paste(frq.obj$reg$incidence[first:last], collapse = " ")
      first <- last + 1
      last <- first + nreg - (i + 2)
    }
  }
  t4b <- rbind(c("#", "# Data flags (for records 1, 0=catch in number; 1=catch in weight)"))
  t4c <- "# Season-region flags"
  t5 <- "# Number of movements per year"
  t6 <- "# Weeks in which movement occurs"
  t7 <- rbind(c("# fishery data", "#", "#",
                "# Datasets / LFIntervals  LFFirst  LFWidth  LFFactor / WFIntervals  WFFirst  WFWidth"))
  t8 <- "# age_nage   age_age1"
  line1 <- paste(c("    ", paste(frq.obj$struct[1:5], collapse = "           "), 
                   "", frq.obj$struct[6:10]), collapse = " ")
  line2 <- frq.obj$reg
  line3 <- paste(c(" ", t(frq.obj$fish$fishreg)), collapse = " ")
  a <- paste(rep(0, frq.obj$struct$nf), collapse = " ")
  line4 <- vector(mode = "character")
  for (i in 1:dim(frq.obj$dflags)[1]) {
    if (i < dim(frq.obj$dflags)[1]) {
      line4 <- paste(line4, paste(as.character(frq.obj$dflags[i,]), collapse = " "), "\n", sep = "")
    }
    else {
      line4 <- paste(line4, paste(as.character(frq.obj$dflags[i,]), collapse = " "), sep = "")
    }
  }
  if (frq.obj$struct$te >= 6) {
    line4 <- rbind(line4, t4c)
    for (ssn in 1:frq.obj$struct$tc) {
      line4 <- rbind(line4, paste(frq.obj$reg$seas_reg_flags[ssn,],  
                  collapse = " "))
    }
  }
  line5 <- frq.obj$mpy
  line6 <- paste(t(frq.obj$mweeks), collapse = " ")
  line7 <- paste(c("   ", paste(frq.obj$dl, collapse = "         ")), 
                 collapse = "")
  if (frq.obj$struct$te >= 6) { 
    line8 <- paste(t8, paste(c("   ", paste(frq.obj$struct$age_inds, 
                      collapse = "         ")), collapse = ""), sep = "\n")
    line9 <- "# year month week fishery catch effort cv samples"
  } else {
    line8 <- ""
    line9 <- "# year month week fishery catch effort samples"
  }
  
                  
  if (l_inc[1] == "") {
    top <- paste(c(ttl, frq.obj$top, defs, t1, line1, t2, paste(frq.obj$reg$relreg, 
                   collapse = " "), t3, line3, t4a, t4b, line4, t5, 
                   line5, t6, line6, t7, line7, line8, line9), collapse = "\n")
  }
  else {
    top <- paste(c(ttl, frq.obj$top, defs, t1, line1, t2, paste(frq.obj$reg$relreg, 
                   collapse = " "), t3, line3, t4a, l_inc, t4b, line4, 
                   t5, line5, t6, line6, t7, line7, line8, line9), collapse = "\n")
  }

    ## make character version of sample matrices
  matout <- vector(mode = "character", length = 0)
  lfint <- frq.obj$dl$lfint
  wfint <- frq.obj$dl$wfint
  if ((lfint != 0 && wfint == 0) || (lfint == 0 && wfint != 0)) {
    for (i in 1:frq.obj$dl$dsets) {
      if (mat.l[i,1] == -1) {
        matout[i] <- " -1"
      }
      else {
        matout[i] <- paste(" ", mat.l[i, ], collapse = " ")
      }
    }
  }
  else if (lfint != 0 && wfint != 0) {
    for (i in 1:frq.obj$dl$dsets) {
     if (mat.l[i,1] == -1) {
        matout[i] <- " -1"
      }
      else {
        matout[i] <- paste(" ", mat.l[i, ], collapse = " ")
      }
      if (mat.w[i,1] == -1) 
        matout[i] <- paste(matout[i], " -1")
      else
        matout[i] <- paste(matout[i], " ", paste(mat.w[i,], collapse = " "))
    }
  }
    ## output the whole mess
  writeLines(top, frqfile)
  if (frq.obj$struct$te < 6) {
    write.table(cbind(format(mat.ce[, 1]),
                      format(mat.ce[, 2:4]),
                      format(formatC(mat.ce[,5],format = "fg",digits = sf),justify = "right"), 
                      format(formatC(mat.ce[,6],format = "fg",digits = sf),justify = "right"),
                      matout),
                frqfile, quote = F, sep = " ", row.names = F, col.names = F, append = T)
  }
  else {
    write.table(cbind(format(mat.ce[, 1]),
                      format(mat.ce[, 2:4]),
                      format(formatC(mat.ce[,5],format = "fg",digits = sf),justify = "right"), 
                      format(formatC(mat.ce[,6],format = "fg",digits = sf),justify = "right"),
                      format(formatC(mat.ce[,7],format = "fg",digits = sf),justify = "right"),
                      matout),
                frqfile, quote = F, sep = " ", row.names = F, col.names = F, append = T)
  }
}
