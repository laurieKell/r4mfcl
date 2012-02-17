POSIX2yr <- function(ptime) {
  as.numeric(strftime(ptime,"%Y"))
}

POSIX2quarter <- function(ptime) {
  POSIX2yr(ptime) + ceiling(POSIX2mo(ptime)/3)*.25-.125
}

POSIX2mo <- function(ptime) {
  as.numeric(strftime(ptime,"%m"))
}

POSIX2hr.t <- function(ptime) {
  as.numeric(strftime(ptime,"%H"))+
  as.numeric(strftime(ptime,"%M"))/60
}

POSIX2yr.t <- function(ptime) {
  yr <- as.numeric(strftime(ptime,"%Y"))
  dpy <- daysPerYear(yr)
  yr + .99999*as.numeric(strftime(ptime,"%j"))/dpy
}

POSIX2day <- function(ptime) {
  as.numeric(strftime(ptime,"%d"))
}
 
daysPerYear <- function(y){
  ifelse(y %% 4 == 0 & (y %% 100 != 0 | y %% 400 == 0),366,365)
} 
