moonphase <-
  function(ptime) {
##========================================================================
## Returns moonphase in radians given given the date in POSIXct form.
## 0 => new; pi/2 => first quarter; pi => full; 3*pi/2 => last quarter.
##     Adapted from Stephen R. Schmitt:  Sky & Telescope, Astronomical
## Computing, April 1994  and
##  http://mysite.verizon.net/res148h4j/zenosamples/zs_lunarphasecalc.html
##    which refs.
##  Jean Meeus. 1991. Astronomical Algorithms. Willmann-Bell, Inc. 429p.
##========================================================================  
    Y <- POSIX2yr(ptime)
    M <- POSIX2mo(ptime)
    D <- POSIX2day(ptime)
    
    ## calculate the Julian date at 12h UT
    YY <- Y - floor( ( 12 - M ) / 10 )      
    MM <- M + 9 
    MM <- ifelse(MM >= 12, MM-12, MM)
    
    K1 <- floor( 365.25 * ( YY + 4712 ) )
    K2 <- floor( 30.6001 * MM + 0.5 )
    K3 <- floor(floor((YY/100)+49)*0.75) - 38
    
    JD <- K1 + K2 + D + 59            ## for dates in Julian calendar
    JD <- ifelse(JD > 2299160, JD - K3, JD) ## for Gregorian calendar
    
    IP <- (JD - 2451550.1)/29.530588853
    IP <- IP-floor(IP)
    IP <- ifelse(IP<0, IP+1, IP)
    ## get moon age in days
    ##IP*29.53
    ## Convert phase to radians
    IP*2*pi                          
  }

moon.illumination <- function(ptime) {
##========================================================================
## returns proportion of moon illuminated given ptime, a POSIXct date.
##========================================================================  
  (1-sin(pi/2-moonphase(ptime)))/2
}
