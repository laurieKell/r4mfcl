pkgname <- "pkpkg"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('pkpkg')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("plotmap")
### * plotmap

flush(stderr()); flush(stdout())

### Name: plotmap
### Title: plot maps
### Aliases: plotmap
### Keywords: package

### ** Examples

plotmap(130,250,-5,65,5,landcolor="grey")



cleanEx()
nameEx("prepplot")
### * prepplot

flush(stderr()); flush(stdout())

### Name: prepplot
### Title: function to set up a plot
### Aliases: prepplot
### Keywords: package

### ** Examples

require(pkpkg)
prepplot(1:10,runif(1000,80,100))
prepplot(1:10,runif(1000,80,100),zero=FALSE)
DF <- data.frame(x=1:20,y1=runif(20,3,5),y2=runif(20,8,15))
prepplot(DF$x,DF[,-1])
with(DF,lines(x,y1))
with(DF,lines(x,y2))



cleanEx()
nameEx("scanText")
### * scanText

flush(stderr()); flush(stdout())

### Name: scanText
### Title: scanText
### Aliases: scanText

### ** Examples
scanText("A few short words")
as.numeric(scanText("1 2 3\n89 90"))
scanText("A B C 4 5 6", what = list("", "", "", 0, 0, 0))


cleanEx()
nameEx("scanpipe")
### * scanpipe

flush(stderr()); flush(stdout())

### Name: scanpipe
### Title: read data through a pipe
### Aliases: scanpipe read.selection

### ** Examples

 x <- scanpipe("awk 'NF>=6{print $6}' myfile.dat",skip=1)



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
