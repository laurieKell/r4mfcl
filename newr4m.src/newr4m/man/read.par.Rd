\name{read.par}
\alias{read.par}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.par(par.file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{par.file}{
%%     ~~Describe \code{par.file} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (par.file) 
{
    txt <- readLines(par.file)
    blanks <- grep("^#* *$|^#[ .0-9]*$", txt)
    if (length(blanks) > 0) 
        txt <- txt[-blanks]
    par <- list()
    hptrs <- c(which(substr(txt, 1, 1) == "#"), length(txt) + 
        1)
    for (hpt in 1:(length(hptrs) - 1)) {
        p1 <- hptrs[hpt] + 1
        p2 <- hptrs[hpt + 1] - 1
        if (p2 >= p1) {
            tt <- scanText(txt[p1:p2])
            nrow = p2 - p1 + 1
            ss <- length(tt)/nrow
            if (identical(ss, floor(ss))) 
                par[[txt[hptrs[hpt]]]] <- drop(matrix(as.numeric(tt), 
                  byrow = TRUE, nrow = nrow))
            else {
                xx <- list()
                for (i in 1:nrow) xx[[i]] <- extract.numeric(txt[p1 + 
                  i - 1])
                names(xx) <- format(1:nrow)
                par[[txt[hptrs[hpt]]]] <- xx
            }
        }
        else par[[txt[hptrs[hpt]]]] <- extract.numeric(txt[hptrs[hpt]])
    }
    par[["num.elements"]] <- length(par)
    names(par)[grep("# The parest_flags", names(par))] <- "pfl"
    names(par)[grep("# The number of age classes", names(par))] <- "nages"
    names(par)[grep("# age flags", names(par))] <- "afl"
    names(par)[grep("# fish flags", names(par))] <- "ffl"
    names(par)[grep("# tag flags", names(par))] <- "tfl"
    names(par)[grep("# tag fish rep$", names(par))] <- "trpfl"
    names(par)[grep("# tag fish rep group flags", names(par))] <- "trpgpfl"
    names(par)[grep("# tag_fish_rep active flags", names(par))] <- "trpacfl"
    names(par)[grep("# tag_fish_rep target", names(par))] <- "treptarg"
    names(par)[grep("# treptarg", names(par))] <- "treppen"
    names(par)[grep("# percent maturity", names(par))] <- "maturity"
    names(par)[grep("# total populations scaling parameter", 
        names(par))] <- "totpop"
    names(par)[grep("# implicit total populations scaling parameter", 
        names(par))] <- "totpop_implicit"
    names(par)[grep("# rec init pop level difference", names(par))] <- "rec_init"
    names(par)[grep("# recruitment times", names(par))] <- "rectimes"
    names(par)[grep("# relative recruitment ", names(par))] <- "rel_recruitment"
    names(par)[grep("# fishery selectivity", names(par))] <- "selectivity"
    names(par)[grep("# natural mortality coefficient", names(par))] <- "Mbase"
    names(par)[grep("# effort deviation coefficients", names(par))] <- "effdevcoffs"
    names(par)[grep("# average catchability coefficients", names(par))] <- "meanqs"
    names(par)[grep("# Objective function value", names(par))] <- "obj"
    names(par)[grep("# The number of parameters", names(par))] <- "npars"
    names(par)[grep("# Maximum magnitude gradient value", names(par))] <- "gradient"
    names(par)[grep("# extra par for Richards", names(par))] <- "Richards"
    par$nfisheries <- dim(par$ffl)[1]
    pos <- grep("# The von Bertalanffy parameters", names(par))
    par$Lmin <- par[[pos]][1, 1]
    par$Lmax <- par[[pos]][2, 1]
    par$K <- par[[pos]][3, 1]
    pos <- grep("# Variance parameters", names(par))
    par$growth_vars <- par[[pos]][, 1]
    pos <- grep("# age-class related parameters", names(par))
    par$M_offsets <- par[[pos]][2, ]
    par$gr_offsets <- par[[pos]][3, ]
    return(par)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
