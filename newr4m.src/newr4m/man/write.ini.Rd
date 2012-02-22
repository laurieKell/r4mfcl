\name{write.ini}
\alias{write.ini}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
write.ini(ini.file, ini.obj, old.format = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ini.file}{
%%     ~~Describe \code{ini.file} here~~
}
  \item{ini.obj}{
%%     ~~Describe \code{ini.obj} here~~
}
  \item{old.format}{
%%     ~~Describe \code{old.format} here~~
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
function (ini.file, ini.obj, old.format = FALSE) 
{
    o <- ini.obj
    con <- file(ini.file, open = "w")
    on.exit(close(con))
    if (with(ini.obj, exists("version")) & !old.format) {
        writeLines(c("# ini version number", o$version), con)
    }
    writeLines(c("# number of age classes", o$nages), con)
    if (with(o, exists("tag.fish.rep"))) {
        writeLines("# tag fish rep", con)
        write.table(o$tag.fish.rep, con, quote = F, append = T, 
            row.names = F, col.names = F)
    }
    if (with(o, exists("grpflags"))) {
        writeLines("# tag fish rep group flags", con)
        write.table(o$grpflags, con, quote = F, append = T, row.names = F, 
            col.names = F)
    }
    if (with(o, exists("activeflags"))) {
        writeLines("# tag_fish_rep active flags", con)
        write.table(o$activeflags, con, quote = F, append = T, 
            row.names = F, col.names = F)
    }
    if (with(o, exists("reptarget"))) {
        writeLines("# tag_fish_rep target", con)
        write.table(o$reptarget, con, quote = F, append = T, 
            row.names = F, col.names = F)
    }
    if (with(o, exists("reppenalty"))) {
        writeLines("# tag_fish_rep penalty", con)
        write.table(o$reppenalty, con, quote = F, append = T, 
            row.names = F, col.names = F)
    }
    writeLines(c("# maturity at age", paste(o$mat, collapse = " ")), 
        con)
    writeLines(c("# natural mortality (per year)", o$M), con)
    if (with(o, exists("movemap"))) 
        writeLines(c("# movement map", paste(o$movemap, collapse = " ")), 
            con)
    if (with(o, exists("diffcoffs"))) 
        writeLines("# diffusion coffs (per year)", con)
    write.table(o$diffcoffs, con, quote = F, append = T, row.names = F, 
        col.names = F)
    writeLines("# age_pars", con)
    write.table(o$age_pars, con, quote = F, append = T, row.names = F, 
        col.names = F)
    writeLines(c("# recruitment distribution by region", paste(o$recbyreg, 
        collapse = " ")), con)
    writeLines(c("# The von Bertalanffy parameters", "# Initial  lower bound  upper bound", 
        "# ML1", paste(paste(o$VBLmin, collapse = " ")), "# ML2", 
        paste(paste(o$VBLmax, collapse = " ")), "# K (per year)", 
        paste(paste(o$VBK, collapse = " ")), "# Length-weight parameters", 
        paste(paste(o$LW, collapse = " ")), "# sv(29)", o$sv29, 
        "# Generic SD of length at age", paste(paste(o$sdLatA, 
            collapse = " ")), "# Length-dependent SD", paste(paste(o$Ldep_sd, 
            collapse = " ")), "# The number of mean constraints", 
        paste(paste(o$Nmeanconstr, collapse = " "))), con)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
