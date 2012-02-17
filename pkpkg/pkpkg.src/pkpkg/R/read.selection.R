read.selection <- function(...){
  con <- pipe("xselection")
  out <- read.table(con, ...)
  #close(con)
  out
}

