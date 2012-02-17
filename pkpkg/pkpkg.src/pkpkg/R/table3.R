"table3" <-
function (x) 
##=======================================================
##  my own table function
##=======================================================
{
    if (sum(!is.na(x)) == 0) {
        a <- length(x)
        names(a) <- "NA"
    }
    else {
        dateformat <- FALSE
        if (is.character(x)) {
            if (!is.na(grep("^[0-9][0-9][/-][0-9][0-9][/-][0-9][0-9]$", 
                x[1])[1])) {
                dateformat <- TRUE
                x <- as.numeric(paste(substring(x, 1, 2), substring(x, 
                  4, 5), substring(x, 7, 8), sep = ""))
            }
        }
        a <- table(x)
        nac <- length(x) - sum(a)
        if (length(a) > 30) {
            .Options$warn <- -1
            aa <- range(as.numeric(names(a)), na.rm = TRUE)
            if (dateformat) {
                aa <- as.character(aa)
                aa <- paste(substring(aa, 1, 2), substring(aa, 
                  3, 4), substring(aa, 5, 6), sep = "/")
            }
            names(aa) <- c("range:  min", "max")
            if (sum(is.na(aa)) == 0) 
                a <- aa
        }
        if (nac > 0) 
            a <- c(a, "NA" = nac)
    }
    a
}
