"scanpipe" <-
function (cmd, ...) 
##=======================================================
##  scan from a connection and then tidy up.
##=======================================================
{
    con <- pipe(cmd)
    out <- scan(con, ...)
    close(con)
    out
}
