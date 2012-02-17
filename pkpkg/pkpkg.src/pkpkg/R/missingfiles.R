"missingfiles" <-
function (dir1, dir2) 
##=======================================================
## finds files in one directory tree that aren't in another
##=======================================================
{
    files1 <- scan(pipe(paste("cd", dir1, "; tar -cvf /dev/null *")), 
        what = "", quiet = TRUE)
    files2 <- scan(pipe(paste("cd", dir2, "; tar -cvf /dev/null *")), 
        what = "", quiet = TRUE)
    files1[!files1 %in% files2]
}
