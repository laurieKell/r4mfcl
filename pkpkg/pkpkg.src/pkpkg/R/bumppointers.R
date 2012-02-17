"bumppointers" <-
function (ptrs, n) 
##=======================================================
## for extracting segment of a vector
##=======================================================
{
    i <- ptrs[length(ptrs)]
    (i + 1):(i + n)
}
