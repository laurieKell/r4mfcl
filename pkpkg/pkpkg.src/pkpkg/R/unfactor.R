"unfactor" <-
function (x) 
##==================================================================
## returns vector x as numeric if possible, otherwise character
##==================================================================
{
    ch <- as.character(x)
    opt <- options(warn = 2)
    num <- try(as.numeric(ch),silent=TRUE)
    options(opt)
    if (class(num)=="try-error") 
        ch
    else num
}
