"defactor" <-
function (list) 
###==================================================================
### returns list or data.frame with all factor elements unfactored
###==================================================================
{
    for (i in names(list[which(sapply(list, is.factor))])) {
        eval(parse(text = paste("list$", i, " <- unfactor(list$", 
            i, ")", sep = "")))
    }
    list
}
