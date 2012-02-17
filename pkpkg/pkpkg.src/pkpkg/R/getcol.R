"getcol" <-
function (v, n, sep = " ") 
{
    awkc <- paste("awk -F '", sep, "' '{print $", n, "}'", sep = "")
    cmd(v, cmd = awkc)
}
