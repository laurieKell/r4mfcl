"dim" <-
  function(x) {
###===================================================
###  If x is only a vector, returns length(x)
###===================================================
  a <- .Primitive("dim")(x)
  if(is.null(a)) length(x) else a
}
