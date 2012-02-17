"plotSymbols" <-
  function (fn=1) {
    i <- 0:255
    ncol <- 16
    opar <- par(cex.axis = 0.7, mar = c(3, 3, 3, 3) + 0.1)
    plot(i%%ncol, 1 + i%/%ncol, pch=i, font=fn, xlab = "", ylab = "", 
         axes = FALSE)
    axis(1, at = 0:15)
    axis(2, at = 1:16, labels = 0:15 * 16, las = 2)
    axis(3, at = 0:15)
    axis(4, at = 1:16, labels = 0:15 * 16 + 15, las = 2)
    par(opar)
  }

