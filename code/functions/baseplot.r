baseplot <- function(ymax = 5, tit = "Normal prior density functions"){
  plot(0, 0, type = "n", main = tit, xlim = range(x), ylim = c(0, ymax), xlab = "", ylab = "density function")
  xlabs <- seq(0, 5, by = 0.1)
  axis(1, at = log(xlabs), labels = xlabs, line = 4.5)
  mtext("hazard ratio", side = 1, line = 6.5)
  mtext("log(hazard ratio)", side = 1, line = 2.2)
}