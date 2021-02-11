## Modified from https://gis.stackexchange.com/questions/219880/plotting-bar-charts-on-maps-in-r

plot_floating_bars <- function (x, xllc = 0, yllc = 0, barwidth=3, maxheight=3, col){
  # calculate how long each bar needs to be
  # bars <- (x/max(x)) * maxheight
  bars <- x / maxheight
  # get some quick colors
  col <- col

  for(i in 1:length(x)){
    # figure out x- and y coordinates for the corners
    leftx   <- xllc + ((i-1) * barwidth)
    rightx  <- leftx + barwidth
    bottomy <- yllc
    topy    <- yllc + bars[i]
    # draw the bar
    polygon(x=c(leftx, rightx, rightx, leftx, leftx),
            y=c(bottomy, bottomy, topy, topy, bottomy),
            col=col[i])
  }
}
