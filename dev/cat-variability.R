gini <- function(x, normalize=TRUE){
  x<- as.factor(x)
  p <- table(x) |> prop.table()
  if(length(p)==1) return(0) else
    sum(p*(1-p)) * if(normalize) 1/(1-1/length(p)) else 1
}
entropy <- function(x, normalize=TRUE){
  x<- as.factor(x)
  p <- table(x) |> prop.table()
  if(length(p)==1) return(0) else
    -sum(p*log(p), na.rm = TRUE) * if(normalize) 1/log(length(p)) else 1
}


x <- c(rep('a', 50), rep('b', 50))  |> factor()
Gs <- 0:50
Ks <- 0:100
eIdx <- gIdx <- matrix(0, ncol = length(Gs), nrow = length(Ks))
Eidx <- Gidx <- matrix(0, ncol = length(Gs), nrow = length(Ks))

for(g in Gs){
  for(k in 0:min(g, 100-g)){
    y1 <- if(g>0) c(rep('a', k), rep('b', g-k)) else NULL
    y2 <- if(g<100) c(rep('a', 50-k), rep('b', 50-g+k)) else NULL
    gIdx[k+1, g+1] <- (gini(y1, F)+gini(y2, F))/2
    eIdx[k+1, g+1] <- (entropy(y1, F)+entropy(y2, F))/2
    Gidx[k+1, g+1] <- (gini(y1, T)+gini(y2, T))/2
    Eidx[k+1, g+1] <- (entropy(y1, T)+entropy(y2, T))/2
  }
}

library(rgl)
xy <- mesh(Ks, Gs)
{
  z <- eIdx
zlim <- range(z)
zlen <- zlim[2] - zlim[1] + 1

colorlut <- terrain.colors(zlen) # height color lookup table

col <- colorlut[ z - zlim[1] + 1 ] # assign colors to heights for each point

open3d()
surface3d(xy$x, xy$y, z, color = col, back = "lines")
}
