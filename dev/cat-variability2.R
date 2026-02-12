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
set.seed(123)

{
    fun = entropy
y <- sample(1:2, length(x), TRUE)
xy <- data.frame(x=factor(x), y=factor(y))
gIdx0 <- lapply(split(xy, f=xy$y),
  function(xy.i) fun(xy.i$x)) |> unlist() |> mean() 

K <- 1000
gArr <- rep(0, K)
for(i in 1:K){
    gArr[i] <- lapply(split(XY, f=sample(xy$y)),
      function(xy.i) fun(xy.i$x)) |> 
      unlist() |> mean() 
}

hist(gArr)
abline(v=gIdx0, col='red')
}
