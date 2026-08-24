# Functions ####
gini <- function(x, normalize=TRUE){
  x<- as.factor(x)
  p <- table(x) |> prop.table()
  if(length(p)==1) return(0) else
    sum(p*(1-p)) * if(normalize) 1/(1-1/length(p)) else 1
}

# H(X)
entropy <- function(x, normalize=TRUE){
  x.name <- deparse(substitute(x))
  x <- as.factor(x)
  p <- table(x) |> prop.table()
  dimnames(p) <- list(names(p)) |> setNames(x.name)
  n <- length(x)
  k <- length(p)
  H <- if(k==1) 0 else
    -sum(p*log(p), na.rm = TRUE) * if(normalize) 1/log(k) else 1
  
  structure(H,
            n=n, k=k, p=p, 
            normalize = normalize,
            content = sprintf('H(%s)', x.name),
            class = 'entropy')
}

joint.entropy <- function(x, y=NULL, normalize=TRUE){
  x.name <- deparse(substitute(x))
  y.name <- deparse(substitute(y))
  n <- length(x)
  if(is.null(y)) return(entropy(x, normalize))
  
  ps <- table(data.frame(x<- as.factor(x), y<- as.factor(y))) |> 
    prop.table()
  k <- nrow(ps)
  m <- ncol(ps)
  
  dimnames(ps) <- list(rownames(ps), colnames(ps)) |> setNames(c(x.name, y.name))
  
  H<- if(all((dim(ps)-1)==0)) 0 else
    -sum(ps*log(ps), na.rm = TRUE) * if(normalize) 1/log(k*m) else 1
  
  structure(H,
            n=n, k=k, m=m, p=ps, 
            normalize = normalize,
            content = sprintf('H(%s, %s)', x.name, y.name),
            class = 'entropy')
}

cond.entropy <- function(x, y=NULL, normalize=TRUE, givenY=FALSE){
  x.name <- deparse(substitute(x))
  y.name <- deparse(substitute(y))
  n <- length(x)
  if(is.null(y)) 
    if(givenY) return(NULL) else return(entropy(x, normalize))
  # data.frame
  DF <- data.frame(x<- as.factor(x), y<- as.factor(y))
  # GivenY
  if(givenY){
    # Conditional ps
    cps <- table(DF) |> prop.table(margin = 2)
    if(all((dim(cps)-1)==0)) return(0)
    
    k <- nrow(cps)
    m <- ncol(cps)
    # Marginal p (recalculate to avoid rounding error?)
    mp <- table(DF[,2]) |> prop.table()
    H <- sapply(1:length(mp), function(j){
      -mp[j] * sum(cps[,j]*log(cps[,j]), na.rm = TRUE)
    }) |> sum()
    return(
      structure(H * if(normalize) 1/log(k) else 1,
                n=n, k=k, m=m, cps=cps, mp = mp, 
                normalize = normalize,
                content = sprintf('H(%s|%s)', x.name, y.name),
                class = 'entropy')
    )
  } else {
    # Conditional ps
    cps <- table(DF) |> prop.table(margin = 1)
    if(all((dim(cps)-1)==0)) return(0)
    
    k <- nrow(cps)
    m <- ncol(cps)
    
    # Marginal p (recalculate to avoid rounding error?)
    mp <- table(DF[,1]) |> prop.table()
    H <- sapply(1:length(mp), function(i){
      -mp[i] * sum(cps[i,]*log(cps[i,]), na.rm = TRUE)
    }) |> sum()
    return(
      structure(H * if(normalize) 1/log(m) else 1,
                n=n, k=k, m=m, cps=cps, mp = mp, 
                normalize = normalize,
                content = sprintf('H(%s|%s)', y.name, x.name),
                class = 'entropy')
    )
  }
}  




# Example ####
## H(cyl, am) = H(cyl) + H(am|cyl)
c(joint.entropy(mtcars$cyl, mtcars$am, F), entropy(mtcars$cyl, F) + cond.entropy(mtcars$cyl, mtcars$am, F, givenY = F))
## H(cyl, am) = H(am) + H(cyl|am)
c(joint.entropy(mtcars$cyl, mtcars$am, F), (entropy(mtcars$am, F) + cond.entropy(mtcars$cyl, mtcars$am, F, givenY = T)))

x<- factor(mtcars$cyl)




# Variability in Categorical variables. ####
## Normalized variability is used so they add up to 1.

n<- 100
m<- 20
emn<- gmn<- expand.grid(p=(0:m)/m, size=1:n) |>
  data.frame()
gmn$gz<- 0
emn$ez<- 0
for(i in 1:nrow(gmn)){
  a<- round(prod(gmn[i, 1:2]))
  b<- gmn$size[i]-a
  cvar<- c(a<- rep('a', a), rep('b', b))
  gmn$gz[i] <- gini(cvar, normalize = TRUE)
  emn$ez[i] <- entropy(cvar, normalize = TRUE)
}

levelplot(gz~p+size, gmn, main='Gini Variability')  
levelplot(ez~p+size, emn, main='Entropy Variability')

# Effect of sample size ####
## Breaking a 50/50 group (variability 1) into K groups of 50/50
##. increases the variability
## We have to adjust for number of groups.

K<- 10
m<- 2*K
emn<- gmn<- expand.grid(grp=1:K, size=seq(2, m, 2)) |>
  data.frame()
gmn$gz<- 0
emn$ez<- 0
for(i in 1:nrow(gmn)){
  dst<- data.frame(
    grp=rep(1:gmn$grp[i], each=gmn$size[i]) |> factor(),
    cvar=rep(c('a','b'), times=gmn$grp[i]*gmn$size[i]/2))
  gmn$gz[i] <- split(dst$cvar, f=dst$grp) |> sapply(gini) |> mean()
  emn$ez[i] <- split(dst$cvar, f=dst$grp) |> sapply(entropy) |> mean()
}

levelplot(gz~grp+size, gmn, main='Gini Variability')  
levelplot(ez~grp+size, emn, main='Entropy Variability')

## Using Mean shows that the size has no impact for 50/50.
## Let's try different proportions for a given group size

# K<- 2 # number of groups
# n<- 100 # sample size
# m<- 20 # Prop breakdown
# emn<- gmn<- expand.grid(p=(0:m)/m, size=1:n) |>
#   data.frame()
# gmn$gz<- 0
# emn$ez<- 0
# for(i in 1:nrow(gmn)){
#   dst<- data.frame(
#     grp=rep(1:K, each=round(prod(gmn[i,1:2]))) |> factor(),
#     cvar=rep(c('a','b'), times=gmn$grp[i]*gmn$size[i]/2))
#   gmn$gz[i] <- split(dst$cvar, f=dst$grp) |> sapply(gini) |> mean()
#   emn$ez[i] <- split(dst$cvar, f=dst$grp) |> sapply(entropy) |> mean()
# }
# 
# levelplot(gz~grp+size, gmn, main='Gini Variability')  
# levelplot(ez~grp+size, emn, main='Entropy Variability')
