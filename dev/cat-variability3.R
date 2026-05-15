# Setup ####
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

# Variability in Categorical variables. ####
## Normalized variablity is used so they add up to 1.

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
