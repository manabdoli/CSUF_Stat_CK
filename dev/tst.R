# Testing Complexity vs Significance

library(coursekata)
y<- mtcars$mpg
x<- mtcars$wt

gf_point(y~x) |> gf_lm()

ngrp<- 50
r<- 10
res <- data.frame(
    r=rep(1:r, ngrp-1),
    g=factor(rep(2:ngrp, each=r)),
    pre=NA, pval=NA)

for(gi in 2:ngrp){
    xcat<- factor(ntile(x, gi))
    for(i in 1:r){
        mod<- lm(shuffle(y)~xcat)
        res$pre[(gi-2)*r+i]<- pre(mod)
        res$pval[(gi-2)*r+i]<- supernova(mod)$tbl$p[1]
    }
}
gf_boxplot(pval~g, data=res, color='blue') |>
  gf_boxplot(pre~g, fill='red', 
  color='orange', alpha=.5)



{
    library(coursekata)
    load('./presentations/data/Fingers.rda')
    sdob1 <- do(500) *
      b1(shuffle(Thumb)~Gender, data=Fingers)
}
{
    n <- dim(Fingers)[1]
    df <- n - 2
    b1.bar <- mean(sdob1$b1)
    b1.se <- sd(sdob1$b1)
    ncdt <- function(x) dt(x, df = df)/b1.se
    gf_dhistogram(~b1, data = sdob1, bins=25) |>
      gf_function(fun=ncdt, xlim=range(sdob1$b1))
    qqnorm(sdob1$b1)
    sdob1$t <- (sdob1$b1-b1.bar)/b1.se
    gf_dhistogram(~t, data = sdob1, bins=30) |>
      gf_function(fun=dt, args=list(df=n-2),xlim=range(sdob1$b1))

}


## Normal
{
    # case 1: non-normal distribution
    set.seed(123)
    mu<- c(10, 20); sd<- c(1, 1)*3; n<-c(1, 1)*15
    dset<- data.frame(
        x=c(rep('A', n[1]), rep('B', n[2])),
        y=c(rnorm(n[1], mu[1], sd[2]),
            rnorm(n[2], mu[2], sd[2])))
    gf_histogram(~y, data=dset) |>
      gf_facet_grid(x~.)
    gf_qq(~y, data=dset) |> gf_qqline(color='blue') |>
      gf_facet_grid(~x)
    gf_boxplot(y~x, data=dset)    
    #
    sdob1 <- do(500) *
      b1(shuffle(y)~x, data=dset)
    #b1.bar <- mean(sdob1$b1)
    #b1.se <- sd(sdob1$b1)
    #sdob1$t <- (sdob1$b1-b1.bar)/b1.se
    sdob1$t <- scale(sdob1$b1)
    gf_dhistogram(~t, data = sdob1, bins=30) |>
      gf_function(fun=dt, args=list(df=n-2),xlim=range(sdob1$t))

}






# normal
{
    # case 1: non-normal distribution
    #

}

# skewed
{
    # case 1: non-normal distribution
    set.seed(123)
    mu<- c(20, 20); sd<- c(1, 1)*3; n<-c(1, 1)*10
    skw <- c(1, 5)
    w<- 0.5 # binwidth
    dset<- data.frame(
        x=c(rep('A', n[1]), rep('B', n[2])),
        y=c(rskw(n[1], mu[1], sd[1], skw),
            rskw(n[2], mu[2], sd[2], 1)
            ))
    gf_histogram(~y, data=dset, binwidth=w) |>
      gf_facet_grid(x~.)
    gf_qq(~y, data=dset) |> gf_qqline(color='blue') |>
      gf_facet_grid(~x)
    gf_boxplot(y~x, data=dset)    
    #
    sdob1 <- do(1000) *
      b1(shuffle(y)~x, data=dset)
    b1.bar <- mean(sdob1$b1)
    b1.sd <- sd(sdob1$b1)
    n<- length(sdob1$b1)
    x.lim<- b1.bar+c(-1,1)*4*b1.sd

    b1.qts<- quantile(sdob1$b1, c(.025, 0.975))
    th.qts<- b1.bar+b1.sd*qt(c(.025, 0.975), df=n-2)
    sdob1$tail<- factor(sdob1$b1<b1.qts[1] | sdob1$b1>b1.qts[2])
    gf_histogram(~b1, data = sdob1, binwidth=w, fill=~tail, show.legend = FALSE) |>
      gf_function(fun=b1dt, args=list(df=n-2, b1=sdob1$b1, w=w),xlim=x.lim)|>
      gf_fun_fill(fun=function(x) b1dt(x, df=n-2, b1=sdob1$b1, w=w), from = x.lim[1], to = th.qts[1], color='red') |>
      gf_fun_fill(fun=function(x) b1dt(x, df=n-2, b1=sdob1$b1, w=w), from=th.qts[2], to=x.lim[2], color='red')
}


# different variane
{
    # case 1: non-normal distribution
    set.seed(123)
    mu<- c(20, 20); sd<- c(1/2, 2)*3; n<-c(1, 1)*10
    skw <- c(1, 1)
    w<- 0.5 # binwidth
    dset<- data.frame(
        x=c(rep('A', n[1]), rep('B', n[2])),
        y=c(rskw(n[1], mu[1], sd[1], skw),
            rskw(n[2], mu[2], sd[2], 1)
            ))
    gf_histogram(~y, data=dset, binwidth=w) |>
      gf_facet_grid(x~.)
    gf_qq(~y, data=dset) |> gf_qqline(color='blue') |>
      gf_facet_grid(~x)
    gf_boxplot(y~x, data=dset)    
    #
    sdob1 <- do(1000) *
      b1(shuffle(y)~x, data=dset)
    b1.bar <- mean(sdob1$b1)
    b1.sd <- sd(sdob1$b1)
    n<- length(sdob1$b1)
    x.lim<- b1.bar+c(-1,1)*4*b1.sd

    b1.qts<- quantile(sdob1$b1, c(.025, 0.975))
    th.qts<- b1.bar+b1.sd*qt(c(.025, 0.975), df=n-2)
    sdob1$tail<- factor(sdob1$b1<b1.qts[1] | sdob1$b1>b1.qts[2])
    gf_histogram(~b1, data = sdob1, binwidth=w, fill=~tail, show.legend = FALSE) |>
      gf_function(fun=b1dt, args=list(df=n-2, b1=sdob1$b1, w=w),xlim=x.lim)|>
      gf_fun_fill(fun=function(x) b1dt(x, df=n-2, b1=sdob1$b1, w=w), from = x.lim[1], to = th.qts[1], color='red') |>
      gf_fun_fill(fun=function(x) b1dt(x, df=n-2, b1=sdob1$b1, w=w), from=th.qts[2], to=x.lim[2], color='red')
}
