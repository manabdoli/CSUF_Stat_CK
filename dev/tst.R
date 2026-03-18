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
