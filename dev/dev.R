# adding model to graphs
library(ggformula)
library(mosaic)



df <- coursekata::Fingers
K <-20
pres <- rep(NA, K-1)
for(k in 2:K){
    df$cat <- ntile(df$Height, k)
    pres[k-1] <- f(lm(Thumb~cat, data=df))
}
plot(x=2:K, y=pres, type='b')



df <- coursekata::Fingers
df$Gender <- factor(df$Gender)
gn <- 3
df$h <- mosaic::ntiles(df$Height, gn)

mod <- lm(Thumb~h, data=df)
supernova::supernova(mod)
lsr::cohensD(Thumb~h, data=df)


mod<- lm(Thumb~NULL, data=df)
mod
gf_jitter(Thumb~'NULL', data=df) |> gf_model(mod)
