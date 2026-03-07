# adding model to graphs
library(ggformula)

df <- coursekata::Fingers
df$Gender <- factor(df$Gender)
mod <- lm(Thumb~Gender, data=df)
x <- 'Gender'
y <- 'Thumb'

mod.df <- data.frame(
    setNames(
        list(factor(levels(df[[x]]), 
          levels = levels(df[[x]]))),
        x)
)
mod.df$pred <- mod.df$ymin <- mod.df$ymax <- predict(mod, newdata=mod.df)
mod.df
gf_jitter(Thumb~Gender, data=coursekata::Fingers) |>
  gf_crossbar(pred+ymin+ymax~Gender, data=mod.df, inherit = TRUE)

gf_jitter(Thumb~Gender, data=coursekata::Fingers) |>
  gf_model(mod) |> gf_model_text(mod)

