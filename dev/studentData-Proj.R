x<- read.table(text='FastFood_month, Restaurant_month, JunkFood_month, CookHome_month, Starbucks_month, Coffee_month, Soda_month, Milk_Chocolate_rate, Dark_Chocolate_rate, Sprite_rate, CocaCola_rate, SpicyFood_rate, ClassPrep_HrsWk, GPA, Commute_miles, Transportation_Sch, Eat_Study, PostGrad, Chk_Phone, ArtSkill_rate, ReadWrite_rate, MathSkills_rate, Memory_score, Intro_Extravert, Fav_Color, Netflix_HrsWk, PhoneTime_HrsWk, Exercise_HrsWk, Alcohol_month, Marijuana_month, Traffic_Ticket, Poli_Party, Religion, Country_rate, EDM_rate, Oldies_rate, Rap_rate, Pop_rate, Undergrnd_HipHop_rate, Metal_rate, ClassRock_rate, AltRock_rate, RnB_rate, Jazz_rate, Classical_rate, IntWorld_rate, VidGames, Online_VidGa, Happiness_rate, Depression_rate, Num_Jobs, Skydive, Age, Birthday_Month, Birthday_Year, Gender, Height_inches, Weight, Num_Languages, Relationship_status, Born_CA, Housing, Roommates, Live_Parents, Found_Love', sep=',')
x<- unlist(x) |> trimws()
'Born_CA' %in% x


# Checking simulaiton of multilevel categorical variables

library(coursekata)
StudentSurvey <- na.omit(StudentSurvey)
mdl0 <- lm(GPA ~ NULL, data=StudentSurvey)
mdl1 <- lm(GPA ~ SAT, data=StudentSurvey)
mdl2 <- lm(GPA ~ SAT + HigherSAT, data=StudentSurvey)
mdl3 <- lm(GPA ~ SAT + HigherSAT + MathSAT, data=StudentSurvey)
summary(mdl2)

# Adding residuals
StudentSurvey <- StudentSurvey |>
  mutate(resid0 = resid(mdl0), resid1 = resid(mdl1),
    resid2 = resid(mdl2), resid3 = resid(mdl3))
# Using residual
bs_obs2 <- coef(lm(resid1 ~ SAT + HigherSAT, data = StudentSurvey)) |> 
  as.data.frame() |> setNames('value') |>
  tibble::rownames_to_column('Estimate') |>
  mutate(Estimate = gsub(pattern='\\((.+)\\)', replacement='\\1', Estimate))
bs2 <- do(100) * coef(lm(shuffle(resid1)  ~ SAT + HigherSAT, data = StudentSurvey))
bs2 |> tidyr::pivot_longer(everything(), names_to='Estimate', values_to='value') |>
  ggplot(aes(x=value)) +
  geom_histogram(bins=30) +
  geom_vline(aes(xintercept = value), data=bs_obs2, inherit.aes = FALSE, color='red') +
  facet_grid(Estimate~.) +
  theme(
    strip.text.y = element_text(size=28),
    text = element_text(size=30))

summary(mdl2)
