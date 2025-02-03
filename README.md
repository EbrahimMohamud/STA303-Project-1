# STA303-Project-1


```{r}
portugal <- portugal %>%
  mutate(family_size =
           children) 

model <- glmmTMB(family_size ~ literacy + ageMarried, data = portugal, family = nbinom2())
knitr::kable(summary(model)$coef, digits = 3)

# Checking for over dispersion: 
plot(portugal$ageMarried, portugal$family_size)

summary_stats <- portugal %>%
  group_by(ageMarried) %>%
  summarize(
    mean_family_size = mean(family_size, na.rm = TRUE),
    var_family_size = sd(family_size, na.rm = TRUE)^2
  )

summary_stats$difference = summary_stats$var_family_size - summary_stats$mean_family_size
summary_stats

print(summary_stats)
```
