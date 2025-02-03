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

```{r}
#install.packages("glmmTMB")
#install.packages("tidyverse")
library("tidyverse")
library('glmmTMB')

lambda = mean(portugal$children)
lambda

model <- glmmTMB(family ~ literacy + ageMarried, data = portugal, family = nbinom2())

knitr::kable(summary(model)$coef, digits = 3)

# Plot histogram with density scaling
hist(portugal$family, probability = TRUE, main = "Histogram with Poisson Density Curve",
     xlab = "Family Size", ylab = "Density", col = "lightblue", border = "black")

# Calculate lambda (mean of children)
lambda = mean(portugal$children)

# Generate integer sequence (Poisson is discrete)
xSeq = 0:20  # Only integers

# Compute Poisson probability mass function (PMF) and scale it to density
poisDensity = dpois(xSeq, lambda = lambda)

# Convert PMF to density scale by adjusting for bin width
bin_width = diff(hist(portugal$family, plot = FALSE)$breaks)[1]
poisDensity = poisDensity / bin_width

# Overlay the density-scaled Poisson distribution
points(xSeq, poisDensity, col = "red", pch = 16) # Add points
lines(xSeq, poisDensity, col = "red", lwd = 2)   # Add connecting lines

```
