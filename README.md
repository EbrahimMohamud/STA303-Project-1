# STA303-Project-1
```{r}
#install.packages("glmmTMB")
#install.packages("tidyverse")
library("tidyverse")
library('glmmTMB')

# Plot histogram with density scaling
hist(portugal$children, probability = TRUE, main = "Histogram with Poisson Density Curve",
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

portugal$logMonthSM = log(pmax(1, portugal$monthsSinceM))

# First Model: Poisson + 3 Predictors
model1 <- glmmTMB(children ~ literacy + ageMarried + region, data = portugal, family = nbinom2())

knitr::kable(summary(model1)$coef, digits = 3)

# Checking for over dispersion: 
plot(portugal$ageMarried, portugal$children)

summary_stats <- portugal %>%
  group_by(ageMarried) %>%
  summarize(
    avg_n_child = mean(children, na.rm = TRUE),
    var_n_child = sd(children, na.rm = TRUE)^2
  )

summary_stats$difference = summary_stats$var_n_child - summary_stats$avg_n_child
summary_stats
```

```{r}
#install.packages("jtools")
#install.packages("broom.mixed")
library("jtools")
library("broom.mixed")
model_overdispersion <- glmmTMB(family_size ~ literacy + ageMarried + region, data = portugal, family = nbinom2())
knitr::kable(summary(model_overdispersion)$coef, digits = 3)

plot_summs(model1, model_overdispersion, omit.corefs = F, model.names = c("w/out offset", "overdispersion"), exp = F)

```

