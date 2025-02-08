```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
#install.packages("tidyverse")
#install.packages('glmmTMB')
#install.packages("jtools")
#install.packages("broom.mixed")
#install.packages("kableExtra")
library(tidyverse)
library("glmmTMB")
library("jtools")
library("broom.mixed")
library(kableExtra)
```

# Introduction
Family size is a powerful reflection of a country's social and economic landscape, making it a crucial area of study. Family size differentials influence birth rates, fertility rates, and population growth—key drivers of economic stability and social welfare. Analyzing the factors that shape family size provides valuable insight into demographic shifts leading to effective policy implementation. 

While fertility and personal preferences are well-established factors behind the number of children that a couple has, the influences behind family size span beyond personal or biological reasons, often tracing to socio-economic and time factors. Freedman (1963) argues that high birth rates in underdeveloped countries can be attributed to social factors such as the age of marriage and societal norms. In underdeveloped countries, children are used as sources of labor and are less likely to survive to adulthood (Freedman, 1963). These findings are relevant to the context of the present report, which analyzes data from the underdeveloped country of Portugal to investigate the primary factors behind family size, allowing for more accurate projections of future demographic trends. 

Freedman (1963) also highlights the importance of age of marriage on the number of children had, citing that underdeveloped areas witness high birth rates to compensate for high mortality rates prompting people to marry and having children at a younger age. Furthermore, related studies of undeveloped countries demonstrate that literacy also play a significant role. In a study of 178 males in Chakwal city, Punjab, Mahmood et al. (2016) found that $19.2\%$ of uneducated men were in favour of smaller families compared to a $77.0\%$ of educated men. Together, these two factors played a primary role in motivating our research question.

Finally, internal factors such as differences between regions also have significant effects. Gutkind (1962) conducted an observational study of married African women in rural and urban areas, concluding that urban populations tend to have smaller families than rural populations, attributing this phenomenon to high cost of urban living. These results encouraged us to consider an additional variable of *region* in our model. The present report expands on the results from the aforementioned studies, using them to guide our analysis of fertility data from a 1979 study of Portuguese families in order to answer the motivating research question: “How do literacy, region, and age of a marriage affect family size?”. Our analysis employs a Generalized Linear Regression Model (GLM) to explain variations in family size based on the aforementioned variables.  We will justify our model choice, interpret the results, and discuss potential limitations, leading to a comprehensive answer to our research question.  


# Methods
Prior to our analysis, we performed simple data wrangling by redefining the age brackets to ensure each level contained a roughly equal number of observations. This adjustment helped prevent sparsely populated categories, reducing the risk of unstable estimates and enhancing the reliability of our analysis. To answer our research question, we required a functional form for the GLM being employed. Since the number of children is a count variable distributed over a unit of time, an appropriate model for this data is a Poisson model. We conducted a preliminary exploratory data analysis by visualizing the data through a histogram and observed whether the data satisfied the properties of a Poisson model. 

Additionally, we compared the mean and variance of each age group within the *age married* variable to assess whether they were equate; an assumption behind the Poisson model. Overdispersion is present when they don't equate, therefore, we decided to create two models: a Poisson regression model featuring *age married*, *literacy*, and *region* as predictor variables and a Negative Binomial model which features the same predictors while accounting for overdispersion. 

As previously discussed, we included *region* in our model because of the surrounding support from the literature. Additionally, we included an offset in both models to account for variation in exposure times. The offset ensures that each observation yields an estimated rate with a standard time unit of one year, thereby maintaining a consistent interpretation of the intensity of births across our data. 

We conclude our analysis by comparing the two models based on their estimated coefficients, p-values, confidence intervals, and standard errors. This evaluation allows us to determine which model better captures the relationship between the predictors and the number of children. The results from the better model will be interpreted, providing meaningful insights that address our research question.

# Results
Our dataset captured key demographic details from 5,148 observations of married couples from 1979 Portugal. Since we are interested in how age at marriage, literacy, and region contribute to family size, we plotted a histogram visualizing our data with respect to our response (the number of children). The histogram (Figure 1) exhibits a heavy right skew with a mean of 2.26 and a standard deviation (spread) of 1.86. To further support our GLM choice, we plotted a Poisson fitted line with parameters based on the global mean, and group means from the sub-categories within *age married* to determine if they matched the data. This initial assessment illustrates that a Poisson model is a reasonable fit both overall and across the different age levels.

```{r dataDownload, include=FALSE}
pUrl = 'http://wfs.dhsprogram.com/pt/ptsr01.dat'
pName = file.path(tempdir(), 'portugal.dat')
if(!file.exists(pName)) {
  download.file(pUrl, pName)
}

datNames = rbind(
		age=c(45,2),
		ageMarried=c(149,2), 
		monthsSinceM = c(157,4),
#		failedPregnancies=c(421,2),
#		failedPregStill=c(423,2),
#		failedPregSpAb=c(425,2),
		pregnancies=c(433,2),
		children=c(435,2),
		sons=c(443,2),
#		firstBirthInterval = c(479,2),
		region = c(641,2),
		literacy = c(649,2)
)
		colnames(datNames ) = c('start','len')
		datNames = cbind(startm1=datNames[,1]-1,datNames, sum=apply(datNames, 1,sum))
		cbind(datNames[-1,1] , datNames[seq(1, nrow(datNames)-1),4])
		datNames[-1,1] = datNames[-1,2] - datNames[seq(1, nrow(datNames)-1),4]
		dWidths = as.vector(t(datNames[,c(1,3)]))
		dNames = paste(rep(rownames(datNames), rep(2, nrow(datNames))),
  	rep(c( "junk",""), nrow(datNames)), sep="") 
		
		dNames = dNames[dWidths > 0]
		dWidths = dWidths[dWidths > 0]
		
		formats = list(
			ageMarried = data.frame(code=1:7,  label=c(0,15,18,20,22,25,30)),
			region = data.frame(code=1:5, 
				label=c('lisbon','porto','20k+', '10-20k', 'lt10k')),
			literacy = data.frame(code=1:2, label=c('yes','no')),
			firstBirthInterval = data.frame(
					code = 1:8,
					label = c(
							'lt0','0-7', '8-11','12-23',
							'24-35','36-47','48-59','60-Inf'
							)
					)
		)

		formats$ageMarried$label = 
  	paste(formats$ageMarried$label, 'to',
  	c(formats$ageMarried$label[-1], 'Inf'), sep='')
  	formats$ageMarried = rbind(formats$ageMarried, data.frame(code=88, label='never'))

   
  portugal = read.fwf(
    pName,
    dWidths, col.names=dNames,
    header=FALSE)
  
  portugal = portugal[,grep("junk$", names(portugal), invert=TRUE)]

for(D in intersect(names(portugal), names(formats))){
  		portugal[[D]] = factor(portugal[[D]],
  			levels=formats[[D]]$code, 
				labels=formats[[D]]$label)
}
portugal$ageMarried = relevel(portugal$ageMarried, '22to25')
portugal$region = relevel(portugal$region, 'lt10k')

if(FALSE) save(portugal, file='portugal.RData')

portugal <- portugal %>%
  mutate(ageMarried = case_when(
    ageMarried %in% c("0to15", "15to18", "18to20") ~ "0to20",  # Combine into "0to20"
    TRUE ~ ageMarried  # Keep other categories unchanged
  ))

portugal <- portugal %>%
  mutate(ageMarried = case_when(
    ageMarried %in% c("25to30", "30toInf") ~ "25+",  # Combine into "0to20"
    TRUE ~ ageMarried  # Keep other categories unchanged
  ))
```

```{r, fig.cap= "Comparison of the density distributions and fitted Poisson models for the number of children (in the 1979 Portugal dataset) across age of marriage categories (0–20, 20–22, 22–25, and 25+ years), with an additional model using the overall dataset mean as the Poisson parameter. Each line represents the fit for a specific age category using the category-specific mean number of children, while the black line corresponds to the model fitted to the entire dataset. The histogram reflects the observed family size distribution, providing context for model fit and differences across age groups.", fig.pos="!h", fig.height=4.5, fig.width=8, fig.align='center',  warning=FALSE}

# Calculate lambda (mean of children) for each group
lambda_values <- portugal %>%
  group_by(ageMarried) %>%
  summarise(lambda = mean(children))

lambda_all <- mean(portugal$children)

# Generate Poisson densities for each group
xSeq <- 0:20  # Only integers

pois_data <- data.frame(
  x = rep(xSeq, 5),
  density = c(
    dpois(xSeq, lambda_all),
    dpois(xSeq, lambda_values$lambda[lambda_values$ageMarried == "0to20"]),
    dpois(xSeq, lambda_values$lambda[lambda_values$ageMarried == "20to22"]),
    dpois(xSeq, lambda_values$lambda[lambda_values$ageMarried == "22to25"]),
    dpois(xSeq, lambda_values$lambda[lambda_values$ageMarried == "25+"])
  ) / diff(hist(portugal$children, plot = FALSE)$breaks)[1], # Scale PMF to density
  group = rep(c("All Ages", "0-20", "20-22", "22-25", "25+"), each = length(xSeq))
)

# Create histogram with overlaid Poisson densities
ggplot(portugal, aes(x = children)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, fill = "gray", color = "black") +
  geom_point(data = pois_data, aes(x = x, y = density, color = group), size = 2) +
  geom_line(data = pois_data, aes(x = x, y = density, color = group), linewidth = 1) +
  scale_color_manual(values = c("purple", "red", "yellow", "lightblue", "black")) +
  labs(title = "Histogram of the Number of Children",
       x = "Family Size",
       y = "Density",
       color = "Age at Marriage") +
  theme_minimal()

```

Before committing to a Poisson model, we needed to ensure all the Poisson assumptions were met within our data. Therefore, we proceeded to compare the means and variances of each age group within the *age married* variable, which demonstrated general positive differences between the variances and means, suggesting possible overdispersion in the data (Table 1). Therefore, our analysis compares the results between a Poisson regression model against a Negative Binomial model to determine which more appropriately answers our research question.

```{r}
summary_stats <- portugal |>
  group_by(ageMarried) |>
  summarize(
    mean_number_child = mean(children, na.rm = TRUE),
    variance_number_child = (sd(children, na.rm = TRUE))^2, 
    var_mean_gap = variance_number_child - mean_number_child
  ) |>
  setNames(c("Age Married", "Mean Num Children", "Variance Num Children", "Variance-Mean Gap")) |>
  as.data.frame()  # Ensures it prints without quotes

summary_stats <- rbind(summary_stats, c("All Levels", mean(portugal$children), sd(portugal$children)^2, sd(portugal$children)^2 - mean(portugal$children)))


knitr::kable(summary_stats, digits = 4, caption = "Summary statistics for the number of children overall and across the age marriage levels, showing mean, variance, and the variance-mean gaps")
  
```


```{r, include=FALSE}
# Poisson Model
portugal$logMonthSM = log(pmax(1, portugal$monthsSinceM)/12) # lambda_i = Intensity Per Year
poisson_mod <- glm(children ~ ageMarried + literacy + region + offset(logMonthSM), data = portugal, family = poisson(link = "log"))

# Table For Poisson: On Natural Scale
poisson_CI = as.data.frame(exp(confint(poisson_mod)))
coefficients <- summary(poisson_mod)$coef[, "Estimate"]
test <- summary(poisson_mod)$coef[, "z value"]
st_error <- summary(poisson_mod)$coef[, "Std. Error"] 
pvalue <- summary(poisson_mod)$coef[, "Pr(>|z|)"] 

# Ensure row names of poisson_CI match the names of the coefficients
if (all(rownames(poisson_CI) %in% names(coefficients))) {
    poisson_CI$Estimate <- exp(coefficients[rownames(poisson_CI)])
    poisson_CI$Standard_Error <- exp(coefficients[rownames(poisson_CI)])*st_error[rownames(poisson_CI)]
    poisson_CI$Test_Statistic <- test[rownames(poisson_CI)]  
    poisson_CI$P_Value <- pvalue[rownames(poisson_CI)]    
} else {
    stop("Row names of poisson_CI do not match coefficient names.")
}

poisson_CI <- poisson_CI[c("literacyno", 
                           setdiff(rownames(poisson_CI), "literacyno")), ]

poisson_CI <- poisson_CI[c("(Intercept)", 
                           setdiff(rownames(poisson_CI), "(Intercept)")), ]

rownames(poisson_CI)[rownames(poisson_CI) == "(Intercept)"] <- "Intercept"
rownames(poisson_CI)[rownames(poisson_CI) == "literacyno"] <- "No Literacy"
rownames(poisson_CI)[rownames(poisson_CI) == "ageMarried20to22"] <- "Age Married: 20-22"
rownames(poisson_CI)[rownames(poisson_CI) == "ageMarried22to25"] <- "Age Married: 22-25"
rownames(poisson_CI)[rownames(poisson_CI) == "ageMarried25+"] <- "Age Married: 25+"
rownames(poisson_CI)[rownames(poisson_CI) == "regionlisbon"] <- "Region: Lisbon"
rownames(poisson_CI)[rownames(poisson_CI) == "regionporto"] <- "Region: Porto"
rownames(poisson_CI)[rownames(poisson_CI) == "region20k+"] <-"Region: Population 20k+"
rownames(poisson_CI)[rownames(poisson_CI) == "region10-20k"] <- "Region: Population 10-20K"

colnames(poisson_CI)[colnames(poisson_CI) == "Standard_Error"] <- "Standard Error"
colnames(poisson_CI)[colnames(poisson_CI) == "Test_Statistic"] <- "Test Statistic"
colnames(poisson_CI)[colnames(poisson_CI) == "P_value"] <- "P-Value"

# Negative Binomial Model
negbin_mod <- glmmTMB(children ~ literacy + ageMarried + region + offset(logMonthSM), data = portugal, family = nbinom2())

# Table For Negtive Binomial: On Natural Scale
negbin_CI = as.data.frame(exp(confint(negbin_mod)))
st_error <- summary(negbin_mod)$coefficients$cond[, "Std. Error"]
test <- summary(negbin_mod)$coefficients$cond[, "z value"]
pvalue <- summary(negbin_mod)$coefficients$cond[, "Pr(>|z|)"] 

# Ensure row names of negbin_CI match the names of the coefficients
if (all(rownames(negbin_CI) %in% names(coefficients))) {
    negbin_CI$Standard_Error <- exp(coefficients[rownames(negbin_CI)])*st_error[rownames(negbin_CI)]
    negbin_CI$Test_Statistic <- test[rownames(negbin_CI)]
    negbin_CI$P_value <- pvalue[rownames(negbin_CI)]
} else {
    stop("Row names of negbin_CI do not match coefficient names.")
}

rownames(negbin_CI)[rownames(negbin_CI) == "(Intercept)"] <- "Intercept"
rownames(negbin_CI)[rownames(negbin_CI) == "literacyno"] <- "No Literacy"
rownames(negbin_CI)[rownames(negbin_CI) == "ageMarried20to22"] <- "Age Married: 20-22"
rownames(negbin_CI)[rownames(negbin_CI) == "ageMarried22to25"] <- "Age Married: 22-25"
rownames(negbin_CI)[rownames(negbin_CI) == "ageMarried25+"] <- "Age Married: 25+"
rownames(negbin_CI)[rownames(negbin_CI) == "regionlisbon"] <- "Region: Lisbon"
rownames(negbin_CI)[rownames(negbin_CI) == "regionporto"] <- "Region: Porto"
rownames(negbin_CI)[rownames(negbin_CI) == "region20k+"] <-"Region: Population 20k+"
rownames(negbin_CI)[rownames(negbin_CI) == "region10-20k"] <- "Region: Population 10-20K"

colnames(negbin_CI)[colnames(negbin_CI) == "Standard_Error"] <- "Standard Error"
colnames(negbin_CI)[colnames(negbin_CI) == "Test_Statistic"] <- "Test Statistic"
colnames(negbin_CI)[colnames(negbin_CI) == "P_value"] <- "P-Value"

```

After fitting the Poisson and Negative Binomial models, we summarized the coefficient estimates and confidence intervals on the natural scale, along with the standard errors, test statistics, and p-values, in Figures 2 and 3, respectively. Comparing the two tables, we notice that both models have similar coefficient estimates, with differences occurring in the hundredths decimal place. Both models had similarly low standard errors (with slightly larger standard errors for the Negative Binomial model), and both models indicate that the variables *Age Married 20-22*, *Age Married 25+*, and *Region Porto* are insignificant in explaining family size (at a significance level of 0.05).  

```{r}
knitr::kable(poisson_CI, digits = 3, caption = "Summary of exponentiated coefficients, 95% confidence intervals, standard errors, test statistics, and p-values from the fitted Poisson models. Results highlight the significance of literacy and regional factors, while age at marriage categories (20–22 and 25+) and Region: Porto are identified as statistically insignificant predictors.")

knitr::kable(negbin_CI, digits = 3, caption = "Summary of exponentiated coefficients, 95% confidence intervals, standard errors, test statistics, and p-values from the fitted Negative Binomial models. Results highlight the significance of literacy and regional factors, while age at marriage categories (20–22 and 25+) and Region: Porto are identified as statistically insignificant predictors.")
```

```{r, include=FALSE}
overdispersion_amount <- 1 / sqrt(confint(negbin_mod, parm = "sigma"))
overdispersion_amount
```

Figure 2 compares the exponentiated coefficients (rate ratios) with confidence intervals for Poisson and Negative Binomial models. Both models yield similar coefficient estimates and trends. However, the Negative Binomial model generally displays slightly wider confidence intervals, reflecting its ability to account for overdispersion. For the *Age Married 25+* category, the Negative Binomial model's confidence interval covers less of the line at 1.0 compared to the Poisson model, indicating improved precision. Also the model estimated an overdispersion parameter of 24.4% (CI 21.4%, 27.8%) indicating that birth rates vary by about 24.4% between families. Therefore, we believe a Negative Binomial model is better at explaining variation in family size and it serves as our model choice.  

```{r, fig.cap="Comparison of exponentiated coefficients (growth factors) and 95% confidence intervals for predictor levels across Poisson and Negative Binomial models. The plot highlights differences in effect sizes and overdispersion adjustments for key predictors, including age at marriage, literacy, and region.", fig.pos="!h", fig.height=4, fig.width=8, fig.align='center', warning=FALSE}
plot_summs(poisson_mod, negbin_mod,
  omit.coefs = "(Intercept)",
  model.names = c("Poisson Model", "Negative Binomial"), exp = TRUE) +
  labs(x = "Exponentiated Confidence Interval (Growth Factor)", y = "Predictor Levels") +
  theme_nice() +
  theme(
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    axis.text.y = element_text(size = 8)
  )
```

# Conclusion
Overall, the results from our data analysis align with ideas from pre-existing literature and demonstrate a positive progression in selecting an appropriate model to fit the data. Both our original Poisson model and the Negative Binomial model demonstrated generally significant p-values for variables such as literacy and region which is supported by the results of Gutkind’s and Mahmood et al. 's studies. However, contrary to our previous literature review, some variable levels did not prove to be as significant as initially thought, such as *Age Married 20-22*, *Age Married 25+*, and  *Region Porto*.

In particular, the variable *literacy* under the Negative Binomial model had an estimated coefficient of 1.126 on the natural scale (Figure 3). Since this represents a growth factor; the birth rate of uneducated couples compared to their educated counterparts increases by a factor of 1.126, holding all else constant. This trend matches Mahmood et al.’s (2016) findings that $77\%$ of educated males favoured smaller families compared to only $19.2\%$ of uneducated males $(p < 0.001)$. Additionally, the variable at level *Age Married 22-25* has an estimated coefficient 0.932 (Figure 3) which is interpreted as a decrease in birth rates by a factor of 0.932 for couples married at an age between 22 to 25 compared to couples married at the age between 0 to 20, holding all else constant. This matches the findings from Freedman (1963) which also found an inverse relationship between age of marriage and the birth rates.

Our final model results indicate that marrying later as well as higher literacy status is generally associated with having fewer children, with indications of overdispersion and the need for an offset within the data. Our Negative Binomial model seems to be an appropriate fit for the 1979 Portugal fertility data, demonstrating a significant relationship between literacy, region, and family size, with room for further investigation of the age of a marriage, in order to deeper understand the population behaviors and trends. 

# References:
Freedman, R. (1963). Norms for family size in underdeveloped areas. *Proceedings of the Royal Society of London. Series B. Biological Sciences*, **159**(974), 220–245. \url{https://doi.org/10.1098/rspb.1963.0074} 

Gutkind, Peter C. W. (1962). African Urban Family Life: Comment on and Analysis of Some Rural-Urban Differences. *Cahiers d’Études Africaines*, **3**(10), 149–217. \url{http://www.jstor.org/stable/4390830}

Mahmood, H., Khan, Z., & Masood, S. (2016). Effects of male literacy on family size: A cross sectional study conducted in Chakwal city. *Journal of the Pakistan Medical Association*, **66**(4), 399–403








