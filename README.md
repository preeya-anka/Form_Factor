library(readxl)
library(dplyr)
library(ggplot2)

ff_dat <- read_excel("/Users/priyankasubedi/Desktop/RESEARCH 2.xlsx", sheet = 1)
ff_dat <- ff_dat[,1:10]
#str(ff_dat)
## Look at the distribution of the form factor in every dia_class
ggplot(data = ff_dat , mapping = aes( x = dia_class, y  = form_factor), fill = dia_class) +
  geom_boxplot()
# remove outlier in every dia_class

grouped <- ff_dat %>%
  group_by(dia_class)
remove_outliers <- function(dia_class) {
  q1 <- quantile(dia_class$form_factor, 0.25)
  q3 <- quantile(dia_class$form_factor, 0.75)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  return(filter(dia_class, form_factor >= lower_bound & form_factor <= upper_bound))
}
# Apply the function to each group and combine the results
filtered_data <- grouped %>%
  do(remove_outliers(.))
filtered_data <- filtered_data %>%
  do(remove_outliers(.))
ff_dat <- filtered_data
ggplot(data = ff_dat , mapping = aes( x = dia_class, y  = form_factor), fill = dia_class) +
  geom_boxplot()
## Normality test of the data

s <- shapiro.test(ff_dat$form_factor)
s

# The result of normality shows that the data is in normal distribution.

freq <- ff_dat %>% 
  group_by(dia_class) %>%
  summarise(N = n())

freq
# for that Levenes test is used because the data follows the normal distribution
library(car)
l <- leveneTest(y = ff_dat$form_factor, group = ff_dat$dia_class)
l
# Unequal number of samples, Variance equal, Normal distributions. So that I chose parametric test called One way Fisher's ANOVA test
anova_result <- aov(ff_dat$form_factor ~ ff_dat$dia_class, data = ff_dat)

summary(anova_result)

library(tidyverse)
library(forestecology)
library(patchwork)
library(blockCV)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(usethis)
library(devtools)
library(ggstatsplot)
library(correlation)
library(GGally)

###
fit <- ggbetweenstats(
  data = ff_dat,
  x = dia_class,
  y = form_factor,
  type = "parametric",
  xlab = "Diameter Class of the trees",
  ylab = "Artificial form factor",
  var.equal = TRUE
)

fit
(stats::aov(ff_dat$form_factor ~ ff_dat$dia_class, data = ff_dat))

## Data rearrange 

re_dat <- ff_dat %>%
  mutate(dia_class_new = if_else(dia_class == "0-30", "0-30", "30 above"))
## 
fit <- ggbetweenstats(
  data = re_dat,
  x = dia_class_new,
  y = form_factor,
  type = "parametric",
  xlab = "Diameter class of the trees",
  ylab = "Artificial form factor",
  var.equal = TRUE
)


## Now for correlation test 

cor_dat <- re_dat[,c("diameter","total_ht","form_factor")]


library(psych)
pairs.panels(cor_dat)

# Sample data for height and form factor
X <- re_dat$total_ht
Y <- re_dat$form_factor


# Calculate Pearson correlation coefficient
correlation_coefficient <- cor(X, Y, method = "pearson")

# Print the correlation coefficient
print(correlation_coefficient)

# Create a scatterplot
plot(X, Y, main = "Form Factor and Height", 
     xlab = "Total Height", ylab = "Form_Factor", pch = 19, col = "brown")

# Add a regression line
abline(lm(Y ~ X), col = "darkblue")

# Add correlation coefficient to the plot
mtext(paste("Correlation coefficient =", round(correlation_coefficient, 2)), side = 3, line = -1, col = "black")

# Sample data for diameter and form factor
P <- re_dat$diameter
Q <- re_dat$form_factor


# Calculate Pearson correlation coefficient
correlation_coefficient <- cor(P, Q, method = "pearson")

# Print the correlation coefficient
print(correlation_coefficient)

# Create a scatterplot
plot(P, Q, main = "Form factor and Diameter", 
     xlab = "Diameter", ylab = "Form_Factor", pch = 19, col = "brown")

# Add a regression line
abline(lm(Q ~ P), col = "darkblue")

# Add correlation coefficient to the plot
mtext(paste("Correlation coefficient =", round(correlation_coefficient, 2)), side = 3, line = -2, col = "black")


#install.packages("faraway")
library(faraway)
vif(cor_dat)

# now for modelling
#install.packages("lme4")
library(lme4)

# install.packages("caret")
library(caret)
library(easystats)

# Set the seed for reproducibility
set.seed(10)

# Split data into training (80%) and test (20%) sets
train_index <- createDataPartition(re_dat$form_factor, p = 0.8, list = FALSE)
train_data <- re_dat[train_index, ]
test_data <- re_dat[-train_index, ]
##
m1 <- lm(form_factor ~ diameter, data = train_data)
library(dplyr)
p <-m1 %>% predict(test_data)
m1
performance(m1)
RMSE(p,test_data$form_factor)

##
m2 <- lm(form_factor ~ diameter + total_ht, data = train_data)
library(dplyr)
p1 <-m2 %>% predict(test_data)
m2
performance(m2)
RMSE(p1,test_data$form_factor)

# 
m3 <- lm(form_factor ~ total_ht, data = train_data)
library(dplyr)
p2 <-m3 %>% predict(test_data)
m3
performance(m3)
RMSE(p2,test_data$form_factor)


# 
m4 <- lm(form_factor ~ diameter+ I(diameter^2) + total_ht + I(total_ht^2), data = train_data)
library(dplyr)
p4 <-m4 %>% predict(test_data)
m4
performance(m4)
RMSE(p4,test_data$form_factor)

# Create a summary table
summary_table <- data.frame(
  Variable = c("Intercept", "diameter", "diameter^2", "total_ht", "total_ht^2", "AIC", "AICc", "BIC", "R2", "R2 (adj.)", "RMSE", "Sigma"),
  Coefficient = c(6.584e-01, -6.489e-03, 6.497e-05, -2.326e-03, 2.741e-05, - 243.680, - 242.303, - 230.363, 0.531, 0.501, 0.037, 0.038)
)

# Print the summary table
print(summary_table)

#MY own regression
data <- data.frame(
  form_factor = re_dat$form_factor,
  diameter = re_dat$diameter,
  height = re_dat$total_ht
)

# Perform multiple linear regression
model <- lm(form_factor ~ diameter + height, data = data)

# View the regression summary
summary(model)

########
form_factor <- ff_dat$form_factor
# Calculate the mean of form_factor
mean_form_factor <- mean(form_factor)
mean_form_factor

# Create a bar plot
barplot(form_factor, names.arg = 1:81, ylim = c(0, 1), 
        main = "Form Factor of 81 Trees",
        xlab = "Tree Number", ylab = "Form Factor",
        col = "skyblue")

# Add a horizontal line for the mean
abline(h = mean_form_factor, col = "red")
mtext(paste("Average Form factor =", round(mean_form_factor, 2)), side = 3, line = -2, col = "black")

# Load the dplyr package
library(dplyr)

tree_counts <- table(your_dataset$DiameterClass)
tree_counts

your_dataset <- data.frame (
DiameterClass = ff_dat$dia_class,
Diameter = ff_dat$diameter,
Height = ff_dat$total_ht,
FormFactor = ff_dat$form_factor
)

# Create the summary table
summary_table <- your_dataset %>%
  group_by(DiameterClass) %>%
  summarize(
    AverageDiameter = mean(Diameter),
    AverageHeight = mean(Height),
    AverageFormFactor = mean(FormFactor)
  ) %>%
  ungroup()

print(summary_table)

tree_data <- data.frame(
  Number_of_Trees = 84,   # Number of trees
  Diameter = ff_dat$diameter,     # Diameter in inches
  Height = ff_dat$total_ht,             # Height in feet
  Logs_Volume = ff_dat$total_volume,     # Logs volume in cubic feet
  Cylindrical_Volume = ff_dat$cyl_volume, # Cylindrical volume in cubic feet
  Form_Factor = ff_dat$form_factor  # Form factor
)

library(dplyr)

summary_table <- tree_data %>%
  summarize(
    Total_Trees = sum(Number_of_Trees),
    Average_Diameter = mean(Diameter),
    Average_Height = mean(Height),
    Total_Logs_Volume = sum(Logs_Volume),
    Total_Cylindrical_Volume = sum(Cylindrical_Volume),
    Average_Form_Factor = mean(Form_Factor)
  )

print(summary_table)

form_factors <- ff_dat$form_factor

# Create a histogram of form factors
hist(form_factors, 
     main = "Histogram of FF", 
     xlab = "Form Factor", 
     ylab = "Frequency",
     col = "lightblue",   # Specify the fill color of bars
     border = "black",    # Specify the border color of bars
     breaks = 10          # Adjust the number of bins as needed
)


# Calculate and plot the mean line
mean_form_factor <- mean(form_factors)
abline(v = mean_form_factor, col = "red", lwd = 2)  # Add a red line for the mean

# Add a legend
legend("topright", legend = paste("Mean =", round(mean_form_factor, 2)), col = "red", lwd = 2, bty = "n")


tree_data <- data.frame(
  Diameter_Class = ff_dat$dia_class,
  Form_Factor = ff_dat$form_factor
)

# Load the ggplot2 package for data visualization
library(ggplot2)

# Create a grouped scatter plot with jitter
ggplot(data = tree_data, aes(x = as.factor(Diameter_Class), y = Form_Factor)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.6) +
  labs(
    title = "Diameter Class vs. Form Factor",
    x = "Diameter Class",
    y = "Form Factor"
  ) +
  theme_minimal()


# Example dataset with Diameter Class and Form Factor columns (replace with your actual data)
tree_data <- data.frame(
  Diameter_Class = ff_dat$dia_class,
  Form_Factor = ff_dat$form_factor
)

# Perform one-way ANOVA
anova_result <- aov(Form_Factor ~ Diameter_Class, data = tree_data)

# Display the ANOVA table and F-statistic
summary(anova_result)
# Perform one-way ANOVA
anova_result <- aov(Form_Factor ~ Diameter_Class, data = tree_data)

# Extract ANOVA table values
anova_table <- as.data.frame(summary(anova_result)[[1]])

# Rename the columns for clarity
colnames(anova_table) <- c( "Df", "Sum_Sq", "Mean_Sq", "F_value", "Pr(>F)")

# Print the ANOVA table
print(anova_table)

# Load the necessary package (if not already loaded)
library(stats)

# Perform Tukey's HSD test
tukey_result <- TukeyHSD(aov(tree_data$Form_Factor~as.factor(tree_data$Diameter_Class)))

# View the results
print(tukey_result)

# Create a data frame to organize the Tukey's results
tukey_table <- data.frame(
  Comparison = c("30-50 vs. 10-30", "50 above vs. 10-30", "50 above vs. 30-50"),
  Difference = c(-0.0555529298, -0.0562727494, -0.0007198196),
  Lower_CI = c(-0.08193556, -0.08670286, -0.03167328),
  Upper_CI = c(-0.02917030, -0.02584264, 0.03023364),
  p_adj = c(0.0000087, 0.0000910, 0.9983019)
)

# Rename columns for clarity (optional)
colnames(tukey_table) <- c("Comparison", "Mean Difference", "Lower CI", "Upper CI", "Adjusted p-value")

# Print the Tukey's results table
print(tukey_table)

# Fit a linear regression model
model <- lm(ff_dat$form_factor ~ ff_dat$diameter + ff_dat$total_ht, data = ff_dat)

# Print the summary of the regression model
summary(model)

model_poly <- lm(form_factor ~ diameter+ I(diameter^2) + total_ht + I(total_ht^2), data = ff_dat)
summary(model_poly)

model_poly <- lm(form_factor ~ diameter+ I(diameter^2) + total_ht + I(total_ht^2), data = train_data)
summary(model_poly)


correlation_test <- cor.test(ff_dat$form_factor, ff_dat$diameter)
correlation_test$p.value  # p-value
correlation_test$estimate  # correlation coefficient
correlation_test$conf.int   # confidence interval
# Load the "knitr" library if not already loaded
library(knitr)

# Create a table with the correlation test results
correlation_results <- data.frame(
  "Statistic" = c("Correlation Coefficient", "P-Value", "Confidence Interval (95%)"),
  "Value" = c(-0.5332784, "1.76367e-07", "[-0.671, -0.360]")
)

# Print the table using knitr
kable(correlation_results, format = "markdown")
# Perform linear regression
model <- lm(ff_dat$form_factor ~ ff_dat$diameter, data = ff_dat)
summary(model)
model1 <- lm(ff_dat$form_factor ~ ff_dat$total_ht, data = ff_dat)
summary(model1)



library(gt)


# Create a summary table
summary_table <- data.frame(
  Coefficient = coef(model)["ff_dat$diameter"],
  Std_Error = summary(model)$coefficients["ff_dat$diameter", "Std. Error"],
  t_Value = summary(model)$coefficients["ff_dat$diameter", "t value"],
  P_Value = summary(model)$coefficients["ff_dat$diameter", "Pr(>|t|)"],
  R_Squared = summary(model)$r.squared,
  F_Statistic = summary(model)$fstatistic[1],
  Residual_Std_Error = summary(model)$sigma
)

# Create a gt table
gt_summary_table <- gt(summary_table) %>%
  tab_header(
    title = "Summary of Linear Regression Model for 'form_factor' and 'diameter'",
    subtitle = "Model: form_factor ~ diameter"
  ) %>%
  fmt_number(
    columns = c("Coefficient", "Std_Error", "t_Value", "P_Value", "R_Squared", "F_Statistic", "Residual_Std_Error"),
    decimals = 4
  )

# Print the table
gt_summary_table


# Create a summary table
summary_table1 <- data.frame(
  Coefficient = coef(model1)["ff_dat$total_ht"],
  Std_Error = summary(model1)$coefficients["ff_dat$total_ht", "Std. Error"],
  t_Value = summary(model1)$coefficients["ff_dat$total_ht", "t value"],
  P_Value = summary(model1)$coefficients["ff_dat$total_ht", "Pr(>|t|)"],
  R_Squared = summary(model1)$r.squared,
  F_Statistic = summary(model1)$fstatistic[1],
  Residual_Std_Error = summary(model1)$sigma
)

# Create a gt table
gt_summary_table1 <- gt(summary_table) %>%
  tab_header(
    title = "Summary of Linear Regression Model for 'form_factor' and 'height'",
    subtitle = "Model: form_factor ~ diameter"
  ) %>%
  fmt_number(
    columns = c("Coefficient", "Std_Error", "t_Value", "P_Value", "R_Squared", "F_Statistic", "Residual_Std_Error"),
    decimals = 4
  )

# Print the table
gt_summary_table1

