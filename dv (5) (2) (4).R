library(tidyverse)
library(ggplot2)
library(dplyr)
library(viridis)
library(dunn.test)

data <- read.csv("DV.csv")


#Variables to compare with Obesity: 
# 1: Gender
# 2: Smoke
# 3: Age
# 4: Mental Distress Level
# 5: Psqi
# 6: Resilience level

# The target variable: Obesity Indicator.

data$Obesity.Indicator <- as.factor(data$Obesity.Indicator)
data$Obesity.Indicator <- factor(data$Obesity.Indicator, levels = names(sort(table(data$Obesity.Indicator), decreasing = TRUE)))

data <- data %>%
  mutate(Obesity.Indicator = recode(Obesity.Indicator,
                                    'Normal_Weight' = 'NW',
                                    'Overweight_Level_I' = 'OW1',
                                    'Overweight_Level_II' = 'OW2',
                                    'Obesity_Type_I' = 'OT1',
                                    'Insufficient_Weight' = 'IW',
                                    'Obesity_Type_II' = 'OT2',
                                    'Obesity_Type_III' = 'OT3'))

OI_counts <- data %>%
  count(Obesity.Indicator)

ggplot(OI_counts, aes(x = Obesity.Indicator, y = n, fill = Obesity.Indicator)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5) +
  scale_fill_viridis_d() +
  xlab("Obesity Indicator") +    
  ylab("Count") +                
  ggtitle("Distribution of Obesity Indicator") +
  theme_minimal()

# Gender with Obesity Indicator. (Two categorical variables.)

data$Gender <- as.factor(data$Gender)

Gender_counts <- data %>%
  count(Gender)

ggplot(Gender_counts, aes(x = Gender, y = n, fill = Gender)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5) +
  scale_fill_viridis_d() +
  xlab("Gender") +    
  ylab("Count") +                
  ggtitle("Gender Distribution") +
  theme_minimal()

# mosaicplot used to visualize the relationship between Gender and Obesity Indicator.
contingency_table_GO <- table(data$Gender, data$Obesity.Indicator)
print(contingency_table_GO)

mosaicplot(contingency_table_GO, main = "Gender & Obesity Indicator"
           , color = c("lightpink", "lavender", "coral", "violet", "brown1", "skyblue", "palegreen"))

# More accurate than mosaicplot:
Gender_obesity_counts <- data %>%
  count(Obesity.Indicator, Gender)

ggplot(Gender_obesity_counts, aes(x = Obesity.Indicator, y = n, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_fill_manual(values = c("pink", "lightblue")) +
  xlab("Obesity Indicator") +
  ylab("Count") +
  ggtitle("Distribution of Gender Status Across Obesity Indicators") +
  theme_minimal()

# chi test to check whether there is a significant association between the two categorical variables.
chi_test_GO <- chisq.test(contingency_table_GO)
print(chi_test_GO)

# H0: There is no association between the variables.
# H1: There is an association between the variables.

if (chi_test_GO$p.value < 0.05) {
  print("The p-value is less than 0.05. We reject the null hypothesis. The result is statistically significant.")
} else {
  print("The p-value is greater than or equal to 0.05. We fail to reject the null hypothesis. The result is not statistically significant.")
}

# Smoke with Obesity Indicator. (Two categorical variables.)

data$SMOKE <- as.factor(data$SMOKE)
data$SMOKE <- factor(data$SMOKE, levels = names(sort(table(data$SMOKE), decreasing = TRUE)))

SMOKE_counts <- data %>%
  count(SMOKE)

ggplot(SMOKE_counts, aes(x = SMOKE, y = n, fill = SMOKE)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5) +
  scale_fill_viridis_d() +
  xlab("Smoke") +    
  ylab("Count") +                
  ggtitle("Smoke Distribution") +
  theme_minimal()

# mosaicplot used to visualize the relationship between Smoke and Obesity Indicator.
contingency_table_SO <- table(data$SMOKE, data$Obesity.Indicator)
print(contingency_table_SO)

mosaicplot(contingency_table_SO, main = "Smoke & Obesity Indicator"
           , color = c("lightpink", "lavender", "coral", "violet", "brown1", "skyblue", "palegreen"))

# More accurate than mosaicplot:
SMOKE_obesity_counts <- data %>%
  count(Obesity.Indicator, SMOKE)

ggplot(SMOKE_obesity_counts, aes(x = Obesity.Indicator, y = n, fill = SMOKE)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_fill_viridis_d() +
  xlab("Obesity Indicator") +
  ylab("Count") +
  ggtitle("Distribution of Smoking Status Across Obesity Indicators") +
  theme_minimal()

# The counts are highly imbalanced in smoke variable.
# Fisher's Exact Test is a better alternative to the Chi-square test.

# fisher test to check whether there is a significant association between the two categorical variables.
fisher_test_SO <- fisher.test(contingency_table_SO, simulate.p.value=TRUE)
print(fisher_test_SO)

# H0: There is no association between the variables.
# H1: There is an association between the variables.

if (fisher_test_SO$p.value < 0.05) {
  print("The p-value is less than 0.05. We reject the null hypothesis. The result is statistically significant.")
} else {
  print("The p-value is greater than or equal to 0.05. We fail to reject the null hypothesis. The result is not statistically significant.")
}

# Age with Obesity Indicator. (categorical with numerical)

# Removing the decimal points:
data$Age <- round(data$Age)

# Histogram to see the Distribution of Age:
ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "lavender", color = "black") +
  labs(title = "Age Distribution", x = "Age", y = "Count") +
  theme_minimal()

qqPlot(data$Age)

# We can see it is not normally distributed from the histogram and qqapalot but to make sure use shapiro test.

# Boxplot of Age by Obesity Indicator:
ggplot(data, aes(x = Obesity.Indicator, y = Age, fill = Obesity.Indicator)) +
  geom_boxplot(outlier.size = 2, outlier.colour = "lightsalmon") +
  labs(title = "Age & Obesity Indicator", x = "Obesity Indicator", y = "Age") +
  theme_minimal()

# Discretization of age: transforming continuous age(numerical) data into discrete intervals (categories): 

ages <- unique(data$Age)
print(sort(ages))

max(data$Age)
min(data$Age)

length(unique(data$Age))

breaks <- c(14, 18, 30, 45, 60, Inf)
labels <- c("Adolescence (14-17)", "Young Adult (18-29)", "Adult (30-44)", 
            "Middle Age (45-59)", "Old (60+)")

data$age_groups <- cut(data$Age, breaks = breaks, labels = labels, right = FALSE)

ggplot(data, aes(x = age_groups)) +
  geom_bar(fill = "lavender", color = "black") +
  labs(title = "Distribution of Age Groups", x = "Age Group", y = "Count") +
  theme_minimal() 

contingency_table_AO <- table(data$age_groups, data$Obesity.Indicator)
print(contingency_table_AO)

mosaicplot(contingency_table_AO, main = "Age groups & Obesity Indicator"
           , color = c("lightpink", "lavender", "coral", "violet", "brown1", "skyblue", "palegreen"))

# More accurate than mosaicplot:
age_counts <- data %>%
  count(Obesity.Indicator, age_groups)

ggplot(age_counts, aes(x = Obesity.Indicator, y = n, fill = age_groups)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_fill_viridis_d() +
  xlab("Obesity Indicator") +
  ylab("Count") +
  ggtitle("Distribution of Age groups Across Obesity Indicators") +
  theme_minimal()

# Chi test to check whether there is a significant association between the two categorical variables.
chi_test_AO <- chisq.test(contingency_table_AO)
print(chi_test_AO)

# H0: There is no association between the variables.
# H1: There is an association between the variables.

if (chi_test_AO$p.value < 0.05) {
  print("The p-value is less than 0.05. We reject the null hypothesis. The result is statistically significant.")
} else {
  print("The p-value is greater than or equal to 0.05. We fail to reject the null hypothesis. The result is not statistically significant.")
}

# Mental Distress Level with Obesity Indicator (Two categorical variables.)

data$Mental.Distress.Level <- as.factor(data$Mental.Distress.Level)
data$Mental.Distress.Level <- factor(data$Mental.Distress.Level, levels = names(sort(table(data$Mental.Distress.Level), decreasing = TRUE)))

mdlevels <- unique(data$Mental.Distress.Level)
print(mdlevels)

data <- data %>%
  mutate( Mental.Distress.Level = recode(Mental.Distress.Level,
                                    '1' = 'well',
                                    '2' = 'moderate disorder',
                                    '3' = 'severe disorder'))

MDLevel_counts <- data %>%
  count(Mental.Distress.Level)

ggplot(MDLevel_counts, aes(x = Mental.Distress.Level, y = n, fill = Mental.Distress.Level)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5) +
  scale_fill_viridis_d() +
  xlab("Mental Distress Level") +    
  ylab("Count") +                
  ggtitle("MDL Distribution") +
  theme_minimal()

# Mosaicplot used to visualize the relationship between Mental Distress Level and Obesity Indicator.
contingency_table_MDLO <- table(data$Mental.Distress.Level, data$Obesity.Indicator)
print(contingency_table_MDLO)

mosaicplot(contingency_table_MDLO, main = "MDL & Obesity Indicator"
           , color = c("lightpink", "lavender", "coral", "violet", "brown1", "skyblue", "palegreen"))


MDLevel_obesity_counts <- data %>%
  count(Obesity.Indicator, Mental.Distress.Level)

# More accurate than mosaicplot:
ggplot(MDLevel_obesity_counts, aes(x = Obesity.Indicator, y = n, fill = Mental.Distress.Level)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_fill_viridis_d() +
  xlab("Obesity Indicator") +
  ylab("Count") +
  ggtitle("Distribution of Mental Distress Levels Across Obesity Indicators") +
  theme_minimal()

# Chi test to check whether there is a significant association between the two categorical variables.
chi_test_MDLO <- chisq.test(contingency_table_MDLO)
print(chi_test_MDLO)

# H0: There is no association between the variables.
# H1: There is an association between the variables.

if (chi_test_MDLO$p.value < 0.05) {
  print("The p-value is less than 0.05. We reject the null hypothesis. The result is statistically significant.")
} else {
  print("The p-value is greater than or equal to 0.05. We fail to reject the null hypothesis. The result is not statistically significant.")
}

# Resilience Level with Obesity Indicator (Two categorical variables.)

data$Resilience_Level <- as.factor(data$Resilience_Level)
data$Resilience_Level <- factor(data$Resilience_Level, levels = names(sort(table(data$Resilience_Level), decreasing = TRUE)))

RLevel_counts <- data %>%
  count(Resilience_Level)

ggplot(RLevel_counts, aes(x = Resilience_Level, y = n, fill = Resilience_Level)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5) +
  scale_fill_viridis_d() +
  xlab("Resilience Level") +    
  ylab("Count") +                
  ggtitle("Resilience Distribution") +
  theme_minimal()

# Mosaicplot used to visualize the relationship between Resiliences Level and Obesity Indicator.
contingency_table_RO <- table(data$Resilience_Level, data$Obesity.Indicator)
print(contingency_table_RO)

mosaicplot(contingency_table_RO, main = "Resilience & Obesity Indicator"
           , color = c("lightpink", "lavender", "coral", "violet", "brown1", "skyblue", "palegreen"))

# More accurate than mosaicplot:
Resilience_obesity_counts <- data %>%
  count(Obesity.Indicator, Resilience_Level)

ggplot(Resilience_obesity_counts, aes(x = Obesity.Indicator, y = n, fill = Resilience_Level)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_fill_viridis_d() +
  xlab("Obesity Indicator") +
  ylab("Count") +
  ggtitle("Distribution of Resilience Levels Across Obesity Indicators") +
  theme_minimal()

# Chi test to check whether there is a significant association between the two categorical variables.
chi_test_RO <- chisq.test(contingency_table_RO)
print(chi_test_RO)

# H0: There is no association between the variables.
# H1: There is an association between the variables.

if (chi_test_RO$p.value < 0.05) {
  print("The p-value is less than 0.05. We reject the null hypothesis. The result is statistically significant.")
} else {
  print("The p-value is greater than or equal to 0.05. We fail to reject the null hypothesis. The result is not statistically significant.")
}

# Pittsburgh Sleep Quality Index (PSQI) with Obesity Indicator (Two categorical variables.)

data$Global_PSQI <- as.factor(data$Global_PSQI)
data$Global_PSQI <- factor(data$Global_PSQI, levels = names(sort(table(data$Global_PSQI), decreasing = TRUE)))

Global_PSQI_counts <- data %>%
  count(Global_PSQI)

ggplot(Global_PSQI_counts, aes(x = Global_PSQI, y = n, fill = Global_PSQI)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5) +
  scale_fill_viridis_d() +
  xlab("Global_PSQI") +    
  ylab("Count") +                
  ggtitle("Global_PSQI Distribution") +
  theme_minimal()

# Mosaicplot used to visualize the relationship between PSQI and Obesity Indicator.
contingency_table_psqiO <- table(data$Global_PSQI, data$Obesity.Indicator)
print(contingency_table_psqiO)

mosaicplot(contingency_table_psqiO, main = "PSQI & Obesity Indicator"
           , color = c("lightpink", "lavender", "coral", "violet", "brown1", "skyblue", "palegreen"))

# More accurate than mosaicplot:
Global_PSQI_obesity_counts <- data %>%
  count(Obesity.Indicator, Global_PSQI)

ggplot(Global_PSQI_obesity_counts, aes(x = Obesity.Indicator, y = n, fill = Global_PSQI)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_fill_viridis_d() +
  xlab("Obesity Indicator") +
  ylab("Count") +
  ggtitle("Distribution of Global PSQI Levels Across Obesity Indicators") +
  theme_minimal() 

# Chi test to check whether there is a significant association between the two categorical variables.
chi_test_psqiO <- chisq.test(contingency_table_psqiO)
print(chi_test_psqiO)

# H0: There is no association between the variables.
# H1: There is an association between the variables.

if (chi_test_psqiO$p.value < 0.05) {
  print("The p-value is less than 0.05. We reject the null hypothesis. The result is statistically significant.")
} else {
  print("The p-value is greater than or equal to 0.05. We fail to reject the null hypothesis. The result is not statistically significant.")
}

