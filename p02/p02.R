library(dplyr)      
library(ggplot2)    
library(survey)     
library(tidyverse)   
library(rstatix)    

load("NSDUH_2024.Rdata")

# Clean mental health score
df <- NSDUH_2024 %>%
  mutate(
    MH_score = as.numeric(KSSLR6MONED),
    MH_score = ifelse(MH_score %in% c(94, 97, 98, 99), NA, MH_score)
  ) %>%
  # Remove rows with missing MH_score or AGE3
  filter(!is.na(MH_score), !is.na(AGE3))

# Recode AGE3 into 3 broad adult age groups
df <- df %>%
  # Remove missing MH_score or AGE3
  filter(!is.na(MH_score), !is.na(AGE3)) %>%
  mutate(
    AGE_GROUP = case_when(
      AGE3 %in% 4:6  ~ "Young adult",   # 18–29
      AGE3 %in% 7:9  ~ "Middle age",    # 30–64
      AGE3 == 10 | AGE3 == 11 ~ "Older adult"
    ),
    AGE_GROUP = factor(AGE_GROUP, levels = c("Young adult", "Middle age", "Older adult"))
  )

# Check counts
table(df$AGE_GROUP)

# Kruskal-Wallis test
kruskal.test(MH_score ~ AGE_GROUP, data = df)

# Effect size
library(rstatix)
kruskal_effsize(df, MH_score ~ AGE_GROUP)

# Pairwise Wilcoxon comparisons
pairwise.wilcox.test(df$MH_score, df$AGE_GROUP, p.adjust.method = "bonferroni")

ggplot(df, aes(x = MH_score, fill = AGE_GROUP)) +
  geom_density(alpha = 0.4) +
  labs(
    x = "Mental Health Score",
    y = "Density",
    fill = "Age Group",
    title = "Layered Density of Mental Health Scores by Broad Age Group"
  ) +
  theme_minimal()






