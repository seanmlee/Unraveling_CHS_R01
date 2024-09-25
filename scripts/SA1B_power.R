
# SA1B power analysis ----------------------------------------------------------
# in our mixed model below, the primary constraint on power when effect size is small is random effect variance
# therefore, in our proposal, we defined "strict criteria" as random effect variance ~3 (ie, high variance)
# we defined "relaxed criteria" as random effect variance ~1
# adjust this and other parameters below to assess power under different criteria


# load library -----------------------------------------------------------------
library(tidyverse)
library(lme4)
library(lmerTest)


# assign number of simulations (nsim) and sample sizes (Nvec) ------------------
nsim <- 1000
Nvec <- seq(25, 200, by = 0.25)
power.N <- numeric(length(Nvec))


# assign slopes ----------------------------------------------------------------
coef_chs <- 1                                          # adjust effect size here
coef_heavy_use <- 0.5
coef_time <- 1
coef_age <- 1
coef_gender <- 1
coef_race <- 0
coef_ses <- 0
coef_hx_gi <- 0
coef_hx_psych <- 0
coef_alcohol <- 0 
coef_tobacco <- 0
coef_other_substance <- 0
coef_other <- 0


# assign variance
random_effect_sd <- 1                                # variance of random effect
residual_sd <- 1                               # standard deviation of residuals


# loop to perform simulation, fit model, extract p value, calculate power ------
for (j in 1:length(Nvec)) {
  
  pval <- numeric(nsim)
  n <- Nvec[j]
  
  for (i in 1:nsim) {
    
    num_subjects <- n
    subject_id <- factor(rep(1:num_subjects, each = 5))  # 5 observations per subject
    total_observations <- length(subject_id)
    
    chs <- factor(sample(c(0, 1), size = total_observations, replace = TRUE, prob = c(0.5, 0.5)))
    heavy_use <- factor(sample(c(0, 1), size = total_observations, replace = TRUE, prob = c(0.5, 0.5)))
    time <- factor(sample(c(0, 1), size = total_observations, replace = TRUE, prob = c(0.5, 0.5)))
    
    age <- rnorm(total_observations)
    gender <- factor(sample(c(0, 1), size = total_observations, replace = TRUE, prob = c(0.5, 0.5)))
    race <- factor(sample(c(1, 2, 3, 4), size = total_observations, replace = TRUE, prob = c(0.25, 0.25, 0.25, 0.25)))
    ses <- factor(sample(c(1, 2, 3, 4), size = total_observations, replace = TRUE, prob = c(0.25, 0.25, 0.25, 0.25)))
    
    hx_gi <- factor(sample(c(0, 1), size = total_observations, replace = TRUE, prob = c(0.5, 0.5)))
    hx_psych <- factor(sample(c(0, 1), size = total_observations, replace = TRUE, prob = c(0.5, 0.5)))
    
    alcohol <- factor(sample(c(0, 1), size = total_observations, replace = TRUE, prob = c(0.5, 0.5)))
    tobacco <- factor(sample(c(0, 1), size = total_observations, replace = TRUE, prob = c(0.5, 0.5)))
    other_substance <- factor(sample(c(0, 1), size = total_observations, replace = TRUE, prob = c(0.5, 0.5)))
    
    other <- rnorm(total_observations)
    
    random_effects <- rnorm(num_subjects, mean = 0, sd = random_effect_sd)
    
    xb <- 
      0 + 
      coef_chs * as.numeric(chs) +    # add/remove covariates and/or interaction terms here and below
      (coef_heavy_use * as.numeric(heavy_use) * coef_time * as.numeric(time)) +
      (coef_heavy_use * as.numeric(heavy_use) * coef_age * age) +
      (coef_heavy_use * as.numeric(heavy_use) * coef_gender * as.numeric(gender)) +
      coef_race * as.numeric(race) +
      coef_ses * as.numeric(ses) +
      coef_hx_gi * as.numeric(hx_gi) +
      coef_hx_psych * as.numeric(hx_psych) +
      coef_alcohol * as.numeric(alcohol) +
      coef_tobacco * as.numeric(tobacco) +
      coef_other_substance * as.numeric(other_substance) +
      coef_other * as.numeric(other) +
      random_effects[as.numeric(subject_id)]
    
    sf36_score <- rnorm(
      total_observations, 
      mean = xb, 
      sd = residual_sd
    )
    
    data <- data.frame(
      sf36_score = sf36_score,
      chs = chs, 
      heavy_use = heavy_use, 
      time = time,
      age = age,
      gender = gender,
      race = race,
      ses = ses,
      hx_gi = hx_gi,
      hx_psych = hx_psych,
      alcohol = alcohol,
      tobacco = tobacco,
      other_substance = other_substance,
      other = other,
      Subject_ID = subject_id
    )

    mixed_mod <- lmer(
      sf36_score ~ 
        chs +                          # add/remove covariates and/or interaction terms here and above
        heavy_use * time +
        heavy_use * age +
        heavy_use * gender +
        race + 
        ses +
        hx_gi +
        hx_psych +
        alcohol +
        tobacco +
        other_substance +
        other +
        (1 | Subject_ID), 
      data = data
    )
    
    if (isSingular(mixed_mod)) {
      next
    }
    
    model_summary <- summary(mixed_mod)
    
    # Print detailed debug information
    print(paste("Sample Size:", n))
    print(summary(mixed_mod))
    
    # Extract p-values for the fixed effect of 'heavy_use'
    if ("heavy_use1:time1" %in% rownames(model_summary$coefficients)) {
      pval[i] <- model_summary$coefficients["heavy_use1:time1", "Pr(>|t|)"]
    } else {
      pval[i] <- NA
    }
    
  }
  
  # Calculate power
  power.N[j] <- sum(pval < 0.05, na.rm = TRUE) / nsim
  
}

# create dataframe and plot ----------------------------------------------------
df <- data.frame(N = Nvec, Power = power.N)

df %>%
  
  ggplot(
    aes(
      x = N,
      y = Power
    )
  ) +
  
  geom_point(
    pch = 21,
    alpha = 0.5,
    size = 5
  ) +
  
  geom_hline(yintercept = 0.8) +
  
  geom_vline(xintercept = 190) +
  
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2)
  ) +
  
  labs(
    title = "1B.2)\nAbstinence*Time β=0.5\nRelaxed Criteria",   # adjust title
    x = "N"
  ) +
  
  theme_bw() +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 18, hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18)
  )
