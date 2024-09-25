
# SA1A power analysis ----------------------------------------------------------
# in our logistic model below, the primary constraint on power when effect size is small is baseline prevalence of CHS among DND users
# therefore, in our proposal, we defined "strict criteria" as baseline prevalence of ~10% (ie, a relatively low prevalence)
# we defined "relaxed criteria" as baseline prevalence of ~40%
# adjust this and other parameters below to assess power under different criteria


# load library -----------------------------------------------------------------
library(tidyverse)


# assign number of simulations (nsim) and sample sizes (Nvec) ------------------
nsim <- 1000
Nvec <- seq(25, 200, by = 0.25)
power.N <- numeric(length(Nvec))


# loop to perform simulation, fit model, extract p value, calculate power ------
for (j in 1:length(Nvec)) {
  
  pval <- numeric(nsim)
  n <- Nvec[j]
  
  for (i in 1:nsim) {
    
    # predictors
    age <- rnorm(n)
    gender <- factor(sample(c(0, 1), size = n, replace = TRUE, prob = c(0.5, 0.5)))
    race <- factor(sample(c(1, 2, 3, 4), size = n, replace = TRUE, prob = c(0.25, 0.25, 0.25, 0.25)))
    ses <- factor(sample(c(1, 2, 3, 4), size = n, replace = TRUE, prob = c(0.25, 0.25, 0.25, 0.25)))
    
    hx_gi <- factor(sample(c(0, 1), size = n, replace = TRUE, prob = c(0.5, 0.5)))
    hx_psych <- factor(sample(c(0, 1), size = n, replace = TRUE, prob = c(0.5, 0.5)))
    
    alcohol <- factor(sample(c(0, 1), size = n, replace = TRUE, prob = c(0.5, 0.5)))
    tobacco <- factor(sample(c(0, 1), size = n, replace = TRUE, prob = c(0.5, 0.5)))
    other_substance <- factor(sample(c(0, 1), size = n, replace = TRUE, prob = c(0.5, 0.5)))
    
    other <- rnorm(n)
    
    heavy_use <- factor(sample(c(0, 1), size = n, replace = TRUE, prob = c(0.5, 0.5)))
    
    # y
    xb <- 
      
      #-0.8 + # 40%                        # adjust baseline CHS prevalence here
      -2.197 + # 10%
      #-2.944 + # 5%
      
      log(1)*age +                         # add/remove covariates and/or interaction terms here and below
      log(1)*as.numeric(gender) +
      log(1)*as.numeric(race) +
      log(1)*as.numeric(ses) +
      
      log(1)*as.numeric(hx_gi) +
      log(1)*as.numeric(hx_psych) +
      
      log(1)*as.numeric(alcohol) + 
      log(1)*as.numeric(tobacco) +
      log(1)*as.numeric(other_substance) +
      
      log(1)*other +
      
      ifelse(
        heavy_use == "0",
        log(1)*as.numeric(heavy_use),
        log(1.5)*as.numeric(heavy_use)                 # adjust effect size here
      )
    
    # probability
    p <- 1/(1 + exp(-xb))
    
    # sample y values
    y <- rbinom(n = n, size = 1, prob = p)
    
    # fit model
    log_mod <- glm(
      y ~ 
        age +                               # add/remove covariates and/or interaction terms here and above
        gender +
        race +
        ses +
        
        hx_gi +
        hx_psych +
        
        alcohol +
        tobacco +
        other_substance +
        
        heavy_use
      , 
      family = "binomial"
    )
    
    model_summary <- summary(log_mod)
    
    # Print detailed debug information
    print(paste("Sample Size:", n))
    print(summary(log_mod))
    
    # Extract p-values
    pval[i] <- summary(log_mod)$coefficients["heavy_use1", "Pr(>|z|)"]
    
  }
  
  # calculate power
  power.N[j] <- sum(pval < 0.05)/nsim
  
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
    title = "1A.1)\nHeavy Use Odds Ratio=1.5\nRelaxed Criteria",  # adjust title
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
