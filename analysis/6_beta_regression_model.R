# # install cmdstanr backend
# remotes::install_github("stan-dev/cmdstanr")
# cmdstanr::check_cmdstan_toolchain(fix = TRUE)
# cmdstanr::install_cmdstan()

# Load required package
library(tidyr)
library(ggplot2)
library(recolorize)
library(dplyr)
library(broom.mixed)
library(emmeans)
library(performance)
library(lme4)
library(brms)

# load in the table
RGB_and_cluster_data <- read.csv("project_data/RGB_and_cluster_data.csv")[,-1]

#  RGB_and_cluster_data <- read.csv("project_data/silver_all_data.csv")[,-1]
# colnames(RGB_and_cluster_data)[3] <- "RGB_silver_area"

# view the type of data
str(RGB_and_cluster_data)

#convert treatment into logical values
RGB_and_cluster_data$treatment <- ifelse(RGB_and_cluster_data$treatment == 1, T, F)

# change variable types
RGB_and_cluster_data$parr_id <- as.factor(RGB_and_cluster_data$parr_id)
RGB_and_cluster_data$stream <- as.factor(RGB_and_cluster_data$stream)
RGB_and_cluster_data$river_section <- as.factor(RGB_and_cluster_data$river_section) # note this may be wrong
RGB_and_cluster_data$family <- as.factor(RGB_and_cluster_data$family)
RGB_and_cluster_data$transect <- as.factor(RGB_and_cluster_data$transect)

# Load the saved palette
parr_palette_all <- readRDS("rds_files/palette_all.rds")

# View palette colours
plotColorPalette(parr_palette_all)

# we would like to measure orange spots, represented by cluster 4. Lets rename this cluster column
colnames(RGB_and_cluster_data)[7] <- "orange_colour_area"

# lets sum clusters 1 and 5 as they both combine to form parr marks in most images
RGB_and_cluster_data$parr_mark_area <- RGB_and_cluster_data$cluster_1_area + RGB_and_cluster_data$cluster_3_area

# From here the brm package removes any "_" in column names, so to prevent confusion we will remove these ourselves.
colnames(RGB_and_cluster_data) <- gsub("_", "", colnames(RGB_and_cluster_data))

# Due to rounding errors the proportion of 2 parrmarkarea values are slightly greater then 1, we will reduce these values to 1
for (row in 1:nrow(RGB_and_cluster_data)){
  RGB_and_cluster_data$parrmarkarea[row] <- ifelse(RGB_and_cluster_data$parrmarkarea[row] >= 1, NA, RGB_and_cluster_data$parrmarkarea[row])
}

#RGB_and_cluster_data <- subset(RGB_and_cluster_data, subset = RGB_and_cluster_data$parrmarkarea < 1)

# Because we have a value of 1 we will need to use a zero-inflated beta regression
# run beta-regression model
formula_1 <- bf(mvbind(RGBsilveringarea, orangecolourarea, parrmarkarea) ~ treatment + length + (1|p|family) + (1|q|stream),
                phi ~ treatment + length + (1|p|family) + (1|q|stream))

# Fit the model
fit_formula_1 <- brm(
  formula_1,
  data = RGB_and_cluster_data,
  family = Beta(),
  chains = 4, iter = 4000, warmup = 1000,
  cores = 2,
  backend = "cmdstanr"
)

 # present results in table
tidy(fit_formula_1, effects = "fixed")

# # lets see the effect of 1 change
# emmeans(fit_formula_1, ~ treatment,
#         transform = "response") %>% 
#   contrast(method = "revpairwise")

# compare the model distributions with the response distributions
pp_check(fit_formula_1, resp = "RGBsilveringarea")
pp_check(fit_formula_1, resp = "orangecolourarea")
pp_check(fit_formula_1, resp = "parrmarkarea")

# view overall model trends
plot(conditional_effects(fit_formula_1, "length", resp = "RGBsilveringarea"), points = T)
conditional_effects(fit_formula_1, "length", resp = "orangecolourarea")
conditional_effects(fit_formula_1, "length", resp = "parrmarkarea")

# Extract predictions
prediction_silver <- predict(fit_formula_1, resp = "RGBsilveringarea")
prediction_orange <- predict(fit_formula_1, resp = "orangecolourarea")
prediction_bar <- predict(fit_formula_1, resp = "parrmarkarea")

# For now we just want the means
silver_estimate <- prediction_silver[1:282]
orange_estimate <- prediction_orange[1:282]
bar_estimate <- prediction_bar[1:282]

# merge collumns
RGB_and_cluster_data <- bind_cols(RGB_and_cluster_data, silver_estimate, orange_estimate, bar_estimate)

# rename added columns
colnames(RGB_and_cluster_data)[18:20] <- c("silver_estimate", "orange_estimate", "bar_estimate")

# Compare model predictions
ggplot(RGB_and_cluster_data) + geom_density(aes(x=silver_estimate)) + geom_density(aes(x=RGBsilveringarea), col = "red")
ggplot(RGB_and_cluster_data) + geom_density(aes(x=orange_estimate)) + geom_density(aes(x=orangecolourarea), col = "red")
ggplot(RGB_and_cluster_data) + geom_density(aes(x=bar_estimate)) + geom_density(aes(x=parrmarkarea), col = "red")

##############################################################################################################################################

# Here we will treat the models separately for each dependant variable
# 
# run beta-regression model
formula_2 <- bf(RGBsilveringarea ~ treatment + length + (1|p|family) + (1|q|stream),
                phi ~ treatment + length + (1|p|family) + (1|q|stream))

# Fit the model
fit_formula_2 <- brm(
  formula_2,
  data = RGB_and_cluster_data,
  family = Beta(),
  chains = 4, iter = 4000, warmup = 1000,
  cores = 2,
  backend = "cmdstanr"
)

summary(fit_formula_2)

# run beta-regression model
formula_3 <- bf(orangecolourarea ~ treatment + length + (1|p|family) + (1|q|stream),
                phi ~ treatment + length + (1|p|family) + (1|q|stream))

# Fit the model
fit_formula_3 <- brm(
  formula_3,
  data = RGB_and_cluster_data,
  family = Beta(),
  chains = 4, iter = 4000, warmup = 1000,
  cores = 2,
  backend = "cmdstanr"
)

# view the results from the model
summary(fit_formula_3)

# run beta-regression model
formula_4 <- bf(parrmarkarea ~ treatment + length + (1|p|family) + (1|q|stream),
                phi ~ treatment + length + (1|p|family) + (1|q|stream))

# Fit the model
fit_formula_4 <- brm(
  formula_4,
  data = RGB_and_cluster_data,
  family = Beta(),
  chains = 4, iter = 4000, warmup = 1000,
  cores = 2,
  backend = "cmdstanr"
)

summary(fit_formula_4)

##########################################################################################################
# Here we check for multi-colinearity
# Load performance package
library("performance")

# Because performance cannot handle bayesian models we will use simple mixed models
mixed_model_silver <- lmer(data = RGB_and_cluster_data,
                                    RGBsilveringarea ~ treatment +
                                      length +
                                      (1|stream) +
                                      (1|family),
                                    REML = F)

mixed_model_orange <- lmer(data = RGB_and_cluster_data,
                                    orangecolourarea ~ treatment +
                                      length +
                                      (1|stream) +
                                      (1|family),
                                    REML = F)

mixed_model_parr_mark <- lmer(data = RGB_and_cluster_data,
                                    parrmarkarea ~ treatment +
                                      length +
                                      (1|stream) +
                                      (1|family),
                                    REML = F)

# test collinearity between interactions
check_collinearity(mixed_model_silver)
check_collinearity(mixed_model_orange)
check_collinearity(mixed_model_parr_mark)

# All have low correlation and therefore we do not need to model interactions.

####################################################################################
# View marginal effects
# fit <- brm(count ~ zAge + zBase * Trt + (1 | patient),
#            data = epilepsy, family = poisson())

## plot all marginal effects
test <- plot(marginal_effects(fit_formula_1, effects = "treatment"), ask = T)

# Plot marginal effects for specified lengths
conditions <- data.frame(length = c(88, 95))

plot(marginal_effects(fit_formula_1, effects = "treatment",
                      conditions = conditions))

## also incorporate random effects variance over patients
## also add data points and a rug representation of predictor values
plot(marginal_effects(fit_formula_1, effects = "treatment", conditions = conditions, re_formula = NA),
     points = TRUE, rug = TRUE)

# the ooga booga
test <- marginal_effects(fit_formula_1, effects = "treatment",
                         conditions = conditions)

#############################################################################################

# create beta-reg model
beta_reg_silvering <- betareg(RGBsilveringarea ~ treatment + length + (1|stream) | treatment + length, data = RGB_and_cluster_data)

# check collinearity
check_collinearity(beta_reg_silvering)

# create beta-reg model
beta_reg_orange_colour <- betareg(orangecolourarea ~ treatment + length | treatment + length, data = RGB_and_cluster_data)

# check collinearity
check_collinearity(beta_reg_orange_colour)

# create beta-reg model
beta_reg_parr_mark <- betareg(parrmarkarea ~ treatment + length | treatment + length, data = RGB_and_cluster_data)

# check collinearity
check_collinearity(beta_reg_parr_mark)