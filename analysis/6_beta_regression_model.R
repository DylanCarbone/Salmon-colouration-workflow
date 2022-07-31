# Load required packages
library("glmmTMB")
library("tidyr")
library("ggplot2")
library("recolorize")
library("broom.mixed")
library("emmeans")
library("performance")
library("lme4")
library("DHARMa")
library("ggeffects")
library("lmtest")
library("Rmisc")
library("dplyr")

h <- colnames(RGB_and_cluster_data)

# load in the table
RGB_and_cluster_data <- read.csv("project_data/RGB_and_cluster_data.csv")[,-1]

# view the type of data
str(RGB_and_cluster_data)

#convert treatment into logical values
RGB_and_cluster_data$treatment <- ifelse(RGB_and_cluster_data$treatment == 1, "Treatment", "Control")

# change variable types
RGB_and_cluster_data$treatment <- as.factor(RGB_and_cluster_data$treatment)
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

# Due to rounding errors the proportion of 2 parrmarkarea values are slightly greater then 1.
# In addition, values of 1 have occured as a result of errrors in k-means clustering as it is not possible to have values of 1 for parr marks to be visible.
RGB_and_cluster_data <- subset(RGB_and_cluster_data, subset = RGB_and_cluster_data$parr_mark_area < 1)

## Silvering models
# Lets begin modelling by creating an initial silvering model. We will include an interaction between length and treatment.
# Likewise, we'll include parr_mark_area and orange_colour_area
silvering_model_1 <- glmmTMB(data = RGB_and_cluster_data, RGB_silvering_area ~ treatment * length + parr_mark_area + orange_colour_area + (1|family) + (1|stream), family = beta_family())

# First, we'll remove the orange_colour_area parameter
silvering_model_2 <- glmmTMB(data = RGB_and_cluster_data, RGB_silvering_area ~ treatment * length + parr_mark_area + (1|family) + (1|stream), family = beta_family())

#Now lets compare the models
lrtest(silvering_model_1, silvering_model_2)
#There is no significant difference and we can therefore remove the orange_colour_area parameter safely

# Now lets remove the parr_mark_area parameter
silvering_model_3 <- glmmTMB(data = RGB_and_cluster_data, RGB_silvering_area ~ treatment * length + (1|family) + (1|stream), family = beta_family())

#Now lets compare the models
lrtest(silvering_model_2, silvering_model_3)
#There is a significant difference and we therefore keep the parr_mark_area parameter

#Now lets remvoe the treatment * length interaction
silvering_model_4 <- glmmTMB(data = RGB_and_cluster_data, RGB_silvering_area ~ treatment + length + parr_mark_area + (1|family) + (1|stream), family = beta_family())

#Now lets compare the models
lrtest(silvering_model_2, silvering_model_4)
# There is no significant difference and we can therefore remove the interaction.

# Now lets remove the length parameter
silvering_model_5 <- glmmTMB(data = RGB_and_cluster_data, RGB_silvering_area ~ treatment + parr_mark_area + (1|family) + (1|stream), family = beta_family())

# Now lets compare the models
lrtest(silvering_model_4, silvering_model_5)
# There is a significant difference and we therefore keep the complex model

#Now lets remove the treatment parameter
silvering_model_6 <- glmmTMB(data = RGB_and_cluster_data, RGB_silvering_area ~ length + parr_mark_area + (1|family) + (1|stream), family = beta_family())

# Now lets compare the models
lrtest(silvering_model_4, silvering_model_6)
# There is a significant difference and we therefore keep the complex model
# Our final model is model 4

## Orange spot area - we'll include parr_mark_area and RGB_silvering_area and their interaction as they too may be predictors of silvering
orange_spot_model_1 <- glmmTMB(data = RGB_and_cluster_data, orange_colour_area ~ treatment * length + parr_mark_area * RGB_silvering_area + (1|family) + (1|stream), family = beta_family())

# Now lets remove the parr_mark_area * RGB_silvering_area interaction
orange_spot_model_2 <- glmmTMB(data = RGB_and_cluster_data, orange_colour_area ~ treatment * length + parr_mark_area + RGB_silvering_area + (1|family) + (1|stream), family = beta_family())

# Compare the 2 models
lrtest(orange_spot_model_1, orange_spot_model_2)
# There is a significant interaction and we should therefore keep model 1

# Remove the treatment * length interaction
orange_spot_model_3 <- glmmTMB(data = RGB_and_cluster_data, orange_colour_area ~ treatment + length + parr_mark_area * RGB_silvering_area + (1|family) + (1|stream), family = beta_family())

# Compare the 2 models
lrtest(orange_spot_model_1, orange_spot_model_3)
# There is no significant difference and we should therefore take model 3

# Remove the length parameter
orange_spot_model_4 <- glmmTMB(data = RGB_and_cluster_data, orange_colour_area ~ treatment + parr_mark_area * RGB_silvering_area + (1|family) + (1|stream), family = beta_family())

# Compare the 2 models
lrtest(orange_spot_model_3, orange_spot_model_4)
# This is a significant parameter and we should therefore keep model 3

# Remove the treatment parameter
orange_spot_model_5 <- glmmTMB(data = RGB_and_cluster_data, orange_colour_area ~ length + parr_mark_area * RGB_silvering_area + (1|family) + (1|stream), family = beta_family())

# Compare the 2 models
lrtest(orange_spot_model_3, orange_spot_model_5)
# This is an insignificant parameter and we could take model 5. However, because treatment is integral to the aims of the study, we will retain model 3.
# Model 3 is therefore the final model

## parr mark area - we'll include orange_colour_area and RGB_silvering_area as they too may be predictors of parr mark area
parr_mark_area_model_1 <- glmmTMB(data = RGB_and_cluster_data, parr_mark_area ~ treatment * length + RGB_silvering_area + orange_colour_area + (1|family) + (1|stream), family = beta_family())

# Lets remove the orange_colour_area parameter
parr_mark_area_model_2 <- glmmTMB(data = RGB_and_cluster_data, parr_mark_area ~ treatment * length + RGB_silvering_area + (1|family) + (1|stream), family = beta_family())

# Compare the 2 models
lrtest(parr_mark_area_model_1, parr_mark_area_model_2)
# There is a significant difference so we therefore keep the complex model

# Now lets remove the RGB_silvering_area parameter
parr_mark_area_model_3 <- glmmTMB(data = RGB_and_cluster_data, parr_mark_area ~ treatment * length + orange_colour_area + (1|family) + (1|stream), family = beta_family())

# Compare the 2 models
lrtest(parr_mark_area_model_1, parr_mark_area_model_3)
# There is a significant difference so we therefore keep the complex model

# Now we remove the treatment * length interaction 
parr_mark_area_model_4 <- glmmTMB(data = RGB_and_cluster_data, parr_mark_area ~ treatment + length + RGB_silvering_area + orange_colour_area + (1|family) + (1|stream), family = beta_family())

# Compare the 2 models
lrtest(parr_mark_area_model_1, parr_mark_area_model_4)
# We can take the simplified model

# Now lets remove the length parameter
parr_mark_area_model_5 <- glmmTMB(data = RGB_and_cluster_data, parr_mark_area ~ treatment + RGB_silvering_area + orange_colour_area + (1|family) + (1|stream), family = beta_family())

# Compare the 2 models
lrtest(parr_mark_area_model_4, parr_mark_area_model_5)
# We can take the simplified model

# Now re can remove the treatment parameter
parr_mark_area_model_6 <- glmmTMB(data = RGB_and_cluster_data, parr_mark_area ~ RGB_silvering_area + orange_colour_area + (1|family) + (1|stream), family = beta_family())

# Compare the 2 models
lrtest(parr_mark_area_model_5, parr_mark_area_model_6)
# This is an insignificant parameter and we could take model 6. However, because treatment is integral to the aims of the study, we will retain model 5.
# Model 5 is therefore the final model

#Review the models
summary(silvering_model_4)
summary(orange_spot_model_3)
summary(parr_mark_area_model_5)

# Create a table combining the model summaries
silver_summary <- tidy(silvering_model_4)
orange_spot_summary <- tidy(orange_spot_model_3)
parr_mark_summary <- tidy(parr_mark_area_model_5)

# Create a model column with the model names
silver_summary$model <- "Silver area"
orange_spot_summary$model <- "Orange area"
parr_mark_summary$model <- "Parr area"

# Merge all the measures for area
summary_table <- rbind(silver_summary, orange_spot_summary, parr_mark_summary)[, c(1, 3,  4, 5, 8, 9)]

# Convert values from a logit scale
#summary_table$estimate <- inv.logit(summary_table$estimate)

# Save to table to format in excel
write.csv(summary_table, "model_summary_table.csv")

# check fit of the above chosen models
simulateResiduals(fittedModel = silvering_model_4, plot = T)
simulateResiduals(fittedModel = orange_spot_model_3, plot = T)
simulateResiduals(fittedModel = parr_mark_area_model_5, plot = T)

# Calculate marginal effects, convert it into a dataframe, and extract the collumns we need
silver_treatment_table <- data.frame(emmeans(silvering_model_4, c("treatment","length", "parr_mark_area")))
orange_treatment_table <- data.frame(emmeans(orange_spot_model_3, c("treatment","length", "parr_mark_area", "RGB_silvering_area")))
parr_mark_treatment_table <- data.frame(emmeans(parr_mark_area_model_5, c("treatment", "orange_colour_area", "RGB_silvering_area")))

# Add a model name label
silver_treatment_table$model <- "Silver area"
orange_treatment_table$model <- "Orange area"
parr_mark_treatment_table$model <- "Parr mark area"

# Extract only the collumns we need and merge into a summary table
treatment_emm_table <- rbind(silver_treatment_table[, c(1, 4, 7, 8, 9)],
                             orange_treatment_table[, c(1, 5, 8, 9, 10)],
                             parr_mark_treatment_table[, c(1, 4, 7, 8, 9)])

# Convert emmean and confidence intervals into proportion by taking the inverse logit values
treatment_emm_table$emmean <- plogis(treatment_emm_table$emmean)
treatment_emm_table$lower.CL <- plogis(treatment_emm_table$lower.CL)
treatment_emm_table$upper.CL <- plogis(treatment_emm_table$upper.CL)

# Save to table to format in excel
write.csv(treatment_emm_table, "treatment_emm_table.csv")


################################################################################################################################################

## Create plots for silvering
# Create a plot showing the spread of values in treatment group
silvering_treatment_SE <- summarySE(RGB_and_cluster_data, measurevar = "RGB_silvering_area", groupvars = c("treatment"))

ggplot() +
  geom_violin(data = RGB_and_cluster_data, aes(x = treatment, y = RGB_silvering_area), width = 1) +
  geom_point(data = silvering_treatment_SE, aes(x = treatment, y = RGB_silvering_area)) +
  geom_errorbar(data = silvering_treatment_SE, aes(x = treatment, y = RGB_silvering_area, ymin = RGB_silvering_area - sd, ymax = RGB_silvering_area + sd), width=.1) +
  theme(panel.grid.major = element_blank(), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportional area of silver colouration") +
  xlab("Treatment group")

# Create a plot with silvering with parr mark area
silvering_length_emm <- ggemmeans(silvering_model_4, terms = c("length"))

# Create plot with silvering with length
ggplot() + 
  geom_line(data = silvering_length_emm, aes(x = x, y = predicted)) +
  geom_ribbon(data = silvering_length_emm, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), alpha = .1) +
  geom_point(data = RGB_and_cluster_data, aes(x = length, y = RGB_silvering_area, shape = treatment)) +
  theme(panel.grid.major = element_blank(), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportional area of silver colouration") +
  xlab("Individual length (mm)") +
  scale_shape_manual(values=c(16, 4), name = "Treatment group")

# Create a plot with silvering with parr mark area
silvering_parr_mark_emm <- ggemmeans(silvering_model_4, terms = c("parr_mark_area"))

ggplot() + 
  geom_line(data = silvering_parr_mark_emm, aes(x = x, y = predicted)) +
  geom_ribbon(data = silvering_parr_mark_emm, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), alpha = .1) +
  geom_point(data = RGB_and_cluster_data, aes(x = parr_mark_area, y = RGB_silvering_area, shape = treatment)) +
  theme(panel.grid.major = element_blank(), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportional area of silver colouration") +
  xlab("Proportional area of parr mark colouration ") +
  scale_shape_manual(values=c(16, 4), name = "Treatment group")

######################################################################################################################################################################################################

## Create plots for orange colouration
# Create a plot showing the spread of values in treatment group
orange_spot_treatment_SE <- summarySE(RGB_and_cluster_data, measurevar = "orange_colour_area", groupvars = c("treatment"))

ggplot() +
  geom_violin(data = RGB_and_cluster_data, aes(x = treatment, y = orange_colour_area), width = 1) +
  geom_point(data = orange_spot_treatment_SE, aes(x = treatment, y = orange_colour_area)) +
  geom_errorbar(data = orange_spot_treatment_SE, aes(x = treatment, y = orange_colour_area, ymin = orange_colour_area - sd, ymax = orange_colour_area + sd), width=.1) +
  theme(panel.grid.major = element_blank(), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportional area of orange colouration") +
  xlab("Treatment group")

# create marginal effect models for length
orange_spot_length_emm <- ggemmeans(orange_spot_model_3, terms = c("length"))

# Create a plot with orange colouration against parr mark area
orange_spot_parr_mark_emm <- ggemmeans(orange_spot_model_3, terms = c("parr_mark_area"))

ggplot() + 
  geom_line(data = orange_spot_parr_mark_emm, aes(x = x, y = predicted)) +
  geom_ribbon(data = orange_spot_parr_mark_emm, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), alpha = .1) +
  geom_point(data = RGB_and_cluster_data, aes(x = parr_mark_area, y = orange_colour_area, shape = treatment)) +
  theme(panel.grid.major = element_blank(), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportional area of orange colouration") +
  xlab("Proportional area of parr mark colouration ") +
  scale_shape_manual(values=c(16, 4), name = "Treatment group")

# Create a plot with orange pagainst silvering
orange_spot_silvering_emm <- ggemmeans(orange_spot_model_3, terms = c("RGB_silvering_area"))

ggplot() + 
  geom_line(data = orange_spot_silvering_emm, aes(x = x, y = predicted)) +
  geom_ribbon(data = orange_spot_silvering_emm, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), alpha = .1) +
  geom_point(data = RGB_and_cluster_data, aes(x = RGB_silvering_area, y = orange_colour_area, shape = treatment)) +
  theme(panel.grid.major = element_blank(), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportional area of orange colouration") +
  xlab("Proportional area of silver colouration ") +
  scale_shape_manual(values=c(16, 4), name = "Treatment group")

######################################################################################################################################################################################################

## Create plots for parr mark area
# Create a plot showing the spread of values in treatment group
parr_mark_treatment_SE <- summarySE(RGB_and_cluster_data, measurevar = "parr_mark_area", groupvars = c("treatment"))

ggplot() +
  geom_violin(data = RGB_and_cluster_data, aes(x = treatment, y = parr_mark_area), width = 1) +
  geom_point(data = parr_mark_treatment_SE, aes(x = treatment, y = parr_mark_area)) +
  geom_errorbar(data = parr_mark_treatment_SE, aes(x = treatment, y = parr_mark_area, ymin = parr_mark_area - sd, ymax = parr_mark_area + sd), width=.1) +
  theme(panel.grid.major = element_blank(), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportional parr mark area") +
  xlab("Treatment group")

# Create marginal effect model against silvering area
parr_mark_sivering_emm <- ggemmeans(parr_mark_area_model_5, terms = c("RGB_silvering_area"))

ggplot() + 
  geom_line(data = parr_mark_sivering_emm, aes(x = x, y = predicted)) +
  geom_ribbon(data = parr_mark_sivering_emm, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), alpha = .1) +
  geom_point(data = RGB_and_cluster_data, aes(x = RGB_silvering_area, y = parr_mark_area, shape = treatment)) +
  theme(panel.grid.major = element_blank(), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportional area of parr marks") +
  xlab("Proportional area of silvering") +
  scale_shape_manual(values=c(16, 4), name = "Treatment group")

# Create marginal effect model against orange spot area
parr_mark_orange_spot_emm <- ggemmeans(parr_mark_area_model_5, terms = c("orange_colour_area"))

ggplot() + 
  geom_line(data = parr_mark_orange_spot_emm, aes(x = x, y = predicted)) +
  geom_ribbon(data = parr_mark_orange_spot_emm, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), alpha = .1) +
  geom_point(data = RGB_and_cluster_data, aes(x = orange_colour_area, y = parr_mark_area, shape = treatment)) +
  theme(panel.grid.major = element_blank(), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportional area of parr marks") +
  xlab("Proportional area of orange colour")  +
  scale_shape_manual(values=c(16, 4), name = "Treatment group")

######################################################################################################################################################################################################
# Create a mean and standard deviation marginal effects
silvering_treatment_emm <- ggemmeans(silvering_model_4, terms = c("treatment"))
orange_spot_treatment_emm <- ggemmeans(orange_spot_model_3, terms = c("treatment"))
parr_mark_treatment_emm <- ggemmeans(parr_mark_area_model_5, terms = c("treatment"))

## Create marginal effects plots
# Silvering
ggplot() + 
  geom_point(data = silvering_treatment_emm, aes(x = x , y = predicted)) +
  geom_errorbar(data = silvering_treatment_emm, aes(x = x , y = predicted, ymin = conf.low, ymax = conf.high), width= 0.1) +
  theme(panel.grid.major = element_blank(), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportional body area of silver") +
  xlab("Treatment group")

# Orange spots
ggplot() + 
  geom_point(data = orange_spot_treatment_emm, aes(x = x , y = predicted)) +
  geom_errorbar(data = orange_spot_treatment_emm, aes(x = x , y = predicted, ymin = conf.low, ymax = conf.high), width= 0.1) +
  theme(panel.grid.major = element_blank(), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportional body area of orange") +
  xlab("Treatment group")

# Parr marks
ggplot() + 
  geom_point(data = parr_mark_treatment_emm, aes(x = x , y = predicted)) +
  geom_errorbar(data = parr_mark_treatment_emm, aes(x = x , y = predicted, ymin = conf.low, ymax = conf.high), width= 0.1) +
  theme(panel.grid.major = element_blank(), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportional body area of parr marks") +
  xlab("Treatment group")

# test <- data.frame("treatment" = RGB_and_cluster_data$treatment, "silver" = RGB_and_cluster_data$RGB_silvering_area)
# 
# 
# add <- data.frame("treatment" = rep("new", 20), "silver" = runif(20, 0.02, 0.04))
# 
# test_2 <- rbind(test, add)
# 
# test_m <- glm(data = test_2, silver ~ treatment)
# 
# test_m2 <- glm(data = test_2, silver ~ 1)
# 
# lrtest(test_m, test_m2)
# 
