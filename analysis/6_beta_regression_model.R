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
library("patchwork")
library("colorblindr")

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

h <- colnames(RGB_and_cluster_data)

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

# Change the random effect values from standard deviation to variance
silver_summary[6, 5] <- as.numeric(summary(silvering_model_4)[["varcor"]][["cond"]][["stream"]])
orange_spot_summary[8, 5] <- as.numeric(summary(orange_spot_model_3)[["varcor"]][["cond"]][["stream"]])
parr_mark_summary[6, 5] <- as.numeric(summary(parr_mark_area_model_5)[["varcor"]][["cond"]][["stream"]])

silver_summary[5, 5] <- as.numeric(summary(silvering_model_4)[["varcor"]][["cond"]][["family"]])
orange_spot_summary[7, 5] <- as.numeric(summary(orange_spot_model_3)[["varcor"]][["cond"]][["family"]])
parr_mark_summary[5, 5] <- as.numeric(summary(parr_mark_area_model_5)[["varcor"]][["cond"]][["family"]])

# Create a model column with the model names
silver_summary$model <- "Silver area"
orange_spot_summary$model <- "Orange area"
parr_mark_summary$model <- "Parr area"

# Merge all the measures for area
summary_table <- rbind(silver_summary, orange_spot_summary, parr_mark_summary)[, c(1, 3,  4, 5, 8, 9)]

# Change the values type
summary_table$term[c(5, 6, 13, 14, 19, 20)] <- "Variance"

# Save to table to format in excel
write.csv(summary_table, "model_summary_table.csv")

# check fit of the above chosen models
simulateResiduals(fittedModel = silvering_model_4, plot = T)
simulateResiduals(fittedModel = orange_spot_model_3, plot = T)
simulateResiduals(fittedModel = parr_mark_area_model_5, plot = T)

# Save to table to format in excel
write.csv(treatment_emm_table, "treatment_emm_table.csv")

#Obtain affect of intercept (to quote in paper)
# silver
plogis(as.numeric(summary_table[1, 4]) + as.numeric(summary_table[2, 4])) - plogis(as.numeric(summary_table[1, 4]))

# orange
plogis(as.numeric(summary_table[7, 4]) + as.numeric(summary_table[8, 4])) - plogis(as.numeric(summary_table[7, 4]))

# parr mark area
plogis(as.numeric(summary_table[17, 4]) + as.numeric(summary_table[16, 4])) - plogis(as.numeric(summary_table[17, 4]))

## Obtain variance of random effects (to quote in paper)
# silver
# Family
plogis(as.numeric(summary_table[1, 4]) + as.numeric(summary_table[5, 4])) - plogis(as.numeric(summary_table[1, 4]))
# stream
plogis(as.numeric(summary_table[1, 4]) + as.numeric(summary_table[6, 4])) - plogis(as.numeric(summary_table[1, 4]))

# orange
# Family
plogis(as.numeric(summary_table[7, 4]) + as.numeric(summary_table[13, 4])) - plogis(as.numeric(summary_table[7, 4]))
# stream
plogis(as.numeric(summary_table[7, 4]) + as.numeric(summary_table[14, 4])) - plogis(as.numeric(summary_table[7, 4]))

# parr mark area
# Family
plogis(as.numeric(summary_table[17, 4]) + as.numeric(summary_table[19, 4])) - plogis(as.numeric(summary_table[17, 4]))
# stream
plogis(as.numeric(summary_table[17, 4]) + as.numeric(summary_table[20, 4])) - plogis(as.numeric(summary_table[17, 4]))

# calculate range of values in orange colour area
RGB_and_cluster_data %>%
  group_by(treatment) %>%
  summarise(min = min(orange_colour_area), max = max(orange_colour_area))

# calculate range of values in parr mark colour area
RGB_and_cluster_data %>%
  group_by(treatment) %>%
  summarise(min = min(parr_mark_area), max = max(parr_mark_area))

#######################################################################################################################################################################################
## Create plots for silvering
# Create a plot showing the spread of values in treatment group
silveringSE <- summarySE(RGB_and_cluster_data, measurevar = "RGB_silvering_area", groupvars = c("treatment"))

# Create plots showing the relationships between silver area and treatment
silver_plot <- ggplot() +
  geom_violin(data = RGB_and_cluster_data, aes(x = treatment, y = RGB_silvering_area, colour = treatment), width = 1) +
  geom_point(data = silveringSE, size = 3, aes(x = treatment, y = RGB_silvering_area, colour = treatment)) +
  geom_errorbar(data = silveringSE, size = 1.25, aes(colour = treatment, x = treatment, y = RGB_silvering_area, ymin = RGB_silvering_area - sd, ymax = RGB_silvering_area + sd), width=.1) +
  theme(legend.title = element_text(size = 15), legend.text = element_text(size = 15),panel.grid.major = element_blank(), axis.title.x = element_text(size = 17), axis.text=element_text(size=17), axis.title.y = element_text(size = 17, margin = margin(t = 0, r = 20, b = 0, l = 0)),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportional area of silver colouration") +
  xlab("Treatment group") +
  scale_color_OkabeIto() +
  guides(colour=guide_legend("Treatment"),
         fill=guide_legend("Treatment"))


# to show differences in treatment interactions, we will include an interaction in a model
silvering_model_plot <- glmmTMB(data = RGB_and_cluster_data, RGB_silvering_area ~ treatment * length + parr_mark_area + (1|family) + (1|stream), family = beta_family())

# Create a plot with silvering with parr mark area
silvering_parr_mark_emm <- ggemmeans(silvering_model_4, terms = c("parr_mark_area"))

silver_parr_mark_plot <- ggplot() + 
  geom_line(data = silvering_parr_mark_emm, colour = "black", aes(x = x, y = predicted)) +
  geom_ribbon(data = silvering_parr_mark_emm, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), alpha = .1) +
  geom_point(data = RGB_and_cluster_data, size = 3, aes(x = parr_mark_area, y = RGB_silvering_area, colour = treatment)) +
  theme(legend.title = element_text(size = 15), legend.text = element_text(size = 15),panel.grid.major = element_blank(), axis.title.x = element_text(size = 17), axis.text=element_text(size=17), axis.title.y = element_text(size = 17, margin = margin(t = 0, r = 20, b = 0, l = 0)),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportional area of silver colouration") +
  xlab("Proportional parr mark area") +
  scale_color_OkabeIto() +
  guides(colour="none",
         fill="none")


# Create an em trends showing the affect of length and treatment
silvering_length_emm <- ggemmeans(silvering_model_4, terms = c("length", "treatment"))

# Create plot with silvering with length
silver_length_plot <- ggplot() + 
  geom_line(data = silvering_length_emm, aes(x = x, y = predicted, colour = group)) +
  geom_ribbon(data = silvering_length_emm, aes(fill = group, x = x, y = predicted, ymin = conf.low, ymax = conf.high), alpha = .1) +
  geom_point(data = RGB_and_cluster_data, size = 3, aes(x = length, y = RGB_silvering_area, colour = treatment)) +
  theme(legend.title = element_text(size = 15), legend.text = element_text(size = 15),panel.grid.major = element_blank(), axis.title.x = element_text(size = 17), axis.text=element_text(size=17), axis.title.y = element_text(size = 17, margin = margin(t = 0, r = 20, b = 0, l = 0)),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportional area of silver colouration") +
  xlab("Individual length (mm)") +
  scale_color_OkabeIto() +
  guides(colour=guide_legend("Treatment"),
         fill=guide_legend("Treatment"))

######################################################################################################################################################################################################
## Create plots for orange colouration
# Create a plot showing the spread of values in treatment group
orange_spotSE <- summarySE(RGB_and_cluster_data, measurevar = "orange_colour_area", groupvars = c("treatment"))

orange_spot_plot <- ggplot() +
  geom_violin(data = RGB_and_cluster_data, aes(x = treatment, y = orange_colour_area, colour = treatment), width = 1) +
  geom_point(data = orange_spotSE, size = 3, aes(x = treatment, y = orange_colour_area, colour = treatment)) +
  geom_errorbar(data = orange_spotSE, size = 1.25, aes(colour = treatment, x = treatment, y = orange_colour_area, ymin = orange_colour_area - sd, ymax = orange_colour_area + sd), width=.1) +
  theme(legend.title = element_text(size = 15), legend.text = element_text(size = 15),panel.grid.major = element_blank(), axis.title.x = element_text(size = 17), axis.text=element_text(size=17), axis.title.y = element_text(size = 17, margin = margin(t = 0, r = 20, b = 0, l = 0)),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportional area of orange colouration") +
  xlab("Treatment group") +
  guides(colour=guide_legend("Treatment"),
         fill=guide_legend("Treatment")) +
  scale_color_OkabeIto()


# Create a plot with orange colouration against parr mark area
orange_spot_parr_mark_emm <- ggemmeans(orange_spot_model_3, terms = c("parr_mark_area"))

orange_spot_parr_mark_plot <- ggplot() + 
  geom_line(data = orange_spot_parr_mark_emm, colour = "black", aes(x = x, y = predicted)) +
  geom_ribbon(data = orange_spot_parr_mark_emm, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), alpha = .1) +
  geom_point(data = RGB_and_cluster_data, size = 3, aes(x = parr_mark_area, y = orange_colour_area, colour = treatment)) +
  theme(legend.title = element_text(size = 15), legend.text = element_text(size = 15),panel.grid.major = element_blank(), axis.title.x = element_text(size = 17), axis.text=element_text(size=17), axis.title.y = element_text(size = 17, margin = margin(t = 0, r = 20, b = 0, l = 0)),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportional area of orange colouration") +
  xlab("Proportional parr mark area") +
  guides(colour="none",
         fill="none") +
  scale_color_OkabeIto()


# Create a plot with orange pagainst silvering
orange_spot_silvering_emm <- ggemmeans(orange_spot_model_3, terms = c("RGB_silvering_area"))

orange_spot_silver_plot <- ggplot() + 
  geom_line(data = orange_spot_silvering_emm, aes(x = x, y = predicted)) +
  geom_ribbon(data = orange_spot_silvering_emm, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), alpha = .1) +
  geom_point(data = RGB_and_cluster_data, size = 3, aes(x = RGB_silvering_area, y = orange_colour_area, colour = treatment)) +
  theme(legend.title = element_text(size = 15), legend.text = element_text(size = 15),panel.grid.major = element_blank(), axis.title.x = element_text(size = 17), axis.text=element_text(size=17), axis.title.y = element_text(size = 17, margin = margin(t = 0, r = 20, b = 0, l = 0)),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportional area of orange colouration") +
  xlab("Proportional area of silver colouration ") +
  scale_color_OkabeIto() +
  guides(colour="none",
         fill="none")


# create marginal effect models for length
orange_spot_model_plot <- glmmTMB(data = RGB_and_cluster_data, orange_colour_area ~ treatment * length + parr_mark_area * RGB_silvering_area + (1|family) + (1|stream), family = beta_family())
orange_spot_length_emm <- ggemmeans(orange_spot_model_plot, terms = c("length", "treatment"))

orange_spot_length_plot <- ggplot() + 
  geom_line(data = orange_spot_length_emm, aes(x = x, y = predicted, colour = group)) +
  geom_ribbon(data = orange_spot_length_emm, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, fill = group), alpha = .1) +
  geom_point(data = RGB_and_cluster_data, size = 3, aes(x = length, y = orange_colour_area, colour = treatment)) +
  theme(legend.title = element_text(size = 15), legend.text = element_text(size = 15),panel.grid.major = element_blank(), axis.title.x = element_text(size = 17), axis.text=element_text(size=17), axis.title.y = element_text(size = 17, margin = margin(t = 0, r = 20, b = 0, l = 0)),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportional area of orange colouration") +
  xlab("Individual length (mm)") +
  scale_color_OkabeIto() +
  guides(colour=guide_legend("Treatment"),
         fill=guide_legend("Treatment"))

######################################################################################################################################################################################################
## Create plots for parr mark area
# Create a plot showing the spread of values in treatment group
parr_mark_SE <- summarySE(RGB_and_cluster_data, measurevar = "parr_mark_area", groupvars = c("treatment"))

parr_mark_plot <- ggplot() +
  geom_violin(data = RGB_and_cluster_data, aes(x = treatment, y = parr_mark_area, colour = treatment), width = 1) +
  geom_point(data = parr_mark_SE, size = 3, aes(x = treatment, y = parr_mark_area, colour = treatment)) +
  geom_errorbar(data = parr_mark_SE, size = 1.25, aes(colour = treatment, x = treatment, y = parr_mark_area, ymin = parr_mark_area - sd, ymax = parr_mark_area + sd), width=.1) +
  theme(legend.title = element_text(size = 15), legend.text = element_text(size = 15),panel.grid.major = element_blank(), axis.title.x = element_text(size = 17), axis.text=element_text(size=17), axis.title.y = element_text(size = 17, margin = margin(t = 0, r = 20, b = 0, l = 0)),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportional parr mark area") +
  xlab("Treatment group") +
    guides(colour=guide_legend("Treatment"),
           fill=guide_legend("Treatment")) +
  scale_color_OkabeIto()


# Create marginal effect model against silvering area
parr_mark_sivering_emm <- ggemmeans(parr_mark_area_model_5, terms = c("RGB_silvering_area"))

parr_mark_sivering_plot <- ggplot() + 
  geom_line(data = parr_mark_sivering_emm, colour = "black", aes(x = x, y = predicted)) +
  geom_ribbon(data = parr_mark_sivering_emm, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), alpha = .1) +
  geom_point(data = RGB_and_cluster_data, size = 3, aes(x = RGB_silvering_area, y = parr_mark_area, colour = treatment)) +
  theme(legend.title = element_text(size = 15), legend.text = element_text(size = 15),panel.grid.major = element_blank(), axis.title.x = element_text(size = 17), axis.text=element_text(size=17), axis.title.y = element_text(size = 17, margin = margin(t = 0, r = 20, b = 0, l = 0)),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportional parr mark area") +
  xlab("Proportional area of silver colouration") +
  scale_color_OkabeIto() +
  guides(colour=guide_legend("Treatment"),
         fill=guide_legend("Treatment"))


# Create marginal effect model against orange spot area
parr_mark_orange_spot_emm <- ggemmeans(parr_mark_area_model_5, terms = c("orange_colour_area"))

parr_mark_orange_spot_plot <- ggplot() + 
  geom_line(data = parr_mark_orange_spot_emm, colour = "black", aes(x = x, y = predicted)) +
  geom_ribbon(data = parr_mark_orange_spot_emm, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), alpha = .1) +
  geom_point(data = RGB_and_cluster_data, size = 3, aes(x = orange_colour_area, y = parr_mark_area, colour = treatment)) +
  theme(legend.title = element_text(size = 15), legend.text = element_text(size = 15), panel.grid.major = element_blank(), axis.title.x = element_text(size = 17), axis.text=element_text(size=17), axis.title.y = element_text(size = 17, margin = margin(t = 0, r = 20, b = 0, l = 0)),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportional parr mark area") +
  xlab("Proportional area of orange colouration") +
  guides(colour=guide_legend("Treatment"),
         fill=guide_legend("Treatment")) +
  scale_color_OkabeIto()

##########################################################################################################################################
# Create a mean and standard deviation marginal effects
silvering_treatment_emm <- ggemmeans(silvering_model_4, terms = c("treatment"))
orange_spot_treatment_emm <- ggemmeans(orange_spot_model_3, terms = c("treatment"))
parr_mark_treatment_emm <- ggemmeans(parr_mark_area_model_5, terms = c("treatment"))

## Create marginal effects plots
# Silvering
silvering_treatment_plot <- ggplot() + 
  geom_point(data = silvering_treatment_emm, size = 3, aes(x = x , y = predicted, colour = c("Control","Treatment"))) +
  geom_errorbar(data = silvering_treatment_emm, size = 1.25, aes(colour = x, x = x, y = predicted, ymin = conf.low, ymax = conf.high), width=.1) +
  theme(legend.title = element_text(size = 15), legend.text = element_text(size = 15),panel.grid.major = element_blank(), axis.title.x = element_text(size = 17), axis.text=element_text(size=17), axis.title.y = element_text(size = 17, margin = margin(t = 0, r = 20, b = 0, l = 0)),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportional area of silver colouration") +
  xlab("Treatment group") +
    guides(colour="none",
           fill="none") +
  scale_color_OkabeIto()

# Orange spots
orange_spot_treatment_plot <- ggplot() + 
  geom_point(data = orange_spot_treatment_emm, size = 3, aes(x = x , y = predicted, colour = c("Control","Treatment"))) +
  geom_errorbar(data = orange_spot_treatment_emm, size = 1.25, aes(colour = x, x = x, y = predicted, ymin = conf.low, ymax = conf.high), width=.1) +
  theme(legend.title = element_text(size = 15), legend.text = element_text(size = 15),panel.grid.major = element_blank(), axis.title.x = element_text(size = 17), axis.text=element_text(size=17), axis.title.y = element_text(size = 17, margin = margin(t = 0, r = 20, b = 0, l = 0)),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportional area of orange colouration") +
  xlab("Treatment group") +
    guides(colour="none",
           fill="none") +
  scale_color_OkabeIto()

# Parr marks
parr_mark_treatment_plot <- ggplot() + 
  geom_point(data = parr_mark_treatment_emm, size = 3, aes(x = x , y = predicted, colour = c("Control","Treatment"))) +
  geom_errorbar(data = parr_mark_treatment_emm, size = 1.25, aes(colour = x, x = x, y = predicted, ymin = conf.low, ymax = conf.high), width=.1) +
  theme(legend.title = element_text(size = 15), legend.text = element_text(size = 15), panel.grid.major = element_blank(), axis.title.x = element_text(size = 17), axis.text=element_text(size=17), axis.title.y = element_text(size = 17, margin = margin(t = 0, r = 20, b = 0, l = 0)),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportional parr mark area") +
  xlab("Treatment group") +
    guides(colour="none",
           fill="none") +
  scale_color_OkabeIto()

# Create plots to use in paper
# Create plots for paper with marginal effects
(silvering_treatment_plot /
  (silver_parr_mark_plot | silver_length_plot)) + plot_layout(guides = "collect") & theme(legend.title = element_text(size = 15), legend.text = element_text(size = 15),legend.position = 'bottom')

(orange_spot_treatment_plot /
  (orange_spot_parr_mark_plot | orange_spot_silver_plot | orange_spot_length_plot)) + plot_layout(guides = "collect") & theme(legend.title = element_text(size = 15), legend.text = element_text(size = 15),legend.position = 'bottom')

(parr_mark_treatment_plot /
  (parr_mark_sivering_plot | parr_mark_orange_spot_plot))  + plot_layout(guides = "collect") & theme(legend.title = element_text(size = 15), legend.text = element_text(size = 15),legend.position = 'bottom')

# create plots that visualise spread of data
(silver_plot + orange_spot_plot + parr_mark_plot) + plot_layout(guides = "collect") & theme(legend.title = element_text(size = 15), legend.text = element_text(size = 15),legend.position = 'bottom')

######################################################################################################################################################################################################
silvering_treatment_SE <- summarySE(RGB_and_cluster_data, measurevar = "RGB_silvering_area", groupvars = c("treatment"))
orange_spot_treatment_SE <- summarySE(RGB_and_cluster_data, measurevar = "orange_colour_area", groupvars = c("treatment"))
parr_mark_treatment_SE <- summarySE(RGB_and_cluster_data, measurevar = "parr_mark_area", groupvars = c("treatment"))

## Here we'll create a table with the mean and standard error bars
silvering_treatment_SE$Model <- "Silver area"
orange_spot_treatment_SE$Model <- "Orange area"
parr_mark_treatment_SE$Model <- "Parr mark area"

colnames(silvering_treatment_SE)[3] <- "Mean proportional area"
colnames(orange_spot_treatment_SE)[3] <- "Mean proportional area"
colnames(parr_mark_treatment_SE)[3] <- "Mean proportional area"

mean_sd_table <- rbind(silvering_treatment_SE, orange_spot_treatment_SE, parr_mark_treatment_SE)[, c(1, 3, 4, 7)]

colnames(mean_sd_table)[3] <- "Standard deviation"
colnames(mean_sd_table)[1] <- "Treatment"

write.csv(mean_sd_table, "mean_sd_table.csv")

# Create a table with estimate marginal effects of treatment with upper and lower confidence intervals
silvering_treatment_emm <- ggemmeans(silvering_model_4, terms = c("treatment"))
orange_spot_treatment_emm <- ggemmeans(orange_spot_model_3, terms = c("treatment"))
parr_mark_treatment_emm <- ggemmeans(parr_mark_area_model_5, terms = c("treatment"))

silvering_treatment_emm$model <- "Silver area"
orange_spot_treatment_emm$model <- "Orange area"
parr_mark_treatment_emm$model <- "Parr mark area"

# Merge the dataframes together and remove columns we do not need
treatment_emm_table <- rbind(silvering_treatment_emm, orange_spot_treatment_emm, parr_mark_treatment_emm)[, -c(3, 6)]

# Change the column names
colnames(treatment_emm_table) <- c("Treatment", "Model estimate value", "Lower 95% confidence interval", "Upper 95% confidence interval", "Model")

# Save dataframe as a csv
write.csv(treatment_emm_table, "treatment_emm_table.csv")

######################################################################################################################################################
# Create a trends summary table with the effect of continuous variables when other variables are at their means
emtrends(orange_spot_model_3, "parr_mark_area", var = "RGB_silvering_area")

emtrends(orange_spot_model_3, "parr_mark_area",  var = "RGB_silvering_area", regrid = "response")

emtrends(orange_spot_model_3, c("RGB_silvering_area", "parr_mark_area"), var = "RGB_silvering_area", regrid = "response", at = list(treatment = c("Treatment", "Control")))

summary(silvering_model_4)
summary(orange_spot_model_3)
summary(parr_mark_area_model_5)

# For the silver model
silver_length_trend <- data.frame(emtrends(silvering_model_4, "length", var = "length", regrid = "response"))[, c(2, 5, 6)]
silver_parr_mark_trend <- data.frame(emtrends(silvering_model_4, "parr_mark_area", var = "parr_mark_area", regrid = "response"))[, c(2, 5, 6)]

# For the orange model
orange_length_trend <- data.frame(emtrends(orange_spot_model_3, "length", var = "length", regrid = "response"))[, c(2, 5, 6)]
orange_parr_mark_trend <- data.frame(emtrends(orange_spot_model_3, "RGB_silvering_area", var = "parr_mark_area", regrid = "response"))[, c(2, 5, 6)]
orange_silver_trend <- data.frame(emtrends(orange_spot_model_3, "parr_mark_area", var = "RGB_silvering_area", regrid = "response"))[, c(2, 5, 6)]

# For the parr mark model
parr_mark_silver_trend <- data.frame(emtrends(parr_mark_area_model_5, "RGB_silvering_area", var = "RGB_silvering_area", regrid = "response"))[, c(2, 5, 6)]
parr_mark_orange_trend <- data.frame(emtrends(parr_mark_area_model_5, "orange_colour_area", var = "orange_colour_area", regrid = "response"))[, c(2, 5, 6)]

# Change the column name to the variable trend
colnames(silver_length_trend)[1] <- "trend"
colnames(silver_parr_mark_trend)[1] <- "trend"
colnames(orange_length_trend)[1] <- "trend"
colnames(orange_parr_mark_trend)[1] <- "trend"
colnames(orange_silver_trend)[1] <- "trend"
colnames(parr_mark_silver_trend) [1] <- "trend"
colnames(parr_mark_orange_trend)[1] <- "trend"

# Change the column names for the trend
colnames(silver_length_trend)[1] <- "trend"
colnames(silver_parr_mark_trend)[1] <- "trend"
colnames(orange_length_trend)[1] <- "trend"
colnames(orange_parr_mark_trend)[1] <- "trend"
colnames(orange_silver_trend)[1] <- "trend"
colnames(parr_mark_silver_trend) [1] <- "trend"
colnames(parr_mark_orange_trend)[1] <- "trend"

# merge the dataframes
trends_table <- rbind(silver_length_trend, silver_parr_mark_trend, orange_length_trend, orange_parr_mark_trend, orange_silver_trend, parr_mark_silver_trend, parr_mark_orange_trend)

# Create a vector of area recordings
model_names <- c(rep("Silver area", 2), rep("Orange area", 3), rep("Parr mark area", 2))
model_params <- c("Length", "Parr mark area", "Length", "Parr mark area", "Silver area", "Silver area", "Orange area")

# Create a models table
trends_table$Model <- model_names
trends_table$model_params <- model_params

# Save the Models table
write.csv(trends_table, "trends_table.csv")

####################################################################################################################











