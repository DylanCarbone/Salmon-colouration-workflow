
## Create plots for silvering
# Create a plot showing the spread of values in treatment group
silvering_treatment_SE <- summarySE(RGB_and_cluster_data, measurevar = "RGB_silvering_area", groupvars = c("treatment"))

#
silvering_model_length_plot <- glmmTMB(data = RGB_and_cluster_data, RGB_silvering_area ~ treatment * length + parr_mark_area + (1|family) + (1|stream), family = beta_family())

silver_treatment_plot <- ggplot() +
  geom_violin(data = RGB_and_cluster_data, aes(x = treatment, y = RGB_silvering_area, colour = treatment), width = 1) +
  geom_point(data = silvering_treatment_SE, aes(x = treatment, y = RGB_silvering_area, colour = treatment)) +
  geom_errorbar(data = silvering_treatment_SE, aes(x = treatment, y = RGB_silvering_area, ymin = RGB_silvering_area - sd,
                                                   ymax = RGB_silvering_area + sd, colour = treatment), width=.1) +
  theme(panel.grid.major = element_blank(), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportional area of silver colouration") +
  xlab("Treatment group")

# Create a plot with silvering with parr mark area
silvering_length_emm <- ggemmeans(silvering_model_4, terms = c("length"))

# Create plot with silvering with length
silver_length_plot <- ggplot() + 
  geom_line(data = silvering_length_emm, aes(x = x, y = predicted)) +
  geom_ribbon(data = silvering_length_emm, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), alpha = .1) +
  geom_point(data = RGB_and_cluster_data, aes(x = length, y = RGB_silvering_area, colour = treatment)) +
  theme(panel.grid.major = element_blank(), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none") +
  ylab("Proportional area of silver colouration") +
  xlab("Individual length (mm)") +
  scale_shape_manual(values=c(16, 4))

# Create a plot with silvering with parr mark area
silvering_parr_mark_emm <- ggemmeans(silvering_model_4, terms = c("parr_mark_area"))

silver_parr_mark_plot <- ggplot() + 
  geom_line(data = silvering_parr_mark_emm, aes(x = x, y = predicted)) +
  geom_ribbon(data = silvering_parr_mark_emm, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), alpha = .1) +
  geom_point(data = RGB_and_cluster_data, aes(x = parr_mark_area, y = RGB_silvering_area, shape = treatment)) +
  theme(panel.grid.major = element_blank(), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportional area of silver colouration") +
  xlab("Proportional area of parr mark colouration ") +
  scale_shape_manual(values=c(16, 4), name = "Treatment group")

# Combine plots
silver_length_plot + silver_parr_mark_plot

######################################################################################################################################################################################################

## Create plots for orange colouration
# Create a plot showing the spread of values in treatment group
orange_spot_treatment_SE <- summarySE(RGB_and_cluster_data, measurevar = "orange_colour_area", groupvars = c("treatment"))

orange_spot_treatment_plot <- ggplot() +
  geom_violin(data = RGB_and_cluster_data, aes(x = treatment, y = orange_colour_area), width = 1) +
  geom_point(data = orange_spot_treatment_SE, aes(x = treatment, y = orange_colour_area)) +
  geom_errorbar(data = orange_spot_treatment_SE, aes(x = treatment, y = orange_colour_area, ymin = orange_colour_area - sd, ymax = orange_colour_area + sd), width=.1) +
  theme(panel.grid.major = element_blank(), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportional area of orange colouration") +
  xlab("Treatment group")

# Create a plot with orange colouration against parr mark area
orange_spot_parr_mark_emm <- ggemmeans(orange_spot_model_3, terms = c("parr_mark_area"))

orange_spot_parr_mark_plot <- ggplot() + 
  geom_line(data = orange_spot_parr_mark_emm, aes(x = x, y = predicted)) +
  geom_ribbon(data = orange_spot_parr_mark_emm, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), alpha = .1) +
  geom_point(data = RGB_and_cluster_data, aes(x = parr_mark_area, y = orange_colour_area, shape = treatment)) +
  theme(panel.grid.major = element_blank(), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none") +
  ylab("Proportional area of orange colouration") +
  xlab("Proportional area of parr mark colouration ") +
  scale_shape_manual(values=c(16, 4))

# Create a plot with orange pagainst silvering
orange_spot_silvering_emm <- ggemmeans(orange_spot_model_3, terms = c("RGB_silvering_area"))

orange_spot_silver_plot <- ggplot() + 
  geom_line(data = orange_spot_silvering_emm, aes(x = x, y = predicted)) +
  geom_ribbon(data = orange_spot_silvering_emm, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), alpha = .1) +
  geom_point(data = RGB_and_cluster_data, aes(x = RGB_silvering_area, y = orange_colour_area, shape = treatment)) +
  theme(panel.grid.major = element_blank(), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none") +
  ylab("Proportional area of orange colouration") +
  xlab("Proportional area of silver colouration ") +
  scale_shape_manual(values=c(16, 4))

# create marginal effect models for length
orange_spot_length_emm <- ggemmeans(orange_spot_model_3, terms = c("length"))

orange_spot_length_plot <- ggplot() + 
  geom_line(data = orange_spot_length_emm, aes(x = x, y = predicted)) +
  geom_ribbon(data = orange_spot_length_emm, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), alpha = .1) +
  geom_point(data = RGB_and_cluster_data, aes(x = length, y = orange_colour_area, shape = treatment)) +
  theme(panel.grid.major = element_blank(), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportional area of orange colouration") +
  xlab("Individual length (mm)") +
  scale_shape_manual(values=c(16, 4), name = "Treatment group")


# Arrange plots
orange_spot_treatment_plot /
  (orange_spot_parr_mark_plot | orange_spot_silver_plot | orange_spot_length_plot)

######################################################################################################################################################################################################

## Create plots for parr mark area
# Create a plot showing the spread of values in treatment group
parr_mark_treatment_SE <- summarySE(RGB_and_cluster_data, measurevar = "parr_mark_area", groupvars = c("treatment"))

parr_mark_treatment_plot <- ggplot() +
  geom_violin(data = RGB_and_cluster_data, aes(x = treatment, y = parr_mark_area), width = 1) +
  geom_point(data = parr_mark_treatment_SE, aes(x = treatment, y = parr_mark_area)) +
  geom_errorbar(data = parr_mark_treatment_SE, aes(x = treatment, y = parr_mark_area, ymin = parr_mark_area - sd, ymax = parr_mark_area + sd), width=.1) +
  theme(panel.grid.major = element_blank(), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportional parr mark area") +
  xlab("Treatment group")

# Create marginal effect model against silvering area
parr_mark_sivering_emm <- ggemmeans(parr_mark_area_model_5, terms = c("RGB_silvering_area"))

parr_mark_sivering_plot <- ggplot() + 
  geom_line(data = parr_mark_sivering_emm, aes(x = x, y = predicted)) +
  geom_ribbon(data = parr_mark_sivering_emm, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), alpha = .1) +
  geom_point(data = RGB_and_cluster_data, aes(x = RGB_silvering_area, y = parr_mark_area, shape = treatment)) +
  theme(panel.grid.major = element_blank(), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none") +
  ylab("Proportional area of parr marks") +
  xlab("Proportional area of silvering") +
  scale_shape_manual(values=c(16, 4))

# Create marginal effect model against orange spot area
parr_mark_orange_spot_emm <- ggemmeans(parr_mark_area_model_5, terms = c("orange_colour_area"))

parr_mark_orange_spot_plot <- ggplot() + 
  geom_line(data = parr_mark_orange_spot_emm, aes(x = x, y = predicted)) +
  geom_ribbon(data = parr_mark_orange_spot_emm, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), alpha = .1) +
  geom_point(data = RGB_and_cluster_data, aes(x = orange_colour_area, y = parr_mark_area, shape = treatment)) +
  theme(panel.grid.major = element_blank(), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportional area of parr marks") +
  xlab("Proportional area of orange colour")  +
  scale_shape_manual(values=c(16, 4), name = "Treatment group")

# Arrange plots
parr_mark_treatment_plot /
  (parr_mark_sivering_plot | parr_mark_orange_spot_plot)
