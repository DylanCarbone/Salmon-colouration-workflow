## Create plots for silvering
# Create a plot showing the spread of values in treatment group
silvering_treatment_SE <- summarySE(RGB_and_cluster_data, measurevar = "RGB_silvering_area", groupvars = c("treatment"))

# Create plots showing the relationships between silver area and treatment
ggplot() +
  geom_violin(data = RGB_and_cluster_data, aes(x = treatment, y = RGB_silvering_area, colour = treatment), width = 1) +
  geom_point(data = silvering_treatment_SE, size = 3, aes(x = treatment, y = RGB_silvering_area, colour = treatment)) +
  geom_errorbar(data = silvering_treatment_SE, aes(colour = treatment, x = treatment, y = RGB_silvering_area, ymin = RGB_silvering_area - sd, ymax = RGB_silvering_area + sd), width=.1) +
  theme(panel.grid.major = element_blank(), axis.title.x = element_text(size = 15), axis.text=element_text(size=15), axis.title.y = element_text(size = 15),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none") +
  ylab("Proportional area of silver colouration") +
  xlab("Treatment group") +
  scale_color_OkabeIto()

# Create a plot with silvering with parr mark area
silver_parr_mark_plot <- ggplot() + 
  geom_smooth(method = "lm", data = RGB_and_cluster_data, se = T, colour = "black", aes(x = parr_mark_area, y = RGB_silvering_area)) +
  geom_point(data = RGB_and_cluster_data, size = 3, aes(x = parr_mark_area, y = RGB_silvering_area, colour = treatment)) +
  theme(panel.grid.major = element_blank(), axis.title.x = element_text(size = 15), axis.text=element_text(size=15), axis.title.y = element_text(size = 15),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none") +
  ylab("Proportional area of silver colouration") +
  xlab("Proportional area of parr mark colouration ") +
  scale_color_OkabeIto()

# Create plot with silvering with length
ggplot() + 
  geom_smooth(method = "lm", data = RGB_and_cluster_data, se = T, aes(x = length, y = RGB_silvering_area, colour = treatment)) +
  geom_point(data = RGB_and_cluster_data, size = 2, aes(x = length, y = RGB_silvering_area, colour = treatment)) +
  theme(panel.grid.major = element_blank(), axis.title.x = element_text(size = 15), axis.text=element_text(size=15), axis.title.y = element_text(size = 15),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportional area of silver colouration") +
  xlab("Individual length (mm)") +
  guides(colour=guide_legend("Treatment"),
         fill=guide_legend("Treatment")) +
  scale_color_OkabeIto()

# Combine plots
silver_parr_mark_plot + silver_length_plot

######################################################################################################################################################################################################
## Create plots for orange colouration
# Create a plot showing the spread of values in treatment group
orange_spot_treatment_SE <- summarySE(RGB_and_cluster_data, measurevar = "orange_colour_area", groupvars = c("treatment"))

orange_spot_treatment_plot <- ggplot() +
  geom_violin(data = RGB_and_cluster_data, aes(x = treatment, y = orange_colour_area, colour = treatment), width = 1) +
  geom_point(data = orange_spot_treatment_SE, size = 3, aes(x = treatment, y = orange_colour_area, colour = treatment)) +
  geom_errorbar(data = orange_spot_treatment_SE, aes(colour = treatment, x = treatment, y = orange_colour_area, ymin = orange_colour_area - sd, ymax = orange_colour_area + sd), width=.1) +
  theme(panel.grid.major = element_blank(), axis.title.x = element_text(size = 12), axis.text=element_text(size=15), axis.title.y = element_text(size = 15),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") +
  ylab("Proportional area of orange colouration") +
  xlab("Treatment group") +
  scale_color_OkabeIto()

# Create a plot with orange colouration against parr mark area
orange_spot_parr_mark_plot <- ggplot() + 
  geom_smooth(method = "lm", data = RGB_and_cluster_data, se = T, colour = "black", aes(x = parr_mark_area, y = orange_colour_area)) +
  geom_point(data = RGB_and_cluster_data, size = 3, aes(x = parr_mark_area, y = orange_colour_area, colour = treatment)) +
  theme(panel.grid.major = element_blank(), axis.title.x = element_text(size = 12), axis.text=element_text(size=15), axis.title.y = element_text(size = 15),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none") +
  ylab("Proportional area of orange colouration") +
  xlab("Proportional area of parr mark colouration ") +
  scale_color_OkabeIto()

# Create a plot with orange pagainst silvering
orange_spot_silver_plot <- ggplot() + 
  geom_smooth(method = "lm", data = RGB_and_cluster_data, se = T, colour = "black", aes(x = RGB_silvering_area, y = orange_colour_area)) +
  geom_point(data = RGB_and_cluster_data, size = 3, aes(x = RGB_silvering_area, y = orange_colour_area, colour = treatment)) +
  theme(panel.grid.major = element_blank(), axis.title.x = element_text(size = 12), axis.text=element_text(size=15), axis.title.y = element_text(size = 15),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none") +
  ylab("Proportional area of orange colouration") +
  xlab("Proportional area of silver colouration ") +
  scale_color_OkabeIto()

# create marginal effect models for length
orange_spot_length_plot <- ggplot() + 
  geom_smooth(method = "lm", data = RGB_and_cluster_data, se = T, aes(x = length, y = orange_colour_area, colour = treatment)) +
  geom_point(data = RGB_and_cluster_data, size = 3, aes(x = length, y = orange_colour_area, colour = treatment)) +
  theme(panel.grid.major = element_blank(), axis.title.x = element_text(size = 12), axis.text=element_text(size=15), axis.title.y = element_text(size = 15),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportional area of orange colouration") +
  xlab("Individual length (mm)") +
  guides(colour=guide_legend("Treatment"),
         fill=guide_legend("Treatment")) +
  scale_color_OkabeIto()

# Arrange plots
orange_spot_treatment_plot /
  (orange_spot_parr_mark_plot | orange_spot_silver_plot | orange_spot_length_plot)

######################################################################################################################################################################################################
## Create plots for parr mark area
# Create a plot showing the spread of values in treatment group
parr_mark_treatment_SE <- summarySE(RGB_and_cluster_data, measurevar = "parr_mark_area", groupvars = c("treatment"))

parr_mark_treatment_plot <- ggplot() +
  geom_violin(data = RGB_and_cluster_data, aes(x = treatment, y = parr_mark_area, colour = treatment), width = 1) +
  geom_point(data = parr_mark_treatment_SE, size = 3, aes(x = treatment, y = parr_mark_area, colour = treatment)) +
  geom_errorbar(data = parr_mark_treatment_SE, aes(x = treatment, y = parr_mark_area, ymin = parr_mark_area - sd, ymax = parr_mark_area + sd, colour = treatment), width=.1) +
  theme(panel.grid.major = element_blank(), axis.title.x = element_text(size = 15), axis.text=element_text(size=15), axis.title.y = element_text(size = 15),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none") +
  ylab("Proportional area of parr marks") +
  xlab("Treatment group") +
  guides(colour=guide_legend("Treatment")) +
  scale_color_OkabeIto()

# Create marginal effect model of parr mark area against silvering area
parr_mark_sivering_plot <- ggplot() + 
  geom_smooth(method = "lm", data = RGB_and_cluster_data, se = T, colour = "black", aes(x = RGB_silvering_area, y = parr_mark_area)) +
  geom_point(data = RGB_and_cluster_data, size = 3, aes(x = RGB_silvering_area, y = parr_mark_area, colour = treatment)) +
  theme(panel.grid.major = element_blank(), axis.title.x = element_text(size = 15), axis.text=element_text(size=15), axis.title.y = element_text(size = 15),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none") +
  ylab("Proportional area of parr marks") +
  xlab("Proportional area of silver") +
  scale_color_OkabeIto()

# Create marginal effect model against orange spot area
parr_mark_orange_spot_plot <- ggplot() + 
  geom_smooth(method = "lm", data = RGB_and_cluster_data, se = T, colour = "black", aes(x = orange_colour_area, y = parr_mark_area)) +
  geom_point(data = RGB_and_cluster_data, size = 3, aes(x = orange_colour_area, y = parr_mark_area, colour = treatment)) +
  theme(panel.grid.major = element_blank(), axis.title.x = element_text(size = 15), axis.text=element_text(size=15), axis.title.y = element_text(size = 15),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportional area of parr marks") +
  xlab("Proportional area of orange colour") +
  guides(colour=guide_legend("Treatment")) +
  scale_color_OkabeIto()

# Arrange plots
parr_mark_treatment_plot /
  (parr_mark_sivering_plot | parr_mark_orange_spot_plot)
