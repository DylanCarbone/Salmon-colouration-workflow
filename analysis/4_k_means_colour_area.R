# Load packages
library(patternize)
library(SOAR)
library(recolorize)
library(viridis)
library(readr)
library(ggplot2)

# Open imposed colour palette
patternize_list_all <- readRDS("rds_files/patternize_list_all.rds")

# Load the saved palette
parr_palette_all <- readRDS("rds_files/palette_all.rds")

# View palette colours
plotColorPalette(parr_palette_all)

# Now lets seperate the vectors for treatment group
patternize_list_treatment <- patternize_list_all[1:115]

# Now lets seperate the vectors for treatment group
patternize_list_control <- patternize_list_all[116:282]

# Set up mask 
mask <- read.table("project_data/outlines/parr_252_calibrated_treatment_rectangle_outline.txt", header = FALSE)

fish_outline_mask <- read.table("project_data/outlines/parr_252_calibrated_treatment_outline.txt", header = FALSE)

# make a list for treatment fish again
imageList_treatment <- makeList(names(patternize_list_treatment), type = "image",
                                prepath = "project_data/all_images",
                                extension = ".jpg")

# make a list for control fish
imageList_control <- makeList(names(patternize_list_control), type = "image",
                              prepath = "project_data/all_images",
                              extension = ".jpg")

# make a list with treatment landmarks
landmarkList_treatment <- makeList(names(patternize_list_treatment),
                                   type = "landmark",
                                   prepath = "project_data/all_landmarks",
                                   extension = "_landmarks.txt")

# make a list with control all_landmarks
landmarkList_control <- makeList(names(patternize_list_control),
                                 type = "landmark",
                                 prepath = "project_data/all_landmarks",
                                 extension = "_landmarks.txt")

#Obtain proportional area of treatment individuals
parr_area_treatment <- patArea(patternize_list_treatment,
                               names(patternize_list_treatment),
                               refShape = 'target',
                               type = 'k', 
                               outline = mask,
                               adjustCoords = TRUE, 
                               imageList = imageList_treatment,
                               landList = landmarkList_treatment,
                               cartoonID = "parr_252_calibrated_treatment")

#Obtain proportional area of control individuals
parr_area_control <- patArea(patternize_list_control,
                               names(patternize_list_control),
                               refShape = 'target',
                               type = 'k', 
                               outline = mask,
                               adjustCoords = TRUE, 
                               imageList = imageList_control,
                               landList = landmarkList_control,
                               cartoonID = "parr_252_calibrated_treatment")

# # sum the colorpatterns
# sum_imageList_treatment <- sumRaster(patternize_list_treatment, names(patternize_list_treatment), type = 'k')
# sum_imageList_control <- sumRaster(patternize_list_control, names(patternize_list_control), type = 'k')
# 
# #Create heatmap colour scale
# col_scale <- inferno(100)
# 
# which_color <- 4
# 
# # Create a heatmap
# plotHeat(sum_imageList_treatment[[which_color]],
#          names(patternize_list_treatment),
#          plotCartoon = TRUE,
#          refShape = 'target',
#          outline = mask,
#          landList = landmarkList_treatment,
#          imageList = imageList_treatment,
#          cartoonFill = 'black',
#          cartoonOrder = 'under',
#          cartoonID = "parr_252_calibrated_treatment",
#          colpalette = col_scale,
#          adjustCoords = TRUE)
# 
# # Create a heatmap
# plotHeat(sum_imageList_control[[which_color]],
#          names(patternize_list_control),
#          plotCartoon = TRUE,
#          refShape = 'target',
#          outline = mask,
#          landList = landmarkList_control,
#          imageList = imageList_control,
#          cartoonFill = 'black',
#          cartoonOrder = 'under',
#          cartoonID = "parr_252_calibrated_control",
#          colpalette = col_scale,
#          adjustCoords = TRUE)
# 
# ## Now lets create a plot visualising differences in colouration
# # subtract rasters
# subtracted_raster <- summed_parr_RGB_treatment_masked/length(IDlist_treatment) - summed_parr_RGB_control_masked/length(IDlist_control)
# 
# # plot subtracted heatmap
# colfunc <- c("blue","lightblue","black","pink","red")
# 
# # Plot differences in colouration
# plotHeat(subtracted_raster,
#          IDlist_treatment,
#          plotCartoon = TRUE,
#          refShape = 'target',
#          outline = mask, 
#          landList = landmarkList_treatment,
#          adjustCoords = TRUE,
#          imageList = imageList_treatment,
#          cartoonID = "parr_252_calibrated_treatment",
#          zlim=c(-1,1),
#          normalized = TRUE,
#          cartoonFill = 'black',
#          refImage = target,
#          cartoonOrder = 'under',
#          colpalette = colfunc,
#          legendTitle = "Differences in proportional occurence of colouration")


#Create an empty cluster list
cluster_list <- list()

# First lets merge the treatment and control groups together and extract the parr Id number
for (colour in 1:nrow(parr_palette_all)){
  
  # Extract treatment
  parr_cluster_treatment <- parr_area_treatment[[colour]]
  
  # Extract control
  parr_cluster_control <- parr_area_control[[colour]]
  
  # merge treatment and control
  parr_cluster_all <- rbind(parr_cluster_treatment, parr_cluster_control)
  
  # Extract the ID number and save it in a new column
  parr_cluster_all$parr_id <- parse_number(parr_cluster_all$SampleId)
  
  # Save it in the new list
  cluster_list[[colour]] <- parr_cluster_all
}

## Now lets combine each colour cluster in the list
# Create a table using just the first colour cluster
cluster_table <- cluster_list[[1]][, -1]

# Rename the first column
colnames(cluster_table)[1] <- paste0("cluster_1_area")

# For each of the remaining clusters
for (colour in 2:length(cluster_list)){
  
  # Merge the cluster in the list with the cluster table
  cluster_table <- merge(cluster_table, cluster_list[[colour]][, -1], by = "parr_id")
  
  # Rename the column corresponding to the cluster area
  colnames(cluster_table)[colour + 1] <- paste0("cluster_", colour, "_area")
}

# Lets import the parr dataset
parr_data <- read.csv("project_data/parr_data.csv")

# Remove_columns we do not need
parr_data <- parr_data[, -c(3, 4, 10, 11)]

# Remove parr with no parental assignment then remove this column
parr_data_Y <- subset(parr_data, subset = Parental.Assignment == "Y")[, -9]

# Rename column names
colnames(parr_data_Y) <- c("stream", "treatment", "river_section", "transect", "parr_id", "length", "weight", "family")

# Merge the cluster data with the stream and fish data
cluster_data <- merge(cluster_table, parr_data_Y, by = "parr_id")

# Save dataset
write.csv(cluster_data, "project_data/cluster_data.csv")
