# load required package
library(patternize)
library(viridis)
library(readr)

# Create a list of file names with the ".jpg" removed for treatment fish
IDlist_treatment <- as.character(sapply(list.files("project_data/all_images", pattern = "*treatment.jpg", recursive = T), FUN = function(filenames_extension){
  substr(filenames_extension, 1, nchar(filenames_extension)-4)
}))

# Create a list of file names with the ".jpg" removed for control fish
IDlist_control <- as.character(sapply(list.files("project_data/all_images", pattern = "*control.jpg", recursive = T), FUN = function(filenames_extension){
  substr(filenames_extension, 1, nchar(filenames_extension)-4)
}))

# # reduce sample size
# sample_size <- 20
# 
# IDlist_treatment <- unique(c('parr_252_calibrated_treatment',  IDlist_treatment[1:sample_size-1]))
# IDlist_control <- IDlist_control[1:sample_size]

# Make a list for treatment fish
imageList_treatment <- makeList(IDlist_treatment, type = "image",
                                prepath = "project_data/all_images",
                                extension = ".jpg")

# Make a list for control fish
imageList_control <- makeList(IDlist_control, type = "image",
                              prepath = "project_data/all_images",
                              extension = ".jpg")

# Make a list with treatment landmarks
landmarkList_treatment <- makeList(IDlist_treatment,
                                   type = "landmark",
                                   prepath = "project_data/all_landmarks",
                                   extension = "_landmarks.txt")

# Make a list with control landmarks
landmarkList_control <- makeList(IDlist_control,
                                 type = "landmark",
                                 prepath = "project_data/all_landmarks",
                                 extension = "_landmarks.txt")

# Set target
target <- landmarkList_treatment[['parr_252_calibrated_treatment']]

# For orange
#RGB_orange = c(172, 78, 42)

# For silver
RGB_silver= c(218, 192, 131) # 0.15

# For brown
#RGB_brown = c(71, 62, 53) # 0.05
 
# Extract RGB threshold colours for treatment images
parr_RGB_treatment_silver <- patLanRGB(imageList_treatment, landmarkList_treatment, RGB_silver, transformRef = target, resampleFactor = 1,
                                colOffset = 0.15, crop = T, adjustCoords = T, plot = 'stack', res = 200)

# Extract RGB threshold colours for control images
parr_RGB_control_silver <- patLanRGB(imageList_control, landmarkList_control, RGB_silver, transformRef = target, resampleFactor = 1,
                              colOffset = 0.15, crop = T, adjustCoords = T, plot = 'stack', res = 200)

# save RGB extraction
saveRDS(parr_RGB_treatment_silver, "rds_files/parr_RGB_treatment_silver.rds")
saveRDS(parr_RGB_control_silver, "rds_files/parr_RGB_control_silver.rds")

## Load the RDS files
parr_RGB_treatment_silver <- readRDS("rds_files/parr_RGB_treatment_silver.rds")
parr_RGB_control_silver <- readRDS("rds_files/parr_RGB_control_silver.rds")

# Sum the colourpatterns
summed_parr_RGB_treatment_silver <- sumRaster(parr_RGB_treatment_silver, IDlist_treatment, type = 'RGB')
summed_parr_RGB_control_silver <- sumRaster(parr_RGB_control_silver, IDlist_control, type = 'RGB')

# Load the target image outline
mask <- read.table("project_data/outlines/parr_252_calibrated_treatment_outline.txt", header = FALSE)

# Use the outline of the target to mask each of the extracted rasters
parr_RGB_treatment_masked <-list()
parr_RGB_control_masked <-list()

# remove everying in the plot outside of the outline
for(e in 1:length(parr_RGB_treatment_silver)){
  ID <- names(parr_RGB_treatment_silver)[[e]]
  parr_RGB_treatment_masked[[ID]] <- maskOutline(parr_RGB_treatment_silver[[ID]], IDlist = IDlist_treatment, mask,
                                            refShape = 'target', imageList = imageList_treatment)
}


# remove everying in the plot outside of the outline
for(e in 1:length(parr_RGB_control_silver)){
  ID <- names(parr_RGB_control_silver)[[e]]
  parr_RGB_control_masked[[ID]] <- maskOutline(parr_RGB_control_silver[[ID]], IDlist = IDlist_control, mask,
                                                 refShape = 'target', imageList = imageList_control)
}

# sum your rasters again
summed_parr_RGB_treatment_masked <- sumRaster(parr_RGB_treatment_masked, IDlist_treatment, type = 'RGB')
summed_parr_RGB_control_masked <- sumRaster(parr_RGB_control_masked, IDlist_control, type = 'RGB')

# Create a sequence of colours
colfunc <- inferno(100)

# Create heatplot for treatment images
plotHeat(summed_parr_RGB_treatment_masked,
         IDlist_treatment,
         plotCartoon = T,
         refShape = 'target',
         outline = mask, 
         landList = landmarkList_treatment,
         adjustCoords = T,
         legend = FALSE,
         imageList = imageList_treatment,
         cartoonID = 'parr_252_calibrated_treatment',
         cartoonFill = "black",
         cartoonOrder = 'under',
         colpalette = colfunc)

# Create heatplot for control images
plotHeat(summed_parr_RGB_control_masked,
         IDlist_control,
         plotCartoon = T,
         refShape = 'target',
         outline = mask, 
         landList = landmarkList_control,
         adjustCoords = T,
         imageList = imageList_control,
         cartoonFill = "black",
         cartoonOrder = 'under',
         colpalette = colfunc,
         refImage = target,
         legendTitle = "Proportional occurence of colouration within RGB threshold")

## Now lets create a plot visualising differences in colouration
# subtract rasters
subtracted_raster <- summed_parr_RGB_treatment_masked/length(IDlist_treatment) - summed_parr_RGB_control_masked/length(IDlist_control)

# plot subtracted heatmap
colfunc <- c("blue","lightblue","black","pink","red")

# Plot differences in colouration
plotHeat(subtracted_raster,
         IDlist_treatment,
         plotCartoon = TRUE,
         refShape = 'target',
         outline = mask, 
         landList = landmarkList_treatment,
         adjustCoords = TRUE,
         imageList = imageList_treatment,
         cartoonID = "parr_252_calibrated_treatment",
         zlim=c(-1,1),
         normalized = TRUE,
         cartoonFill = 'black',
         refImage = target,
         cartoonOrder = 'under',
         colpalette = colfunc,
         legendTitle = "Differences in proportional occurence of colouration")

# Calculate proportional area in treatment images
area_RGB_treatment_silver <- patArea(parr_RGB_treatment_silver, IDlist_treatment, refShape = 'target', type = 'RGB', 
                              outline = mask, adjustCoords = TRUE, landList = landmarkList_treatment,
                              imageList = imageList_treatment, cartoonID = 'parr_252_calibrated_treatment')

# Calculate proportional area in control images
area_RGB_control_silver <- patArea(parr_RGB_control_silver, IDlist_control, refShape = 'target', type = 'RGB', 
                            outline = mask, adjustCoords = TRUE, landList = landmarkList_control,
                            imageList = imageList_control, cartoonID = 'parr_252_calibrated_treatment')

# merge the control and treatment tables together
RGB_data <- rbind(area_RGB_treatment_silver, area_RGB_control_silver)

# Extract the ID number and save it in a new column
RGB_data$parr_id <- parse_number(RGB_data$SampleId)

# rename column name
colnames(RGB_data)[2] <- "RGB_silvering_area"

# Lets import the cluster dataset
cluster_data <- read.csv("project_data/cluster_data.csv")[,-1]

# Merge the cluster data with the stream and fish data
RGB_and_cluster_data <- merge(RGB_data, cluster_data, by = "parr_id")

# Save dataset
write.csv(RGB_and_cluster_data, "project_data/RGB_and_cluster_data.csv")
