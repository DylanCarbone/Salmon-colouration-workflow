# load libraries
library(patternize)
library(raster)
library(recolorize)
library(base)

# Note: Here I would recommend restarting R to clear space

###################################################################################
###################################################################################

## For treatment images
# load the imageList_aligned object:
imageList_aligned_treatment <- readRDS("rds_files/imageList_aligned_treatment.rds")

# convert from RasterBricks to image arrays:
imgs_treatment <- lapply(imageList_aligned_treatment, brick_to_array)
names(imgs_treatment) <- names(imageList_aligned_treatment)

# save raster extents:
extent_list_treatment <- lapply(imageList_aligned_treatment, extent)

# fit initial recolorize fits
rc_list_treatment <- lapply(imgs_treatment, 
                            function(i) recolorize2(i, bins = 10,
                                                    plotting = F, cutoff = 15))

# # Lets remove n number of brightest layers
# num_brightest_cols <- 2
# 
# #Create template list
# rc_list_recolorised_treatment <- list()
# 
# # For each image...
# for (parr in 1:length(rc_list_treatment)) {
#   
#   # Print image name
#   print(paste0("processing treatment image: ", names(rc_list_treatment)[parr]))
#   
#   # Obtain sample image from list
#   parr_image <- rc_list_treatment[[parr]]
#   
#   # Repeat this process 3 times
#   for (rep in 1:num_brightest_cols){
#     
#     # Obtain cluster RGB values
#     RGB_vals <- parr_image[["centers"]]
#     
#     #Create an empty brightness vector
#     brightness <- c()
#     
#     # Obtain the brightness
#     for (colour in 1:nrow(RGB_vals)){
#       
#       # Calculate the greyscale
#       brightness[colour] <- sum(RGB_vals[colour,])/3
#       
#     }
#     
#     # Obtain the colour with the largest brightness
#     pos <- which.max(brightness)
#     
#     # I use this to bypass any errors
#     tryCatch({
#       
#       # Absorb the brightest layer
#       parr_image <- absorbLayer(
#         parr_image,
#         pos,
#         size_condition = function(s) s <= 1000,
#         #x_range = c(.25, .75),
#         y_range = c(0, .4),
#         highlight_color = "yellow",
#         remove_empty_layers = TRUE,
#         plotting =  F
#       )}, error=function(e){cat("Bypassed error:",conditionMessage(e), "\n")})
#   }
#   
#   # Plot image seperately (as turning the plotting function into the absorbLayer() function sometimes crashes R)
#   plotImageArray(recoloredImage(parr_image))
#   
#   # Store values in a list
#   rc_list_recolorised_treatment[[parr]] <- parr_image
#   
# }
# 
# # Rename the names of the list
# names(rc_list_recolorised_treatment) <- names(imgs_treatment)

# Save object as RDS file
saveRDS(rc_list_treatment, "rds_files/rc_list_treatment.rds")

#############################################################################################################################
## For control images
# load the imageList_aligned object:
imageList_aligned_control <- readRDS("rds_files/imageList_aligned_control.rds")

# convert from RasterBricks to image arrays:
imgs_control <- lapply(imageList_aligned_control, brick_to_array)
names(imgs_control) <- names(imageList_aligned_control)

# save raster extents:
extent_list_control <- lapply(imageList_aligned_control, extent)

# fit initial recolorize fits
rc_list_control <- lapply(imgs_control, 
                          function(i) recolorize2(i, bins = 10,
                                                  plotting = F, cutoff = 15))

# #Create template list
# rc_list_recolorised_control <- list()
# 
# # For each image...
# for (parr in 1:length(rc_list_control)) {
# 
#   # Print image name
#   print(paste0("processing control image: ", names(rc_list_control)[parr]))
# 
#   # Obtain sample image from list
#   parr_image <- rc_list_control[[parr]]
# 
#   # Repeat this process 3 times
#   for (rep in 1:num_brightest_cols){
# 
#     # Obtain cluster RGB values
#     RGB_vals <- parr_image[["centers"]]
# 
#     #Create an empty brightness vector
#     brightness <- c()
# 
#     # Obtain the brightness
#     for (colour in 1:nrow(RGB_vals)){
# 
#       # Calculate the greyscale
#       brightness[colour] <- sum(RGB_vals[colour,])/3
# 
#     }
# 
#     # Obtain the colour with the largest brightness
#     pos <- which.max(brightness)
# 
#     # I use this to bypass any errors
#     tryCatch({
# 
#       # Absorb the brightest layer
#       parr_image <- absorbLayer(
#         parr_image,
#         pos,
#         size_condition = function(s) s <= 1000,
#         #x_range = c(.25, .75),
#         y_range = c(0, .4),
#         highlight_color = "yellow",
#         remove_empty_layers = TRUE,
#         plotting =  F
#       )}, error=function(e){cat("Bypassed error:",conditionMessage(e), "\n")})
#   }
# 
#   # Plot image seperately (as turning the plotting function into the absorbLayer() function sometimes crashes R)
#   plotImageArray(recoloredImage(parr_image))
# 
#   # Store values in a list
#   rc_list_recolorised_control[[parr]] <- parr_image
# 
# }
# 
# # Rename the names of the list
# names(rc_list_recolorised_control) <- names(imgs_control)

# Save object as RDS file
saveRDS(rc_list_control, "rds_files/rc_list_control.rds")

#################################################################################################################
#################################################################################################################
## here we will obtain the colour palettes across all individuals

# read the saved rds files
rc_list_treatment <- readRDS("rds_files/rc_list_treatment.rds")
rc_list_control <- readRDS("rds_files/rc_list_control.rds")

# merge the lists into one for obtaining palettes
rc_list_all <- c(rc_list_treatment, rc_list_control)
imgs_all <- c(imgs_treatment, imgs_control)
extent_list_all <- c(extent_list_treatment, extent_list_control)

# Here I would recommend removing objects we do not need and clearing memory
# rm(rc_list_treatment)
# rm(rc_list_control)
# rm(imgs_treatment)
# rm(imgs_control)
# rm(extent_list_treatment)
# rm(extent_list_control)
# gc()

# Obtain all the palettes
# get all palettes and sizes
all_palettes_all <- do.call(rbind, lapply(rc_list_all, function(i) i$centers))
all_sizes_all <- do.call(c, lapply(rc_list_all, function(i) i$sizes / sum(i$sizes)))

# cluster colors
cluster_list_all <- hclust_color(all_palettes_all,
                                  cutoff = 60,
                                 )

# make an empty matrix for storing the new palette
parr_palette_all <- matrix(NA, ncol = 3, nrow = length(cluster_list_all))

# for every color in cluster_list...
for (i in 1:length(cluster_list_all)) {
  
  # get the center indices
  idx <- cluster_list_all[[i]]
  
  # get the average value for each channel, using cluster size to get a weighted average
  ctr <- apply(all_palettes_all, 2, 
               function(j) weighted.mean(j[idx], 
                                         w = all_sizes_all[idx]))
  
  # store in the palette matrix
  parr_palette_all[i, ] <- ctr
}

# Save the colour palette
saveRDS(parr_palette_all, "rds_files/palette_all.rds")

# View palette
plotColorPalette(parr_palette_all)

# and apply pallete across images
impose_list_all <- lapply(imgs_all, function(i) imposeColors(i, parr_palette_all, 
                                                             adjust_centers = FALSE))

# Save imposed pallete
saveRDS(impose_list_all, "rds_files/recolorize_fits_all.rds")

# convert back to patternize (including extent)
patternize_list_all <- lapply(impose_list_all, recolorize_to_patternize)
for (i in 1:length(patternize_list_all)) {
  for (j in 1:length(patternize_list_all[[1]])) {
    raster::extent(patternize_list_all[[i]][[j]]) <- extent_list_all[[i]]
  }
}

# and save
saveRDS(patternize_list_all, "rds_files/patternize_list_all.rds")

#######################################################################################
## to quickly load samples of colour cluster imposed images
# the imageList_aligned object:
# imageList_aligned_treatment <- readRDS("rds_files/imageList_aligned_treatment.rds")
# 
# # convert from RasterBricks to image arrays:
# imgs_treatment <- lapply(imageList_aligned_treatment, brick_to_array)
# names(imgs_treatment) <- names(imageList_aligned_treatment)
# 
# # save raster extents:
# extent_list_treatment <- lapply(imageList_aligned_treatment, extent)
# 
# ## For control images
# # load the imageList_aligned object:
# imageList_aligned_control <- readRDS("rds_files/imageList_aligned_control.rds")
# 
# # convert from RasterBricks to image arrays:
# imgs_control <- lapply(imageList_aligned_control, brick_to_array)
# names(imgs_control) <- names(imageList_aligned_control)
# 
# # save raster extents:
# extent_list_control <- lapply(imageList_aligned_control, extent)
# 
# # Load the colour palette
# parr_palette_all <- readRDS("rds_files/palette_all.rds")
# 
# # View palette
# plotColorPalette(parr_palette_all)
# 
# #merge all images
# imgs_all <- c(imgs_treatment, imgs_control)
# 
# # and apply pallete across images
# impose_list_all <- lapply(imgs_all, function(i) imposeColors(i, parr_palette_all,
#                                                              adjust_centers = FALSE))
