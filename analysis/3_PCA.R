# Load packages
library(patternize)
library(SOAR)
library(recolorize)

# Increase memory cap if necessary
#memory.limit(size=16000) 

# Open imposed colour palette
patternize_list_all <- readRDS("rds_files/patternize_list_all.rds")

# Store the list to reduce vector size
Store(patternize_list_all)

# Cleanup space, removing objects that are not in the environment
gc()

# Load background function for calculation of PCA
source("background_code_to_source/patPCA_total.R")

# Load the saved palette
parr_palette_all <- readRDS("rds_files/palette_all.rds")

# View palette colours
plotColorPalette(parr_palette_all)

#Input patternize list, number of images in each treatment group, and select colour clusters you would like to compare
parr_pca <- patPCA_total(patternize_list_all,
                         num_images = c(115, 167),
                         which_colors = c(10,12))


################################################################################

# State the number of images in each treatment group with the numbers corresponding to treatment and control respectively
num_images <- c(115, 167)

# plot images instead of points:
PCx <- 1; PCy <- 2
pca_summary <- summary(parr_pca)
limits <- apply(parr_pca$x[ , c(PCx, PCy)], 2, range)
images <- lapply(dir("project_data/all_images", full.names = TRUE)[-1],
                 function(i) readImage(i, resize = 1/4))

plot(parr_pca$x[ , c(PCx, PCy)], type = "n",
     asp = 1,
     xlim = limits[ , 1] + c(-5, 5), 
     ylim = limits[ , 2] + c(-10, 10),
     xlab=paste0('PC1 (', round(pca_summary$importance[2, PCx]*100, 1), ' %)'),
     ylab=paste0('PC2 (', round(pca_summary$importance[2, PCy]*100, 1), ' %)'))
for (i in 1:length(images)) {
  add_image(images[[i]], 
            x = parr_pca$x[i, PCx],
            y = parr_pca$x[i, PCy],
            width = 15)
}

# plot color maps instead of points:
plot(parr_pca$x[ , c(PCx, PCy)], type = "n",
     asp = 1,
     xlim = limits[ , 1] + c(-5, 5), 
     ylim = limits[ , 2] + c(-10, 10),
     xlab=paste0('PC1 (', round(pca_summary$importance[2, PCx]*100, 1), ' %)'),
     ylab=paste0('PC2 (', round(pca_summary$importance[2, PCy]*100, 1), ' %)'))
for (i in 1:length(imgs)) {
  add_image(recoloredImage(impose_list[[i]]), 
            x = parr_pca$x[i, PCx],
            y = parr_pca$x[i, PCy],
            width = 15)
}

################################################################################
# # determine the number of colors
# n_layers <- length(patternize_list_all[[1]])
# IDlist <- names(patternize_list_all)
# 
# # restructure the raster list by layer instead of image
# patternize_list_all_reconstructed <- lapply(1:n_layers,
#                                             function(i) lapply(patternize_list_all,
#                                                                function(j) j[[i]]))
# 
# # redefine to only specified layers (if not "all")
# if (sum(which_colors != "all") > 0) {
#   if (!is.numeric(which_colors)) {
#     stop("'which_colors' must be 'all' or a numeric vector indicating which
#            colors should be used for the summed PCA")
#   }
#   patternize_list_all_reconstructed <- patternize_list_all[which_colors]
#   n_layers <- length(which_colors)
# }
# 
# # # sum rasters after masking
# # summedRasters <- lapply(patternize_list_all_reconstructed,
# #                         function(i) patternize::sumRaster(i, IDlist, type = "RGB"))
# # Store(summedRasters)
# 
# # make a list for storing the raster dataframes
# rasDFlist <- vector("list", length = n_layers)
# 
# # make one dataframe per layer
# for (l in 1:n_layers) {
#   for (r in 1:length(patternize_list_all_reconstructed[[1]])) {
#     
#     # isolate layer from image
#     layer <- patternize_list_all_reconstructed[[l]][[r]]
#     
#     # swap out NA values
#     layer[is.na(layer)] <- 0
#     
#     # convert to a dataframe
#     ras <- raster::as.data.frame(layer)
#     
#     # either start or append the dataframe for this layer
#     if (r == 1) { rasDF <- ras } else { rasDF <- cbind(rasDF, ras) }
#   }
#   
#   # set column names and add to the list
#   colnames(rasDF) <- names(patternize_list_all_reconstructed[[l]])
#   rasDFlist[[l]] <- rasDF
# }
# 
# Store(rasDFlist)
# 
# gc()
# 
# # make a stacked version for the full PCA
# rasDFstack <- do.call(rbind, rasDFlist)
# 
# { message(paste("Running PCA on", n_layers, 
#                 "colors and", ncol(rasDFstack), "images...")) }
# # run a PCA
# comp <- prcomp(t(rasDFstack))
# pcdata <- comp$x
# rotation <- comp$rotation
# summ <- summary(comp)
# 
# PCx = 1
# PCy = 2
# 
# xrange <- range(pcdata[ , PCx])
# yrange <- range(pcdata[ , PCy])
# 
# # be polite
# current_par <- par(no.readonly = TRUE)
# on.exit(par(current_par))
# 
# # Create a warning to ensure that the sample size is correct
# if (sum(num_images)!=282){message("Warning: the total number of images entered in each treatment group is incorrect")}
# 
# # set parameters and plot
# par(mfrow=c(1,1), mar=c(4,4,2,2))
# plot(comp$x[,c(PCx,PCy)], col=c(rep("red", num_images[1]), rep("yellow", num_images[2])), pch=19,
#      xlim = xrange, ylim = yrange,
#      xlab=paste0('PC',PCx,' (', round(summ$importance[2,PCx]*100, 1), ' %)'),
#      ylab=paste0('PC',PCy,' (', round(summ$importance[2,PCy]*100, 1), ' %)'))

################################################################################