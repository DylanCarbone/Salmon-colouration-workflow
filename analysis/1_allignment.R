# library(patternize)
# 
# IDlist <- list.files("test", full.names = T)
# 
# colorChecker(IDlist)


# install and load patternize
#devtools::install_github("StevenVB12/patternize")
library(patternize)
library(SOAR)

# Create a list of file names with the ".jpg" removed for treatment fish
IDlist_treatment <- as.character(sapply(list.files("project_data/all_images", pattern = "*treatment.jpg", recursive = T), FUN = function(filenames_extension){
  substr(filenames_extension, 1, nchar(filenames_extension)-4)
}))

# Create a list of file names with the ".jpg" removed for control fish
IDlist_control <- as.character(sapply(list.files("project_data/all_images", pattern = "*control.jpg", recursive = T), FUN = function(filenames_extension){
  substr(filenames_extension, 1, nchar(filenames_extension)-4)
}))

# make a list for treatment fish
imageList_treatment <- makeList(IDlist_treatment, type = "image",
                              prepath = "project_data/all_images",
                              extension = ".jpg")

# make a list for control fish
imageList_control <- makeList(IDlist_control, type = "image",
                      prepath = "project_data/all_images",
                      extension = ".jpg")

# make a list with treatment all_landmarks
landmarkList_treatment <- makeList(IDlist_treatment,
                                 type = "landmark",
                                 prepath = "project_data/all_landmarks",
                                 extension = "_landmarks.txt")

# make a list with control all_landmarks
landmarkList_control <- makeList(IDlist_control,
                         type = "landmark",
                         prepath = "project_data/all_landmarks",
                         extension = "_landmarks.txt")

# Set target
target <- landmarkList_treatment[['parr_252_calibrated_treatment']]

# Set up mask 
#mask1 <- read.table("project_data/outlines/parr_252_calibrated_treatment_outline.txt", header = FALSE)
rectangle_mask <- read.table("project_data/outlines/parr_252_calibrated_treatment_rectangle_outline.txt", header = FALSE)

### Alignment ###

# For treatment all_images
imageList_aligned_treatment <- alignLan(imageList_treatment, landmarkList_treatment, transformRef = target,
                              adjustCoords = T,
                              plotTransformed = T,
                              resampleFactor = 3,
                              cartoonID = 'parr_252_calibrated_treatment',
                              maskOutline = rectangle_mask,
                              inverse = F, res = c(100, 500))

# For control all_images
imageList_aligned_control <- alignLan(imageList_control, landmarkList_control, transformRef = target,
                                        adjustCoords = T,
                                        plotTransformed = F,
                                        resampleFactor = 3,
                                        refImage =  imageList_treatment[['parr_252_calibrated_treatment']],
                                        maskOutline = rectangle_mask,
                                        inverse = F, res = c(100, 500))

#save aligned treatment bodies
saveRDS(imageList_aligned_treatment, file = 'rds_files/imageList_aligned_treatment.rds')

#save aligned control bodies
saveRDS(imageList_aligned_control, file = 'rds_files/imageList_aligned_control.rds')
