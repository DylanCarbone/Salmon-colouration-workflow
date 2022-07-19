# #Here we will adjust the file name numbers
# #list the dirs
# dirs <- list.dirs("project_data/images", recursive = T)[-c(1,2,8)]
# 
# # set directory name and parr number
# dir <- dirs[10]
# start_number <- 237
# 
# # List all files in the chosen directory
# files <- list.files(dir, full.names = T, recursive = T)
# 
# # for each listed file
# for (file in files){
#   
#   # rename whole path
#   file.rename(from = file, to= paste0(dir, "/parr_", start_number, ".jpg"))
#   
#   start_number = start_number + 1
# }

###############################################################################################################

# # Now we will callibrate the images
# library(patternize)
# 
# # list directories
# dirs <- list.dirs("project_data/images", recursive = T)[-c(1,2,8)]
# 
# # I am now working on dir 4
# dir <- dirs[10]
# 
# # Create a list of file names with the ".jpg" removed
# IDlist <- as.character(sapply(list.files(dir, full.names = T), FUN = function(filenames_extension){
#   substr(filenames_extension, 1, nchar(filenames_extension)-4)
# }))
# 
# # Add name extension
# extension <- ".jpg"
# 
# # Adjust colouration
# colorChecker(IDlist = IDlist, extension = extension)

###################################################################################################################
#here we adjust the file names for landmarks and images
# # Create a list of the directory names
# dirs <- list.dirs("project_data/images", recursive = F)
# 
# for (dir in dirs){
# 
#   if(dir == dirs[1]){
# 
#     files = list.files(dir, recursive = T, full.names = T, pattern = "*.jpg")
# 
#     for (file in files){
# 
#       #Rename each file
#       file.rename(from = file, to = paste0(substr(file, 1, nchar(file)-4), "_control.jpg"))}
# 
#     } else {
# 
#         files = list.files(dir, recursive = T, full.names = T, pattern = "*.jpg")
# 
#         for (file in files){
# 
#           #Rename each file
#           file.rename(from = file, to = paste0(substr(file, 1, nchar(file)-4), "_treatment.jpg"))}
#         }
# 
# }
# 

# #List all files
# landmark_files <- list.files("project_data/treatment_landmarks", pattern = "*.csv", full.names = T)
# 
# # obtain the base file names
# names <- substr(list.files("project_data/treatment_landmarks", pattern = "*.csv"), 1, nchar(list.files("project_data/treatment_landmarks", pattern = "*.csv"))-4)
# 
# # for each file...
# for (file in 1:length(landmark_files)){
#   
# file.rename(from = landmark_files[file], to = paste0("project_data/all_data/parr_", names[file],"_calibrated_treatment_landmarks.csv"))
#   
#   }
#   
#   

######################################################################################################################
# # Now we need to create txt files for the landmarks
# files <- list.files("project_data/all_data", pattern = "*.csv", full.names = T)
# 
# for (file in files) {
#   
#   table <- read.csv(file)
#   table <- table[,-1]
#   names(table) <- ""
#   
#   name <- substr(file, 1, nchar(file)-4)
#   
#   write.table(table, file = paste0(name, ".txt"), sep = "\t",
#               row.names = FALSE, col.names = FALSE)
# }
# 
# test <- files[1]
# 
# hmm <- read.csv(test)
# 
# hmm <- hmm[,-1]
# 
# names(hmm) <- ""
# 
# write.table(hmm, file = "hmm.txt", sep = "\t",
#             row.names = FALSE, col.names = FALSE)




