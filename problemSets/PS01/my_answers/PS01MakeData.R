############
# Problem Set 1 trial 1 - 22/09/22
# Data Import script 
##########

## Here i am importing the data from the question and converting it to a s
# ingle-column.csv file for future access. I'm also organising my repository 
# into subfolders for an easier workflow and to reflect best practices

# loading packages if needed library()

setwd("StatsI_Fall2022") # changed to capital I - check if any effect
dir.create(path = "PS01") # creating new sub-folder for problem set work
setwd("PS01") # specify folder working in
getwd() # checking working directory is correct


iq <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 
       80, 97, 95, 111, 114, 89, 95, 126, 98) # creating vector for data

psdata <- data.frame(iq) # idk if this is necessary

# Save data to a separate .csv file in the data directory for ease of access in
# future
write.csv(psdata, file = "~/StatsI_Fall2022/PS01/iq.csv", 
          row.names = FALSE)




