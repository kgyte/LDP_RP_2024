#Acquire data from existing local folder, merge csvs, and save that to raw data folder in LDP folder

library(dplyr)

# List all CSV files in the D1_filteredcsv folder
D1_files <- list.files(path = "../../Tseng Lab/MP Directed Studies/D1_filteredcsv", pattern = "\\.csv$", full.names = TRUE)

# Read all CSV files into a list and add a column with the file name
D1_data_list <- lapply(D1_files, function(file) {
  df <- read.csv(file)
  df$filename <- tools::file_path_sans_ext(basename(file))
  return(df)
})

#combine individual dataframes into one large csv file
full_D1_data <- bind_rows(D1_data_list)

#save this csv file to LDP rawdata folder
write.csv(full_D1_data, file = "00_rawdata/D1data.csv", row.names = FALSE)
