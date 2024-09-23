#Acquire data from existing local folder, merge csvs, and save that to raw data folder in LDP folder

library(dplyr)

# List all CSV files in the D1_filteredcsv folder
D1_files <- list.files(path = "/Users/katie/Documents/Tseng Lab/MP Directed Studies/D1_filteredcsv", pattern = "\\.csv$", full.names = TRUE)

# Read all CSV files into a list and add a column with the file name
D1_data_list <- lapply(D1_files, function(file) {
  df <- read.csv(file)
  df$filename <- tools::file_path_sans_ext(basename(file))
  return(df)
})

#combine individual dataframes into one large csv file
D1data <- bind_rows(D1_data_list)

####Add columns to show sample info (day, treatment, tube #, replicate #)####
# do this by subsetting the 'filename' column info

# Use extract to split the column into new columns -- use [a-zA-Z] for letters and (\\d+) for numbers

add_filename_columns <- function(df) {
  df <- extract(df, filename,
                into = c("Day", "Treatment", "Tube", "Replicate"),
                regex = "(D\\d+)([a-zA-Z]+)(\\d{1})_(\\d{1})",
                remove = FALSE)
  return(df)
}

#apply add filename function
D1data <- add_filename_columns(D1data)

####ERRORS HERE#### 
#move columns to the front & rename treatment groups
D1data <- relocate(D1data, c("Day", "Treatment", "Tube", "Replicate"), .before = "Particle.ID") %>% 
  mutate(Treatment = factor(Treatment, levels = c("cn", "cntw", "low", "med", "high"))) %>%
  arrange(Treatment)

#rename treatment groups
levels(Treatment) <- c("cn" = "Control",
                       "cntw" = "Control + Tween",
                       "low" = "Low",
                       "med" = "Medium",
                       "high" = "High")


#reorder treatment groups so "high" is last
D1data <- D1data %>%
  mutate(Treatment = factor(Treatment, levels = c("cn", "cntw", "low", "med", "high"))) %>%
  arrange(Treatment)

##filter to only one biological replicate per treatment (tube #1) to make csv file small enough for github! 
D1data_tube1 <- D1data %>% 
  group_by()
  filter()


#save this csv file to LDP rawdata folder
write.csv(full_D1_data, file = "00_rawdata/D1data.csv", row.names = FALSE)
