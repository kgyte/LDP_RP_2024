#### Cleaning raw data

#load libraries
library(tidyr)
library(dplyr)
library(ggplot2)

#set working directory
setwd("/Users/katie/Documents/Living Data Project/LDP_RP/LDP_RP_2024")

#read in rawdata file
D1data <- read.csv("00_rawdata/D1data_tube1.csv")

####Only import select columns from raw data file####
#Define your columns_to_import function
columns_to_import <- function(df) {
  selected_columns <- df[, c("Day", "Treatment", "Tube", "Replicate", "Particle.ID", "Convex.Perimeter", "Diameter..ABD.", "Area..ABD.", "Biovolume..Cylinder.", "Biovolume..P..Spheroid.", "Biovolume..Sphere.",
                             "Volume..ABD.", "Date", "Image.File")] 
  return(selected_columns)
} 

#apply columns_to_import function to full D1 data csv
D1data_filt <- columns_to_import(D1data) #new dataframe containing day 1 data with only necessary columns

#calculate summary statistics for key variables
calculate_summary_statistics <- function(df) {
  df %>%
    group_by(Day, Treatment, Tube) %>%
    #this keeps these columns in the final df
    summarize(
      across(
        c(Diameter..ABD., Area..ABD., Biovolume..P..Spheroid., Volume..ABD.),
        list(
          Mean = ~mean(.x, na.rm = TRUE),
          Median = ~median(.x, na.rm = TRUE),
          SD = ~sd(.x, na.rm = TRUE),
          Range = ~max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE)
        ),
        .names = "{col}_{fn}"
      ),
      .groups = "drop"
    )
}

#apply function to full data df to get summary stats by replicate
D1data_summary <- calculate_summary_statistics(D1data_filt)

#save csv file of day 1 summary statistics
write.csv(D1data_summary, file = "02_outdata/D1data_summarystats.csv")

####Filtering aggregate data####

#filter day one's data to include only particles with >80 convex perimeter 
ag_D1_full <- filter(D1data_filt, Convex.Perimeter >= 80) %>% 
  mutate(Treatment = factor(Treatment, levels = c("Control", "Control + Tween", "Low", "Medium", "High"))) %>%
  arrange(Treatment)

# Function to calculate total number of aggregates
calculate_ag_count_by_treatment <- function(df) {
  summary_df <- df %>%
    group_by(Day, Treatment, Tube, Replicate) %>%
    summarize(
      total_aggregates = n(), # Count number of rows (aggs) in each group
      .groups = "drop"
    )
  return(summary_df)
}

#calculate aggregate count
D1_ag_count <- calculate_ag_count_by_treatment(ag_D1_full)

#write csv for day 1 aggregate counts
write.csv(D1_ag_count, file = "02_outdata/D1data_aggcounts.csv")

#define new Treatment labels for graphs
new_labels <- c("cn" = "Control", "cntw" = "Control + Tween", "low" = "Low MP", "med" = "Medium MP", "high" = "High MP")

#plot of aggregate count on day 1 by treatment
D1_agcount <- ggplot(D1_ag_count, aes(x=Treatment, y=total_aggregates, fill = Treatment)) +
  geom_col() +
  theme_minimal()+
  scale_fill_manual(values = c("Control" = "lightgrey", "Control + Tween" = "darkgrey", "Low" = "lightblue", "Medium" = "deepskyblue", "High" = "blue"), labels = new_labels) +
  labs(title = "Aggregate count by treatment", y = "Number of aggregates per replicate")

#save plot as .png in figures folder
ggsave(filename = "03_figures/D1_agcount.png", plot = D1_agcount, device = "png")

