# 1. import libraries ####
# install packages
install.packages("readxl")  # reading excel files
install.packages("moments") # calculating skewness and kurtosis
install.packages("dplyr")   # more efficient data manipulation
install.packages("tidyr")   # more efficient data manipulation (pivot_longer)
install.packages("ggplot2") # better visualization graphs
install.packages("ggpubr")  # for combining multiple ggplot2 plots
install.packages("rstatix") # identify outliers
install.packages("forcats") # contains fct_infreq() function, which sorts factor levels by frequency

# activate packages
library(readxl)
library(moments)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(rstatix)
library(forcats)  

# czech formatting for numbers
options(OutDec = ",",
        digits = 7,
        pillar.sigfig = 7)


# 2. data gathering and cleaning ####
# we have 4 GPUs and their performance before and after patch 

# Nvidia RTX 2080 Ti
nvidia_2080 = read_excel("data/ukol_191.xlsx",
                         sheet = "Nvidia RTX 2080 Ti",
                         skip = 1)

# Nvidia RTX 3070 Ti
nvidia_3070 = read_excel("data/ukol_191.xlsx",
                         sheet = "Nvidia RTX 3070 Ti",
                         skip = 1)

# AMD Radeon RX 6800 XT 
amd_6800 = read_excel("data/ukol_191.xlsx",
                      sheet = "AMD Radeon RX 6800 XT",
                      skip = 1)

# AMD Radeon RX 7700 XT
amd_7700 = read_excel("data/ukol_191.xlsx",
                      sheet = "AMD Radeon RX 7700 XT",
                      skip = 1)

# give each GPU columns id, average_fps_release, average_fps_patch
new_column_names = c("id", "average_fps_release", "average_fps_patch")
nvidia_2080 = nvidia_2080 %>% rename_with(~new_column_names)
nvidia_3070 = nvidia_3070 %>% rename_with(~new_column_names)
amd_6800 = amd_6800 %>% rename_with(~new_column_names)
amd_7700 = amd_7700 %>% rename_with(~new_column_names)

# change data structure to data.frame for potential future mismanipulation
nvidia_2080 = as.data.frame(nvidia_2080)
nvidia_3070 = as.data.frame(nvidia_3070)
amd_6800 = as.data.frame(amd_6800)
amd_7700 = as.data.frame(amd_7700)

# 3. data analysis ####
# calculate number of rows
# calculate minimum, lower_quartile, median, average, upper_quartile, maximum
# calculate standard deviation, coefficient of variation (%)
# calculate skewness, kurtosis

# function to all the statistics
calculate_statistics = function(data, gpu_name, patch = FALSE) {
  if (patch) { # if patch is TRUE, use average_fps_patch column
    data = data$average_fps_patch
  } else { # if patch is FALSE, use average_fps_release column
    data = data$average_fps_release
  }
  
  number_of_rows = length(data)
  minimum = min(data, na.rm = TRUE)
  lower_quartile = quantile(data, 0.25, na.rm = TRUE)
  median = median(data, na.rm = TRUE)
  average = mean(data, na.rm = TRUE)
  upper_quartile = quantile(data, 0.75, na.rm = TRUE)
  maximum = max(data, na.rm = TRUE)
  standard_deviation = sd(data, na.rm = TRUE)
  coefficient_of_variation = standard_deviation / average * 100
  skewness = (moments::skewness(data, na.rm = TRUE))
  kurtosis = (moments::kurtosis(data, na.rm = TRUE) - 3) # subtract 3 to get excess kurtosis

  # print all statistics
  cat("GPU: ", gpu_name, "\n")
  cat("Number of rows: ", number_of_rows, "\n")
  cat("Minimum: ", minimum, "\n")
  cat("Lower quartile: ", lower_quartile, "\n")
  cat("Median: ", median, "\n")
  cat("Average: ", average, "\n")
  cat("Upper quartile: ", upper_quartile, "\n")
  cat("Maximum: ", maximum, "\n")
  cat("Standard deviation: ", standard_deviation, "\n")
  cat("Coefficient of variation: ", coefficient_of_variation, "%\n")
  cat("Skewness: ", skewness, "\n")
  cat("Kurtosis: ", kurtosis, "\n\n")
}

# calculate statistics for Nvidia 3070 and AMD 7700 after release

# Nvidia 3070 after release
calculate_statistics(nvidia_3070, "Nvidia RTX 3070 Ti", patch = FALSE)

# calculate statistics for AMD 7700 after release
calculate_statistics(amd_7700, "AMD Radeon RX 7700 XT", patch = FALSE)

# calculate statistics for Nvidia 3070 and AMD 7700 after patch

# Nvidia 3070 after patch
calculate_statistics(nvidia_3070, "Nvidia RTX 3070 Ti", patch = TRUE)

# calculate statistics for AMD 7700 after patch
calculate_statistics(amd_7700, "AMD Radeon RX 7700 XT", patch = TRUE)