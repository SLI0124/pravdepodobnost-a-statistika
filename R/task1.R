# 1. import libraries ####
# install packages
install.packages("readxl")  # reading excel files
install.packages("openxlsx")# writing excel files
install.packages("moments") # calculating skewness and kurtosis
install.packages("dplyr")   # more efficient data manipulation
install.packages("tidyr")   # more efficient data manipulation (pivot_longer)
install.packages("ggplot2") # better visualization graphs
install.packages("ggpubr")  # for combining multiple ggplot2 plots
install.packages("rstatix") # identify outliers
install.packages("forcats") # contains fct_infreq() function, which sorts factor levels by frequency

# activate packages
library(readxl)
library(openxlsx)
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


# 2. data gathering, cleaning and preprocessing ####
# we have 4 GPUs and their performance before and after patch 

input_file = "data/ukol_191.xlsx"

# Nvidia RTX 2080 Ti
nvidia_2080 = read_excel(input_file,
                         sheet = "Nvidia RTX 2080 Ti")

# Nvidia RTX 3070 Ti
nvidia_3070 = read_excel(input_file,
                         sheet = "Nvidia RTX 3070 Ti")

# AMD Radeon RX 6800 XT 
amd_6800 = read_excel(input_file,
                      sheet = "AMD Radeon RX 6800 XT")

# AMD Radeon RX 7700 XT
amd_7700 = read_excel(input_file,
                      sheet = "AMD Radeon RX 7700 XT")

# add column names: id, average_fps_release, average_fps_patch
new_column_names = c("id", "average_fps_release", "average_fps_patch")
nvidia_2080 = setNames(nvidia_2080, new_column_names)
nvidia_3070 = setNames(nvidia_3070, new_column_names)
amd_6800 = setNames(amd_6800, new_column_names)
amd_7700 = setNames(amd_7700, new_column_names)

# add column with GPU name 
nvidia_2080$gpu = "nvidia_2080"
nvidia_3070$gpu = "nvidia_3070"
amd_6800$gpu = "amd_6800"
amd_7700$gpu = "amd_7700"

# combine all data
all_data = rbind(nvidia_2080, nvidia_3070, amd_6800, amd_7700)

# convert to data frame format for future mismanagement, just to be sure
all_data = as.data.frame(all_data)

# drop the id column
all_data = all_data %>% select(-id)

# delete old data frames, just for cleaner environment table
rm(nvidia_2080, nvidia_3070, amd_6800, amd_7700, new_column_names)

# 3. data analysis ####
# 3.1.
# add a new column preformance_increase
all_data$performance_increase = all_data$average_fps_patch - all_data$average_fps_release

# 3.2.
# create new column, that will tell if the row is an outlier or not
all_data$outlier = FALSE

# Identify outliers using the IQR method
all_data = all_data %>%
  group_by(gpu) %>%
  mutate(
    Q1 = quantile(performance_increase, 0.25),
    Q3 = quantile(performance_increase, 0.75),
    IQR = Q3 - Q1,
    lower_bound = Q1 - 1.5 * IQR,
    upper_bound = Q3 + 1.5 * IQR,
    outlier = performance_increase < lower_bound | performance_increase > upper_bound
  ) %>%
  select(-Q1, -Q3, -IQR, -lower_bound, -upper_bound) # Clean up intermediate columns

# 3.3.
# for each gpu calculate: with outliers and without outliers:
# number of rows, minimum value, lower quartile, median value, average value, 
# upper quartile, maximum value, standard deviation, variance coefficient (%), 
# skewness, kurtosis, lowest bound of quartile, highest bound of quartile 
summarize_gpu = function(gpu_name) {
  gpu_data = all_data %>% filter(gpu == gpu_name)
  
  # with outliers
  with_outliers = gpu_data %>%
    summarize(
      rozsah = n(),
      minimum = min(performance_increase),
      dolni_kvartil = quantile(performance_increase, 0.25),
      median = median(performance_increase),
      prumer = mean(performance_increase),
      horni_kvartil = quantile(performance_increase, 0.75),
      maximum = max(performance_increase),
      smerodatna_odchylka = sd(performance_increase),
      variacni_koeficient_procento = smerodatna_odchylka / prumer * 100,
      sikmost = skewness(performance_increase),
      spicatost = kurtosis(performance_increase),
      dolni_mez = dolni_kvartil - 1.5 * IQR(performance_increase),
      horni_mez = horni_kvartil + 1.5 * IQR(performance_increase)
    )
  
  # without outliers
  without_outliers = gpu_data %>% filter(outlier == FALSE) %>%
    summarize(
      rozsah = n(),
      minimum = min(performance_increase),
      dolni_kvartil = quantile(performance_increase, 0.25),
      median = median(performance_increase),
      prumer = mean(performance_increase),
      horni_kvartil = quantile(performance_increase, 0.75),
      maximum = max(performance_increase),
      smerodatna_odchylka = sd(performance_increase),
      variacni_koeficient_procento = smerodatna_odchylka / prumer * 100,
      sikmost = skewness(performance_increase),
      spicatost = kurtosis(performance_increase),
      dolni_mez = dolni_kvartil - 1.5 * IQR(performance_increase),
      horni_mez = horni_kvartil + 1.5 * IQR(performance_increase)
    )
  
  return(rbind(with_outliers, without_outliers))
}

# calculate summary statistics nvidia 3070
summary_nvidia_3070 = summarize_gpu("nvidia_3070")
print(summary_nvidia_3070)

# calculate summary statistics amd 7700
summary_amd_7700 = summarize_gpu("amd_7700")
print(summary_amd_7700)

# 4. data visualization ####
# 4.1.
# create boxplot with outliers for Nvidia 3070 and AMD 7700
filtered_data <- all_data %>% filter(gpu %in% c("nvidia_3070", "amd_7700"))
png("output/boxplot_with_outliers.png", width = 800, height = 600)
par(mfrow = c(1, 1))
boxplot(filtered_data$performance_increase ~ filtered_data$gpu,
        names = c("Nvidia 3070", "AMD 7700"),
        main = "Krabicový graf s odlehlými pozorováními",
        xlab = "Grafické karty",
        ylab = "FPS",
        col="gray",
        border="black")
dev.off()
        
# 4.2.
# create boxplot without outliers for Nvidia 3070 and AMD 7700
filtered_data <- all_data %>% filter(gpu %in% c("nvidia_3070", "amd_7700"), outlier == FALSE)
png("output/boxplot_without_outliers.png", width = 800, height = 600)
par(mfrow = c(1, 1))
boxplot(filtered_data$performance_increase ~ filtered_data$gpu,
        names = c("Nvidia 3070", "AMD 7700"),
        main = "Krabicový graf bez odlehlých pozorování",
        xlab = "Grafické karty",
        ylab = "FPS",
        col="gray",
        border="black")
dev.off()

# 4.3. 
# combine QQ plot and histogram for Nvidia 3070
filtered_data <- all_data %>% filter(gpu == "nvidia_3070", outlier == FALSE)
png("output/qqplot_histogram_nvidia_3070.png", width = 1200, height = 600)
par(mfrow = c(1, 2))
# QQ plot
qqnorm(filtered_data$performance_increase,
       main = "QQ plot pro Nvidia 3070",
       xlab = "Norm. teoretické kvantily",
       ylab = "Výběrové kvantily")
qqline(filtered_data$performance_increase, col = "black")
# histogram
hist(filtered_data$performance_increase,
     main = "Histogram pro Nvidia 3070",
     xlab = "FPS",
     ylab = "Počet",
     col = "gray",
     border = "black")
par(mfrow = c(1, 1))
dev.off()

# 4.4.
# combine QQ plot and histogram for AMD 7700
filtered_data <- all_data %>% filter(gpu == "amd_7700", outlier == FALSE)
png("output/qqplot_histogram_amd_7700.png", width = 1200, height = 600)
par(mfrow = c(1, 2))
# QQ plot
qqnorm(filtered_data$performance_increase,
       main = "QQ plot pro AMD 7700",
       xlab = "Norm. teoretické kvantily",
       ylab = "Výběrové kvantily")
qqline(filtered_data$performance_increase, col = "black")
# histogram
hist(filtered_data$performance_increase,
     main = "Histogram pro AMD 7700",
     xlab = "FPS",
     ylab = "Počet",
     col = "gray",
     border = "black")
par(mfrow = c(1, 1))
dev.off()

# 5. calculate sigma interval ####
calculate_sigma_interval = function(data, column_name, sigma) {
  mean_value = mean(data[[column_name]], na.rm = TRUE)
  sd_value = sd(data[[column_name]], na.rm = TRUE)
  
  lower_bound = mean_value - sigma * sd_value
  upper_bound = mean_value + sigma * sd_value
  
  return(c(lower_bound, upper_bound))
}

# calculate 2-sigma interval for Nvidia 3070
nvidia_3070 = all_data %>% filter(gpu == "nvidia_3070", outlier == FALSE)
sigma_interval_2_3070 = calculate_sigma_interval(nvidia_3070, "performance_increase", 2)

# calculate 2-sigma interval for AMD 7700
amd_7700 = all_data %>% filter(gpu == "amd_7700", outlier == FALSE)
sigma_interval_2_7700 = calculate_sigma_interval(amd_7700, "performance_increase", 2)


