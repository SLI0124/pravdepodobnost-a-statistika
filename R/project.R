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
install.packages("lawstat") # for symmetry.test() function
install.packages("stats")   # for f.test() and many more

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
library(lawstat)
library(stats)

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


# TASK ONE
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

# 3.4.
# print all outliers with their id, before and after patch performance
outliers = all_data %>% filter(outlier == TRUE)
# print all 3070 outliers 
outliers_3070 = outliers %>% filter(gpu == "nvidia_3070")
print(outliers_3070)
# print all 7700 outliers
outliers_7700 = outliers %>% filter(gpu == "amd_7700")
print(outliers_7700)

# 4. data visualization ####
# 4.1. create a histogram for performance increase grouped by GPU
# create boxplot with outliers for Nvidia 3070 and AMD 7700
# Filter data for Nvidia 3070 and AMD 7700
selected_gpus = all_data %>% filter(gpu %in% c("nvidia_3070", "amd_7700"))

# Define GPU name mapping
gpu_name_mapping <- c(
  "nvidia_3070" = "Nvidia RTX 3070 Ti", 
  "amd_7700" = "AMD Radeon RX 7700 XT"
)

# Filter and map GPU names to human-readable names
selected_gpus <- all_data %>%
  filter(gpu %in% names(gpu_name_mapping)) %>%
  mutate(gpu = gpu_name_mapping[gpu])

# Define GPU colors
gpu_colors <- c("Nvidia RTX 3070 Ti" = "grey", "AMD Radeon RX 7700 XT" = "white")

# Create the boxplot for performance increase grouped by GPU
boxplot_plot <- ggplot(selected_gpus, aes(x = gpu, y = performance_increase, fill = gpu)) +
  stat_boxplot(geom = "errorbar", width = 0.2) +  # Add error bars
  geom_boxplot() +                               # Create the boxplot
  scale_fill_manual(values = gpu_colors) +       # Set custom colors
  labs(x = "", y = "Zvýšení výkonu (FPS)") +     # Axis labels
  theme_bw() +                                   # White background theme
  theme(legend.position = "none")                # Remove legend

# Save the plot
ggsave("output/boxplot_with_outliers_task1.png", plot = boxplot_plot, width = 10, height = 6, units = "in", dpi = 300)

# 4.2. create a histogram and qqplot for performance increase grouped by GPU
# without outliers
# on left there will be histogram, on right there will be qqplot
# both sets of plots will will have same x range

# 4.2.1. create a histogram
# Filter data for Nvidia 3070 and AMD 7700 without outliers
selected_gpus_no_outliers <- all_data %>% filter(gpu %in% c("nvidia_3070", "amd_7700"), outlier == FALSE)

# apply the name mapping
selected_gpus_no_outliers <- selected_gpus_no_outliers %>%
  mutate(gpu = gpu_name_mapping[gpu])

# Create the histogram for performance increase grouped by GPU without outliers
histogram_plot <- ggplot(selected_gpus_no_outliers, 
                         aes(x = performance_increase, fill = gpu)) +
  geom_histogram(color = "black", 
                 fill = 'lightgrey',
                 binwidth = 0.25) + 
  labs(x = "Zvýšení výkonu (FPS)", 
       y = "Četnost") +
  theme_bw() + 
  theme(axis.text = element_text(color = 'black', size = 12)) +
  facet_wrap(~gpu, , nrow = 4) 


# 4.2.2. create a qqplot
qqplot_plot <- ggplot(selected_gpus_no_outliers, 
                      aes(sample = performance_increase)) +
  stat_qq() + 
  stat_qq_line() +
  labs(x = "Normované teoretické kvantily", 
       y = "Výběrové kvantily") +
  theme_bw() +
  theme(axis.text = element_text(color = 'black', size = 12)) +
  facet_wrap(~gpu, , nrow = 4,
             scales = "free")

# 4.2.3. combine the plots
combined_plot <- ggarrange(histogram_plot, qqplot_plot,
                           nrow = 1)
ggarrange(histogram_plot, qqplot_plot, 
          nrow = 2,
          heights = c(2,4))
          

# save the plot
ggsave("output/histogram_qqplot_no_outliers_task1.png", plot = combined_plot, width = 10, height = 6, units = "in", dpi = 300)

# 5. calculate Cebyshev's inequality ####
calculate_cebyshev = function(data, k) {
  # Calculate the mean and standard deviation
  mean_value = mean(data)
  sd_value = sd(data)
  
  # Calculate the lower and upper bounds
  lower_bound = mean_value - k * sd_value
  upper_bound = mean_value + k * sd_value
  
  return(c(lower_bound, upper_bound))
  
}

# 5.1. calculate Cebyshev's inequality for Nvidia 3070 and AMD 7700
# 5.1.1. Nvidia 3070
# Filter data for Nvidia 3070 and with no outliers

nvidia_3070_data = all_data %>% filter(gpu == "nvidia_3070") %>% filter(outlier == FALSE)
# Calculate the proportion of data within 2 standard deviation
proportion_nvidia_3070 = calculate_cebyshev(nvidia_3070_data$performance_increase, 2)
print(proportion_nvidia_3070)

# 5.1.2. AMD 7700
# Filter data for AMD 7700
amd_7700_data = all_data %>% filter(gpu == "amd_7700") %>% filter(outlier == FALSE)
# Calculate the proportion of data within 2 standard deviation
proportion_amd_7700 = calculate_cebyshev(amd_7700_data$performance_increase, 2)
print(proportion_amd_7700)


# TASK TWO
# 6. add boxplot without outliers for nvidia 3070 and amd 7700 ####
# 6.1. create a boxplot for performance increase grouped by GPU without outliers
# Filter data for Nvidia 3070 and AMD 7700 without outliers
selected_gpus_no_outliers = all_data %>% filter(gpu %in% c("nvidia_3070", "amd_7700"), outlier == FALSE)

# apply the name mapping
selected_gpus_no_outliers = selected_gpus_no_outliers %>%
  mutate(gpu = gpu_name_mapping[gpu])

# Create the boxplot for performance increase grouped by GPU without outliers
boxplot_plot_no_outliers = ggplot(selected_gpus_no_outliers, aes(x = gpu, y = performance_increase, fill = gpu)) +
  stat_boxplot(geom = "errorbar", width = 0.2) +  # Add error bars
  geom_boxplot() +                               # Create the boxplot
  scale_fill_manual(values = gpu_colors) +       # Set custom colors
  labs(x = "", y = "Zvýšení výkonu (FPS)") +     # Axis labels
  theme_bw() +                                   # White background theme
  theme(
    legend.position = "none",                    # Remove legend
    axis.text.x = element_text(size = 30, face = "bold"), # Make x-axis names larger
    axis.text.y = element_text(size = 30, face = "bold"), # Make y-axis labels larger
    axis.title.y = element_text(size = 30, face = "bold"), # Make y-axis title larger
  )

# Save the plot
ggsave("output/boxplot_no_outliers_task2.png", plot = boxplot_plot_no_outliers, width = 10, height = 6, units = "in", dpi = 300)

# 7. testing the hypothesis ####
# 7.1. Shapiro-Wilk test
# 7.1.1. Nvidia 3070

shapiro_test_nvidia_3070_p_value = shapiro.test(nvidia_3070_data$performance_increase)$p.value
print(shapiro_test_nvidia_3070_p_value)
# data:  nvidia_3070_data$performance_increase -> W = 0,98121, p-value = 0,3297
# p-value > 0.05, therefore we do not reject the null hypothesis that the data is normally distributed

# 7.1.2. AMD 7700
shapiro_test_amd_7700_p_value = shapiro.test(amd_7700_data$performance_increase)$p.value
print(shapiro_test_amd_7700_p_value)
# data:  amd_7700_data$performance_increase -> W = 0,97382, p-value = 0,2064
# p-value > 0.05, therefore we do not reject the null hypothesis that the data is normally distributed

# 8. compare the performance increase between Nvidia 3070 and AMD 7700 ####
# 8.1. point estimate
# Calculate the point estimate for the difference in performance increase between Nvidia 3070 and AMD 7700
point_estimate = mean(amd_7700_data$performance_increase) - mean(nvidia_3070_data$performance_increase)
print(point_estimate)

# 8.2. confidence interval
# calculate f test to compare variances of two groups
confidence_interval = t.test(amd_7700_data$performance_increase, nvidia_3070_data$performance_increase, alternative = "greater", conf.level = 0.95, conf.int = TRUE, mu=0)
print(confidence_interval)

# TASK 3
# 9. create a boxplot for all GPUs with outliers ####
# create 4 rows of plots for each GPU
# on left, plot histogram with data trend, on right plot qqplot
# both sets of plots will have same x range
# 9. 1. create a boxplot for all GPUs with outliers
all_data_no_outliers = all_data %>% filter(outlier == FALSE)
box = 
    ggplot(all_data_no_outliers,
           aes(x = gpu,
               y = performance_increase)) +
    stat_boxplot(geom = "errorbar", 
                 width = 0.2) +
    geom_boxplot() +
    labs(x = "", 
         y = "Zvýšení výkonu (FPS)",
         title = "") +
    theme_bw() +
    stat_summary(geom = "point", 
                 fun = mean, 
                 shape = 3,
                 size = 2, 
                 color = "grey") +
    theme(
      axis.text = element_text(size = 10, 
                               face = "bold",
                               color = "black"),
      
    )
      
ggarrange(box,
          nrow = 1)

ggsave("output/boxplot_all_gpus_task3.png", plot = all_gpu_boxplot, width = 10, height = 6, units = "in", dpi = 300)

# 9.2 create a histogram and qqplot for all GPUs without outliers
# 9.2.1. create a histogram
hist =
  ggplot(all_data_no_outliers,
         aes(x = performance_increase)) +
  geom_histogram(binwidth = 0.25,
                 color = "black",           # barva ohraničení
                 fill = "lightgray") +      # barva výplně
  labs(x = "nárůst FPS",
       y = "hustota pravděpodobnosti")+
  theme_bw()+
  theme(
    axis.text = element_text(color = "black", size = 20),   # Velikost popisků os
    axis.title = element_text(color = "black", size = 30), # Velikost názvů os
    strip.text = element_text(color = "black", size = 25)  # Velikost názvů facetů
  ) +
  facet_wrap("gpu",nrow = 4) # "facety" umožňují přípravu sad grafů stejného typu v závislosti na kategoriální proměnné

# 9.2.2. create a qqplot
qq =
  ggplot(all_data_no_outliers, 
         aes(sample = performance_increase))+
  stat_qq()+
  stat_qq_line()+
  labs(x = "teoretické normované kvantily",
       y = "výběrové kvantily",)+
  theme_bw() +
  theme(
    axis.text = element_text(color = "black", size = 20),   # Velikost popisků os
    axis.title = element_text(color = "black", size = 30), # Velikost názvů os
    strip.text = element_text(color = "black", size = 25)  # Velikost názvů facetů
  ) +
  facet_wrap("gpu",nrow = 4)

# 9.3. combine the plot
pom = ggarrange(hist,qq,
                nrow = 1)

ggsave("output/histogram_qqplot_all_gpus_task3.png", plot = pom, width = 10, height = 12, units = "in", dpi = 300)

# 10. for all gpus, calculate skewness, kutosis and shapiro-wilk test ####
# 10.1. calculate skewness and kurtosis for all GPUs
skewness_kurtosis = all_data_no_outliers %>%
  group_by(gpu) %>%
  summarize(
    skewness = skewness(performance_increase),
    kurtosis = kurtosis(performance_increase),
    shapiro_wilk_p_value = shapiro.test(performance_increase)$p.value
  )

print(skewness_kurtosis)
