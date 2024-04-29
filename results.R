#loading libraries
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(kableExtra)
library(knitr)
library(gridExtra)
library(grid)
library(e1071)
library(ggpubr)
library(broom)
library(pgirmess)

#STEP 1: Loading data----------------------------------------------------------------------------------------

#loading data prescribed by Prof. M Gibbons
jellyfish <- read.csv2("C:/Users/User/Desktop/Capstone/jellyfish.csv")

#STEP 2: Plotting tmp vs cum----------------------------------------------------------------------------------

jellyfish_plot <- ggplot(jellyfish, aes(x = Age, y = Cum.N, colour = Species)) +
  geom_smooth() +  # Add a line plot
  facet_wrap(~ Temp, axes = "margins", scales = "free_x") +  # Facet by Temperature with free scales
  labs(x = "Age (days)",
       y = "Cumulative Number of Polyps") +  # Add labels
  scale_colour_manual(values = c("grey", "black")) +  # Set line colors to grey and black
  theme_minimal()

# Display the plot
jellyfish_plot

#STEP 3: Proloferation rate------------------------------------------------
# Calculating the rate in cumulative number of polyps for each series
rate <- jellyfish %>%
  group_by(Series, Polyp, Temp, Species) %>%
  mutate(r = Cum.N / lag(Cum.N, default = first(Cum.N)))

#calculating the means of asexual reproduction for each species groups
mean_rate <- rate %>%
  group_by(Species, Temp, Polyp, Series) %>%
  summarise(mean_r = mean(r, na.rm = TRUE))

#calculating the means of asexual reproduction for each species groups observing temp alone
mean_rate_temp <- rate %>%
  group_by(Species, Temp) %>%
  summarise(mean_r = round(mean(r, na.rm = TRUE), 3))

#creating table for word
species_table <- kable(mean_rate_temp, format = "markdown",
                       col.names = c("Species", "Temp", "Mean Growth Rate")) %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)

species_table

#STEP 4: Assumption testing------------------------------------------------

#filtering cx and cf from mean r rates with polyps

agulensis <- mean_rate %>% 
  filter(Species == "CX")

fulgida <- mean_rate %>% 
  filter(Species == "Cf")

#testing normality for agulensis
agulensis_shapiro <- shapiro.test(agulensis$mean_r)

fulgida_shapiro <- shapiro.test(fulgida$mean_r)

#STEP 5: STATS Analysis--------------------------------------------------------

# testing Kruskal-Wallice H for C.agulensis
agulensis_kruskal <- compare_means(mean_r ~ Temp, data = agulensis, method = "kruskal.test")

# Create kable table
# Create a data frame with the test results
agulensis_kruskal_results <- data.frame(
  Variable = "Temperature vs mean proliferartion",
  Dataset = "agulensis_kruskal",
  Test = "Kruskal-Wallis Rank",
  group = "C.agulhensis",
  p_value = 5.101566e-08
)

# Format p-value and confidence interval
agulensis_kruskal_results$p_value <- format(agulensis_kruskal_results$p_value, scientific = FALSE)


agulensis_kruskal_table <- agulensis_kruskal_results %>%
  kable("html", align = "c") %>%
  kable_styling("striped", full_width = TRUE)

agulensis_kruskal_table

# testing Kruskal-Wallice H for C.fulgida
fulgida_kruskal <- compare_means(mean_r ~ Temp, data = fulgida, method = "kruskal.test")

# Create kable table
# Create a data frame with the test results
fulgida_kruskal_results <- data.frame(
  Variable = "Temperature vs mean proliferartion",
  Dataset = "fulgida_kruskal",
  Test = "Kruskal-Wallis Rank",
  group = "C.fulgida",
  p_value = 7.875e-13
)

# Format p-value and confidence interval
fulgida_kruskal_results$p_value <- format(fulgida_kruskal_results$p_value, scientific = FALSE)


fulgida_kruskal_table <- fulgida_kruskal_results %>%
  kable("html", align = "c") %>%
  kable_styling("striped", full_width = TRUE)

fulgida_kruskal_table

#STEP 6: Determine statistical differences-------------------------------

output <- kruskalmc(mean_r ~ Temp, data = fulgida)

# Convert the output to a dataframe
output_df <- as.data.frame(output)

# Filter the output dataframe to include only rows where stat.signif is TRUE
significant_differences <-output_df %>% 
  filter(dif.com.stat.signif == "TRUE")

# Print the resulting dataframe
significant_differences


# Create a dataframe from the provided data
data <- data.frame(
  comparison = c("12-16", "12-18", "12-20", "12-22", "12-24", "14-24"),
  observation.diff = c("126.8583", "117.7083", "104.4583", "122.0833", "162.9000", "99.6500"),
  critical.diff = rep("67.33065", 6)
)

# Convert the dataframe to a table
table <- knitr::kable(data, format = "markdown",
                      col.names = c("Comparison", "Observational difference", "Critical difference"),
                      caption = "Statistical Test Results")

# Apply formatting to the table
table <- kable_styling(table, bootstrap_options = "striped", full_width = FALSE)

# Print the table
print(table)
