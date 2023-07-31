library(readxl)
library(dplyr)
library(rstatix)
library(ggplot2) # Optional for data visualization
library(ggpubr)

Df <- read_excel("~/Ergonomics Lab (PERROS 69)/CK_Lactate.xlsx",
                 sheet = "Cortisol")
View(Df)


# Assuming 'Df' is your data frame with the columns "ID", "Time", "Condition", "Workload", and "Lactate"
# Convert the relevant columns to factors with specified levels
Df$Time <- factor(Df$Time, levels = c("PRE", "POST"))
Df$Condition <- factor(Df$Condition, levels = c("S", "NS"))
Df$Workload <- factor(Df$Workload, levels = c("25", "50", "75"))

library(dplyr)
library(broom)

# Filter the data for each condition
data_S <- Df %>% filter(Condition == "S")
data_NS <- Df %>% filter(Condition == "NS")

# Assuming you have already filtered the data for "POST" results in the "S" and "NS" conditions
# If not, you can filter the data first
filtered_data_S <- Df %>% filter(Condition == "S" & Time == "POST")
filtered_data_NS <- Df %>% filter(Condition == "NS" & Time == "POST")

# Perform the ANOVA for each condition
anova_S <- aov(Concentration ~ Workload, data = filtered_data_S)
anova_NS <- aov(Concentration ~ Workload, data = filtered_data_NS)

# Print the ANOVA results
print(summary(anova_S))
print(summary(anova_NS))

# Create a combined data frame for plotting
combined_data <- rbind(filtered_data_S, filtered_data_NS)

# Bar plot for "POST" results in the "S" and "NS" conditions
bar_plot <- ggplot(combined_data, aes(x = Workload, y = Concentration, fill = Condition)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  labs(x = "Workload", y = "Concentration", fill = "Condition", 
       title = "Comparison of Cortisol Concentration Levels at Different Workloads") +
  theme_minimal()

# Print the plot
print(bar_plot)


###

# Bar plot for "POST" results in the "S" and "NS" conditions
bar_plot <- ggplot(combined_data, aes(x = Workload, y = Concentration, fill = Condition)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  labs(x = "Workload (W)", y = "Concentration (mcg/dL)", fill = "Condition", 
       title = "Comparison of Cortisol Concentration Levels at Different Workloads") +
  theme_minimal()

# Modify the y-axis limits to include negative values
bar_plot <- bar_plot + scale_y_continuous(limits = c(-100, 2000))

# Print the plot
print(bar_plot)
