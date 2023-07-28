library(readxl)
library(dplyr)
library(rstatix)
library(ggplot2) # Optional for data visualization
library(ggpubr)

Df <- read_excel("~/Ergonomics Lab (PERROS 69)/CK_Lactate.xlsx",
                 sheet = "Lactate")
View(Df)

# Assuming 'Df' is your data frame with the columns "ID", "Time", "Condition", "Workload", and "Lactate"
# Convert the relevant columns to factors with specified levels
Df$Time <- factor(Df$Time, levels = c("PRE", "POST"))
Df$Condition <- factor(Df$Condition, levels = c("S", "NS"))
Df$Workload <- factor(Df$Workload, levels = c("25", "50", "75"))

# Perform ANOVA using "Lactate" as the dependent variable
anova_model <- aov(Lactate ~ Time * Condition * Workload, data = Df)

# Obtain ANOVA table
anova_results <- anova(anova_model)

# Print ANOVA results
print(anova_results)

# Post-hoc tests (if ANOVA is significant)
posthoc_results <- TukeyHSD(anova_model, "Time:Workload")

# View post-hoc results
print(posthoc_results)



# Bar plot
bar_plot <- ggplot(Df, aes(x = Workload, y = Lactate, fill = Time)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  facet_grid(. ~ Condition) +
  labs(x = "Workload", y = "Lactate", fill = "Time", title = "Bar Plot: Lactate by Time, Workload, and Condition") +
  theme_minimal()

# Print the plot
print(bar_plot)

# Interaction plot
interaction_plot <- ggplot(Df, aes(x = Workload, y = Lactate, group = Time, color = Time)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(x = "Workload", y = "Lactate", color = "Time", title = "Interaction Plot: Lactate by Time and Workload") +
  theme_minimal()

# Print the plot
print(interaction_plot)



# Bar plot
bar_plot <- ggplot(Df, aes(x = Workload, y = Lactate, fill = Time)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  facet_grid(. ~ Condition) +
  labs(x = "Workload", y = "Lactate", fill = "Time", title = "Bar Plot: Lactate by Time, Workload, and Condition") +
  theme_minimal()

# Perform ANOVA using "Lactate" as the dependent variable
anova_model <- aov(Lactate ~ Time * Condition * Workload, data = Df)

# Post-hoc tests (if ANOVA is significant)
posthoc_results <- TukeyHSD(anova_model, "Time:Workload")

# Extract significant comparisons
signif_comparisons <- posthoc_results$`Time:Workload` %>%
  filter(p.adj < 0.05)


# Perform ANOVA using "Lactate" as the dependent variable
anova_model <- aov(Lactate ~ Time * Condition * Workload, data = Df)

# Post-hoc tests (if ANOVA is significant)
posthoc_results <- TukeyHSD(anova_model, "Time:Workload")

# Extract significant comparisons
signif_comparisons <- posthoc_results$`Time:Workload` %>%
  filter(p.adj < 0.05)

# Bar plot with significance annotations
bar_plot <- ggplot(Df, aes(x = Workload, y = Lactate, fill = Time)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  facet_grid(. ~ Condition) +
  labs(x = "Workload", y = "Lactate", fill = "Time", title = "Bar Plot: Lactate by Time, Workload, and Condition") +
  theme_minimal() +
  stat_compare_means(
    aes(label = ..p.format..),
    comparisons = list(c("POST", "PRE")),
    method = "t.test",
    step.increase = 0.1,
    tip.length = 0.01
  ) +
  geom_text(data = signif_comparisons, aes(x = Workload, y = ymax + 0.2, label = ..p.signif..),
            position = position_dodge(width = 0.7))

# Print the plot
print(bar_plot)


##try2
# Perform ANOVA using "Lactate" as the dependent variable
anova_model <- aov(Lactate ~ Time * Condition * Workload, data = Df)

# Post-hoc tests (if ANOVA is significant)
posthoc_results <- TukeyHSD(anova_model, "Time:Workload")

# Extract significant comparisons
posthoc_summary <- summary(posthoc_results)
signif_comparisons <- posthoc_summary$`Time:Workload` %>%
  filter(p.adj < 0.05)

# Bar plot with significance annotations
bar_plot <- ggplot(Df, aes(x = Workload, y = Lactate, fill = Time)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  facet_grid(. ~ Condition) +
  labs(x = "Workload", y = "Lactate", fill = "Time", title = "Bar Plot: Lactate by Time, Workload, and Condition") +
  theme_minimal() +
  stat_compare_means(
    aes(label = ..p.format..),
    comparisons = list(c("POST", "PRE")),
    method = "t.test",
    step.increase = 0.1,
    tip.length = 0.01
  ) +
  geom_text(data = signif_comparisons, aes(x = Workload, y = ymax + 0.2, label = ..p.signif..),
            position = position_dodge(width = 0.7))

# Print the plot
print(bar_plot)



##try##
# Perform ANOVA using "Lactate" as the dependent variable
anova_model <- aov(Lactate ~ Time * Condition * Workload, data = Df)

# Post-hoc tests (if ANOVA is significant)
posthoc_results <- TukeyHSD(anova_model, "Time:Workload")

# Get the p-values for the post-hoc comparisons
p_values <- posthoc_results$"Time:Workload"


# Convert the matrix of p-values to a data frame
posthoc_summary <- as.data.frame(p_values)

# Add a column for comparison names (optional but helpful for interpretation)
posthoc_summary$Comparison <- rownames(posthoc_summary)

# Filter significant comparisons
signif_comparisons <- posthoc_summary %>%
  filter(`p adj` < 0.05)

# Bar plot with significance annotations
bar_plot <- ggplot(Df, aes(x = Workload, y = Lactate, fill = Time)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  facet_grid(. ~ Condition) +
  labs(x = "Workload", y = "Lactate", fill = "Time", title = "Bar Plot: Lactate by Time, Workload, and Condition") +
  theme_minimal() +
  stat_compare_means(
    aes(label = ..p.format..),
    comparisons = list(c("POST", "PRE")),
    method = "t.test",
    step.increase = 0.1,
    tip.length = 0.01
  ) +
  geom_text(data = signif_comparisons, aes(x = Workload, y = ymax + 0.2, label = Comparison),
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3)

# Print the plot
print(bar_plot)


##try3##


# Check the column names in the data frame
colnames(Df)

# Check the data type of the "Workload" column
class(Df$Workload)
# Perform ANOVA using "Lactate" as the dependent variable
anova_model <- aov(Lactate ~ Time * Condition * Workload, data = Df)

# Post-hoc tests (if ANOVA is significant)
posthoc_results <- TukeyHSD(anova_model, "Time:Workload")

# Extract the p-values from the posthoc_results for the "Time:Workload" interaction
p_values <- posthoc_results[[1]][, "p adj"]

# Create a data frame to store the posthoc results and p-values
signif_comparisons <- data.frame(Comparison = rownames(posthoc_results[[1]]), p.adj = p_values)

# Filter significant comparisons (p-values < 0.05)
signif_comparisons <- signif_comparisons %>%
  filter(p.adj < 0.05)

# Create a data frame with unique Workload values and corresponding x positions
workload_positions <- data.frame(
  Workload = factor(unique(Df$Workload), levels = levels(Df$Workload)),
  x_pos = 1:length(unique(Df$Workload))
)

unique(Df$Workload)
unique(signif_comparisons$Workload)


levels(Df$Workload)
levels(signif_comparisons$Workload)

sum(is.na(signif_comparisons$Workload))

# Check the posthoc_results data frame
print(posthoc_results)

# Bar plot with significance annotations
bar_plot <- ggplot(Df, aes(x = Workload, y = Lactate, fill = Time)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  facet_grid(. ~ Condition) +
  labs(x = "Workload", y = "Lactate", fill = "Time", title = "Bar Plot: Lactate by Time, Workload, and Condition") +
  theme_minimal() +
  # Add significance annotations for significant comparisons only
  geom_text(data = posthoc_results$`Time:Workload`, aes(x = Workload, y = max(Df$Lactate, na.rm = TRUE) + 0.2, label = ifelse(p.adj < 0.05, "*", "")),
            position = position_dodge(width = 0.7), vjust = -0.5, size = 5)

# Print the plot
print(bar_plot)



# Convert the Workload column in signif_comparisons to a factor with the same levels as in Df
signif_comparisons$Workload <- factor(signif_comparisons$Workload, levels = levels(Df$Workload))

# Perform a left join to merge the two data frames based on the Workload variable
signif_comparisons <- dplyr::left_join(signif_comparisons, workload_positions, by = "Workload")

# Bar plot with significance annotations using annotate function
bar_plot <- ggplot(Df, aes(x = Workload, y = Lactate, fill = Time)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  facet_grid(. ~ Condition) +
  labs(x = "Workload", y = "Lactate", fill = "Time", title = "Bar Plot: Lactate by Time, Workload, and Condition") +
  theme_minimal() +
  stat_compare_means(
    aes(label = ..p.format..),
    comparisons = list(c("POST", "PRE")),
    method = "t.test",
    step.increase = 0.1,
    tip.length = 0.01
  ) +
  # Add significance annotations using annotate function for significant comparisons only
  annotate("text", data = signif_comparisons,
           aes(x = x_pos, y = max(Df$Lactate, na.rm = TRUE) + 0.2,
               label = Comparison), vjust = -0.5, size = 3, position = position_dodge(width = 0.7))

# Print the plot
print(bar_plot)



##try 5

# Convert the matrix to a data frame
posthoc_df <- as.data.frame(posthoc_results$`Time:Workload`)

# Filter significant comparisons
signif_comparisons <- posthoc_df %>%
  filter(`p adj` < 0.05)

# Bar plot with significance annotations for significant comparisons only
bar_plot <- ggplot(Df, aes(x = Workload, y = Lactate, fill = Time)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  facet_grid(. ~ Condition) +
  labs(x = "Workload", y = "Lactate", fill = "Time", title = "Bar Plot: Lactate by Time, Workload, and Condition") +
  theme_minimal() +
  # Add significance annotations for significant comparisons only
  geom_text(data = signif_comparisons, aes(x = Workload, y = max(Df$Lactate, na.rm = TRUE) + 0.2, label = "*"),
            position = position_dodge(width = 0.7), vjust = -0.5, size = 5)

# Print the plot
print(bar_plot)


# Filter significant comparisons
signif_comparisons <- posthoc_df %>%
  filter(`p adj` < 0.05)

# Bar plot with significance annotations for significant comparisons only
bar_plot <- ggplot(Df, aes(x = Workload, y = Lactate, fill = Time)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  facet_grid(. ~ Condition) +
  labs(x = "Workload", y = "Lactate", fill = "Time", title = "Bar Plot: Lactate by Time, Workload, and Condition") +
  theme_minimal() +
  # Add significance annotations for significant comparisons only
  geom_text(data = signif_comparisons, aes(x = Workload, y = ymax + 0.2, label = "*"),
            position = position_dodge(width = 0.7), vjust = -0.5, size = 5)

# Print the plot
print(bar_plot)


# Convert the Workload column in signif_comparisons to a factor with the same levels as in Df
signif_comparisons$Workload <- factor(signif_comparisons$Workload, levels = levels(Df$Workload))

# Bar plot with significance annotations for significant comparisons only
bar_plot <- ggplot(Df, aes(x = Workload, y = Lactate, fill = Time)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  facet_grid(. ~ Condition) +
  labs(x = "Workload", y = "Lactate", fill = "Time", title = "Bar Plot: Lactate by Time, Workload, and Condition") +
  theme_minimal() +
  # Add significance annotations using annotate function for significant comparisons only
  annotate("text", data = signif_comparisons, x = Workload, y = max(Df$Lactate, na.rm = TRUE) + 0.2, label = "*", 
           position = position_dodge(width = 0.7), vjust = -0.5, size = 5)

# Print the plot
print(bar_plot)

######
# Perform ANOVA using "Lactate" as the dependent variable
anova_model <- aov(Lactate ~ Time * Condition * Workload, data = Df)

# Post-hoc tests (if ANOVA is significant)
posthoc_results <- TukeyHSD(anova_model, "Time:Workload")

# Extract the p-values from the posthoc_results for the "Time:Workload" interaction
p_values <- posthoc_results$`Time:Workload`[, "p adj"]

# Create a data frame to store the posthoc results and p-values
posthoc_df <- data.frame(
  Comparison = rownames(posthoc_results$`Time:Workload`),
  p.adj = p_values
)

# Filter significant comparisons
signif_comparisons <- posthoc_df %>%
  filter(`p.adj` < 0.05)

# Bar plot with significance annotations for significant comparisons only
bar_plot <- ggplot(Df, aes(x = Workload, y = Lactate, fill = Time)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  facet_grid(. ~ Condition) +
  labs(x = "Workload", y = "Lactate", fill = "Time", title = "Bar Plot: Lactate by Time, Workload, and Condition") +
  theme_minimal() +
  # Add significance annotations using annotate function for significant comparisons only
  annotate("text", data = signif_comparisons, x = Workload, y = max(Df$Lactate, na.rm = TRUE) + 0.2, label = "*", 
           position = position_dodge(width = 0.7), vjust = -0.5, size = 5)

# Print the plot
print(bar_plot)

##try8

# Perform ANOVA using "Lactate" as the dependent variable
anova_model <- aov(Lactate ~ Time * Condition * Workload, data = Df)

# Post-hoc tests (if ANOVA is significant)
posthoc_results <- TukeyHSD(anova_model, "Time:Workload")

# Extract the p-values from the posthoc_results for the "Time:Workload" interaction
p_values <- posthoc_results$`Time:Workload`[, "p adj"]

# Create a data frame to store the posthoc results and p-values
posthoc_df <- data.frame(
  Comparison = rownames(posthoc_results$`Time:Workload`),
  p.adj = p_values
)

# Filter significant comparisons
signif_comparisons <- posthoc_df %>%
  filter(`p.adj` < 0.05)

# Convert the Workload column in signif_comparisons to a factor with the same levels as in Df
signif_comparisons$Workload <- factor(signif_comparisons$Comparison, levels = levels(Df$Workload))

# Bar plot with significance annotations for significant comparisons only
bar_plot <- ggplot(Df, aes(x = Workload, y = Lactate, fill = Time)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  facet_grid(. ~ Condition) +
  labs(x = "Workload", y = "Lactate", fill = "Time", title = "Bar Plot: Lactate by Time, Workload, and Condition") +
  theme_minimal() +
  # Add significance annotations using annotate function for significant comparisons only
  annotate("text", data = signif_comparisons, aes(x = Workload, y = max(Lactate, na.rm = TRUE) + 0.2, label = "*"), 
           position = position_dodge(width = 0.7), vjust = -0.5, size = 5)

# Print the plot
print(bar_plot)

###try10

# Perform ANOVA using "Lactate" as the dependent variable
anova_model <- aov(Lactate ~ Time * Condition * Workload, data = Df)

# Post-hoc tests (if ANOVA is significant)
posthoc_results <- TukeyHSD(anova_model, "Time:Workload")

# Extract the p-values from the posthoc_results for the "Time:Workload" interaction
p_values <- posthoc_results$`Time:Workload`[, "p adj"]

# Create a data frame to store the posthoc results and p-values
posthoc_df <- data.frame(
  Comparison = rownames(posthoc_results$`Time:Workload`),
  p.adj = p_values
)

# Filter significant comparisons
signif_comparisons <- posthoc_df %>%
  filter(`p.adj` < 0.05)

# Convert the Workload column in signif_comparisons to a factor with the same levels as in Df
signif_comparisons$Workload <- factor(signif_comparisons$Comparison, levels = levels(Df$Workload))

# Bar plot with significance annotations for significant comparisons only
bar_plot <- ggplot(Df, aes(x = Workload, y = Lactate, fill = Time)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  facet_grid(. ~ Condition) +
  labs(x = "Workload", y = "Lactate", fill = "Time", title = "Bar Plot: Lactate by Time, Workload, and Condition") +
  theme_minimal()

# Add significance annotations using geom_text() for significant comparisons only
bar_plot <- bar_plot +
  geom_text(data = signif_comparisons, aes(x = Workload, y = max(Df$Lactate, na.rm = TRUE) + 0.2, label = "*"), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 5)

# Print the plot
print(bar_plot)


###try12

# Perform ANOVA using "Lactate" as the dependent variable
anova_model <- aov(Lactate ~ Time * Condition * Workload, data = Df)

# Post-hoc tests (if ANOVA is significant)
posthoc_results <- TukeyHSD(anova_model, "Time:Workload")

# Extract the p-values from the posthoc_results for the "Time:Workload" interaction
p_values <- posthoc_results$`Time:Workload`[, "p adj"]

# Create a data frame to store the posthoc results and p-values
posthoc_df <- data.frame(
  Comparison = rownames(posthoc_results$`Time:Workload`),
  p.adj = p_values
)

# Filter significant comparisons
signif_comparisons <- posthoc_df %>%
  filter(`p.adj` < 0.05)

# Convert the Workload column in signif_comparisons to a factor with the same levels as in Df
signif_comparisons$Workload <- factor(signif_comparisons$Comparison, levels = levels(Df$Workload))

# Bar plot with significance annotations for significant comparisons only
bar_plot <- ggplot(Df, aes(x = Workload, y = Lactate, fill = Time)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  facet_grid(. ~ Condition) +
  labs(x = "Workload", y = "Lactate", fill = "Time", title = "Bar Plot: Lactate by Time, Workload, and Condition") +
  theme_minimal()

# Add significance annotations using geom_text() for significant comparisons only
bar_plot <- bar_plot +
  geom_text(data = signif_comparisons, aes(x = Workload, y = max(Df$Lactate, na.rm = TRUE) + 0.2, label = "*"), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 5)

# Print the plot
print(bar_plot)


###
# Perform ANOVA using "Lactate" as the dependent variable
anova_model <- aov(Lactate ~ Time * Condition * Workload, data = Df)

# Post-hoc tests (if ANOVA is significant)
posthoc_results <- TukeyHSD(anova_model, "Time:Workload")

# Extract the p-values from the posthoc_results for the "Time:Workload" interaction
p_values <- posthoc_results$`Time:Workload`[, "p adj"]

# Create a data frame to store the posthoc results and p-values
posthoc_df <- data.frame(
  Comparison = rownames(posthoc_results$`Time:Workload`),
  p.adj = p_values
)

# Filter significant comparisons
signif_comparisons <- posthoc_df %>%
  filter(`p.adj` < 0.05)

# Convert the Workload column in signif_comparisons to a factor with the same levels as in Df
signif_comparisons$Workload <- factor(signif_comparisons$Comparison, levels = levels(Df$Workload))

# Bar plot with significance annotations for significant comparisons only
bar_plot <- ggplot(Df, aes(x = Workload, y = Lactate, fill = Time)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  facet_grid(. ~ Condition) +
  labs(x = "Workload", y = "Lactate", fill = "Time", title = "Bar Plot: Lactate by Time, Workload, and Condition") +
  theme_minimal()

# Add significance annotations using geom_text() for significant comparisons only
bar_plot <- bar_plot +
  geom_text(data = signif_comparisons, aes(x = Workload, y = max(Df$Lactate, na.rm = TRUE) + 0.2, label = "*"), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 5)

# Print the plot
print(bar_plot)


####
# Perform ANOVA using "Lactate" as the dependent variable
anova_model <- aov(Lactate ~ Time * Condition * Workload, data = Df)

# Post-hoc tests (if ANOVA is significant)
posthoc_results <- TukeyHSD(anova_model, "Time:Workload")

# Extract the p-values from the posthoc_results for the "Time:Workload" interaction
p_values <- posthoc_results$`Time:Workload`[, "p adj"]

# Create a data frame to store the posthoc results and p-values
posthoc_df <- data.frame(
  Comparison = rownames(posthoc_results$`Time:Workload`),
  p.adj = p_values
)

# Filter significant comparisons
signif_comparisons <- posthoc_df %>%
  filter(`p.adj` < 0.05)

# Convert the Workload column in signif_comparisons to a factor with the same levels as in Df
signif_comparisons$Workload <- factor(signif_comparisons$Comparison, levels = levels(Df$Workload))

# Bar plot with significance annotations for significant comparisons only
bar_plot <- ggplot(Df, aes(x = Workload, y = Lactate, fill = Time)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  facet_grid(. ~ Condition) +
  labs(x = "Workload", y = "Lactate", fill = "Time", title = "Bar Plot: Lactate by Time, Workload, and Condition") +
  theme_minimal()

# Add significance annotations using geom_text() for significant comparisons only
bar_plot <- bar_plot +
  geom_text(data = signif_comparisons, aes(x = Workload, y = max(Df$Lactate, na.rm = TRUE) + 0.2, label = "*"), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 5)

# Print the plot
print(bar_plot)

####
# Perform ANOVA using "Lactate" as the dependent variable
anova_model <- aov(Lactate ~ Time * Condition * Workload, data = Df)

# Post-hoc tests (if ANOVA is significant)
posthoc_results <- TukeyHSD(anova_model, "Time:Workload")

# Extract the p-values from the posthoc_results for the "Time:Workload" interaction
p_values <- posthoc_results$`Time:Workload`[, "p adj"]

# Create a data frame to store the posthoc results and p-values
posthoc_df <- data.frame(
  Comparison = rownames(posthoc_results$`Time:Workload`),
  p.adj = p_values
)

# Filter significant comparisons
signif_comparisons <- posthoc_df %>%
  filter(`p.adj` < 0.05)

# Convert the Workload column in signif_comparisons to a factor with the same levels as in Df
signif_comparisons$Workload <- factor(signif_comparisons$Comparison, levels = levels(Df$Workload))

# Bar plot with significance annotations for significant comparisons only
bar_plot <- ggplot(Df, aes(x = Workload, y = Lactate, fill = Time)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  facet_grid(. ~ Condition) +
  labs(x = "Workload", y = "Lactate", fill = "Time", title = "Bar Plot: Lactate by Time, Workload, and Condition") +
  theme_minimal()

# Add significance annotations using annotate() for significant comparisons only
bar_plot <- bar_plot +
  annotate("text", x = signif_comparisons$Workload, y = max(Df$Lactate, na.rm = TRUE) + 0.2, 
           label = "*", position = position_dodge(width = 0.7), vjust = -0.5, size = 5)

# Print the plot
print(bar_plot)


###

# Perform ANOVA using "Lactate" as the dependent variable
anova_model <- aov(Lactate ~ Time * Condition * Workload, data = Df)

# Post-hoc tests (if ANOVA is significant)
posthoc_results <- TukeyHSD(anova_model, "Time:Workload")

# Extract the p-values from the posthoc_results for the "Time:Workload" interaction
p_values <- posthoc_results$`Time:Workload`[, "p adj"]

# Create a data frame to store the posthoc results and p-values
posthoc_df <- data.frame(
  Comparison = rownames(posthoc_results$`Time:Workload`),
  p.adj = p_values
)

# Filter significant comparisons
signif_comparisons <- posthoc_df %>%
  filter(`p.adj` < 0.05)

# Convert the Workload column in signif_comparisons to a factor with the same levels as in Df
signif_comparisons$Workload <- factor(signif_comparisons$Comparison, levels = levels(Df$Workload))

# Bar plot with significance annotations for significant comparisons only
bar_plot <- ggplot(Df, aes(x = Workload, y = Lactate, fill = Time)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  facet_grid(. ~ Condition) +
  labs(x = "Workload", y = "Lactate", fill = "Time", title = "Bar Plot: Lactate by Time, Workload, and Condition") +
  theme_minimal()

# Create a data frame to store the positions of the workloads for annotation
workload_positions <- data.frame(
  Workload = factor(unique(Df$Workload), levels = levels(Df$Workload)),
  x_pos = 1:length(unique(Df$Workload))
)

# Add significance annotations using annotate() for significant comparisons only
bar_plot <- bar_plot +
  annotate("text", x = workload_positions$x_pos, y = max(Df$Lactate, na.rm = TRUE) + 0.2,
           label = ifelse(workload_positions$Workload %in% signif_comparisons$Workload, "*", ""),
           position = position_dodge(width = 0.7), vjust = -0.5, size = 5)

# Print the plot
print(bar_plot)

####
# Bar plot with significance annotations for significant comparisons only
bar_plot <- ggplot(Df, aes(x = Workload, y = Lactate, fill = Time)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  facet_grid(. ~ Condition) +
  labs(x = "Workload", y = "Lactate", fill = "Time", title = "Bar Plot: Lactate by Time, Workload, and Condition") +
  theme_minimal()

# Create a data frame to store the positions of the workloads for annotation
workload_positions <- data.frame(
  Workload = factor(unique(Df$Workload), levels = levels(Df$Workload)),
  x_pos = 1:length(unique(Df$Workload))
)

# Add significance annotations using annotate() for significant comparisons only
bar_plot <- bar_plot +
  annotate("text", x = workload_positions$x_pos[workload_positions$Workload %in% signif_comparisons$Workload], 
           y = max(Df$Lactate, na.rm = TRUE) + 0.2,
           label = "*", vjust = -0.5, size = 5)

# Print the plot
print(bar_plot)

