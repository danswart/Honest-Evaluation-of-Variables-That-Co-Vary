# Recreate the original simulated data
library(ggplot2)
library(dplyr)

set.seed(123)  # For reproducible results
ages <- 50:79
hrt_effect <- ifelse(ages <= 60,
                    rnorm(length(ages[ages <= 60]), mean = 0.2, sd = 0.1),  # Beneficial for younger
                    rnorm(length(ages[ages > 60]), mean = -0.3, sd = 0.1))  # Harmful for older

whi_demo <- data.frame(
  Age = ages,
  HRT_Effect = hrt_effect,
  Age_Group = ifelse(ages <= 60, "Younger Women (50-60)", "Older Women (61-79)")
)

# Create comprehensive ggplot2 visualizations showing Simpson's Paradox

# Plot 1: The main Simpson's Paradox visualization
p_paradox <- ggplot(whi_demo, aes(x = Age, y = HRT_Effect)) +
  # Overall regression line (ignoring age groups) - this is what the original study saw
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 1.5,
              linetype = "solid", alpha = 0.3) +
  # Points colored by age group
  geom_point(aes(color = Age_Group), size = 3, alpha = 0.8) +
  # Individual regression lines for each age group
  geom_smooth(aes(color = Age_Group), method = "lm", se = FALSE, linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.7) +
  scale_color_manual(values = c("#FF6B6B", "#74C0FC")) +
  labs(title = "Simpson's Paradox in Women's Health Initiative",
       subtitle = "Black line = Original study (age ignored), Colored lines = True relationships within age groups",
       x = "Age",
       y = "HRT Effect (positive = beneficial, negative = harmful)",
       color = "Age Group") +
  annotate("text", x = 75, y = 0.1, label = "Original Study:\n'HRT is harmful'",
           fontface = "bold", color = "black", size = 3.5) +
  annotate("text", x = 55, y = 0.35, label = "Reality: Beneficial\nfor younger women",
           fontface = "bold", color = "#FF6B6B", size = 3.5) +
  annotate("text", x = 70, y = -0.45, label = "Reality: Harmful\nfor older women",
           fontface = "bold", color = "#74C0FC", size = 3.5) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12, face = "bold"))

print(p_paradox)

# Plot 2: Side-by-side comparison showing the paradox more clearly
library(gridExtra)

# Create separate plots for each perspective
p_overall <- ggplot(whi_demo, aes(x = Age, y = HRT_Effect)) +
  geom_point(color = "gray40", size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Original Study View",
       subtitle = "Age ignored - HRT appears harmful",
       x = "Age", y = "HRT Effect") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", color = "red"))

p_disaggregated <- ggplot(whi_demo, aes(x = Age, y = HRT_Effect, color = Age_Group)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("#FF6B6B", "#74C0FC")) +
  labs(title = "Disaggregated View",
       subtitle = "Age considered - True relationships revealed",
       x = "Age", y = "HRT Effect", color = "Age Group") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", color = "darkgreen"),
        legend.position = "bottom")

# Combine the plots
combined_plot <- grid.arrange(p_overall, p_disaggregated, ncol = 2,
                             top = "Simpson's Paradox: Two Different Stories from the Same Data")
print(combined_plot)

# # Plot 3: Box plots showing mean effects by age group
# p_boxplot <- ggplot(whi_demo, aes(x = Age_Group, y = HRT_Effect, fill = Age_Group)) +
#   geom_boxplot(alpha = 0.7, width = 0.6) +
#   geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
#   geom_hline(yintercept = mean(whi_demo$HRT_Effect), linetype = "solid",
#              color = "red", linewidth = 1) +
#   scale_fill_manual(values = c("#FF6B6B", "#74C0FC")) +
#   labs(title = "HRT Effects by Age Group",
#        subtitle = "Red line = Overall mean (what original study saw)",
#        x = "Age Group", y = "HRT Effect",
#        fill = "Age Group") +
#   theme_minimal() +
#   theme(legend.position = "none",
#         plot.title = element_text(face = "bold"),
#         axis.title = element_text(face = "bold"))
#
# print(p_boxplot)

# Calculate the overall regression when age is ignored
overall_model <- lm(HRT_Effect ~ Age, data = whi_demo)
overall_slope <- coef(overall_model)[2]
overall_intercept <- coef(overall_model)[1]

# Calculate regression within each age group
younger_model <- lm(HRT_Effect ~ Age, data = whi_demo[whi_demo$Age <= 60, ])
older_model <- lm(HRT_Effect ~ Age, data = whi_demo[whi_demo$Age > 60, ])

# Summary table showing the paradox
paradox_summary <- data.frame(
  Analysis = c("Original Study (Age Ignored)", "Younger Women Only (50-60)", "Older Women Only (61-79)"),
  Sample_Size = c(nrow(whi_demo),
                  sum(whi_demo$Age <= 60),
                  sum(whi_demo$Age > 60)),
  Mean_Effect = c(round(mean(whi_demo$HRT_Effect), 3),
                  round(mean(whi_demo$HRT_Effect[whi_demo$Age <= 60]), 3),
                  round(mean(whi_demo$HRT_Effect[whi_demo$Age > 60]), 3)),
  Slope = c(round(overall_slope, 4),
            round(coef(younger_model)[2], 4),
            round(coef(older_model)[2], 4)),
  Conclusion = c("HRT appears harmful overall",
                 "HRT beneficial for younger women",
                 "HRT harmful for older women"),
  stringsAsFactors = FALSE
)

print("Simpson's Paradox Summary:")
print(paradox_summary)

# The key insight:
cat("\n=== THE PARADOX EXPLAINED ===\n")
cat("Overall slope (ignoring age):", round(overall_slope, 4), "\n")
cat("This suggests HRT effect", ifelse(overall_slope > 0, "improves", "worsens"), "with age\n")
cat("But within each age group, the relationship may be different!\n\n")

cat("Why this happens:\n")
cat("- More older women (", sum(whi_demo$Age > 60), ") than younger women (", sum(whi_demo$Age <= 60), ") in study\n")
cat("- Older women have much worse outcomes (mean effect:", round(mean(whi_demo$HRT_Effect[whi_demo$Age > 60]), 3), ")\n")
cat("- This dominates the overall trend, masking the beneficial effect in younger women\n")
