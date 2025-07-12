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

# Create the plot showing Simpson's Paradox
p_paradox <- ggplot(whi_demo, aes(x = Age, y = HRT_Effect)) +
  # Overall regression line (ignoring age groups) - this is what the original study saw
  geom_smooth(method = "lm", se = TRUE, color = "black", size = 1.5,
              linetype = "solid", alpha = 0.3) +
  # Points colored by age group
  geom_point(aes(color = Age_Group), size = 3, alpha = 0.8) +
  # Individual regression lines for each age group
  geom_smooth(aes(color = Age_Group), method = "lm", se = FALSE, size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("#FF6B6B", "#74C0FC")) +
  labs(title = "Simpson's Paradox in Women's Health Initiative",
       subtitle = "Black line = Original study (age ignored), Colored lines = True relationships within age groups",
       x = "Age",
       y = "HRT Effect (positive = beneficial, negative = harmful)",
       color = "Age Group") +
  annotate("text", x = 75, y = 0.1, label = "Original Study:\n'HRT is harmful'",
           fontface = "bold", color = "black", size = 3) +
  annotate("text", x = 55, y = 0.35, label = "Reality: Beneficial\nfor younger women",
           fontface = "bold", color = "#FF6B6B", size = 3) +
  annotate("text", x = 70, y = -0.45, label = "Reality: Harmful\nfor older women",
           fontface = "bold", color = "#74C0FC", size = 3) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_paradox)

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
