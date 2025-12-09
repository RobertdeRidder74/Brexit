# Maak een boxplot van de spreiding in june_polls per type peiling (poll type).
library(ggplot2)

ggplot(june_polls, aes(x = poll_type, y = spread)) +
  geom_boxplot() +
  labs(
    title = "Boxplot van de spreiding per type peiling (juni 2016)",
    x = "Type peiling",
    y = "Spreiding"
  ) +
  theme_minimal()

# Plot van spread over tijd, gekleurd naar poll_type
brexit_polls %>%
  ggplot(aes(x = enddate, y = spread, color = poll_type)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", span = 0.4, se = FALSE) +
  geom_hline(yintercept = -0.038, linetype = "dashed", color = "black") +
  labs(title = "Spread over tijd per type peiling",
       x = "Datum",
       y = "Spread (Remain - Leave)",
       color = "Type peiling") +
  theme_minimal()

# Plot met confidence bands rond de curves
brexit_long %>%
  ggplot(aes(x = enddate, y = proportion, color = vote)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", span = 0.3, se = TRUE) +  # se=TRUE voegt 95% BI toe
  labs(title = "Ruwe percentages over tijd met 95% BI",
       x = "Datum",
       y = "Proportie",
       color = "Stemoptie") +
  theme_minimal()