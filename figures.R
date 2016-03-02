### Figures for model match slides
### Kellie Ottoboni
### Last modified: 2/17/2016

source("simulation_tools.R")
set.seed(5)
report_theme <- theme(
  panel.background = element_rect(fill = "#E8EBEF"),
  axis.text = element_text(size = 14, color = "#143264"),
  axis.title = element_text(size = 16, color = "#143264"),
  title = element_text(color = "#143264"),
  legend.title = element_text(color = "#143264", size = 14),
  legend.text = element_text(color = "#143264", size = 14)
)


### Estimation RMSE plot

gamma <- 1
estimates <- simulate_estimation(gamma, B = 1000, N = 100)
colnames(estimates) <- c("MM (Pairs)", "MM (5 Strata)",
                         "Pscore (Pairs)", "Pscore (5 Strata)",
                         "Entropy \n Balancing", "Unadjusted\n Estimate",
                         "Gamma")
plot_est <- plot_est_by_gamma(estimates, color_mm = TRUE) +
  ggtitle("Estimated Average Treatment Effect in 1000 Simulations \n Random Treatment Assignment") +
  xlab("") +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  report_theme

png("fig/estimates.png", width = 1000)
plot_est
dev.off()


### Testing power

gamma <- c(0, 2, 5)
pvalues <- simulate_tests_nonconstant(gamma, B = 1000, N = 100)
colnames(pvalues) <- c("MM (2 Strata)", "MM (5 Strata)", "Wilcoxon", "OLS", "Gamma")
pvalues$Gamma <- paste("Effect Magnitude", pvalues$Gamma)
pvalues$Gamma[pvalues$Gamma == "Effect Magnitude 0"] <- "No Effect"
plot_power <- plot_power_curves(pvalues) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#143264") +
  ggtitle("Power Comparison in 1000 Simulations \n Random Treatment Assignment, Sign of Effect Correlated with Covariate") +
  report_theme +
  theme(strip.background = element_rect(fill = "#FFFFFF"),
        strip.text.x = element_text(color = "#143264", size = 12),
        axis.text.x = element_text(angle = 0)) +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c(0, 0.25, 0.5, 0.75, 1))

png("fig/power.png", width = 900)
plot_power
dev.off()
