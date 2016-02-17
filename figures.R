### Figures for model match slides
### Kellie Ottoboni
### Last modified: 2/17/2016

source("simulation_tools.R")
set.seed(5)
report_theme <- theme(
  panel.background = element_rect(fill = "#E8EBEF"),
  strip.background = element_blank(),
  strip.text.x = element_blank(),
  axis.text = element_text(size = 12, color = "#143264"),
  axis.title = element_text(size = 14, color = "#143264"),
  title = element_text(color = "#143264")
)


### Estimation RMSE plot

gamma <- 1
estimates <- simulate_estimation(gamma, B = 1000, N = 100)
colnames(estimates) <- c("MM (Pairs)", "MM (5 Strata)",
                         "Pscore (Pairs)", "Pscore (5 Strata)",
                         "Entropy Balancing", "Unadjusted Estimate",
                         "Gamma")
plot_est <- plot_est_by_gamma(estimates) +
  ggtitle("Estimated Average Treatment Effect in 1000 Simulations \n Random Treatment Assignment") +
  xlab("") +
  report_theme

png("estimates.png", width = 800)
plot_est
dev.off()
