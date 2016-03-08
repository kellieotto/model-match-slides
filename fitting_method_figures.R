### Figures for QE model match slides
### Kellie Ottoboni
### Last modified: 3/08/2016

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



### Estimation comparison
B <- 1000
N <- 100
mmcomp <- simulate_estimation_vary_mm(gamma = 1, B, N, selection = "correlated", nu = 1)

png("fig/est_by_fit_method.png", width = 900)
plot_est_by_gamma(mmcomp, fill_by_fitmethod = TRUE) +
  ggtitle("Estimates of Constant Additive Treatment Effect with Cov(T, X_1) = 1") +
  theme(axis.text.x = element_text(angle = 35, hjust = 1),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  xlab("") +
  report_theme
dev.off()


### Testing comparison
gamma <- c(0, 0.25, 0.5)
B <- 1000
N <- 100

mmcomp_test <- simulate_tests_which_residuals(gamma, B, N, selection = "correlated", nu = 1,
                                              refit_method = FALSE)
png("fig/power_by_fit_method.png", width = 900)
plot_power_curves_which_residuals(mmcomp_test)+
  ggtitle("Power Curves for Varying Magnitude of Treatment Effects, Cov(T, X_1) = 1") +
  geom_abline(intercept=0, slope=1, linetype = "dashed") +
  report_theme +
  guides(color=guide_legend(nrow=2,byrow=TRUE))
dev.off()
