### Last edited: February 25, 2016
### Kellie Ottoboni
### Test for association between change in Na+ consumption and change in life expectancy, from 1990 to 2010

set.seed(38)
library(dplyr)
library(Hmisc)
setwd("~/Documents/Salt/Data")
source("../Code/modeling.R")
library(ModelMatch)
report_theme <- theme(
  panel.background = element_rect(fill = "#E8EBEF"),
  axis.text = element_text(size = 14, color = "#143264"),
  axis.title = element_text(size = 16, color = "#143264"),
  title = element_text(color = "#143264"),
  legend.title = element_text(color = "#143264", size = 14),
  legend.text = element_text(color = "#143264", size = 14)
)

########################################## Data prep ########################################## 

salt <- read.table("omnibus_data.csv", sep = "\t", header = TRUE)
smoke <- read.table("smoking_t14.csv", sep = ",", header = TRUE)
smoke <- dplyr::select(smoke, country_name_p, annual_pc_smoking_1990, annual_pc_smoking_2010)


salt <- mutate(salt, popF_prop = popF/(popF + popM), popM_prop = popM/(popF + popM))
salt$popF_prop[is.na(salt$popF)] <- 0.5; salt$popM_prop[is.na(salt$popM)] <- 0.5
salt$etohboth <- ifelse(is.na(salt$etohboth), salt$popM_prop*salt$etohM + salt$popF_prop*salt$etohF, salt$etohboth)

salt <- arrange(salt, country, year)
salt_filt <- dplyr::select(salt, country, country_name_p, e0_M, e0_F, year, Na_M, Na_F, etohboth, etohM, etohF, pc_gdp, popM_prop, popF_prop)

salt2010 <- filter(salt_filt, year == 2010)
salt2010 <- merge(smoke, salt2010)
salt2010 <- dplyr::select(salt2010, -annual_pc_smoking_1990, -country_name_p) %>% mutate(smoking = annual_pc_smoking_2010) %>% dplyr::select(-annual_pc_smoking_2010)
etohM_mod <- lm(etohM~ e0_M + e0_F + Na_M + Na_F + pc_gdp + smoking, data = salt2010)
etohF_mod <- lm(etohF~ e0_M + e0_F + Na_M + Na_F + pc_gdp + smoking, data = salt2010)
twn <- which(is.na(salt2010$etohM))
salt2010[twn, "etohM"] <- predict(etohM_mod, salt2010[twn,]); salt2010[twn, "etohF"] <- predict(etohF_mod, salt2010[twn,])
salt2010[twn, "etohboth"] <- (salt2010$popM_prop*salt2010$etohM + salt2010$popF_prop*salt2010$etohF)[twn]

salt1990 <- filter(salt_filt, year == 1990)
salt1990 <- merge(smoke, salt1990)
salt1990 <- dplyr::select(salt1990, -annual_pc_smoking_2010, -country_name_p)  %>% mutate(smoking = annual_pc_smoking_1990) %>% dplyr::select(-annual_pc_smoking_1990)
etohboth_mod <- lm(etohboth ~ e0_M + e0_F + Na_M + Na_F + pc_gdp + smoking, data = salt1990)
twn <- which(is.na(salt1990$etohboth))
salt1990[twn, "etohboth"] <- predict(etohboth_mod, salt1990[twn,])
salt1990$etohM     <- salt1990$etohboth * (salt2010$etohM)/(salt2010$popM_prop*salt2010$etohM+salt2010$popF_prop*salt2010$etohF)
salt1990$etohF     <- salt1990$etohboth * (salt2010$etohF)/(salt2010$popM_prop*salt2010$etohM+salt2010$popF_prop*salt2010$etohF)


### Take difference between 2010 and 1990, split into male and female.
countries <- salt2010$country
salt_diff <- salt2010[,-1]-salt1990[,-1]
salt_diff <- cbind(countries, salt_diff)
salt_diff <- filter(salt_diff, !is.na(e0_M))

male <- dplyr::select(salt_diff, e0_M, etohM, smoking, pc_gdp)
male_Na <- salt_diff$Na_M
colnames(male) <- gsub("_M", "", colnames(male))
female <- dplyr::select(salt_diff,  e0_F, etohF, smoking, pc_gdp)
female_Na <- salt_diff$Na_F
colnames(female) <- gsub("_F", "", colnames(female))

### Modeling step - test a bunch of different models and pick the one with the best in-sample fit

mod_M <- choose_model(male)
pred_M <- predict(mod_M, male)
mod_F <- choose_model(female)
pred_F <- predict(mod_F, female)


### Permutation test


res_M <- permu_pearson(prediction = pred_M, response = male$e0, treatment = male_Na, iters = 10000)
res_F <- permu_pearson(prediction = pred_F, response = female$e0, treatment = female_Na, iters = 10000)


### Confidence intervals - by inverting the permutation test



# ci_M <- permu_CI_pearson(prediction = pred_M, response = male$e0, treatment = male_Na, iters = 10000, side = "both", verbosity = TRUE)
# ci_F <- permu_CI_pearson(prediction = pred_F, response = female$e0, treatment = female_Na, iters = 10000, side = "both", verbosity = TRUE)
ci_M <- ci_F <- c(0,0)


res_table <- data.frame(rbind(c(res_M$estimate, res_M$pvalue, ci_M), c(res_F$estimate, res_F$pvalue, ci_F)))
rownames(res_table) <- c("Male", "Female"); colnames(res_table) <- c("Estimate", "Upper p", "Lower p", "Two-sided p", "Lower CI", "Upper CI")
print(res_table)



########################################## Plots ########################################## 
library(ggplot2)
library(grid)
library(gridExtra)

setwd("~/Documents/model-match-slides/fig")
png("sodium_lifeexp.png", width = 900)

dat <- data.frame("sex"     = c(rep("Male",nrow(male)), rep("Female",nrow(female))),
                  "sodium"  = as.numeric(c(male_Na, female_Na)),
                  "le"      = c(male$e0, female$e0),
                  "country" = rep(salt_diff$countries, 2),
                  "ex_mort" = c(male$e0-pred_M, female$e0-pred_F),
                  stringsAsFactors = FALSE)

p1 <- ggplot(dat, aes(x = sodium, y = le, color = sex)) + 
      geom_point() +
      facet_grid(sex~.) +
      scale_color_manual(values = c("#DB7093", "#629e1f")) + 
      theme(legend.position = "none") + 
      labs(x = expression(paste(Delta, " Sodium (g/day)")), 
           y = expression(paste(Delta, " Life Expectancy (years)")), 
           title = "Life Expectancy, 1990 to 2010") + 
      geom_text(aes(label=ifelse(le<0,paste(country),"")) , hjust=-0.2, size = 3.5) +
      geom_smooth(method = "lm", linetype = "dashed", se=FALSE) +
      report_theme +
      theme(strip.background = element_blank(),
        strip.text = element_blank())
p2 <- ggplot(dat, aes(x = sodium, y = ex_mort, color = sex)) + 
  geom_point() +
  facet_grid(sex~.) +
  scale_color_manual(values = c("#DB7093", "#629e1f")) +
  labs(x = expression(paste(Delta, " Sodium (g/day)")), 
       y = "", 
       title = "Residual (true - predicted) Life Expectancy, 1990 to 2010") + 
  geom_text(aes(label=ifelse(le<0,paste(country),"")) , hjust=-0.2, size = 3.5) +
  geom_smooth(method = "lm", linetype = "dashed", se=FALSE, show_guide=FALSE) +
  report_theme +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(p2)

p3 <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                               p2 + theme(legend.position="none"),
                               nrow=1),
                   mylegend, nrow=2,heights=c(10, 1))

dev.off()
