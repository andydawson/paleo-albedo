library(ggplot2)
library(mgcv)
library(brms)
library(gratia)
library(reshape2)
library(dplyr)

alb_prod = "bluesky"
# alb_prod = "albclim"

cal_data =readRDS(paste0('data/calibration_modern_lct_', alb_prod, '.RDS'))

###############################################################################################################
## GAM: compare models
###############################################################################################################

mod1 = readRDS(paste0('data/calibration_mod1_', alb_prod, '.RDS'))
mod2 = readRDS(paste0('data/calibration_mod2_', alb_prod, '.RDS'))
mod3 = readRDS(paste0('data/calibration_mod3_', alb_prod, '.RDS'))
mod4 = readRDS(paste0('data/calibration_mod4_', alb_prod, '.RDS'))
mod5 = readRDS(paste0('data/calibration_mod5_', alb_prod, '.RDS'))
mod6 = readRDS(paste0('data/calibration_mod6_', alb_prod, '.RDS'))
mod7 = readRDS(paste0('data/calibration_mod7_', alb_prod, '.RDS'))
mod7_free = readRDS(paste0('data/calibration_mod7_free_', alb_prod, '.RDS'))
mod8 = readRDS(paste0('data/calibration_mod8_', alb_prod, '.RDS'))

AIC(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod7_free, mod8)

anova_gam = anova.gam(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, test = "Chisq")
anova_gam

anova_gam = anova.gam(mod5, mod7, mod7_free, mod8, test = "Chisq")
anova_gam

anova_gam = anova.gam(mod7_free, mod8, test = "Chisq")
anova_gam

anova_gam = anova.gam(mod2, mod5, test = "Chisq")
anova_gam

anova_gam = anova.gam(mod1, mod2, mod7, test = "Chisq")
anova_gam


summary(mod7_free)
summary(mod8)


plot(mod7, shade = TRUE, seWithMean = TRUE, residuals = TRUE, pch = 16, cex = 0.8)


cal_model = mod8

saveRDS(cal_model, paste0('data/calibration_model_selected_', alb_prod, '.RDS'))

cal_predict_gam = predict(cal_model, 
                     newdata = cal_data,
                     type = 'response')#,
# se.fit = TRUE)
cal_eval_gam = data.frame(cal_data, alb_mean = cal_predict_gam)  



cor(cal_eval_gam$mar, cal_eval_gam$alb_mean, use='complete')

ggplot(data=cal_eval_gam) + 
  geom_point(aes(x=mar, y=alb_mean), size=2, alpha=0.5) +
  geom_abline(slope=1, intercept=0, colour="red", lwd=1, lty=2) +
  xlim(c(0,1)) + 
  ylim(c(0,1)) +
  coord_fixed() +  
  theme_bw(18) +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=14)) +
  xlab('albedo (data)') +
  ylab('albedo (model)')
ggsave('figures/cal_model_vs_data_gam.png')
ggsave('figures/cal_model_vs_data_gam.pdf')

cal_sim_gam = simulate(cal_model,
                       nsim = 100,
                       data = cal_data)

cal_sim_gam = data.frame(cal_data[,c('long', 'lat', 'mar')], cal_sim_gam)
cal_sim_gam_melt = melt(cal_sim_gam, id.vars = c('long', 'lat', 'mar'))

cal_sim_gam_sum = cal_sim_gam_melt %>% 
  group_by(long, lat, mar) %>%
  summarize(alb_mean = mean(value), 
            alb_lo = quantile(value, c(0.025)), 
            alb_mid = quantile(value, c(0.5)), 
            alb_hi = quantile(value, c(0.975)),
            .groups = "keep")

ggplot(data=cal_sim_gam_sum) + 
  geom_point(aes(x=mar, y=alb_mean), size=2, alpha=0.5) +
  geom_linerange(aes(x=mar, ymin=alb_lo, ymax=alb_hi), alpha=0.5) +
  geom_abline(slope=1, intercept=0, colour="red", lwd=1, lty=2) +
  xlim(c(0,1)) + 
  ylim(c(0,1)) +
  coord_fixed() +  
  theme_bw(18) +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=14)) +
  xlab('albedo (data)') +
  ylab('albedo (model)')
ggsave('figures/cal_model_vs_data_gam_error.png')
ggsave('figures/cal_model_vs_data_gam_error.pdf')
###############################################################################################################
## BRMS: compare models
###############################################################################################################

brm1 = readRDS(paste0('data/calibration_brms_m1_', alb_prod, '.RDS'))
brm2 = readRDS(paste0('data/calibration_brms_m2_', alb_prod, '.RDS'))
brm3 = readRDS(paste0('data/calibration_brms_m3_', alb_prod, '.RDS'))
brm4 = readRDS(paste0('data/calibration_brms_m4_', alb_prod, '.RDS'))
brm5 = readRDS(paste0('data/calibration_brms_m5_', alb_prod, '.RDS'))
brm6 = readRDS(paste0('data/calibration_brms_m6_', alb_prod, '.RDS'))
brm7 = readRDS(paste0('data/calibration_brms_m7_', alb_prod, '.RDS'))
brm8 = readRDS(paste0('data/calibration_brms_m8_', alb_prod, '.RDS'))


loo = loo_compare(brm1, brm2, brm3, brm4, brm5, brm6, brm7, brm8, criterion="loo")
print(loo, simplify=FALSE)

loo_sub = data.frame(loo[, c('elpd_diff', 'se_diff')])
loo_sub$sig_diff = abs(loo_sub$elpd_diff / loo_sub$se_diff)
loo_sub

# so brm5 is better than

loo = loo_compare(brm5, brm6, brm7, brm8, criterion="waic")
print(loo, simplify=FALSE)


cal_predict_brm= predict(brm5, 
                     newdata = cal_data,
                     summary = TRUE)
colnames(cal_predict_brm) = c('alb_mean', 'alb_sd', 'alb_lo', 'alb_hi')
cal_eval_brm = data.frame(cal_data, cal_predict_brm)  

cor(cal_eval_brm$mar, cal_eval_brm$alb_mean, use='complete')

ggplot(data=cal_eval_brm) + 
  geom_point(aes(x=mar, y=alb_mean), size=2, alpha=0.5) +
  geom_abline(slope=1, intercept=0, colour="red", lwd=1, lty=2) +
  xlim(c(0,1)) + 
  ylim(c(0,1)) +
  coord_fixed() +  
  theme_bw(18) +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=14)) +
  xlab('albedo (data)') +
  ylab('albedo (model)')
ggsave('figures/cal_model_brm_vs_data.png')
ggsave('figures/cal_model_brm_vs_data.pdf')


ggplot(data=cal_eval_brm) + 
  geom_point(aes(x=mar, y=alb_mean), size=2, alpha=0.5) +
  geom_linerange(aes(x=mar, ymin=alb_lo, ymax=alb_hi), alpha=0.5) +
  geom_abline(slope=1, intercept=0, colour="red", lwd=1, lty=2) +
  xlim(c(0,1)) + 
  ylim(c(0,1)) +
  coord_fixed() +  
  theme_bw(18) +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=14)) +
  xlab('albedo (data)') +
  ylab('albedo (model)')
ggsave('figures/cal_model_vs_data_brm_error.png')
ggsave('figures/cal_model_vs_data_brm_error.pdf')

ggplot() + 
  geom_point(data=cal_eval_brm, aes(x=mar, y=alb_mean), size=2, alpha=0.5) +
  geom_point(data=cal_eval_gam, aes(x=mar, y=alb_mean), size=2, alpha=0.5, colour="blue") +
  geom_abline(slope=1, intercept=0, colour="red", lwd=1, lty=2) +
  xlim(c(0,1)) + 
  ylim(c(0,1)) +
  coord_fixed() +  
  theme_bw(18) +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=14)) +
  xlab('albedo (data)') +
  ylab('albedo (model)')
ggsave('figures/cal_model_both_vs_data.png')
ggsave('figures/cal_model_both_vs_data.pdf')


