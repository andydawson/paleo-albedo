library(ggplot2)
library(mgcv)
library(brms)
library(gratia)
library(reshape2)
library(dplyr)

alb_prod = "bluesky"
# alb_prod = "albclim"

months = c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')

cal_data =readRDS(paste0('data/calibration_modern_lct_', alb_prod, '.RDS'))

cal_interp_data =readRDS(paste0('data/calibration_modern_lct_interp_', alb_prod, '.RDS'))

###############################################################################################################
## calibration model: model vs data by month
## one month each page
###############################################################################################################
pdf(paste0('figures/cal_model_vs_data_gam_error_months.pdf'), width=10, height=10)
for (n in 1:length(months)){
  
  month = months[n]
  
  print(month)
  
  mod8_interp = readRDS(paste0('output/calibration/calibration_mod8_interp_', month, '_', alb_prod, '.RDS'))
  summary(mod8_interp)
  
  cal_interp_model = mod8_interp
  
  saveRDS(cal_interp_model, paste0('output/calibration/calibration_mod_interp_selected_', month, '_', alb_prod, '.RDS'))
  
  cal_interp_predict_gam = predict(cal_interp_model, 
                                   newdata = cal_interp_data,
                                   type = 'response')#,
  # se.fit = TRUE)
  cal_interp_eval_gam = data.frame(cal_interp_data[, c('x', 'y', 'elev', 'ET', 'OL', 'ST', month)], 
                                   alb_mean = cal_interp_predict_gam)  
  
  cor_month = cor(cal_interp_eval_gam[,month], cal_interp_eval_gam$alb_mean, use='complete')
  
  cor_month
  # ggplot(data=cal_interp_eval_gam) + 
  #   geom_point(aes(x=mar, y=alb_mean), size=2, alpha=0.5) +
  #   geom_abline(slope=1, intercept=0, colour="red", lwd=1, lty=2) +
  #   xlim(c(0,1)) + 
  #   ylim(c(0,1)) +
  #   coord_fixed() +  
  #   theme_bw(18) +
  #   theme(axis.title = element_text(size=14),
  #         axis.text = element_text(size=14)) +
  #   xlab('albedo (data)') +
  #   ylab('albedo (model)')
  # ggsave(paste0('figures/cal_model_vs_data_gam_interp_', month, '.png'))
  # ggsave(paste0('figures/cal_model_vs_data_gam_interp_', month, '.pdf'))
  
  cal_interp_sim_gam = simulate(cal_interp_model,
                                nsim = 100,
                                data = cal_interp_data[,c('x', 'y', 'elev', 'ET', 'OL', 'ST', month)])
  
  cal_interp_sim_gam = data.frame(cal_interp_data[,c('x', 'y', month)], 
                                  cal_interp_sim_gam)
  cal_interp_sim_gam_melt = melt(cal_interp_sim_gam, 
                                 id.vars = c('x', 'y', month))
  colnames(cal_interp_sim_gam_melt) = c('x', 'y', 'month', 'variable', 'value')
  
  # cal_interp_sim_gam_sum = cal_interp_sim_gam_melt %>% 
  #   group_by(x, y, !!! syms(month)) %>%
  #   summarize(alb_mean = mean(value), 
  #             alb_lo = quantile(value, c(0.025), na.rm=TRUE), 
  #             alb_mid = quantile(value, c(0.5), na.rm=TRUE), 
  #             alb_hi = quantile(value, c(0.975), na.rm=TRUE),
  #             .groups = "keep")
  
  cal_interp_sim_gam_sum = cal_interp_sim_gam_melt %>% 
    group_by(x, y, month) %>%
    summarize(alb_mean = mean(value), 
              alb_lo = quantile(value, c(0.025), na.rm=TRUE), 
              alb_mid = quantile(value, c(0.5), na.rm=TRUE), 
              alb_hi = quantile(value, c(0.975), na.rm=TRUE),
              .groups = "keep")
  
  p = ggplot(data=cal_interp_sim_gam_sum) + 
    geom_point(aes(x=month, y=alb_mean), size=2, alpha=0.4) +
    geom_linerange(aes(x=month, ymin=alb_lo, ymax=alb_hi), alpha=0.5) +
    geom_abline(slope=1, intercept=0, colour="red", lwd=1, lty=2, alpha=0.4) +
    xlim(c(0,1)) + 
    ylim(c(0,1)) +
    coord_fixed() +  
    theme_bw(24) +
    theme(axis.title = element_text(size=14),
          axis.text = element_text(size=14)) +
    xlab('albedo (data)') +
    ylab('albedo (model)') +
    labs(title = paste0(month, '; cor ', round(cor_month, 2)))
  # ggsave(paste0('figures/cal_model_vs_data_gam_error_interp_', month, '.png'))
  # ggsave(paste0('figures/cal_model_vs_data_gam_error_interp_', month, '.pdf'))
  
  print(p)
  
}
dev.off()


###############################################################################################################
## calibration model: model vs data by month
## facet months
###############################################################################################################

cal_interp_sim_gam_all = data.frame(x = numeric(0),
                                    y = numeric(0),
                                    alb_data = numeric(0),
                                    variable = character(0),
                                    alb_model = numeric(0),
                                    month = character(0))

cor_all = rep(NA, 12)

# pdf(paste0('figures/cal_model_vs_data_gam_error_interp_months.pdf'), width=10, height=10)
for (n in 1:length(months)){
  
  month = months[n]
  
  print(month)
  
  mod8_interp = readRDS(paste0('output/calibration/calibration_mod8_interp_', month, '_', alb_prod, '.RDS'))
  summary(mod8_interp)
  
  cal_interp_model = mod8_interp
  
  saveRDS(cal_interp_model, paste0('output/calibration/calibration_mod_interp_selected_', month, '_', alb_prod, '.RDS'))
  
  cal_interp_predict_gam = predict(cal_interp_model,
                                   newdata = cal_interp_data,
                                   type = 'response')#,
  # se.fit = TRUE)
  cal_interp_eval_gam = data.frame(cal_interp_data[, c('x', 'y', 'elev', 'ET', 'OL', 'ST', month)],
                                   alb_mean = cal_interp_predict_gam)
  
  cor_month = cor(cal_interp_eval_gam[,month], cal_interp_eval_gam$alb_mean, use='complete')
  cor_month
  cor_all[n] = cor_month
  
  cal_interp_sim_gam = simulate(cal_interp_model,
                                nsim = 100,
                                data = cal_interp_data[,c('x', 'y', 'elev', 'ET', 'OL', 'ST', month)])
  
  cal_interp_sim_gam = data.frame(cal_interp_data[,c('x', 'y', month)],
                                  cal_interp_sim_gam)
  cal_interp_sim_gam_melt = melt(cal_interp_sim_gam,
                                 id.vars = c('x', 'y', month))
  colnames(cal_interp_sim_gam_melt) = c('x', 'y', 'alb_data', 'variable', 'alb_model')
  
  cal_interp_sim_gam_melt$month = rep(month)
  
  cal_interp_sim_gam_all = rbind(cal_interp_sim_gam_all, cal_interp_sim_gam_melt)
  
}

cal_interp_sim_gam_all$month = factor(cal_interp_sim_gam_all$month, levels = months)

cal_interp_sim_gam_quants = cal_interp_sim_gam_all %>%
  group_by(x, y, alb_data, month) %>%
  summarize(alb_mean = mean(alb_model),
            alb_lo = quantile(alb_model, c(0.025), na.rm=TRUE),
            alb_mid = quantile(alb_model, c(0.5), na.rm=TRUE),
            alb_hi = quantile(alb_model, c(0.975), na.rm=TRUE),
            .groups = "keep")

p = ggplot(data=cal_interp_sim_gam_quants) +
  geom_point(aes(x=alb_data, y=alb_mean), size=2, alpha=0.3) +
  geom_linerange(aes(x=alb_data, ymin=alb_lo, ymax=alb_hi), alpha=0.3) +
  geom_abline(slope=1, intercept=0, colour="red", lwd=1, lty=2, alpha=0.4) +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  coord_fixed() +
  theme_bw(18) +
  # theme(axis.title = element_text(size=12),
  #       axis.text = element_text(size=12)) +
  xlab('albedo (data)') +
  ylab('albedo (model)') +
  # labs(title = paste0(month, '; cor ', round(cor_month, 2))) +
  facet_wrap(~month)
print(p)
ggsave('figures/cal_model_vs_data_gam_error_facet.pdf', width=12, height=10)
ggsave('figures/cal_model_vs_data_gam_error_facet.png', width=12, height=10)

###############################################################################################################
## calibration model: spatial effects experiment
## 
###############################################################################################################

cal_sim_SPE = data.frame(x = numeric(0),
                         y = numeric(0),
                         alb_data = numeric(0),
                         variable = character(0),
                         alb_pred = numeric(0),
                         month = character(0),
                         model = character(0))

# cor_all = rep(NA, 12)

# print(paste0('Fitting calibration models for ', month, ' albedo'))
# 
# print('Fitting model: (x,y)')
# mod_spatial = mgcv::bam(get(month) ~ s(x, y, bs="gp", k=500),
#                         data=cal_interp_data, 
#                         family=betar(link="logit"), 
#                         method="REML", 
#                         na.action=na.omit, 
#                         control=ctrl)
# 
# saveRDS(mod_spatial, paste0('output/calibration/calibration_mod_spatial_', month, '.RDS'))
# 
# print('Fitting model: (x,y) + elevation')
# mod_spatial_elev = mgcv::bam(get(month) ~ s(x, y, bs="gp", k=500) + s(elev, k=50),
#                              data=cal_interp_data, 
#                              family=betar(link="logit"), 
#                              method="REML", 
#                              na.action=na.omit, 
#                              control=ctrl)
# 
# saveRDS(mod_spatial_elev, paste0('output/calibration/calibration_mod_spatial_elev_', month, '.RDS'))
# 
# print('Fitting model: (x,y) + tp(OL, ET, ST)')
# mod_spatial_cover = mgcv::bam(get(month) ~ s(x, y, bs="gp", k=500) +  s(OL, ET, ST, bs='tp', k=200),
#                               data=cal_interp_data, 
#                               family=betar(link="logit"), 
#                               method="REML", 
#                               na.action=na.omit, 
#                               control=ctrl)
# 
# saveRDS(mod_spatial_cover, paste0('output/calibration/calibration_mod_spatial_cover_', month, '.RDS'))
# 
# 
# print('Fitting model: tp(OL, ET, ST)')
# mod_cover = mgcv::bam(get(month) ~ s(OL, ET, ST, bs='tp', k=200),
#                       data=cal_interp_data, 
#                       family=betar(link="logit"), 
#                       method="REML", 
#                       na.action=na.omit, 
#                       control=ctrl)
# 
# saveRDS(mod_cover, paste0('output/calibration/calibration_mod_cover_', month, '.RDS'))
# 
# print('Fitting model: tp(OL, ET, ST) + elev')
# mod_cover_elev = mgcv::bam(get(month) ~ s(elev, k=50) + s(OL, ET, ST, bs='tp', k=200),
#                            data=cal_interp_data, 
#                            family=betar(link="logit"), 
#                            method="REML", 
#                            na.action=na.omit, 
#                            control=ctrl)
# 
# saveRDS(mod_cover_elev, paste0('output/calibration/calibration_mod_cover_elev_', month, '.RDS'))


# model_SP_ELEV_LC = readRDS(paste0('output/calibration/calibration_mod_interp_selected_', month, '_', alb_prod, '.RDS'))
# 
# model_ELEV_LC = readRDS(paste0('output/calibration/calibration_mod_cover_elev_', month, '.RDS'))
# model_LC = readRDS(paste0('output/calibration/calibration_mod_cover_', month, '.RDS'))
# model_SP_LC = readRDS(paste0('output/calibration/calibration_mod_spatial_cover_', month, '.RDS'))
# model_SP = readRDS(paste0('output/calibration/calibration_mod_spatial_', month, '.RDS'))
# model_SP_ELEV = readRDS(paste0('output/calibration/calibration_mod_spatial_elev_', month, '.RDS'))
# model_ELEV = readRDS(paste0('output/calibration/calibration_mod_elev_', month, '.RDS'))

models = c('SP_ELEV_LC', 'ELEV_LC', 'SP_LC', 'SP', 'SP_ELEV', 'ELEV')

cal_sims = data.frame(matrix(nrow=0, ncol=7))
colnames(cal_sims) = c('x', 'y', 'alb_data', 'variable', 'alb_pred', 'month', 'model')


# pdf(paste0('figures/cal_model_vs_data_gam_error_interp_months.pdf'), width=10, height=10)
for (n in 1:length(months)){
  
  month = months[n]
  
  print(month)
  
  # mod8_interp = readRDS(paste0('output/calibration/calibration_mod8_interp_', month, '_', alb_prod, '.RDS'))
  # summary(mod8_interp)
  # 
  # cal_interp_model = mod8_interp
  
  model_SP_ELEV_LC = readRDS(paste0('output/calibration/calibration_mod_interp_selected_', month, '_', alb_prod, '.RDS'))
  
  model_ELEV_LC = readRDS(paste0('output/calibration/calibration_mod_cover_elev_', month, '.RDS'))
  model_LC = readRDS(paste0('output/calibration/calibration_mod_cover_', month, '.RDS'))
  model_SP_LC = readRDS(paste0('output/calibration/calibration_mod_spatial_cover_', month, '.RDS'))
  model_SP = readRDS(paste0('output/calibration/calibration_mod_spatial_', month, '.RDS'))
  model_SP_ELEV = readRDS(paste0('output/calibration/calibration_mod_spatial_elev_', month, '.RDS'))
  model_ELEV = readRDS(paste0('output/calibration/calibration_mod_elev_', month, '.RDS'))
  
  # cal_interp_predict_gam = predict(cal_interp_model,
  #                                  newdata = cal_interp_data,
  #                                  type = 'response')
  # 
  # cal_interp_eval_gam = data.frame(cal_interp_data[, c('x', 'y', 'elev', 'ET', 'OL', 'ST', month)],
  #                                  alb_pred = cal_interp_predict_gam)
  # 
  # cor_month = cor(cal_interp_eval_gam[,month], cal_interp_eval_gam$alb_mean, use='complete')
  # cor_month
  # cor_all[n] = cor_month
  
  cal_sims_month
  
  for (model in models){
    
    model_name = eval(as.name(paste0('model_', model)))
    
    sim = simulate(model_name,
                              nsim = 100,
                              data = cal_interp_data[,c('x', 'y', 'elev', 'ET', 'OL', 'ST', month)])
    sim_df = data.frame(cal_interp_data[,c('x', 'y', month)],
                                   sim)
    sim_melt = melt(sim_df,
                               id.vars = c('x', 'y', month))
    colnames(sim_melt) = c('x', 'y', 'alb_data', 'variable', 'alb_pred')
    sim_melt$month = rep(month)
    sim_melt$model = rep(model)
    
    cal_sims = rbind(cal_sims,
                           sim_melt)
    
  }
  
  # sim_SP_ELEV_LC = simulate(model_SP_ELEV_LC,
  #                           nsim = 100,
  #                           data = cal_interp_data[,c('x', 'y', 'elev', 'ET', 'OL', 'ST', month)])
  # sim_SP_ELEV_LC_df = data.frame(cal_interp_data[,c('x', 'y', month)],
  #                                 sim_SP_ELEV_LC)
  # sim_SP_ELEV_LC_melt = melt(sim_SP_ELEV_LC_df,
  #                                id.vars = c('x', 'y', month))
  # colnames(sim_SP_ELEV_LC_melt) = c('x', 'y', 'alb_data', 'variable', 'alb_pred')
  # sim_SP_ELEV_LC_melt$month = rep(month)
  # sim_SP_ELEV_LC_melt$model = rep('SP_ELEV_LC')
  
  
  
  
  
  
  
  
  

  cal_interp_sim_gam_all = rbind(cal_interp_sim_gam_all, 
                                 cal_interp_sim_gam_melt)
  
}

cal_interp_sim_gam_all$month = factor(cal_interp_sim_gam_all$month, levels = months)

cal_interp_sim_gam_quants = cal_interp_sim_gam_all %>%
  group_by(x, y, alb_data, month) %>%
  summarize(alb_mean = mean(alb_model),
            alb_lo = quantile(alb_model, c(0.025), na.rm=TRUE),
            alb_mid = quantile(alb_model, c(0.5), na.rm=TRUE),
            alb_hi = quantile(alb_model, c(0.975), na.rm=TRUE),
            .groups = "keep")

p = ggplot(data=cal_interp_sim_gam_quants) +
  geom_point(aes(x=alb_data, y=alb_mean), size=2, alpha=0.3) +
  geom_linerange(aes(x=alb_data, ymin=alb_lo, ymax=alb_hi), alpha=0.3) +
  geom_abline(slope=1, intercept=0, colour="red", lwd=1, lty=2, alpha=0.4) +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  coord_fixed() +
  theme_bw(18) +
  # theme(axis.title = element_text(size=12),
  #       axis.text = element_text(size=12)) +
  xlab('albedo (data)') +
  ylab('albedo (model)') +
  # labs(title = paste0(month, '; cor ', round(cor_month, 2))) +
  facet_wrap(~month)
print(p)
ggsave('figures/cal_model_vs_data_gam_error_facet.pdf', width=12, height=10)
ggsave('figures/cal_model_vs_data_gam_error_facet.png', width=12, height=10)


# ###############################################################################################################
# ## MARCH GAM INTERP: COMPARE MODELS
# ###############################################################################################################
# month = "feb"
# 
# mod1_interp = readRDS(paste0('output/calibration/calibration_mod1_interp_', month, '_', alb_prod, '.RDS'))
# mod2_interp = readRDS(paste0('output/calibration/calibration_mod2_interp_', month, '_', alb_prod, '.RDS'))
# mod3_interp = readRDS(paste0('output/calibration/calibration_mod3_interp_', month, '_', alb_prod, '.RDS'))
# mod4_interp = readRDS(paste0('output/calibration/calibration_mod4_interp_', month, '_', alb_prod, '.RDS'))
# mod5_interp = readRDS(paste0('output/calibration/calibration_mod5_interp_', month, '_', alb_prod, '.RDS'))
# mod6_interp = readRDS(paste0('output/calibration/calibration_mod6_interp_', month, '_', alb_prod, '.RDS'))
# mod7_interp = readRDS(paste0('output/calibration/calibration_mod7_interp_', month, '_', alb_prod, '.RDS'))
# mod8_interp = readRDS(paste0('output/calibration/calibration_mod8_interp_', month, '_', alb_prod, '.RDS'))
# 
# AIC(mod1_interp, mod2_interp, mod3_interp, mod4_interp, mod5_interp, 
#     mod6_interp, mod7_interp, mod8_interp)
# 
# anova_gam_interp = anova.gam(mod1_interp, mod2_interp, mod3_interp, mod4_interp, 
#                              mod5_interp, mod6_interp, mod7_interp, mod8_interp, 
#                              test = "Chisq")
# anova_gam_interp
# 
# summary(mod8_interp)
# 
# cal_interp_model = mod8_interp
# 
# # plot(mod5_interp, shade = TRUE, seWithMean = TRUE, residuals = TRUE, pch = 16, cex = 0.8)
# 
# saveRDS(cal_interp_model, paste0('output/calibration/calibration_mod_interp_selected_', month, '_', alb_prod, '.RDS'))
# 
# cal_interp_predict_gam = predict(cal_interp_model, 
#                                  newdata = cal_interp_data,
#                                  type = 'response')#,
# # se.fit = TRUE)
# cal_interp_eval_gam = data.frame(cal_interp_data, 
#                                  alb_mean = cal_interp_predict_gam)  
# 
# 
# 
# cor(cal_interp_eval_gam[,month], cal_interp_eval_gam$alb_mean, use='complete')
# 
# ggplot(data=cal_interp_eval_gam) + 
#   geom_point(aes(x=mar, y=alb_mean), size=2, alpha=0.5) +
#   geom_abline(slope=1, intercept=0, colour="red", lwd=1, lty=2) +
#   xlim(c(0,1)) + 
#   ylim(c(0,1)) +
#   coord_fixed() +  
#   theme_bw(18) +
#   theme(axis.title = element_text(size=14),
#         axis.text = element_text(size=14)) +
#   xlab('albedo (data)') +
#   ylab('albedo (model)')
# ggsave(paste0('figures/cal_model_vs_data_gam_interp_', month, '.png'))
# ggsave(paste0('figures/cal_model_vs_data_gam_interp_', month, '.pdf'))
# 
# cal_interp_sim_gam = simulate(cal_interp_model,
#                               nsim = 100,
#                               data = cal_interp_data)
# 
# cal_interp_sim_gam = data.frame(cal_interp_data[,c('x', 'y', 'mar')], 
#                                 cal_interp_sim_gam)
# cal_interp_sim_gam_melt = melt(cal_interp_sim_gam, 
#                                id.vars = c('x', 'y', 'mar'))
# 
# cal_interp_sim_gam_sum = cal_interp_sim_gam_melt %>% 
#   group_by(x, y, mar) %>%
#   summarize(alb_mean = mean(value), 
#             alb_lo = quantile(value, c(0.025)), 
#             alb_mid = quantile(value, c(0.5)), 
#             alb_hi = quantile(value, c(0.975)),
#             .groups = "keep")
# 
# ggplot(data=cal_interp_sim_gam_sum) + 
#   geom_point(aes(x=mar, y=alb_mean), size=2, alpha=0.4) +
#   geom_linerange(aes(x=mar, ymin=alb_lo, ymax=alb_hi), alpha=0.5) +
#   geom_abline(slope=1, intercept=0, colour="red", lwd=1, lty=2, alpha=0.4) +
#   xlim(c(0,1)) + 
#   ylim(c(0,1)) +
#   coord_fixed() +  
#   theme_bw(18) +
#   theme(axis.title = element_text(size=14),
#         axis.text = element_text(size=14)) +
#   xlab('albedo (data)') +
#   ylab('albedo (model)')
# ggsave(paste0('figures/cal_model_vs_data_gam_error_interp_', month, '.png'))
# ggsave(paste0('figures/cal_model_vs_data_gam_error_interp_', month, '.pdf'))
# 
# ###############################################################################################################
# ## MAY GAM INTERP: COMPARE MODELS
# ###############################################################################################################
# month = "may"
# 
# mod1_interp = readRDS(paste0('output/calibration/calibration_mod1_interp_', month, '_', alb_prod, '.RDS'))
# mod2_interp = readRDS(paste0('output/calibration/calibration_mod2_interp_', month, '_', alb_prod, '.RDS'))
# mod3_interp = readRDS(paste0('output/calibration/calibration_mod3_interp_', month, '_', alb_prod, '.RDS'))
# mod4_interp = readRDS(paste0('output/calibration/calibration_mod4_interp_', month, '_', alb_prod, '.RDS'))
# mod5_interp = readRDS(paste0('output/calibration/calibration_mod5_interp_', month, '_', alb_prod, '.RDS'))
# mod6_interp = readRDS(paste0('output/calibration/calibration_mod6_interp_', month, '_', alb_prod, '.RDS'))
# mod7_interp = readRDS(paste0('output/calibration/calibration_mod7_interp_', month, '_', alb_prod, '.RDS'))
# mod8_interp = readRDS(paste0('output/calibration/calibration_mod8_interp_', month, '_', alb_prod, '.RDS'))
# 
# AIC(mod1_interp, mod2_interp, mod3_interp, mod4_interp, mod5_interp, 
#     mod6_interp, mod7_interp, mod8_interp)
# 
# anova_gam_interp = anova.gam(mod1_interp, mod2_interp, mod3_interp, mod4_interp, 
#                              mod5_interp, mod6_interp, mod7_interp, mod8_interp, 
#                              test = "Chisq")
# anova_gam_interp
# 
# summary(mod8_interp)
# 
# cal_interp_model = mod8_interp
# 
# # plot(mod5_interp, shade = TRUE, seWithMean = TRUE, residuals = TRUE, pch = 16, cex = 0.8)
# 
# saveRDS(cal_interp_model, paste0('output/calibration/calibration_mod_interp_selected_', month, '_', alb_prod, '.RDS'))
# 
# cal_interp_predict_gam = predict(cal_interp_model, 
#                                  newdata = cal_interp_data,
#                                  type = 'response')#,
# # se.fit = TRUE)
# cal_interp_eval_gam = data.frame(cal_interp_data, 
#                                  alb_mean = cal_interp_predict_gam)  
# 
# 
# 
# cor(cal_interp_eval_gam[,month], cal_interp_eval_gam$alb_mean, use='complete')
# 
# ggplot(data=cal_interp_eval_gam) + 
#   geom_point(aes(x=get(month), y=alb_mean), size=2, alpha=0.5) +
#   geom_abline(slope=1, intercept=0, colour="red", lwd=1, lty=2) +
#   xlim(c(0,1)) + 
#   ylim(c(0,1)) +
#   coord_fixed() +  
#   theme_bw(18) +
#   theme(axis.title = element_text(size=14),
#         axis.text = element_text(size=14)) +
#   xlab('albedo (data)') +
#   ylab('albedo (model)')
# ggsave(paste0('figures/cal_model_vs_data_gam_interp_', month, '.png'))
# ggsave(paste0('figures/cal_model_vs_data_gam_interp_', month, '.pdf'))
# 
# cal_interp_sim_gam = simulate(cal_interp_model,
#                               nsim = 100,
#                               data = cal_interp_data)
# 
# cal_interp_sim_gam = data.frame(cal_interp_data[,c('x', 'y', month)], 
#                                 cal_interp_sim_gam)
# cal_interp_sim_gam_melt = melt(cal_interp_sim_gam, 
#                                id.vars = c('x', 'y', month))
# 
# vars_to_group = c('x', 'y', month)
# 
# cal_interp_sim_gam_sum = cal_interp_sim_gam_melt %>% 
#   group_by_at(vars_to_group) %>%
#   summarize(alb_mean = mean(value), 
#             alb_lo = quantile(value, c(0.025)), 
#             alb_mid = quantile(value, c(0.5)), 
#             alb_hi = quantile(value, c(0.975)),
#             .groups = "keep")
# 
# ggplot(data=cal_interp_sim_gam_sum) + 
#   geom_point(aes(x=get(month), y=alb_mean), size=2, alpha=0.4) +
#   geom_linerange(aes(x=get(month), ymin=alb_lo, ymax=alb_hi), alpha=0.5) +
#   geom_abline(slope=1, intercept=0, colour="red", lwd=1, lty=2, alpha=0.4) +
#   xlim(c(0,1)) + 
#   ylim(c(0,1)) +
#   coord_fixed() +  
#   theme_bw(18) +
#   theme(axis.title = element_text(size=14),
#         axis.text = element_text(size=14)) +
#   xlab('albedo (data)') +
#   ylab('albedo (model)')
# ggsave(paste0('figures/cal_model_vs_data_gam_error_interp_', month, '.png'))
# ggsave(paste0('figures/cal_model_vs_data_gam_error_interp_', month, '.pdf'))
# 
# ###############################################################################################################
# ## MAY GAM INTERP: COMPARE MODELS
# ###############################################################################################################
# month = "aug"
# 
# mod1_interp = readRDS(paste0('output/calibration/calibration_mod1_interp_', month, '_', alb_prod, '.RDS'))
# mod2_interp = readRDS(paste0('output/calibration/calibration_mod2_interp_', month, '_', alb_prod, '.RDS'))
# mod3_interp = readRDS(paste0('output/calibration/calibration_mod3_interp_', month, '_', alb_prod, '.RDS'))
# mod4_interp = readRDS(paste0('output/calibration/calibration_mod4_interp_', month, '_', alb_prod, '.RDS'))
# mod5_interp = readRDS(paste0('output/calibration/calibration_mod5_interp_', month, '_', alb_prod, '.RDS'))
# mod6_interp = readRDS(paste0('output/calibration/calibration_mod6_interp_', month, '_', alb_prod, '.RDS'))
# mod7_interp = readRDS(paste0('output/calibration/calibration_mod7_interp_', month, '_', alb_prod, '.RDS'))
# mod8_interp = readRDS(paste0('output/calibration/calibration_mod8_interp_', month, '_', alb_prod, '.RDS'))
# 
# AIC(mod1_interp, mod2_interp, mod3_interp, mod4_interp, mod5_interp, 
#     mod6_interp, mod7_interp, mod8_interp)
# 
# anova_gam_interp = anova.gam(mod1_interp, mod2_interp, mod3_interp, mod4_interp, 
#                              mod5_interp, mod6_interp, mod7_interp, mod8_interp, 
#                              test = "Chisq")
# anova_gam_interp
# 
# summary(mod8_interp)
# 
# cal_interp_model = mod8_interp
# 
# # plot(mod5_interp, shade = TRUE, seWithMean = TRUE, residuals = TRUE, pch = 16, cex = 0.8)
# 
# saveRDS(cal_interp_model, paste0('output/calibration/calibration_mod_interp_selected_', month, '_', alb_prod, '.RDS'))
# 
# cal_interp_predict_gam = predict(cal_interp_model, 
#                                  newdata = cal_interp_data,
#                                  type = 'response')#,
# # se.fit = TRUE)
# cal_interp_eval_gam = data.frame(cal_interp_data, 
#                                  alb_mean = cal_interp_predict_gam)  
# 
# 
# 
# cor(cal_interp_eval_gam[,month], cal_interp_eval_gam$alb_mean, use='complete')
# 
# ggplot(data=cal_interp_eval_gam) + 
#   geom_point(aes(x=get(month), y=alb_mean), size=2, alpha=0.5) +
#   geom_abline(slope=1, intercept=0, colour="red", lwd=1, lty=2) +
#   xlim(c(0,1)) + 
#   ylim(c(0,1)) +
#   coord_fixed() +  
#   theme_bw(18) +
#   theme(axis.title = element_text(size=14),
#         axis.text = element_text(size=14)) +
#   xlab('albedo (data)') +
#   ylab('albedo (model)')
# ggsave(paste0('figures/cal_model_vs_data_gam_interp_', month, '.png'))
# ggsave(paste0('figures/cal_model_vs_data_gam_interp_', month, '.pdf'))
# 
# cal_interp_sim_gam = simulate(cal_interp_model,
#                               nsim = 100,
#                               data = cal_interp_data)
# 
# cal_interp_sim_gam = data.frame(cal_interp_data[,c('x', 'y', month)], 
#                                 cal_interp_sim_gam)
# cal_interp_sim_gam_melt = melt(cal_interp_sim_gam, 
#                                id.vars = c('x', 'y', month))
# 
# vars_to_group = c('x', 'y', month)
# 
# cal_interp_sim_gam_sum = cal_interp_sim_gam_melt %>% 
#   group_by_at(vars_to_group) %>%
#   summarize(alb_mean = mean(value), 
#             alb_lo = quantile(value, c(0.025)), 
#             alb_mid = quantile(value, c(0.5)), 
#             alb_hi = quantile(value, c(0.975)),
#             .groups = "keep")
# 
# ggplot(data=cal_interp_sim_gam_sum) + 
#   geom_point(aes(x=get(month), y=alb_mean), size=2, alpha=0.4) +
#   geom_linerange(aes(x=get(month), ymin=alb_lo, ymax=alb_hi), alpha=0.5) +
#   geom_abline(slope=1, intercept=0, colour="red", lwd=1, lty=2, alpha=0.4) +
#   xlim(c(0,1)) + 
#   ylim(c(0,1)) +
#   coord_fixed() +  
#   theme_bw(18) +
#   theme(axis.title = element_text(size=14),
#         axis.text = element_text(size=14)) +
#   xlab('albedo (data)') +
#   ylab('albedo (model)')
# ggsave(paste0('figures/cal_model_vs_data_gam_error_interp_', month, '.png'))
# ggsave(paste0('figures/cal_model_vs_data_gam_error_interp_', month, '.pdf'))

###############################################################################################################
## MAY GAM INTERP: COMPARE MODELS
###############################################################################################################
month = "nov"

mod1_interp = readRDS(paste0('output/calibration/calibration_mod1_interp_', month, '_', alb_prod, '.RDS'))
mod2_interp = readRDS(paste0('output/calibration/calibration_mod2_interp_', month, '_', alb_prod, '.RDS'))
mod3_interp = readRDS(paste0('output/calibration/calibration_mod3_interp_', month, '_', alb_prod, '.RDS'))
mod4_interp = readRDS(paste0('output/calibration/calibration_mod4_interp_', month, '_', alb_prod, '.RDS'))
mod5_interp = readRDS(paste0('output/calibration/calibration_mod5_interp_', month, '_', alb_prod, '.RDS'))
mod6_interp = readRDS(paste0('output/calibration/calibration_mod6_interp_', month, '_', alb_prod, '.RDS'))
mod7_interp = readRDS(paste0('output/calibration/calibration_mod7_interp_', month, '_', alb_prod, '.RDS'))
mod8_interp = readRDS(paste0('output/calibration/calibration_mod8_interp_', month, '_', alb_prod, '.RDS'))

AIC(mod1_interp, mod2_interp, mod3_interp, mod4_interp, mod5_interp, 
    mod6_interp, mod7_interp, mod8_interp)

anova_gam_interp = anova.gam(mod1_interp, mod2_interp, mod3_interp, mod4_interp, 
                             mod5_interp, mod6_interp, mod7_interp, mod8_interp, 
                             test = "Chisq")
anova_gam_interp

summary(mod8_interp)

cal_interp_model = mod8_interp

# plot(mod5_interp, shade = TRUE, seWithMean = TRUE, residuals = TRUE, pch = 16, cex = 0.8)

saveRDS(cal_interp_model, paste0('output/calibration/calibration_mod_interp_selected_', month, '_', alb_prod, '.RDS'))

cal_interp_predict_gam = predict(cal_interp_model, 
                                 newdata = cal_interp_data,
                                 type = 'response')#,
# se.fit = TRUE)
cal_interp_eval_gam = data.frame(cal_interp_data, 
                                 alb_mean = cal_interp_predict_gam)  



cor(cal_interp_eval_gam[,month], cal_interp_eval_gam$alb_mean, use='complete')

ggplot(data=cal_interp_eval_gam) + 
  geom_point(aes(x=get(month), y=alb_mean), size=2, alpha=0.5) +
  geom_abline(slope=1, intercept=0, colour="red", lwd=1, lty=2) +
  xlim(c(0,1)) + 
  ylim(c(0,1)) +
  coord_fixed() +  
  theme_bw(18) +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=14)) +
  xlab('albedo (data)') +
  ylab('albedo (model)')
ggsave(paste0('figures/cal_model_vs_data_gam_interp_', month, '.png'))
ggsave(paste0('figures/cal_model_vs_data_gam_interp_', month, '.pdf'))

cal_interp_sim_gam = simulate(cal_interp_model,
                              nsim = 100,
                              data = cal_interp_data)

cal_interp_sim_gam = data.frame(cal_interp_data[,c('x', 'y', month)], 
                                cal_interp_sim_gam)
cal_interp_sim_gam_melt = melt(cal_interp_sim_gam, 
                               id.vars = c('x', 'y', month))

vars_to_group = c('x', 'y', month)

cal_interp_sim_gam_sum = cal_interp_sim_gam_melt %>% 
  group_by_at(vars_to_group) %>%
  summarize(alb_mean = mean(value), 
            alb_lo = quantile(value, c(0.025)), 
            alb_mid = quantile(value, c(0.5)), 
            alb_hi = quantile(value, c(0.975)),
            .groups = "keep")

ggplot(data=cal_interp_sim_gam_sum) + 
  geom_point(aes(x=get(month), y=alb_mean), size=2, alpha=0.4) +
  geom_linerange(aes(x=get(month), ymin=alb_lo, ymax=alb_hi), alpha=0.5) +
  geom_abline(slope=1, intercept=0, colour="red", lwd=1, lty=2, alpha=0.4) +
  xlim(c(0,1)) + 
  ylim(c(0,1)) +
  coord_fixed() +  
  theme_bw(18) +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=14)) +
  xlab('albedo (data)') +
  ylab('albedo (model)')
ggsave(paste0('figures/cal_model_vs_data_gam_error_interp_', month, '.png'))
ggsave(paste0('figures/cal_model_vs_data_gam_error_interp_', month, '.pdf'))

# ###############################################################################################################
# ## BRMS: compare models
# ###############################################################################################################
# 
# brm1 = readRDS(paste0('data/calibration_brms_m1_', alb_prod, '.RDS'))
# brm2 = readRDS(paste0('data/calibration_brms_m2_', alb_prod, '.RDS'))
# brm3 = readRDS(paste0('data/calibration_brms_m3_', alb_prod, '.RDS'))
# brm4 = readRDS(paste0('data/calibration_brms_m4_', alb_prod, '.RDS'))
# brm5 = readRDS(paste0('data/calibration_brms_m5_', alb_prod, '.RDS'))
# brm6 = readRDS(paste0('data/calibration_brms_m6_', alb_prod, '.RDS'))
# brm7 = readRDS(paste0('data/calibration_brms_m7_', alb_prod, '.RDS'))
# brm8 = readRDS(paste0('data/calibration_brms_m8_', alb_prod, '.RDS'))
# 
# 
# loo = loo_compare(brm1, brm2, brm3, brm4, brm5, brm6, brm7, brm8, criterion="loo")
# print(loo, simplify=FALSE)
# 
# loo_sub = data.frame(loo[, c('elpd_diff', 'se_diff')])
# loo_sub$sig_diff = abs(loo_sub$elpd_diff / loo_sub$se_diff)
# loo_sub
# 
# # so brm5 is better than
# 
# loo = loo_compare(brm5, brm6, brm7, brm8, criterion="waic")
# print(loo, simplify=FALSE)
# 
# 
# cal_predict_brm= predict(brm5, 
#                          newdata = cal_data,
#                          summary = TRUE)
# colnames(cal_predict_brm) = c('alb_mean', 'alb_sd', 'alb_lo', 'alb_hi')
# cal_eval_brm = data.frame(cal_data, cal_predict_brm)  
# 
# cor(cal_eval_brm$mar, cal_eval_brm$alb_mean, use='complete')
# 
# ggplot(data=cal_eval_brm) + 
#   geom_point(aes(x=mar, y=alb_mean), size=2, alpha=0.5) +
#   geom_abline(slope=1, intercept=0, colour="red", lwd=1, lty=2) +
#   xlim(c(0,1)) + 
#   ylim(c(0,1)) +
#   coord_fixed() +  
#   theme_bw(18) +
#   theme(axis.title = element_text(size=14),
#         axis.text = element_text(size=14)) +
#   xlab('albedo (data)') +
#   ylab('albedo (model)')
# ggsave('figures/cal_model_brm_vs_data.png')
# ggsave('figures/cal_model_brm_vs_data.pdf')
# 
# 
# ggplot(data=cal_eval_brm) + 
#   geom_point(aes(x=mar, y=alb_mean), size=2, alpha=0.5) +
#   geom_linerange(aes(x=mar, ymin=alb_lo, ymax=alb_hi), alpha=0.5) +
#   geom_abline(slope=1, intercept=0, colour="red", lwd=1, lty=2) +
#   xlim(c(0,1)) + 
#   ylim(c(0,1)) +
#   coord_fixed() +  
#   theme_bw(18) +
#   theme(axis.title = element_text(size=14),
#         axis.text = element_text(size=14)) +
#   xlab('albedo (data)') +
#   ylab('albedo (model)')
# ggsave('figures/cal_model_vs_data_brm_error.png')
# ggsave('figures/cal_model_vs_data_brm_error.pdf')
# 
# ggplot() + 
#   geom_point(data=cal_eval_brm, aes(x=mar, y=alb_mean), size=2, alpha=0.5) +
#   geom_point(data=cal_eval_gam, aes(x=mar, y=alb_mean), size=2, alpha=0.5, colour="blue") +
#   geom_abline(slope=1, intercept=0, colour="red", lwd=1, lty=2) +
#   xlim(c(0,1)) + 
#   ylim(c(0,1)) +
#   coord_fixed() +  
#   theme_bw(18) +
#   theme(axis.title = element_text(size=14),
#         axis.text = element_text(size=14)) +
#   xlab('albedo (data)') +
#   ylab('albedo (model)')
# ggsave('figures/cal_model_both_vs_data.png')
# ggsave('figures/cal_model_both_vs_data.pdf')
# 
# 


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
# mod7_free = readRDS(paste0('data/calibration_mod7_free_', alb_prod, '.RDS'))
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


