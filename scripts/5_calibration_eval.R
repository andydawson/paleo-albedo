# 5_calibration_eval.R: how well do the calibration models fit the modern albedo?
# Interp (spatially complete) path only: the point-based code and its guards are
# removed. Since issue I04a the selected model, the draw count and the random seed come
# from config.R and files are read and written through the run manifest; the evaluation
# itself is unchanged.


library(ggplot2)
library(mgcv)
library(gratia)
library(reshape2)
library(dplyr)

# Settings, file locations and this run's manifest: run_begin() reads config.R (or the file
# given with --config), sets up the paths of R/paths.R (issue I07) and starts the run's
# manifest; files are then read and written through it (issues I05, I04a).
source('R/run_manifest.R')
run   = run_begin('5_calibration_eval')
cfg   = run$cfg
paths = run$paths
alb_prod = paths$tag   # the albedo product in file names

# The model saved as the selected calibration model for script 6, by id (config.R
# analysis$calibration$model_id, question B4; formulas in R/model_registry.R), recorded in
# the manifest with its terms.
model_id = cfg$analysis$calibration$model_id
run_record(run, 'selected_model', list(id = model_id, terms = calibration_terms(model_id),
                                       family = 'betar(link = "logit")', method = 'REML',
                                       control = CALIBRATION_CONTROL))

# Simulated draws per evaluation (config.R analysis$draws, questions A2, A3), and the base
# random seed of this run, recorded in its manifest (R/random.R).
source('R/random.R')
n_draws   = cfg$analysis$draws$evaluation
seed_base = run_record(run, 'random_seed', list(setting = cfg$analysis$draws$seed,
                                                base = base_seed(cfg$analysis$draws$seed)))$base


# The months analysed, from config.R (analysis$months, issue I04b); month names and
# numbers come from the shared calendar, R/calendar.R (issue I02).
source('R/calendar.R')
months = cfg$analysis$months

cal_interp_data = read_input(run, path_file(paths, 'calibration_table'))

pdf(path_figure(paths, paste0('cal_model_vs_data_gam_error_months.pdf')), width=10, height=10)
for (n in 1:length(months)){

  month = months[n]

  print(month)

  # The selected model (model_id above), saved for script 6.
  cal_interp_model = read_input(run, path_calibration_model(paths, model_id, month))
  summary(cal_interp_model)

  write_output(run, cal_interp_model, path_calibration_model(paths, 'selected', month))

  cal_interp_predict_gam = predict(cal_interp_model,
                                   newdata = cal_interp_data,
                                   type = 'response')#,

  cal_interp_eval_gam = data.frame(cal_interp_data[, c('x', 'y', 'elev', 'ET', 'OL', 'ST', month)],
                                   alb_mean = cal_interp_predict_gam)

  cor_month = cor(cal_interp_eval_gam[,month], cal_interp_eval_gam$alb_mean, use='complete')

  cor_month

  cal_interp_sim_gam = simulate(cal_interp_model,
                                nsim = n_draws,
                                seed = derive_seed(seed_base, '5_calibration_eval', month, 'model_vs_data'),
                                data = cal_interp_data[,c('x', 'y', 'elev', 'ET', 'OL', 'ST', month)])

  cal_interp_sim_gam = data.frame(cal_interp_data[,c('x', 'y', month)],
                                  cal_interp_sim_gam)
  cal_interp_sim_gam_melt = melt(cal_interp_sim_gam,
                                 id.vars = c('x', 'y', month))
  colnames(cal_interp_sim_gam_melt) = c('x', 'y', 'month', 'variable', 'value')

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

  print(p)

}
dev.off()

cal_interp_sim_gam_all = data.frame(x = numeric(0),
                                    y = numeric(0),
                                    alb_data = numeric(0),
                                    variable = character(0),
                                    alb_model = numeric(0),
                                    month = character(0))

cor_all = rep(NA, length(months))

for (n in 1:length(months)){

  month = months[n]

  print(month)

  # The selected model (model_id above), saved for script 6.
  cal_interp_model = read_input(run, path_calibration_model(paths, model_id, month))
  summary(cal_interp_model)

  write_output(run, cal_interp_model, path_calibration_model(paths, 'selected', month))

  cal_interp_predict_gam = predict(cal_interp_model,
                                   newdata = cal_interp_data,
                                   type = 'response')#,

  cal_interp_eval_gam = data.frame(cal_interp_data[, c('x', 'y', 'elev', 'ET', 'OL', 'ST', month)],
                                   alb_mean = cal_interp_predict_gam)

  cor_month = cor(cal_interp_eval_gam[,month], cal_interp_eval_gam$alb_mean, use='complete')
  cor_month
  cor_all[n] = cor_month

  cal_interp_sim_gam = simulate(cal_interp_model,
                                nsim = n_draws,
                                seed = derive_seed(seed_base, '5_calibration_eval', month, 'statistics'),
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

annotation_data = data.frame(month=months, label=round(cor_all,2), x=-Inf, y=Inf)

p = ggplot(data=cal_interp_sim_gam_quants) +
  geom_point(aes(x=alb_data, y=alb_mean), size=0.8, alpha=0.3) +
  geom_linerange(aes(x=alb_data, ymin=alb_lo, ymax=alb_hi), alpha=0.2) +
  geom_abline(slope=1, intercept=0, colour="red", lwd=0.5, lty=2, alpha=0.5) +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  coord_fixed() +
  theme_light(15) +
  xlab('albedo (data)') +
  ylab('albedo (model)') +
  facet_wrap(~factor(month, levels = months), ncol=4)
p = p + geom_text(
  data = annotation_data,
  aes(x = x, y = y, label = label),
  hjust = -0.2,    # 0 for left justification
  vjust = 1.3,    # 1 for top justification
  inherit.aes = FALSE, # Do not inherit aesthetics from the main plot
  color = "grey30",
  size = 4
)
print(p)
ggsave(path_figure(paths, 'cal_model_vs_data_gam_error_facet.pdf'), width=12, height=10)
ggsave(path_figure(paths, 'cal_model_vs_data_gam_error_facet.png'), width=12, height=10)

cal_interp_sim_gam_quants = cal_interp_sim_gam_quants %>%
  mutate(in_credible=((alb_data>alb_lo)&(alb_data<alb_hi)))

cal_interp_sim_gam_quants$alb_diff = cal_interp_sim_gam_quants$alb_mid - cal_interp_sim_gam_quants$alb_data

p = ggplot(data=cal_interp_sim_gam_quants) +
  geom_histogram(aes(x=alb_diff, y=..density..)) +
  theme_light(15) +
  facet_wrap(~factor(month, levels = months), ncol=4)
print(p)
ggsave(path_figure(paths, 'cal_model_data_diff_histogram_facet.pdf'), width=12, height=10)
ggsave(path_figure(paths, 'cal_model_data_diff_histogram_facet.png'), width=12, height=10)

cal_interp_sim_gam_quants %>%
  group_by(month) %>%
  summarize(frac_in=sum(in_credible, na.rm=TRUE)/n())

cal_diff = cal_interp_sim_gam_quants %>%
  group_by(month) %>%
  summarize(alb_corr = round(cor(alb_mid, alb_data,, use = "pairwise.complete.obs"),2),
            frac_in=round(sum(in_credible, na.rm=TRUE)/n(),2),
            diff_lo = round(quantile(alb_diff, c(0.025), na.rm=TRUE),2),
            diff_hi = round(quantile(alb_diff, c(0.975), na.rm=TRUE),2),
            .groups = "keep")

cal_diff

colnames(cal_diff) = c('month', 'correlation', 'data in credible interval', 'diff lower (model - data; 2.5%)', 'diff upper (model - data; 97.5%)')

write_output(run, cal_diff, path_file(paths, 'calibration_stats'), write.csv, row.names = FALSE)

cal_sim_SPE = data.frame(x = numeric(0),
                         y = numeric(0),
                         alb_data = numeric(0),
                         variable = character(0),
                         alb_pred = numeric(0),
                         month = character(0),
                         model = character(0))

models = c('SP_ELEV_LC', 'ELEV_LC', 'LC', 'SP_LC', 'SP', 'SP_ELEV', 'ELEV')

cal_sims = data.frame(matrix(nrow=0, ncol=7))
colnames(cal_sims) = c('x', 'y', 'alb_data', 'variable', 'alb_pred', 'month', 'model')

for (n in 1:length(months)){

  month = months[n]

  print(month)

  # CM: these seven were read from output/calibration/spatial_experiment/, a directory
  # script 4 never writes, so the script always stopped here. They are now read from where
  # script 4 writes them (issue I07). SP_ELEV_LC is script 4's spatial_elev_cover fit, the
  # same formula as model 8; until issue I04a it was the selected model, which would be
  # mislabelled once the selected model is a setting (the repeated fits of model 8 are a
  # question for Andria, general 7).
  model_SP_ELEV_LC = read_input(run, path_calibration_model(paths, 'spatial_elev_cover', month))

  model_ELEV_LC = read_input(run, path_calibration_model(paths, 'cover_elev', month))
  model_LC = read_input(run, path_calibration_model(paths, 'cover', month))
  model_SP_LC = read_input(run, path_calibration_model(paths, 'spatial_cover', month))
  model_SP = read_input(run, path_calibration_model(paths, 'spatial', month))
  model_SP_ELEV = read_input(run, path_calibration_model(paths, 'spatial_elev', month))
  model_ELEV = read_input(run, path_calibration_model(paths, 'elev', month))

  for (model in models){

    model_name = eval(as.name(paste0('model_', model)))

    sim = simulate(model_name,
                              nsim = n_draws,
                              seed = derive_seed(seed_base, '5_calibration_eval', month, paste0('experiment_', model)),
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

}

cal_sims$month = factor(cal_sims$month, levels = months)

cal_sims_quants = cal_sims %>%
  group_by(x, y, alb_data, month, model) %>%
  summarize(alb_mean = mean(alb_pred),
            alb_lo = quantile(alb_pred, c(0.025), na.rm=TRUE),
            alb_mid = quantile(alb_pred, c(0.5), na.rm=TRUE),
            alb_hi = quantile(alb_pred, c(0.975), na.rm=TRUE),
            .groups = "keep")

p = ggplot(data=cal_sims_quants) +
  geom_point(aes(x=alb_data, y=alb_mean), size=2, alpha=0.3) +
  geom_linerange(aes(x=alb_data, ymin=alb_lo, ymax=alb_hi), alpha=0.3) +
  geom_abline(slope=1, intercept=0, colour="red", lwd=1, lty=2, alpha=0.4) +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  coord_fixed() +
  theme_bw(18) +
  xlab('albedo (data)') +
  ylab('albedo (model)') +
  facet_wrap(~month)
print(p)

p = ggplot(data=cal_sims_quants) +
  geom_point(aes(x=alb_data, y=alb_mean), size=2, alpha=0.3) +
  geom_linerange(aes(x=alb_data, ymin=alb_lo, ymax=alb_hi), alpha=0.3) +
  geom_abline(slope=1, intercept=0, colour="red", lwd=1, lty=2, alpha=0.4) +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  coord_fixed() +
  theme_bw(18) +
  xlab('albedo (data)') +
  ylab('albedo (model)') +
  facet_wrap(~model)
print(p)

p = ggplot(data=cal_sims_quants) +
  geom_point(aes(x=alb_data, y=alb_mean), size=2, alpha=0.3) +
  geom_linerange(aes(x=alb_data, ymin=alb_lo, ymax=alb_hi), alpha=0.3) +
  geom_abline(slope=1, intercept=0, colour="red", lwd=1, lty=2, alpha=0.4) +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  coord_fixed() +
  theme_bw(18) +
  xlab('albedo (data)') +
  ylab('albedo (model)') +
  facet_grid(month~model)
print(p)

cal_sims$alb_error = cal_sims$alb_pred - cal_sims$alb_data

p = ggplot(data=cal_sims) +
  geom_histogram(aes(x=alb_error, y=..density..), size=2, alpha=0.3) +
  theme_bw(18) +
  facet_wrap(~model)
print(p)

# CM: November-only block (model comparison, re-save of the November selected model, and its
# figures), to the end of the script. It runs only when November is among the selected
# months; not re-indented, to keep the diff against Andria's lines small (issue I02).
if ('nov' %in% months) {
month = "nov"

mod1_interp = read_input(run, path_calibration_model(paths, 'mod1', month))
mod2_interp = read_input(run, path_calibration_model(paths, 'mod2', month))
mod3_interp = read_input(run, path_calibration_model(paths, 'mod3', month))
mod4_interp = read_input(run, path_calibration_model(paths, 'mod4', month))
mod5_interp = read_input(run, path_calibration_model(paths, 'mod5', month))
mod6_interp = read_input(run, path_calibration_model(paths, 'mod6', month))
mod7_interp = read_input(run, path_calibration_model(paths, 'mod7', month))
mod8_interp = read_input(run, path_calibration_model(paths, 'mod8', month))

AIC(mod1_interp, mod2_interp, mod3_interp, mod4_interp, mod5_interp,
    mod6_interp, mod7_interp, mod8_interp)

anova_gam_interp = anova.gam(mod1_interp, mod2_interp, mod3_interp, mod4_interp,
                             mod5_interp, mod6_interp, mod7_interp, mod8_interp,
                             test = "Chisq")
anova_gam_interp

# The selected model (model_id above), saved again for November.
cal_interp_model = read_input(run, path_calibration_model(paths, model_id, month))
summary(cal_interp_model)

write_output(run, cal_interp_model, path_calibration_model(paths, 'selected', month))

cal_interp_predict_gam = predict(cal_interp_model,
                                 newdata = cal_interp_data,
                                 type = 'response')#,

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
ggsave(path_figure(paths, paste0('cal_model_vs_data_gam_interp_', month, '.png')))
ggsave(path_figure(paths, paste0('cal_model_vs_data_gam_interp_', month, '.pdf')))

cal_interp_sim_gam = simulate(cal_interp_model,
                              nsim = n_draws,
                              seed = derive_seed(seed_base, '5_calibration_eval', month, 'november'),
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
ggsave(path_figure(paths, paste0('cal_model_vs_data_gam_error_interp_', month, '.png')))
ggsave(path_figure(paths, paste0('cal_model_vs_data_gam_error_interp_', month, '.pdf')))
} # CM: end of the November-only block

run_finish(run)
