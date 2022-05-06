library(gam)
library(mgcv)
library(ggplot2)
library(dplyr)
library(tidyr)

cal_data = readRDS('data/lct_albedo_snow_modern_glob.RDS')
cal_data = data.frame(site=seq(1,nrow(cal_data)), cal_data)
cal_data = cal_data[,c('site', 'ET', 'OL', 'ST')]

# dat_clim_alb = readRDS('data/calibration-albedo-climateNA.RDS')
# foo = pivot_wider(dat_clim_alb[,1:5], names_from = month, values_from = c('snowpack', 'tsnow', 'et'))
# dat_clim_alb$snowpack

dat_clim_alb = readRDS('data/calibration-albedo-climateNA-season.RDS')


dat = merge(dat_clim_alb, cal_data, by=c('site'))
# 
# dat$snowpack = dat$snowpack_sm2
# dat$log_snowpack = log(dat$snowpack_sm2+0.1)
# 
# dat$log_snowpack_sm1 = log(dat$snowpack_sm1+0.1)

###############################################################################################################
## may calibration model
###############################################################################################################

ctrl <- list(nthreads=4)

# dat_sub_alb = dat[which(dat$month==4),which(!(colnames(dat) %in% 'dd0'))]
# 
# dat_sub_dd0 = dat[which(dat$month==1), c('site', 'dd0')]

# dat_sub = merge(dat_sub_alb, dat_sub_dd0)

dat_sub = dat[which(dat$season=='annual'),]
dat_sub = dat_sub[which(dat_sub$month==4),]


# some NA values for albedo
# fix this to pull closest non-NA value
dim(dat_sub[which(is.na(dat_sub$alb)),])

# just x and y
mod = gam(alb ~ s(x, y, bs="gp", k=130),
          data=dat_sub, 
          family=betar(link="logit"), 
          method="REML", 
          na.action=na.omit, 
          control=ctrl)

gam.check(mod)

vis.gam(mod,theta=30)

# # just x and y
# mod = gam(alb ~ s(x, y, bs='tp', k=175),
#           data=dat_sub, 
#           family=betar(link="logit"), 
#           method="REML", 
#           na.action=na.omit)
# 
# gam.check(mod)
# 
# vis.gam(mod,theta=30)

# # add eleveation
# mod2 = gam(alb ~ s(x, y, bs='tp', k=175) + s(elev, k=30),
#           data=dat_sub, 
#           family=betar(link="logit"), 
#           method="REML", 
#           na.action=na.omit)

# add eleveation
mod2 = gam(alb ~ s(x, y, bs="gp", k=130) + s(elev, k=30),
           data=dat_sub, 
           family=betar(link="logit"), 
           method="REML", 
           na.action=na.omit, 
           control=ctrl)

gam.check(mod2)

vis.gam(mod,theta=30)

AIC(mod, mod2)

# add OL
mod3 = gam(alb ~ s(x, y, bs='gp', k=130) + s(elev, k=30) + s(OL, k=20),
           data=dat_sub, 
           family=betar(link="logit"), 
           method="REML", 
           na.action=na.omit, 
           control=ctrl)

gam.check(mod3)

AIC(mod, mod2, mod3)

# add ET
mod4 = gam(alb ~ s(x, y, bs='gp', k=130) + s(elev, k=30) + s(OL, k=20) + s(ET, k=20),
           data=dat_sub, 
           family=betar(link="logit"), 
           method="REML", 
           na.action=na.omit, 
           control=ctrl)

gam.check(mod4)

AIC(mod, mod2, mod3, mod4)

# add psnow
mod5 = gam(alb ~ s(x, y, bs='gp', k=130) +  s(elev, k=30) + s(OL, ET,  k=20),# + s(ET), # s(psnow),
           data=dat_sub, 
           family=betar(link="logit"), 
           method="REML", 
           na.action=na.omit, 
           control=ctrl)

gam.check(mod5)

AIC(mod, mod2, mod3, mod4, mod5)

# add snowpack
mod6 = gam(alb ~ s(x, y, bs='gp', k=130) + s(elev, k=30) + s(OL, k=20) + s(ET, k=20) + s(ST, k=20), #s(snowpack),
           data=dat_sub, 
           family=betar(link="logit"), 
           method="REML", 
           na.action=na.omit,
           control=ctrl)

gam.check(mod6)

AIC(mod, mod2, mod3, mod4, mod5, mod6)

# add snowpack
mod7 = gam(alb ~ s(x, y, bs='gp', k=130) + s(elev, k=30) + s(OL, ET, ST, k=20), #s(snowpack),
           data=dat_sub, 
           family=betar(link="logit"), 
           method="REML", 
           na.action=na.omit,
           control=ctrl)

gam.check(mod7)

AIC(mod, mod2, mod3, mod4, mod5, mod6, mod7)

predict_test = predict.gam(mod7, 
                            newdata = dat_sub , 
                            type    = 'response')

saveRDS(mod7, 'data/calibration_model.RDS')


# add snowpack
mod8 = gam(alb ~ s(x, y, bs='gp', k=130) + s(elev, k=30) + s(OL, ET, ST, k=20) + s(dd0_s, k=20),
           data=dat_sub, 
           family=betar(link="logit"), 
           method="REML", 
           na.action=na.omit,
           control=ctrl)

vis.gam(mod8,theta=30)

gam.check(mod8)

AIC(mod, mod2, mod3, mod4, mod5, mod6, mod7, mod8)

# add snowpack
mod9 = gam(alb ~ s(x, y, bs='gp', k=130) + s(elev, k=30) + s(OL, ET, ST, k=20) + s(dd5_s, k=20),
           data=dat_sub, 
           family=betar(link="logit"), 
           method="REML", 
           na.action=na.omit,
           control=ctrl)

gam.check(mod9)

AIC(mod, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9)

saveRDS(mod9, 'data/calibration_model.RDS')

# 
# saveRDS(mod9, 'data/calibration_model.RDS')


# add snowpack
mod10 = gam(alb ~ s(x, y, bs='gp', k=130) + s(elev, k=30) + s(OL, ET, ST, k=20)+ s(dd5_s, k=20) + s(tmin, k=20),
           data=dat_sub, 
           family=betar(link="logit"), 
           method="REML", 
           na.action=na.omit,
           control=ctrl)

gam.check(mod10)

AIC(mod, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10)

saveRDS(mod10, 'data/calibration_model.RDS')

# add snowpack
mod11 = gam(alb ~ s(x, y, bs='gp', k=130) + s(elev, k=30) + s(OL, ET, ST, k=20)+ s(dd5_s, tmin, k=20),
            data=dat_sub, 
            family=betar(link="logit"), 
            method="REML", 
            na.action=na.omit,
            control=ctrl)

gam.check(mod11)

AIC(mod, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10, mod11)


# add snowpack
mod12 = gam(alb ~ s(x, y, bs='tp', k=175) + s(elev, k=30) + s(OL, ET, ST, k=150) + s(tmin, k=50),
            data=dat_sub, 
            family=betar(link="logit"), 
            method="REML", 
            na.action=na.omit)

gam.check(mod12)

# plot(mod)
# plot(mod6, residuals=TRUE, all.terms=TRUE, pch=1)

preds = predict.gam(mod12, 
                    type="response", 
                    newdata=dat_sub,
                    se.fit=TRUE)

dat_sub$preds_mod12 = preds$fit
#dat_all$predict = preds$fit

ggplot(data=dat_sub) + 
  geom_point(aes(x=alb, y=preds_mod12)) +
  geom_abline(slope=1, intercept=0, colour="red") +
  xlim(c(0,1)) + 
  ylim(c(0,1))

AIC(mod, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10, mod11, mod12)

# add snowpack
mod13 = gam(alb ~ s(x, y, bs='tp', k=175) + s(elev, k=50) + s(OL, ET, ST, k=150)  + s(et, k=50),#s(OL, ET, ST, k=150) + s(et, k=75),
            data=dat_sub, 
            family=betar(link="logit"), 
            method="REML", 
            na.action=na.omit)

gam.check(mod13)

# plot(mod)
# plot(mod6, residuals=TRUE, all.terms=TRUE, pch=1)

preds = predict.gam(mod13, 
                    type="response", 
                    newdata=dat_sub,
                    se.fit=TRUE)

dat_sub$preds_mod13 = preds$fit
#dat_all$predict = preds$fit

ggplot(data=dat_sub) + 
  geom_point(aes(x=alb, y=preds_mod13)) +
  geom_abline(slope=1, intercept=0, colour="red") +
  xlim(c(0,1)) + 
  ylim(c(0,1))

AIC(mod, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10, mod11, mod12, mod13)


# add snowpack
mod13 = gam(alb ~ s(x, y, bs='tp', k=175) + s(elev, k=30) + s(OL, ET, ST, k=50) + s(et, k=50),
            data=dat_sub, 
            family=quasibinomial(link="logit"), 
            method="REML", 
            na.action=na.omit)

gam.check(mod13)

# plot(mod)
# plot(mod6, residuals=TRUE, all.terms=TRUE, pch=1)

preds = predict.gam(mod13, 
                    type="response", 
                    newdata=dat_sub,
                    se.fit=TRUE)

dat_sub$preds_mod13 = preds$fit
#dat_all$predict = preds$fit

ggplot(data=dat_sub) + 
  geom_point(aes(x=alb, y=preds_mod13)) +
  geom_abline(slope=1, intercept=0, colour="red") +
  xlim(c(0,1)) + 
  ylim(c(0,1))

AIC(mod, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10, mod11, mod12, mod13)

# saveRDS(mod13, 'data/calibration_model.RDS')

# # add snowpack
# mod7 = gam(alb ~ s(x, y, bs='tp', k=60) + s(elev) + s(OL) + s(ET) + s(snowpack),
#            data=dat_sub, 
#            family=betar(link="logit"), 
#            method="REML", 
#            na.action=na.omit)
# 
# gam.check(mod7)
# 
# # plot(mod)
# # plot(mod6, residuals=TRUE, all.terms=TRUE, pch=1)
# 
# preds = predict.gam(mod7, 
#                     type="response", 
#                     newdata=dat_sub,
#                     se.fit=TRUE)
# 
# dat_sub$preds_mod7 = preds$fit
# #dat_all$predict = preds$fit
# 
# ggplot(data=dat_sub) + 
#   geom_point(aes(x=alb, y=preds_mod7)) +
#   geom_abline(slope=1, intercept=0, colour="red") +
#   xlim(c(0,1)) + 
#   ylim(c(0,1))
# 
# AIC(mod, mod2, mod3, mod5, mod6, mod7)

###############################################################################################################
## may calibration model
###############################################################################################################

cal_model = readRDS('data/calibration_model.RDS')

preds = predict.gam(cal_model, 
                    type="response", 
                    newdata=dat_sub,
                    se.fit=TRUE)

dat_sub$preds = preds$fit
#dat_all$predict = preds$fit

ggplot(data=dat_sub) + 
  geom_point(aes(x=alb, y=preds)) +
  geom_abline(slope=1, intercept=0, colour="red", size=1) +
  xlim(c(0,1)) + 
  ylim(c(0,1)) +
  coord_fixed() +  
  theme_bw() +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=14)) +
  xlab('Albedo Data') +
  ylab('Albedo Model')
ggsave('figures/cal_model_vs_data.png')
ggsave('figures/cal_model_vs_data.pdf')

dat_sub$preds = cal_model$fitted.values


dat_sub$diff_mod5 = dat_sub$preds_mod5 - dat_sub$alb
dat_sub$diff_mod6 = dat_sub$preds_mod6 - dat_sub$alb

# saveRDS(dat_all, 'data/april_modern_pred.RDS')

ggplot()+
  geom_polygon(data=world_proj, aes(x=x, y=y, group=group), fill='white')+
  geom_point(data=dat_sub, aes(x=x, y=y, colour = diff_mod5))+
  scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white', limits = c(-0.3,0.3))+
  theme_bw()
# ggsave('figures/april_modern_diff.png')
# scale_fill_brewer(type = "div", palette = 'RdBu')+

ggplot()+
  geom_polygon(data=world_proj, aes(x=x, y=y, group=group), fill='white')+
  geom_point(data=dat_sub, aes(x=x, y=y, colour = diff_mod6))+
  scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white', limits = c(-0.2,0.2))+
  theme_bw()


ggplot(data=dat_sub) + 
  geom_histogram(aes(x=diff_mod6)) +
  theme_bw()


# #############################################################################
# ## MODEL TEST
# #############################################################################
# #thornthwaite model
# 
# mod2 = gam(finealb05/1000 ~ s(x, y, bs='tp', k=50) + s(ET) + s(ST) + s(OL) + s(psnow05, k=20)
#           + s(elevation),
#           data=dat, 
#           family=betar(link="logit"), 
#           method="REML", na.action=na.omit)
# 
# gam.check(mod2)
# 
# plot(mod2)
# 
# plot(mod2, residuals=TRUE, all.terms=TRUE, pch=1)
# 
# new_data = dat
# 
# 
# preds = predict.gam(mod2, 
#                     type="response", 
#                     newdata=dat,
#                     se.fit=TRUE)
# 
# dat$alb_pred_5 = preds$fit
# #dat_all$predict = preds$fit
# 
# ggplot(data=dat) + 
#   geom_point(aes(x=finealb05/1000, y=alb_pred_5)) +
#   geom_abline(slope=1, intercept=0, colour="red") +
#   xlim(c(0,1)) + 
#   ylim(c(0,1))
# 
# dat$diff = dat$alb_pred_5 - dat$finealb05/1000
# # saveRDS(dat_all, 'data/april_modern_pred.RDS')
# 
# ggplot()+
#   geom_polygon(data=world_proj, aes(x=x, y=y, group=group), fill='white')+
#   geom_point(data=dat, aes(x=x, y=y, colour = diff))+
#   scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white', limits = c(-0.4,0.4))+
#   theme_bw()
# # ggsave('figures/april_modern_diff.png')
# # scale_fill_brewer(type = "div", palette = 'RdBu')+
# 
# #############################################################################
# ## AIC
# #############################################################################
# 
# AIC(mod,mod2)
# 
# 
# #############################################################################
# ## MODEL 2
# #############################################################################
# #thornthwaite model
# 
# mod = gam(finealb05/1000 ~ s(x, y, bs='tp') + s(ET) + s(ST) + s(OL) + s(tsnow5)
#           + s(elevation),
#           data=dat, 
#           family=betar(link="logit"), 
#           method="REML", na.action=na.omit)
# 
# gam.check(mod)
# 
# plot(mod)
# 
# plot(mod, residuals=TRUE, all.terms=TRUE, pch=1)
# 
# new_data = dat
# 
# 
# preds = predict.gam(mod, 
#                     type="response", 
#                     newdata=dat,
#                     se.fit=TRUE)
# 
# dat$alb_may_pred = preds$fit
# #dat_all$predict = preds$fit
# 
# ggplot(data=dat) + 
#   geom_point(aes(x=finealb05/1000, y=alb_may_pred)) +
#   geom_abline(slope=1, intercept=0, colour="red") +
#   xlim(c(0,1)) + 
#   ylim(c(0,1))
# 
# dat_all$diff = dat_all$alb_april_pred - dat_all$finealb04/1000
# saveRDS(dat_all, 'data/april_modern_pred.RDS')
# 
# ggplot()+
#   geom_polygon(data=world_proj, aes(x=x, y=y, group=group), fill='white')+
#   geom_point(data=dat_all, aes(x=x, y=y, colour = diff))+
#   scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white', limits = c(-0.4,0.4))+
# #  theme_bw()
# ggsave('figures/april_modern_diff.png')
# # scale_fill_brewer(type = "div", palette = 'RdBu')+
# 
# 
# 
# #############################################################################
# ## MODEL 2
# #############################################################################
# #first model with no snow cover, or elevation
# 
# mod = gam(alb ~ s(x, y, bs='tp', k=150) + s(ET, k=5) + s(ST, k=20) + s(OL, k=19), 
#           data=dat_all, 
#           family=betar(link="logit"), 
#           method="REML")
# 
# gam.check(mod)
# 
# plot(mod)
# 
# plot(mod, residuals=TRUE, all.terms=TRUE, pch=1)
# 
# new_data = dat_all
# 
# preds = predict.gam(mod, 
#                     type="response", 
#                     data=new_data,
#                     se.fit=TRUE)
# 
# dat_all$summer_pred = preds$fit
# 
# predictions = data.frame(predictions, dat_all[-c(1:8)])
# 
# ggplot(data=dat_all) + 
#   geom_point(aes(x=alb, y=alb_pred)) +
#   geom_abline(slope=1, intercept=0, colour="red") +
#   xlim(c(0,1)) + 
#   ylim(c(0,1))
# 
# 
# #############################################################################
# ## MODEL 3
# #############################################################################
# #snowprobability model using MODIS data 
# 
# mod2 = gam(finealb05/1000 ~ s(x, y, bs='tp') + s(ET) + s(ST) + s(OL) + s(psnow05)
#           + s(elevation),
#           data=dat, 
#           family=betar(link="logit"), 
#           method="REML", na.action=na.omit)
# 
# gam.check(mod2)
# 
# plot(mod2)
# 
# plot(mod2, residuals=TRUE, all.terms=TRUE, pch=1)
# 
# new_data = dat
# 
# 
# preds2 = predict.gam(mod2, 
#                     type="response", 
#                     newdata=dat,
#                     se.fit=TRUE)
# 
# dat$alb_may_pred2 = preds2$fit
# #dat_all$predict = preds$fit
# 
# ggplot(data=dat) + 
#   geom_point(aes(x=finealb05/1000, y=alb_may_pred2)) +
#   geom_point(aes(x=finealb05/1000, y=alb_may_pred), colour= 'blue') +
#   geom_abline(slope=1, intercept=0, colour="red") +
#   xlim(c(0,1)) + 
#   ylim(c(0,1))
# 
# 
# #############################################################################
# ## MODEL 4
# #############################################################################
# #snowPACK model 
# 
# mod3 = gam(finealb05/1000 ~ s(x, y, bs='tp') + s(ET) + s(ST) + s(OL) + s(snowpack5)
#            + s(elevation),
#            data=dat, 
#            family=betar(link="logit"), 
#            method="REML", na.action=na.omit)
# 
# gam.check(mod3)
# 
# plot(mod3)
# 
# plot(mod2, residuals=TRUE, all.terms=TRUE, pch=1)
# 
# new_data = dat
# 
# 
# preds3 = predict.gam(mod3, 
#                      type="response", 
#                      newdata=dat,
#                      se.fit=TRUE)
# 
# dat$alb_may_pred3 = preds3$fit
# #dat_all$predict = preds$fit
# 
# ggplot(data=dat) + 
#   geom_point(aes(x=finealb05/1000, y=alb_may_pred3), colour = 'orange') +
#   geom_point(aes(x=finealb05/1000, y=alb_may_pred2), colour= 'blue') +
#   geom_abline(slope=1, intercept=0, colour="red") +
#   xlim(c(0,1)) + 
#   ylim(c(0,1))
# 
# 
# ################################################################################
# AIC(mod,mod2,mod3)
# 
# 
# ################################################################################
# 
# # mod = gam(alb ~ te(x, y, k=8) + s(ST, k=5) + s(OL, k=5) + s(ET, k=5), 
# #           data=dat_all, 
# #           family=binomial(link="logit"),
# #           method="REML")
# # 
# # gam.check(mod)
# # 
# # plot(mod)
# # 
# # plot(mod, residuals=TRUE, all.terms=TRUE, pch=1)
# # 
# # new_data = dat_all
# # 
# # preds = predict.gam(mod, 
# #                     type="response", 
# #                     data=new_data,
# #                     se.fit=TRUE)
# # 
# # dat_all$alb_pred = preds$fit
# # 
# # ggplot(data=dat_all) + 
# #   geom_point(aes(x=alb, y=alb_pred)) +
# #   geom_abline(slope=1, intercept=0, colour="red") +
# #   xlim(c(0,1)) + 
# #   ylim(c(0,1))
# # 
# # AIC(mod,mod2,mod3)