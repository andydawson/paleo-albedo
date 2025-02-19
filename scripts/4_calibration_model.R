library(gam)
library(mgcv)
library(ggplot2)
library(dplyr)
library(tidyr)


alb_prod = "bluesky"
# alb_prod = "albclim"

###############################################################################################################
## point calibration model
###############################################################################################################

cal_data =readRDS(paste0('data/calibration_modern_lct_', alb_prod, '.RDS'))
# cal_data =readRDS('data/lct_albedo_snow_modern_bluesky.RDS')

cal_data2 = data.frame(site=seq(1,nrow(cal_data)), cal_data)
cal_data3 = cal_data2[,c('site', 'ET', 'OL', 'ST')]

###############################################################################################################
## march calibration model
###############################################################################################################

ctrl <- list(nthreads=8, maxit=500)

# # just x and y
#works better with smaller k values 
# # was 900
# mod = mgcv::bam(mar ~ s(x, y, bs="gp", k=300),
#           data=cal_data, 
#           family=betar(link="logit"), 
#           method="REML", 
#           na.action=na.omit, 
#           control=ctrl)
# gam.check(mod)
# #vis.gam(mod,theta=30)


# # just x and y
mod1 = mgcv::bam(mar ~ s(x, y, bs="gp", k=350),
          data=cal_data, 
          family=betar(link="logit"), 
          method="REML", 
          na.action=na.omit, 
          control=ctrl)
gam.check(mod1)

saveRDS(mod1, paste0('data/calibration_mod1_', alb_prod, '.RDS'))


vis.gam(mod1,theta=30)

# # add elevation
# #tp vs gp model
# mod2 = mgcv::bam(mar ~ s(x, y, bs='tp', k=400) + s(elev, k=30),
#           data=cal_data,
#           family=betar(link="logit"),
#           method="REML",
#           na.action=na.omit)

# add eleveation
#gp model works better than tp
mod2 = mgcv::bam(mar ~ s(x, y, bs="gp", k=350) + s(elev, k=50),
           data=cal_data, 
           family=betar(link="logit"), 
           method="REML", 
           na.action=na.omit, 
           control=ctrl)
gam.check(mod2)
#vis.gam(mod,theta=30)

saveRDS(mod2, paste0('data/calibration_mod2_', alb_prod, '.RDS'))


AIC(mod1, mod2)

# add OL
mod3 = mgcv::bam(mar ~ s(x, y, bs='gp', k=350) + s(elev, k=50) + s(OL, k=30),
           data=cal_data, 
           family=betar(link="logit"), 
           method="REML", 
           na.action=na.omit, 
           control=ctrl)
gam.check(mod3)

saveRDS(mod3, paste0('data/calibration_mod3_', alb_prod, '.RDS'))


AIC(mod1, mod2, mod3)

anova_gam = anova.gam(mod1, mod2, mod3, test = "Chisq")
anova_gam

summary(mod3)

# add ET
mod4 = mgcv::bam(mar ~ s(x, y, bs='gp', k=350) + s(elev, k=50) + s(OL, k=30) + s(ET, k=30),
           data=cal_data, 
           family=betar(link="logit"), 
           method="REML", 
           na.action=na.omit, 
           control=ctrl)
gam.check(mod4)

saveRDS(mod4, paste0('data/calibration_mod4_', alb_prod, '.RDS'))


AIC(mod1, mod2, mod3, mod4)

anova_gam = anova.gam(mod1, mod2, mod3, mod4, test = "Chisq")
anova_gam

summary(mod4)

# add ST
#this takes a while 
# mod5 = mgcv::gam(mar ~ s(x, y, bs='gp', k=30) + s(elev, k=10) + s(OL, k=10) + s(ET, k=10) + s(ST, k=10),
#            data=cal_data, 
#            family=betar(link="logit"), 
#            method="REML", 
#            na.action=na.omit, 
#            control=ctrl)
mod5 = mgcv::bam(mar ~ s(x, y, bs='gp', k=350) + s(elev, k=50) + s(OL, k=30) + s(ET, k=30) + s(ST, k=30),
                 data=cal_data, 
                 family=betar(link="logit"), 
                 method="REML", 
                 na.action=na.omit, 
                 control=ctrl)
gam.check(mod5)

saveRDS(mod5, paste0('data/calibration_mod5_', alb_prod, '.RDS'))

AIC(mod1, mod2, mod3, mod4, mod5)

anova_gam = anova.gam(mod1, mod2, mod3, mod4, test = "Chisq")
anova_gam

summary(mod4)

mod6 = mgcv::bam(mar ~ s(x, y, bs='gp', k=350) + s(elev, k=50) + s(OL, ET, ST, k=30),
                 data=cal_data, 
                 family=betar(link="logit"), 
                 method="REML", 
                 na.action=na.omit, 
                 control=ctrl)
gam.check(mod6)

saveRDS(mod6, paste0('data/calibration_mod6_', alb_prod, '.RDS'))

AIC(mod1, mod2, mod3, mod4, mod5, mod6)

anova_gam = anova.gam(mod1, mod2, mod3, mod4, mod5, mod6, test = "Chisq")
anova_gam

summary(mod6)

mod7 = mgcv::bam(mar ~ s(x, y, bs='gp', k=350) + s(elev, k=50) + s(OL, ET, ST, bs='gp', k=30),
                 data=cal_data, 
                 family=betar(link="logit"), 
                 method="REML", 
                 na.action=na.omit, 
                 control=ctrl)
gam.check(mod7)

saveRDS(mod7, paste0('data/calibration_mod7_', alb_prod, '.RDS'))

mod7_free = mgcv::bam(mar ~ s(x, y, bs='gp', k=350) + s(elev, k=50) + s(OL, ET, ST, bs='gp', k=50),
                 data=cal_data, 
                 family=betar(link="logit"), 
                 method="REML", 
                 na.action=na.omit, 
                 control=ctrl)
gam.check(mod7_free)

saveRDS(mod7, paste0('data/calibration_mod7_free_', alb_prod, '.RDS'))

AIC(mod1, mod2, mod3, mod4, mod5, mod6, mod7)

anova_gam = anova.gam(mod1, mod2, mod3, mod4, mod5, mod6, mod7, test = "Chisq")
anova_gam

summary(mod7)


mod8 = mgcv::bam(mar ~ s(x, y, bs='gp', k=350) + s(elev, k=50) + s(OL, ET, ST, bs='tp', k=50),
                 data=cal_data, 
                 family=betar(link="logit"), 
                 method="REML", 
                 na.action=na.omit, 
                 control=ctrl)
gam.check(mod8)

saveRDS(mod8, paste0('data/calibration_mod8_', alb_prod, '.RDS'))

AIC(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8)

anova_gam = anova.gam(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, test = "Chisq")
anova_gam

anova_gam = anova.gam(mod5, mod7, mod8, test = "Chisq")
anova_gam

anova_gam = anova.gam(mod7, mod8, test = "Chisq")
anova_gam

anova_gam = anova.gam(mod2, mod5, test = "Chisq")
anova_gam

anova_gam = anova.gam(mod1, mod2, mod7, test = "Chisq")
anova_gam

summary(mod7)

mod9 = mgcv::bam(mar ~ s(x, y, bs='gp', k=350) + s(elev, k=50) + s(OL, ET, ST, bs='tp', k=30),
                 data=cal_data, 
                 family=betar(link="logit"), 
                 method="REML", 
                 na.action=na.omit, 
                 control=ctrl,
                 select = TRUE)
gam.check(mod9)

###############################################################################################################
## compare models
###############################################################################################################

paste0('data/calibration_mod6_', alb_prod, '.RDS')

################################################################################################
#difference maps 
#################################################################################################
#calculating difference between model and predicted values from model #5
#this is for march 
cal_data$diff_mod5 = cal_data$preds_mod5 - cal_data$mar
saveRDS(cal_data, 'data/cal_data.RDS')
######################################################################################################
#FIGURES
######################################################################################################
pbs_ll = readRDS('data/map-data/geographic/pbs_ll.RDS')

pbs = readRDS('data/map-data/geographic/pbs.RDS')


#difference figure 
ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), fill='grey') +
  geom_point(data=cal_data, aes(x=long, y=lat, colour = diff_mod5))+
  scale_colour_gradient2(low = 'green', high = 'pink', mid= 'yellow', limits = c(-0.2,0.2))+
  ggtitle("Difference between predicted values and data")+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
ggsave("marchmod5.differencemap.png")


#frequency of differences 
#we can see that it is mainly focused at 0
ggplot(data=cal_data) + 
  geom_histogram(aes(x=diff_mod5), fill = "dark blue") +
  labs( x="albedo differences", 
       title="distribution of albedo differences using model 5" )+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
ggsave('figures/march_histo_frequency.png')


###############################################################################################################
## interp calibration model
###############################################################################################################

cal_interp_data =readRDS(paste0('data/calibration_modern_lct_interp_', alb_prod, '.RDS'))
# cal_data =readRDS('data/lct_albedo_snow_modern_bluesky.RDS')

cal_data2 = data.frame(site=seq(1,nrow(cal_interp_data)), cal_interp_data)
cal_data3 = cal_data2[,c('site', 'ET', 'OL', 'ST')]

###############################################################################################################
## march calibration model
###############################################################################################################

ctrl <- list(nthreads=8, maxit=500)

# # just x and y
#works better with smaller k values 
# # was 900
# mod = mgcv::bam(mar ~ s(x, y, bs="gp", k=300),
#           data=cal_data, 
#           family=betar(link="logit"), 
#           method="REML", 
#           na.action=na.omit, 
#           control=ctrl)
# gam.check(mod)
# #vis.gam(mod,theta=30)


# # just x and y
mod1_interp = mgcv::bam(mar ~ s(x, y, bs="gp", k=500),
                 data=cal_interp_data, 
                 family=betar(link="logit"), 
                 method="REML", 
                 na.action=na.omit, 
                 control=ctrl)
gam.check(mod1_interp)

saveRDS(mod1_interp, paste0('data/calibration_mod1_interp_', alb_prod, '.RDS'))


# vis.gam(mod1_interp,theta=30)

# # add elevation
# #tp vs gp model
# mod2 = mgcv::bam(mar ~ s(x, y, bs='tp', k=400) + s(elev, k=30),
#           data=cal_data,
#           family=betar(link="logit"),
#           method="REML",
#           na.action=na.omit)

# add eleveation
#gp model works better than tp
mod2_interp = mgcv::bam(mar ~ s(x, y, bs="gp", k=500) + s(elev, k=50),
                 data=cal_interp_data, 
                 family=betar(link="logit"), 
                 method="REML", 
                 na.action=na.omit, 
                 control=ctrl)
gam.check(mod2_interp)
#vis.gam(mod,theta=30)

saveRDS(mod2_interp, paste0('data/calibration_mod2_interp_', alb_prod, '.RDS'))


AIC(mod1_interp, mod2_interp)

# add OL
mod3_interp = mgcv::bam(mar ~ s(x, y, bs='gp', k=500) + s(elev, k=50) + s(OL, k=50),
                 data=cal_interp_data, 
                 family=betar(link="logit"), 
                 method="REML", 
                 na.action=na.omit, 
                 control=ctrl)
gam.check(mod3_interp)

saveRDS(mod3_interp, paste0('data/calibration_mod3_interp_', alb_prod, '.RDS'))


AIC(mod1_interp, mod2_interp, mod3_interp)

anova_gam_interp = anova.gam(mod1_interp, mod2_interp, mod3_interp, test = "Chisq")
anova_gam_interp

summary(mod3_interp)

# add ET
mod4_interp = mgcv::bam(mar ~ s(x, y, bs='gp', k=500) + s(elev, k=50) + s(OL, k=50) + s(ET, k=50),
                 data=cal_interp_data, 
                 family=betar(link="logit"), 
                 method="REML", 
                 na.action=na.omit, 
                 control=ctrl)
gam.check(mod4_interp)

saveRDS(mod4_interp, paste0('data/calibration_mod4_interp_', alb_prod, '.RDS'))


AIC(mod1_interp, mod2_interp, mod3_interp, mod4_interp)

anova_gam_interp = anova.gam(mod1_interp, mod2_interp, mod3_interp, mod4_interp, test = "Chisq")
anova_gam_interp

summary(mod4_interp)

# add ST
#this takes a while 
# mod5 = mgcv::gam(mar ~ s(x, y, bs='gp', k=30) + s(elev, k=10) + s(OL, k=10) + s(ET, k=10) + s(ST, k=10),
#            data=cal_data, 
#            family=betar(link="logit"), 
#            method="REML", 
#            na.action=na.omit, 
#            control=ctrl)
mod5_interp = mgcv::bam(mar ~ s(x, y, bs='gp', k=500) + s(elev, k=50) + s(OL, k=50) + s(ET, k=50) + s(ST, k=50),
                 data=cal_interp_data, 
                 family=betar(link="logit"), 
                 method="REML", 
                 na.action=na.omit, 
                 control=ctrl)
gam.check(mod5_interp)

saveRDS(mod5_interp, paste0('data/calibration_mod5_interp_', alb_prod, '.RDS'))

AIC(mod1_interp, mod2_interp, mod3_interp, mod4_interp, mod5_interp)

anova_gam_interp = anova.gam(mod1_interp, mod2_interp, mod3_interp, mod4_interp, mod5_interp, test = "Chisq")
anova_gam_interp

summary(mod5_interp)

mod6_interp = mgcv::bam(mar ~ s(x, y, bs='gp', k=500) + s(elev, k=50) + s(OL, ET, ST, k=75),
                 data=cal_interp_data, 
                 family=betar(link="logit"), 
                 method="REML", 
                 na.action=na.omit, 
                 control=ctrl)
gam.check(mod6_interp)

saveRDS(mod6_interp, paste0('data/calibration_mod6_interp_', alb_prod, '.RDS'))

AIC(mod1_interp, mod2_interp, mod3_interp, mod4_interp, mod5_interp, mod6_interp)

anova_gam_interp = anova.gam(mod1_interp, mod2_interp, mod3_interp, mod4_interp, mod5_interp, mod6_interp, test = "Chisq")
anova_gam_interp

summary(mod6_interp)

mod7_interp = mgcv::bam(mar ~ s(x, y, bs='gp', k=500) + s(elev, k=50) + s(OL, ET, ST, bs='gp', k=200),
                 data=cal_interp_data, 
                 family=betar(link="logit"), 
                 method="REML", 
                 na.action=na.omit, 
                 control=ctrl)
gam.check(mod7_interp)

saveRDS(mod7_interp, paste0('data/calibration_mod7_interp_', alb_prod, '.RDS'))

# mod7_free_interp = mgcv::bam(mar ~ s(x, y, bs='gp', k=500) + s(elev, k=50) + s(OL, ET, ST, bs='gp', k=75),
#                       data=cal_interp_data, 
#                       family=betar(link="logit"), 
#                       method="REML", 
#                       na.action=na.omit, 
#                       control=ctrl)
# gam.check(mod7_free_interp)

# saveRDS(mod7_free_interp, paste0('data/calibration_mod7_free_interp_', alb_prod, '.RDS'))

AIC(mod1_interp, mod2_interp, mod3_interp, mod4_interp, mod5_interp, mod6_interp, mod7_interp)

anova_gam_interp = anova.gam(mod1_interp, mod2_interp, mod3_interp, mod4_interp, mod5_interp, mod6_interp, mod7_interp, test = "Chisq")
anova_gam_interp

summary(mod7_interp)


mod8_interp = mgcv::bam(mar ~ s(x, y, bs='gp', k=500) + s(elev, k=50) + s(OL, ET, ST, bs='tp', k=200),
                 data=cal_interp_data, 
                 family=betar(link="logit"), 
                 method="REML", 
                 na.action=na.omit, 
                 control=ctrl)
gam.check(mod8_interp)

saveRDS(mod8_interp, paste0('data/calibration_mod8_interp_', alb_prod, '.RDS'))

AIC(mod1_interp, mod2_interp, mod3_interp, mod4_interp, mod5_interp, mod6_interp, mod7_interp, mod8_interp)

anova_gam_interp = anova.gam(mod1_interp, mod2_interp, mod3_interp, mod4_interp, mod5_interp, mod6_interp, mod7_interp, mod8_interp, test = "Chisq")
anova_gam_interp

anova_gam_interp = anova.gam(mod5_interp, mod7_interp, mod8_interp, test = "Chisq")
anova_gam_interp

anova_gam_interp = anova.gam(mod7_interp, mod8_interp, test = "Chisq")
anova_gam_interp

anova_gam_interp = anova.gam(mod2_interp, mod5_interp, test = "Chisq")
anova_gam_interp

anova_gam_interp = anova.gam(mod1_interp, mod2_interp, mod7_interp, test = "Chisq")
anova_gam_interp

summary(mod7_interp)

mod9_interp = mgcv::bam(mar ~ s(x, y, bs='gp', k=350) + s(elev, k=50) + s(OL, ET, ST, bs='tp', k=30),
                 data=cal_interp_data, 
                 family=betar(link="logit"), 
                 method="REML", 
                 na.action=na.omit, 
                 control=ctrl,
                 select = TRUE)
gam.check(mod9_interp)

# ###############################################################################################################
# ## compare models
# ###############################################################################################################
# 
# paste0('data/calibration_mod6_', alb_prod, '.RDS')
# 
# ################################################################################################
# #difference maps 
# #################################################################################################
# #calculating difference between model and predicted values from model #5
# #this is for march 
# cal_data$diff_mod5 = cal_data$preds_mod5 - cal_data$mar
# saveRDS(cal_data, 'data/cal_data.RDS')
# ######################################################################################################
# #FIGURES
# ######################################################################################################
# pbs_ll = readRDS('data/map-data/geographic/pbs_ll.RDS')
# 
# pbs = readRDS('data/map-data/geographic/pbs.RDS')
# 
# 
# #difference figure 
# ggplot()+
#   geom_polygon(data=pbs_ll, aes(long,lat, group = group), fill='grey') +
#   geom_point(data=cal_data, aes(x=long, y=lat, colour = diff_mod5))+
#   scale_colour_gradient2(low = 'green', high = 'pink', mid= 'yellow', limits = c(-0.2,0.2))+
#   ggtitle("Difference between predicted values and data")+
#   theme_bw()+
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
# ggsave("marchmod5.differencemap.png")
# 
# 
# #frequency of differences 
# #we can see that it is mainly focused at 0
# ggplot(data=cal_data) + 
#   geom_histogram(aes(x=diff_mod5), fill = "dark blue") +
#   labs( x="albedo differences", 
#         title="distribution of albedo differences using model 5" )+
#   theme_bw()+
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
# ggsave('figures/march_histo_frequency.png')
