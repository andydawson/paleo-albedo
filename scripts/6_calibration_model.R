library(gam)
library(mgcv)
library(ggplot2)
library(dplyr)
library(tidyr)


alb_prod = "albclim"

cal_data =readRDS('data/lct_albedo_snow_modern_albclim.RDS')
# cal_data =readRDS('data/lct_albedo_snow_modern_bluesky.RDS')

cal_data2 = data.frame(site=seq(1,nrow(cal_data)), cal_data)
cal_data3 = cal_data2[,c('site', 'ET', 'OL', 'ST')]

###############################################################################################################
## march calibration model
###############################################################################################################

ctrl <- list(nthreads=8)

# # just x and y
#works better with smaller k values 
# was 900
mod = mgcv::bam(bs03 ~ s(x, y, bs="gp", k=150),
          data=cal_data, 
          family=betar(link="logit"), 
          method="REML", 
          na.action=na.omit, 
          control=ctrl)
gam.check(mod)
#vis.gam(mod,theta=30)


# # just x and y
mod1 = mgcv::bam(bs03 ~ s(x, y, bs="gp", k=150),
          data=cal_data, 
          family=betar(link="logit"), 
          method="REML", 
          na.action=na.omit, 
          control=ctrl)
gam.check(mod1)

saveRDS(mod1, paste0('data/calibration_mod1_', alb_prod, '.RDS'))


vis.gam(mod,theta=30)

# add elevation
#tp vs gp model
mod2 = mgcv::bam(bs03 ~ s(x, y, bs='tp', k=100) + s(elev, k=30),
          data=cal_data,
          family=betar(link="logit"),
          method="REML",
          na.action=na.omit)

# add eleveation
#gp model works better than tp
mod2 = mgcv::bam(bs03 ~ s(x, y, bs="gp", k=150) + s(elev, k=30),
           data=cal_data, 
           family=betar(link="logit"), 
           method="REML", 
           na.action=na.omit, 
           control=ctrl)
gam.check(mod2)
#vis.gam(mod,theta=30)

saveRDS(mod2, paste0('data/calibration_mod2_', alb_prod, '.RDS'))


AIC(mod, mod2)

# add OL
mod3 = mgcv::bam(bs03 ~ s(x, y, bs='gp', k=150) + s(elev, k=30) + s(OL, k=30),
           data=cal_data, 
           family=betar(link="logit"), 
           method="REML", 
           na.action=na.omit, 
           control=ctrl)
gam.check(mod3)

saveRDS(mod3, paste0('data/calibration_mod3_', alb_prod, '.RDS'))


AIC(mod, mod2, mod3)
anova.gam(mod, mod2, mod3)

# add ET
mod4 = mgcv::bam(bs03 ~ s(x, y, bs='gp', k=150) + s(elev, k=30) + s(OL, k=30) + s(ET, k=30),
           data=cal_data, 
           family=betar(link="logit"), 
           method="REML", 
           na.action=na.omit, 
           control=ctrl)
gam.check(mod4)

saveRDS(mod4, paste0('data/calibration_mod4_', alb_prod, '.RDS'))


AIC(mod, mod2, mod3, mod4)


# add ST
#this takes a while 
# mod5 = mgcv::gam(bs03 ~ s(x, y, bs='gp', k=30) + s(elev, k=10) + s(OL, k=10) + s(ET, k=10) + s(ST, k=10),
#            data=cal_data, 
#            family=betar(link="logit"), 
#            method="REML", 
#            na.action=na.omit, 
#            control=ctrl)
mod5 = mgcv::bam(bs03 ~ s(x, y, bs='gp', k=150) + s(elev, k=30) + s(OL, k=30) + s(ET, k=30) + s(ST, k=30),
                 data=cal_data, 
                 family=betar(link="logit"), 
                 method="REML", 
                 na.action=na.omit, 
                 control=ctrl)
gam.check(mod5)

saveRDS(mod5, paste0('data/calibration_mod5_', alb_prod, '.RDS'))

AIC(mod, mod2, mod3, mod4, mod5)


saveRDS(mod, 'data/calibration_model1.RDS')

mod6 = mgcv::bam(bs03 ~ s(x, y, bs='gp', k=150) + s(elev, k=30) + s(OL, ET, ST, k=30),
                 data=cal_data, 
                 family=betar(link="logit"), 
                 method="REML", 
                 na.action=na.omit, 
                 control=ctrl)
gam.check(mod6)

saveRDS(mod6, paste0('data/calibration_mod6_', alb_prod, '.RDS'))

AIC(mod, mod2, mod3, mod4, mod5, mod6)

###############################################################################################################
## plotting model fit for bs03
###############################################################################################################
preds3 = predict.gam(mod3, 
                     type="response", 
                     newdata=cal_data,
                     se.fit=TRUE)

cal_data$preds_mod3 = preds3$fit
#dat_all$predict = preds$fit

ggplot(data=cal_data) + 
  geom_point(aes(x=bs03, y=preds_mod3)) +
  geom_abline(slope=1, intercept=0, colour="red") +
  xlim(c(0,1)) + 
  ylim(c(0,1))

plot(mod3)
plot(mod3, residuals=TRUE, all.terms=TRUE, pch=1)

#model 4 
preds = predict.gam(mod4, 
                    type="response", 
                    newdata=cal_data,
                    se.fit=TRUE)

cal_data$preds_mod4 = preds$fit
#dat_all$predict = preds$fit

ggplot(data=cal_data) + 
  geom_point(aes(x=bs03, y=preds_mod4)) +
  geom_abline(slope=1, intercept=0, colour="red") +
  xlim(c(0,1)) + 
  ylim(c(0,1))



###############################################################################################################
## march full model fit with x,y,OL,ET,ST,elevation
###############################################################################################################

cal_model = readRDS('data/calibration_model5.RDS')

preds = predict.gam(cal_model, 
                    type="response", 
                    newdata=cal_data,
                    se.fit=TRUE)

cal_data$preds_mod5 = preds$fit
#dat_all$predict = preds$fit

ggplot(data=cal_data) + 
  geom_point(aes(x=bs03, y=preds_mod5), size=2, alpha=0.5) +
  geom_abline(slope=1, intercept=0, colour="red", size=1, lty=2) +
  xlim(c(0,1)) + 
  ylim(c(0,1)) +
  coord_fixed() +  
  theme_bw(18) +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=14)) +
  xlab('albedo (data)') +
  ylab('albedo (model)')
ggsave('figures/cal_model_vs_datamod5.png')
ggsave('figures/cal_model_vs_datamod5.pdf')

# cal_data$preds = cal_model$fitted.values
cor(cal_data$bs03, cal_data$preds_mod5)
summary(lm(preds_mod5~bs03, data=cal_data))
################################################################################################
#difference maps 
#################################################################################################
#calculating difference between model and predicted values from model #5
#this is for march 
cal_data$diff_mod5 = cal_data$preds_mod5 - cal_data$bs03
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
## april calibration model
###############################################################################################################

ctrl <- list(nthreads=16)

# # just x and y
#works better with smaller k values 
april_mod = gam(bs04 ~ s(x, y, bs="gp", k=900),
          data=cal_data, 
          family=betar(link="logit"), 
          method="REML", 
          na.action=na.omit, 
          control=ctrl)
gam.check(april_mod)
#vis.gam(mod,theta=30)


# # just x and y
april_mod = gam(bs04 ~ s(x, y, bs="gp", k=175),
          data=cal_data, 
          family=betar(link="logit"), 
          method="REML", 
          na.action=na.omit, 
          control=ctrl)
gam.check(april_mod)

vis.gam(april_mod,theta=30)

# add elevation
#tp vs gp model
april_mod2 = gam(bs04 ~ s(x, y, bs='tp', k=900) + s(elev, k=30),
           data=cal_data,
           family=betar(link="logit"),
           method="REML",
           na.action=na.omit)

# add eleveation
#gp model works better than tp
april_mod2 = gam(bs04 ~ s(x, y, bs="gp", k=100) + s(elev, k=10),
           data=cal_data, 
           family=betar(link="logit"), 
           method="REML", 
           na.action=na.omit, 
           control=ctrl)
gam.check(april_mod2)
#vis.gam(mod,theta=30)

AIC(april_mod, april_mod2)

# add OL
april_mod3 = gam(bs04 ~ s(x, y, bs='gp', k=100) + s(elev, k=10) + s(OL, k=10),
           data=cal_data, 
           family=betar(link="logit"), 
           method="REML", 
           na.action=na.omit, 
           control=ctrl)
gam.check(april_mod3)

AIC(april_mod, april_mod2, april_mod3)
anova.gam(april_mod, april_mod2, april_mod3)

# add ET
april_mod4 = gam(bs04 ~ s(x, y, bs='gp', k=100) + s(elev, k=10) + s(OL, k=10) + s(ET, k=10),
           data=cal_data, 
           family=betar(link="logit"), 
           method="REML", 
           na.action=na.omit, 
           control=ctrl)
gam.check(april_mod4)

AIC(april_mod, april_mod2, april_mod3, april_mod4)


# add ST
#this takes a while 
april_mod5 = gam(bs04 ~ s(x, y, bs='gp', k=500) + s(elev, k=10) + s(OL, k=10) + s(ET, k=10) + s(ST, k=5),
           data=cal_data, 
           family=betar(link="logit"), 
           method="REML", 
           na.action=na.omit, 
           control=ctrl)
gam.check(april_mod5)

AIC(april_mod, april_mod2, april_mod3, april_mod4, april_mod5)


saveRDS(april_mod, 'data/calibration_model1_april.RDS')

saveRDS(april_mod5, 'data/calibration_model5_april.RDS')

###############################################################################################################
## plotting model fit for bs03
###############################################################################################################
preds3 = predict.gam(mod3, 
                     type="response", 
                     newdata=cal_data,
                     se.fit=TRUE)

cal_data$preds_mod3 = preds3$fit
#dat_all$predict = preds$fit

ggplot(data=cal_data) + 
  geom_point(aes(x=bs03, y=preds_mod3)) +
  geom_abline(slope=1, intercept=0, colour="red") +
  xlim(c(0,1)) + 
  ylim(c(0,1))

plot(mod3)
plot(mod3, residuals=TRUE, all.terms=TRUE, pch=1)

#model 4 
preds = predict.gam(mod4, 
                    type="response", 
                    newdata=cal_data,
                    se.fit=TRUE)

cal_data$preds_mod4 = preds$fit
#dat_all$predict = preds$fit

ggplot(data=cal_data) + 
  geom_point(aes(x=bs03, y=preds_mod4)) +
  geom_abline(slope=1, intercept=0, colour="red") +
  xlim(c(0,1)) + 
  ylim(c(0,1))



###############################################################################################################
## march full model fit with x,y,OL,ET,ST,elevation
###############################################################################################################

cal_model = readRDS('data/calibration_model5.RDS')

preds = predict.gam(cal_model, 
                    type="response", 
                    newdata=cal_data,
                    se.fit=TRUE)

cal_data$preds_mod5 = preds$fit
#dat_all$predict = preds$fit

ggplot(data=cal_data) + 
  geom_point(aes(x=bs03, y=preds_mod5)) +
  geom_abline(slope=1, intercept=0, colour="red", size=1, lty=2) +
  xlim(c(0,1)) + 
  ylim(c(0,1)) +
  coord_fixed() +  
  theme_bw() +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=14)) +
  xlab('Albedo Data') +
  ylab('Albedo Model')
ggsave('figures/cal_model_vs_datamod5.png')
#ggsave('figures/cal_model_vs_data.pdf')

cal_data$preds = cal_model$fitted.values

################################################################################################
#difference maps 
#################################################################################################
#calculating difference between model and predicted values from model #5
#this is for march 
cal_data$diff_mod5 = cal_data$preds_mod5 - cal_data$bs03
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


