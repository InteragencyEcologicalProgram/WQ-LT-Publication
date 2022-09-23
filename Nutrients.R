#Nutrient analyses


library(tidyverse)
library(lubridate)

library(lme4)
library(lmerTest)
library(DHARMa)
library(effects)
library(DroughtData)


#Replace my data with teh official version
names(raw_chla_1975_2021)


#Start with Ammonia
#####################################################################
#replace values below the reporting limit
Ammonia <- raw_nutr_1975_2021 %>%
  # Remove NA values in DissAmmonia
  tidyr::drop_na(DissAmmonia) %>%
  # Replace <RL values with random value
  drt_replace_rl(DissAmmonia, DissAmmonia_Sign) %>%
  # Calculate seasonal-regional averages
  drt_avg_data(DissAmmonia, avg_type = "both", month_na = "relaxed") %>%
  # Add year assignments
  drt_add_yr_assign() %>%
  mutate(YearType = factor(YearType, levels = c("Critical", "Dry", "Below Normal",
                                                "Above Normal", "Wet")))


logAmmonia <- raw_nutr_1975_2021 %>%
  # Remove NA values in DissAmmonia
  tidyr::drop_na(DissAmmonia) %>%
  # Replace <RL values with random value
  drt_replace_rl(DissAmmonia, DissAmmonia_Sign) %>%
  #Log transformation
  mutate(LogAm = log(DissAmmonia)) %>%
  # Calculate seasonal-regional averages
  drt_avg_data(LogAm, avg_type = "both", month_na = "relaxed") %>%
  # Add year assignments
  drt_add_yr_assign() %>%
  mutate(YearType = factor(YearType, levels = c("Critical", "Dry", "Below Normal",
                                                "Above Normal", "Wet")))

#plot of Ammonia by water year type
ggplot(logAmmonia, aes(x = YearType, y = LogAm, fill = YearType))+
  geom_boxplot(alpha = 0.7)+
  facet_grid(Season~Region)+
  drt_color_pal_yrtype()+
  ylab("Ammonia mg/L log transformed")+
  xlab("Year Type")+
  theme_bw()+
  scale_x_discrete(labels = c("C", "D", "B", "A", "W"))

#plot of Ammonia by water year type
ggplot(Ammonia, aes(x = YearType, y = DissAmmonia, fill = YearType))+
  geom_boxplot(alpha = 0.7)+
  facet_grid(Season~Region)+
  drt_color_pal_yrtype()+
  ylab("Ammonia mg/L")+
  xlab("Year Type")+
  theme_bw()+
  scale_x_discrete(labels = c("C", "D", "B", "A", "W"))

#plot of Ammonia by Sac valley index
ggplot(logAmmonia, aes(x = SVIndex, y = LogAm))+
  geom_point(alpha = 0.7, aes(color =Drought))+
  geom_smooth(method = "lm")+
  facet_grid(Season~Region)+
  drt_color_pal_drought(aes_type = "color")+
  ylab("Ammonia mg/L (log-transformed)")+
  xlab("Sac Valley Index")+
  theme_bw()

#plot ofammonia by drought
ggplot(logAmmonia, aes(x = Drought, y = LogAm, fill = Drought))+
  geom_boxplot(alpha = 0.7)+
  facet_grid(Season~Region)+
  drt_color_pal_drought()+
  ylab("Ammonia mg/L log transformed")+
  xlab("Year Type")+
  theme_bw()

#Ammonia by year
ggplot(Ammonia, aes(x = YearAdj, y = DissAmmonia, fill = YearType))+
  geom_col(alpha = 0.7)+
  facet_grid(Season~Region)+
  drt_color_pal_yrtype()+
  ylab("Ammonia mg/L")+
  theme_bw()

####################################################################3
#Ammonia models and box plots
hist(Ammonia$DissAmmonia)
hist(log(Ammonia$DissAmmonia))
hist(logAmmonia$LogAm)

#log-transformation is definitely better

Am1 = lm(LogAm ~ Drought*Region*Season, data = logAmmonia)
summary(Am1)
plot(Am1)
eff1 = allEffects(Am1, x.var = "Drought")
plot(eff1)
amp = emmeans(Am1, pairwise ~ Drought*Region*Season)
plot(amp, comparisons = T)

# I hate three-way interactions. Impossible to interpret


Am2 = lm(LogAm ~Drought*Region + Drought*Season, data = logAmmonia)
plot(Am2)
eff2 = allEffects(Am2, x.var = "Drought")
plot(eff2)
summary(Am2)
amp2 = emmeans(Am2, pairwise ~ Drought*Region)
plot(amp2, comparisons = T)
amp2b = emmeans(Am2, pairwise ~ Drought*Season)
plot(amp2b, comparisons = T)

#not very interesting

#####################################################################
#Nitrate and nitrite

#replace values below the reporting limit
Nitrate <- raw_nutr_1975_2021 %>%
  # Remove NA values in DissNitrate
  tidyr::drop_na(DissNitrateNitrite) %>%
  # Replace <RL values with random value
  drt_replace_rl(DissNitrateNitrite, DissNitrateNitrite_Sign) %>%
  # Calculate seasonal-regional averages
  drt_avg_data(DissNitrateNitrite, avg_type = "both", month_na = "relaxed") %>%
  # Add year assignments
  drt_add_yr_assign() %>%
  mutate(YearType = factor(YearType, levels = c("Critical", "Dry", "Below Normal",
                                                "Above Normal", "Wet")))


logNitrate <- raw_nutr_1975_2021 %>%
  # Remove NA values in DissNitrate
  tidyr::drop_na(DissNitrateNitrite) %>%
  # Replace <RL values with random value
  drt_replace_rl(DissNitrateNitrite, DissNitrateNitrite_Sign) %>%
  #Log transformation
  mutate(LogNat = log(DissNitrateNitrite)) %>%
  # Calculate seasonal-regional averages
  drt_avg_data(LogNat, avg_type = "both", month_na = "relaxed") %>%
  # Add year assignments
  drt_add_yr_assign() %>%
  mutate(YearType = factor(YearType, levels = c("Critical", "Dry", "Below Normal",
                                                "Above Normal", "Wet")))

#plot of Nitrate by water year type
ggplot(logNitrate, aes(x = YearType, y = LogNat, fill = YearType))+
  geom_boxplot(alpha = 0.7)+
  facet_grid(Season~Region)+
  drt_color_pal_yrtype()+
  ylab("Nitrate Nitrite (log-transformed)")+
  xlab("Year Type")+
  theme_bw()+
  scale_x_discrete(labels = c("C", "D", "B", "A", "W"))

ggplot(Nitrate, aes(x = YearType, y = DissNitrateNitrite, fill = YearType))+
  geom_boxplot(alpha = 0.7)+
  facet_grid(Season~Region)+
  drt_color_pal_yrtype()+
  ylab("Nitrate mg/L")+
  xlab("Year Type")+
  theme_bw()+
  scale_x_discrete(labels = c("C", "D", "B", "A", "W"))

#plot of Nitrate by Sac valley index
ggplot(logNitrate, aes(x = SVIndex, y = LogNat))+
  geom_point(alpha = 0.7, aes(color =Drought))+
  geom_smooth(method = "lm")+
  facet_grid(Season~Region)+
  drt_color_pal_drought(aes_type = "color")+
  ylab("Nitrate nitrite mg/L (log-transformed)")+
  xlab("Sac Valley Index")+
  theme_bw()

#plot ofNitrate by drought
ggplot(Nitrate, aes(x = Drought, y = DissNitrateNitrite, fill = Drought))+
  geom_boxplot(alpha = 0.7)+
  facet_grid(Season~Region)+
  drt_color_pal_drought()+
  ylab("Nitrate mg/L")+
  xlab("Year Type")+
  theme_bw()

#Nitrate by year
ggplot(Nitrate, aes(x = YearAdj, y = DissNitrateNitrite, fill = YearType))+
  geom_col(alpha = 0.7)+
  facet_grid(Season~Region)+
  drt_color_pal_yrtype()+
  ylab("Nitrate mg/L")+
  theme_bw()

####################################################################3
#Nitrate models and box plots
hist(Nitrate$DissNitrateNitrite)
hist(log(Nitrate$DissNitrateNitrite))
hist(logNitrate$LogNat)

#log-transformation is definitely better

Nat1 = lm(LogNat ~ Drought*Region*Season, data = logNitrate)
summary(Nat1)
plot(Am1)
eff1 = allEffects(Chl1, x.var = "Drought")
plot(eff1)
# I hate three-way interactions. Impossible to interpret


Nat2 = lm(LogNat ~Drought*Region + Drought*Season, data = logNitrate)
plot(Nat2)
eff2 = allEffects(Nat2, x.var = "Drought")
plot(eff2)
summary(Nat2)
pairsNN =  emmeans(Nat2, pairwise ~ Drought*Region)
plot(pairsNN, comparisons = T)
pairsNN2 =  emmeans(Nat2, pairwise ~ Drought*Season)
plot(pairsNN2, comparisons = T)

#OK, higher nitriate during drought years in the Confluence and Suisun.

ggplot(Nitrate, aes(x = YearAdj, y = DissNitrateNitrite)) + geom_point(aes(color = Region))+
  geom_smooth(aes(color = Region))+
  theme_bw()+ ylab("Dissolved Nitrate + Nitrite")

#####################################################################
#Phosphorus

#replace values below the reporting limit
Phos <- raw_nutr_1975_2021 %>%
  # Remove NA values in DissNitrate
  tidyr::drop_na(DissOrthophos) %>%
  # Replace <RL values with random value
  drt_replace_rl(DissOrthophos, DissOrthophos_Sign) %>%
  # Calculate seasonal-regional averages
  drt_avg_data(DissOrthophos, avg_type = "both", month_na = "relaxed") %>%
  # Add year assignments
  drt_add_yr_assign() %>%
  mutate(YearType = factor(YearType, levels = c("Critical", "Dry", "Below Normal",
                                                "Above Normal", "Wet")))


logPhos <- raw_nutr_1975_2021 %>%
  # Remove NA values in DissNitrate
  tidyr::drop_na(DissOrthophos) %>%
  # Replace <RL values with random value
  drt_replace_rl(DissOrthophos, DissOrthophos_Sign) %>%
  # Calculate seasonal-regional averages
  mutate(LogPhos = log(DissOrthophos)) %>%
  # Calculate seasonal-regional averages
  drt_avg_data(LogPhos, avg_type = "both", month_na = "relaxed") %>%
  # Add year assignments
  drt_add_yr_assign() %>%
  mutate(YearType = factor(YearType, levels = c("Critical", "Dry", "Below Normal",
                                                "Above Normal", "Wet")))

#plot of Phosphorus by water year type
ggplot(logPhos, aes(x = YearType, y = LogPhos, fill = YearType))+
  geom_boxplot(alpha = 0.7)+
  facet_grid(Season~Region)+
  drt_color_pal_yrtype()+
  ylab("Orthophosphate mg/L (log-transformed)")+
  xlab("Year Type")+
  theme_bw()+
  scale_x_discrete(labels = c("C", "D", "B", "A", "W"))

ggplot(Phos, aes(x = YearType, y = DissOrthophos, fill = YearType))+
  geom_boxplot(alpha = 0.7)+
  facet_grid(Season~Region)+
  drt_color_pal_yrtype()+
  ylab("Orthophosphate mg/L")+
  xlab("Year Type")+
  theme_bw()+
  scale_x_discrete(labels = c("C", "D", "B", "A", "W"))

#plot of Nitrate by Sac valley index
ggplot(logPhos, aes(x = SVIndex, y = LogPhos))+
  geom_point(alpha = 0.7, aes(color =Drought))+
  geom_smooth(method = "lm")+
  facet_grid(Season~Region)+
  drt_color_pal_drought(aes_type = "color")+
  ylab("Orthophosphate mg/L (log-transformed)")+
  xlab("Sac Valley Index")+
  theme_bw()


ggplot(Phos, aes(x = YearAdj, y = DissOrthophos)) + geom_point(aes(color = Region))+
  geom_smooth(aes(color = Region))+
  theme_bw()+ ylab("Dissolved Orthophosphate")
#Wow. Phosphorus actuallylooks pretty good there.

ggplot(Phos, aes(x = Drought, y = DissOrthophos, fill = Drought))+
  geom_boxplot(alpha = 0.7)+
  facet_grid(Season~Region)+
  drt_color_pal_drought()+
  ylab("Phosphorus mg/L")+
  xlab("Year Type")+
  theme_bw()

#Nitrate by year
ggplot(Phos, aes(x = YearAdj, y = DissOrthophos, fill = YearType))+
  geom_col(alpha = 0.7)+
  facet_grid(Season~Region)+
  drt_color_pal_yrtype()+
  ylab("Nitrate mg/L")+
  theme_bw()

####################################################################3
#Nitrate models and box plots
hist(Phos$DissOrthophos)
hist(log(Phos$DissOrthophos))
hist(logPhos$LogPhos)

#Meh. Not sure there
Phosx = lm(DissOrthophos ~ Drought*Region*Season, data = Phos)
Phos1 = lm(LogPhos ~ Drought*Region*Season, data = logPhos)
summary(Phos1)
plot(Phos1)
eff1 = allEffects(Chl1, x.var = "Drought")
plot(eff1)
# I hate three-way interactions. Impossible to interpret


Phos2 = lm(LogPhos ~Drought*Region + Drought*Season, data = logPhos)
plot(Phos2)
eff2 = allEffects(Phos2, x.var = "Drought")
plot(eff2)
summary(Phos2)
pairsP =  emmeans(Phos2, pairwise ~ Drought*Region)
plot(pairsP, comparisons = T)
plot(emmeans(Phos2, pairwise ~ Drought), comparisons = T)
#major effect of drought on phosphorus! NOthing else, but yest phosphorus!
