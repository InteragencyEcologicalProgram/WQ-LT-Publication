# Chlorophyll

#devtools::install_github("mountaindboz/DroughtData", ref = "v2.0.0", build = FALSE)

library(tidyverse)
library(lubridate)

library(lme4)
library(lmerTest)
library(DHARMa)
library(effects)
library(DroughtData)
library(emmeans)
# #test
#
# stas = c("P8", "D8", "MD10", "MD10A", "D7", "D6", "D4", "D28A", "D26", "D22", "D19", "D16", "D12", "D10", "C3", "C3A")
#
# #Chlorophyll data from KEith
# Chl = read_csv("data/chla_data_stats_LT2.csv")
# yrs = read_csv("data/yearassignments.csv")
#
# #Add NCRO
# NCRO = read.csv("data/hab_nutr_chla_mvi.csv") %>%
#   filter(Source %in% c("DWR_NCRO", "NCRO")) %>%
#   rename(chlaAvg = Chlorophyll) %>%
#   mutate(chlaAvg_log10 = log(chlaAvg, 10),
#          month = month(Date),
#          ds_year = case_when(month %in% c(11,12) ~ year(Date) + 1,
#                              TRUE ~ year(Date)))

#Replace my data with teh official version
names(raw_chla_1975_2021)

#replace values below the reporting limit
Chla <- raw_chla_1975_2021 %>%
  # Remove NA values in DissAmmonia
  tidyr::drop_na(Chlorophyll) %>%
  # Replace <RL values with random value
  drt_replace_rl(Chlorophyll, Chlorophyll_Sign) %>%
  #log transform chlorophyll
  mutate(LogChl = log(Chlorophyll)) %>%
  # Calculate seasonal-regional averages
  drt_avg_data(LogChl, avg_type = "both", month_na = "relaxed") %>%
  # Add year assignments
  drt_add_yr_assign() %>%
  mutate(YearType = factor(YearType, levels = c("Critical", "Dry", "Below Normal",
                                                "Above Normal", "Wet")))


#replace values below the reporting limit
ChlaA <- raw_chla_1975_2021 %>%
  # Remove NA values in DissAmmonia
  tidyr::drop_na(Chlorophyll) %>%
  # Replace <RL values with random value
  drt_replace_rl(Chlorophyll, Chlorophyll_Sign) %>%
   # Calculate seasonal-regional averages
  drt_avg_data(Chlorophyll, avg_type = "both", month_na = "relaxed") %>%
  # Add year assignments
  drt_add_yr_assign() %>%
  mutate(YearType = factor(YearType, levels = c("Critical", "Dry", "Below Normal",
                                                "Above Normal", "Wet")))


#plot of chlorophyll by water year type
ggplot(Chla, aes(x = YearType, y = LogChl, fill = YearType))+
  geom_boxplot(alpha = 0.7)+
  facet_grid(Season~Region)+
  drt_color_pal_yrtype()+
  ylab("Chlorophyl ug/L (log-transformed)")+
  xlab("Year Type")+
  theme_bw()+
  scale_x_discrete(labels = c("C", "D", "B", "A", "W"))

#plot of chlorophyll by Sac valley index
ggplot(Chla, aes(x = SVIndex, y = LogChl))+
  geom_point(alpha = 0.7, aes(color =Drought))+
  geom_smooth(method = "lm")+
  facet_grid(Season~Region)+
  drt_color_pal_drought(aes_type = "color")+
  ylab("Chlorophyl ug/L (log-transformed)")+
  xlab("Sac Valley Index")+
  theme_bw()

#plot of chlorophyll by drought
ggplot(Chla, aes(x = Drought, y = LogChl, fill = Drought))+
  geom_boxplot(alpha = 0.7)+
  facet_grid(Season~Region)+
 drt_color_pal_drought()+
  ylab("Chlorophyl ug/L (log-transformed)")+
  xlab("Year Type")+
  theme_bw()

ggplot(ChlaA, aes(x = Drought, y = Chlorophyll, fill = Drought))+
  geom_boxplot(alpha = 0.7)+
  facet_grid(Season~Region)+
  drt_color_pal_drought()+
  ylab("Chlorophyl ug/L")+
  xlab("Year Type")+
  theme_bw()

#plot of chlorophyll by water year type - just hte more recent years
ggplot(filter(Chla, YearAdj > 1989), aes(x = YearType, y = LogChl, fill = YearType))+
  geom_boxplot(alpha = 0.7)+
  facet_grid(Season~Region)+
  drt_color_pal_yrtype()+
  ylab("Chlorophyl ug/L (log-transformed)")+
  xlab("Year Type")+
  theme_bw()+
  scale_x_discrete(labels = c("C", "D", "B", "A", "W"))
#Bleh, nothing going on there

#plot of chlorophyll by year
ggplot(Chla, aes(x = YearAdj, y = LogChl, fill = YearType))+
  geom_col(alpha = 0.7)+
  facet_grid(Season~Region)+
  drt_color_pal_yrtype()+
  ylab("Chlorophyl ug/L (log-transformed)")+
  theme_bw()

ggplot(Chla, aes(x = YearAdj, y = LogChl, fill = Drought))+
  geom_col(alpha = 0.7)+
  facet_grid(Season~Region)+
  drt_color_pal_drought()+
  ylab("Chlorophyl ug/L (log-transformed)")+
  theme_bw()


ggplot(Chla, aes(x = YearAdj, y = LogChl))+
  geom_point(alpha = 0.7)+
  geom_smooth()+
 # drt_color_pal_drought()+
  ylab("Chlorophyl ug/L (log-transformed)")+
  theme_bw()


# #plot of raw chlrophyll data by day of year
# Chl = mutate(raw_chla_1975_2021, Yday = yday(Date))
#
# ggplot(Chl, aes(x = Yday, y = chlaAvg_log10, color = Yr_type))+
#   geom_point(alpha = 0.3)+
#   geom_smooth()+
#   drt_color_pal_yrtype(aes_type = "color")+
#   facet_wrap(~Region)+
#   theme_bw()

###############################################################################
#Let's bin the data by >10 versus < 10
# Chl = mutate(Chl, bloom = case_when(chlaAvg > 10 ~ TRUE,
#                                     TRUE ~ FALSE)) %>%
#   filter(Region != "North Delta")
#
#
# #Binomial model of blooms
# #we don't have enough long-term data from the North Delta to include it.
#
# cmod = glmer(bloom ~ Region + Season+Yr_type + (1|Station) + (1|ds_year), family = "binomial", data = Chl)
# summary(cmod)
# library(visreg)
# visreg(cmod)
#
# cmod2 = glmer(bloom ~ Region + Season+Yr_type + (1|Station) + (1|ds_year), family = "binomial", data = Chl)
# summary(cmod2)
# plot(cmod2)
# visreg(cmod2)
#
#
#
# #Try it another way, I"m not sure how to tdo this right
# #get rid of Winter, because it's not interesting, and filter it to post-clam crash.
#
# Chlsum = Chl %>%
#   rename(Year = ds_year) %>%
#   left_join(yrs) %>%
#   group_by(Region, Season, Drought, Yr_type, month, Year) %>%
#   summarize(Presence = length(bloom[which(bloom)]), Absence = length(bloom[which(!bloom)]), N = n()) #%>%
#   #filter(Season != "Winter",ds_year >1989)
#
# cmod3 = glmer(cbind(Presence, Absence) ~ Region*Yr_type+ (1|Year), family = "binomial", data = Chlsum)
# summary(cmod3)
# plot(cmod3)
# visreg(cmod3)
# visreg(cmod3, xvar = "Yr_type", by = "Region")
# visreg(cmod3, xvar = "Region", by = "Yr_type")
#
# res = simulateResiduals(cmod3)
# plot(res)
#
# #let's visualize
#
# Chlsumlong = pivot_longer(Chlsum, cols = c(Presence, Absence), names_to = "bloom", values_to = "frequency")
# ggplot(Chlsumlong, aes(x = Year, y = frequency, fill = bloom)) +
#   geom_col()+ facet_grid(Season~Region)
#
# Chlsum = mutate(Chlsum, percent = Presence/(Absence + Presence),
#                 Yr_type = factor(Yr_type,
#                                  levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"),
#                                  ordered = TRUE))
#
# ggplot(Chlsum, aes(x = Year, y = percent, fill = Yr_type)) + geom_col()+
#   facet_grid(Season~Region) + drt_color_pal_yrtype()+
#   ylab("Percent of CHLa samples over 10ug/L")
#
# ggplot(Chlsum, aes(x = Year, y = percent, fill = Drought)) + geom_col()+
#   facet_grid(Season~Region) + drt_color_pal_drought()+
#   ylab("Percent of CHLa samples over 10ug/L")
#
# ggplot(Chlsum, aes(x = Drought, y = percent, fill = Drought)) + geom_boxplot()+
#   facet_grid(Season~Region) + drt_color_pal_drought()+
#   ylab("Percent of CHLa samples over 10ug/L")
#
# ggplot(Chlsum, aes(x = Yr_type, y = percent, fill = Yr_type)) +
#   geom_boxplot(alpha = 0.5)+
#   facet_grid(Season~Region) + drt_color_pal_yrtype()+
#   ylab("Percent of CHLa samples over 10ug/L")+
#   theme_bw()
#
############################################################
#Pre-2000 versus post-2000

Chl2b = mutate(Chla, Regime = case_when(YearAdj < 2000 ~ "long-term",
                                     YearAdj >= 2000 ~ "Short-Term"))

Chlb = mutate(Chl, Regime = case_when(ds_year < 2000 ~ "long-term",
                                      ds_year >= 2000 ~ "Short-Term"), Date = date(Date)) %>%
  rename(Year = ds_year) %>%
  left_join((yrs)) %>%
  left_join(DF)

Chlbreg = mutate(Chl2reg, Regime = case_when(Year < 2000 ~ "long-term",
                                      Year >= 2000 ~ "Short-Term"))

ggplot(Chlb, aes(x = log(OUT), y = chlaAvg_log10, color = Regime)) + geom_point()+
  geom_smooth(method = "lm")

ggplot(Chl2b, aes(x = SVIndex, y = LogChl, color = Regime)) + geom_point()+
  geom_smooth(method = "lm")+
  facet_grid(~Season)+ ylab("Seasonal average chlorophyll")+
  xlab("Sac Valley Index (annual)")


ggplot(Chlbreg, aes(x = Index, y = logChl, color = Regime)) + geom_point()+
  geom_smooth(method = "lm")+
  facet_grid(Region~Season)+ ylab("Seasonal average chlorophyll")+
  xlab("Sac Valley Index (annual)")

####################################################################3
#final models and box plots
library(broom)
Chl1 = lm(LogChl ~ Drought*Region*Season, data = Chla)
summary(Chl1)
plot(Chl1)
eff1 = allEffects(Chl1, x.var = "Drought")
plot(eff1)
Chl1res = tidy(Chl1) %>%
  mutate(Stars = case_when(p.value < 0.05 & p.value > 0.01 ~ "*",
                           p.value < 0.01 & p.value > 0.001 ~ "**",
                           p.value < 0.001 ~ "***"))
# I hate three-way interactions. Impossible to interpret


Chl2 = lm(LogChl ~Drought*Region + Drought*Season, data = Chla)
plot(Chl2)
eff2 = allEffects(Chl2, x.var = "Drought")
plot(eff2)
summary(Chl2)
pairsCHL = emmeans(Chl2, pairwise ~ Drought*Region)
plot(pairsCHL, comparison = T)
#Nada. Nothing going on.
pairsCHL2 = emmeans(Chl2, pairwise ~ Drought*Season)
plot(pairsCHL2, comparison = T)

Chl3 = lm(LogChl ~Region*Drought + Season, data = Chla)
plot(Chl3)
eff3 = allEffects(Chl3)
plot(eff3, x.var = "Drought")
summary(Chl3)
pairsCHL = emmeans(Chl2, pairwise ~ Drought*Region)
plot(pairsCHL, comparison = T)
#Nada. Nothing going on.
pairsCHL2 = emmeans(Chl2, pairwise ~ Drought*Season)
plot(pairsCHL2, comparison = T)

library(MuMIn)
AICc(Chl2)
AICc(Chl1)
AICc(Chl3)

