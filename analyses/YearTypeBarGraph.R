#COde for figure 1 - water year index graph

library(tidyverse)

#import water year assignments
WYs = read.csv("data/yearassignments.csv")
WYs = mutate(WYs,Yr_type = factor(Yr_type, levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet")))


#without the drought periods, and with lines for year type cut offs
cutoffs = data.frame(Yr_type = c("Critical", "Dry", "Below \nNormal", "Above \nNormal", "Wet"),
                     cutval = c(0, 5.4, 6.5, 7.8, 9.2))



#try a simpler version
pal_drought <- c(D = "#FDE333", N = "#53CC67", W = "#00588B")
ggplot(filter(WYs, Year > 1974, Year <2022))+
  geom_bar(aes(x = Year, y = Index, fill = Drought), stat = "identity")+
  scale_fill_manual(values = pal_drought, name = NULL, labels = c("Drought", "Neutral", "Wet"))+
  theme_bw()+
  geom_hline(data = cutoffs, aes(yintercept = cutval), linetype = 2)+
  geom_text(data = cutoffs, aes(x = 1970, y = cutval, label = Yr_type), vjust =0, hjust = 0, size = 3, lineheight = .9)+
  scale_x_continuous(breaks = c(1975, 2000, 2021))+
  theme(legend.position = "top") +
  ylab("Sacramento Valley Index")+
  xlab(NULL)


ggsave("plots/Wateryears1975.tiff", device = "tiff", width = 6, height = 5)
