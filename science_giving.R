science_giving <- read.csv("science_federal_giving.csv")
attach(science_giving)
library(dplyr)
library(magrittr)
library(tidyr)

x <- science_giving %>%
  group_by(cleanedoccupation, cycle, state, cand_pty_affiliation, classification) %>%
  summarise(n=sum(X2016_dollars),
            ct=n()) %>%
  arrange(desc(n))

x <- x[0:41325,]

gph1 <- x %>%
  group_by(cleanedoccupation, cand_pty_affiliation, classification) %>%
  filter(cand_pty_affiliation %in% c("REP", "DEM")) %>%
  filter(classification %in% c("Engineer", "Scientist")) %>%
  summarise(total = sum(n)) %>%
  arrange(desc(total)) %>%
  ungroup() %>%
  group_by(cand_pty_affiliation, classification) %>%
  top_n(n=7) %>%
  arrange(classification, desc(total))
  
options(scipen = 999)
plot1 <- ggplot(data=gph1, aes(x=reorder(cleanedoccupation, total), y=total))+
  geom_col(aes(fill=cand_pty_affiliation), position="dodge")+
  coord_flip()+
  theme_bw()+
  xlab("")+
  ylab("Millions of dollars")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_line(color="grey90"),
        axis.ticks = element_line(color="grey90"),
        axis.ticks.y=element_blank(),
        strip.background=element_blank(),
        panel.border=element_rect(color="grey60"))+
  facet_grid(classification~.,scales = "free")+
  ggtitle("Scientific donations to politicians by party, 2008-2016")+
  scale_fill_manual(values=c("#377eb8", "#e41a1c"), labels=c("Democrat", "Republican"))+
  scale_y_continuous(expand=c(0,0), limits=c(0,25000000), labels = c("0", "5", "10", "15", "20", "25"))

print(plot1)



xdem <- subset(x, cand_pty_affiliation=="DEM")
xrep <- subset(x, cand_pty_affiliation=="REP")
xdem <- subset(xdem, cleanedoccupation %in% xrep$cleanedoccupation)

xrep_group <- xrep %>%
  group_by(cleanedoccupation, cand_pty_affiliation, classification) %>%
  summarise(total = sum(n)) %>%
  arrange(cleanedoccupation)

xdem_group <- xdem %>%
  group_by(cleanedoccupation, cand_pty_affiliation, classification) %>%
  summarise(total = sum(n)) %>%
  arrange(cleanedoccupation)

xrep_group <- subset(xrep_group, cleanedoccupation %in% xdem_group$cleanedoccupation)

xdem_group$difference <- xdem_group$total - xrep_group$total

dem_top <- xdem_group %>%
  arrange(desc(difference))
dem_top <- dem_top[0:13,]
dem_top$party <- "dem"

dem_bottom <- xdem_group %>%
  arrange(difference)
dem_bottom <- dem_bottom[0:13,]
dem_bottom$party <- "rep"
topbottom <- rbind(dem_top, dem_bottom)

plot2 <- ggplot(data=topbottom, aes(x=reorder(cleanedoccupation, difference), y=difference))+
  geom_col(aes(fill=party))+
  coord_flip()+
  theme_bw(base_size=13)+
  xlab("")+
  ylab("")+
  theme(legend.position = "bottom",
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_line(color="grey90"),
        axis.ticks = element_line(color="grey90"),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        strip.background=element_blank(),
        plot.margin=margin(0.6,0.6,0,0.6, "cm"),
        legend.box.margin=margin(0,0,0,0, "cm"),
        legend.box.spacing=unit(0, "cm"),
        legend.title=element_text(size=11),
        plot.title = element_text(size=12),
        panel.border=element_rect(color="grey60"))+
  ggtitle("Party differences in political donations by scientists, 2008-16")+
  scale_fill_manual(values=c("#377eb8", "#e41a1c"), labels=c("Democrats", "Republicans"), name="Millions of dollars more donated to")+
  scale_y_continuous(expand=c(0,0), limits=c(-2500000,7500000), labels=c("2.5", "0", "2.5", "5.0", "7.5"))

print(plot2)







  
  
