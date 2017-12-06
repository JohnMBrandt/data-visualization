ozone <- read.csv("indexed_data.csv")
ozone_data <- ozone %>%
  filter(age_group == "total") %>%
  filter(Pollutant == "NO2") %>%
  group_by(date, provinces) %>%
  summarise(ozone = mean(Concentration), temp = mean(Temp_C))


ozone_data$date <- as.Date(ozone_data$date, format='%Y-%m-%d')

ozone_data <- ozone_data %>%
  arrange(date)
ozone_data <- ozone_data[0:2862,]

ozone_data <- subset(ozone_data, !(provinces %in% c("Malaga", "Murcia")))

dateplot <- ggplot(data=ozone_data, aes(x=date, y=ozone))+
  geom_line(aes(group=provinces), alpha=0.2, color="grey30")+
  geom_line(data=subset(ozone_data, provinces=="Madrid"), color="red")+
  theme_bw()+
  ylab(expression(paste("Nitrogen dioxide ", ug/m^3)))+
  xlab("")+
  theme(panel.grid.minor.y=element_blank(),
    legend.title=element_blank(),
    legend.position=c(0.75,1.06),
    panel.grid.major.y=element_blank(),
    panel.grid.minor.x=element_blank(),
    axis.ticks = element_line(color="grey90"))+
  scale_x_date(expand=c(0,0), date_breaks = "1 month", date_labels = "%b")+
  ggtitle("Nitrogen dioxide concentrations are higher in Madrid than in the rest of Spain")

print(dateplot)


