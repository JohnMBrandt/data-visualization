
mydata <- read.csv("convertcsv.csv")
mydatacount <- subset(mydata, X_unit=="count")

mydatacount$X_startDate <- lubridate::as_datetime(mydatacount$X_startDate)
mydatacount$X_value <- as.numeric(mydatacount$X_value)
library(ggplot2)


library(lubridate)
library(dplyr)
library(magittr)
mydatahourly <- mydatacount %>%
  group_by(rounded_hour = floor_date(X_startDate, unit="hour")) %>%
  summarise(hourly_sum=sum(as.numeric(as.character(X_value))))


activityplot <- ggplot(data=mydatahourly, aes(x=rounded_hour, y=hourly_sum))+
  geom_line(alpha=0.85)+
  theme_bw()+
  scale_x_datetime(expand=c(0,0),date_breaks = "month", date_labels = "%b-%y")+
  ylab("Steps per hour")+
  xlab("")

print(activityplot)




mydatacount$hour <- hour(mydatacount$X_startDate)
mydatacount$minute <- minute(mydatacount$X_startDate)
mydatacount$month <- month(mydatacount$X_startDate)
mydatacount$year <- year(mydatacount$X_startDate)

mydatacount <- subset(mydatacount, year==2017)
mydatacount$bin <- 0

mydatacount$bin[mydatacount$minute <= 29 & mydatacount$minute >= 0] <- 1
mydatacount$bin[mydatacount$minute <= 59 & mydatacount$minute >= 30] <- 2
mydatacount$degree <- mydatacount$bin * (mydatacount$hour+1)


forcirclegraph <- mydatacount %>%
  group_by(hour, month) %>%
  summarise(n=sum(as.numeric(as.character(X_value))))

forcirclegraph$ampm <- 0
forcirclegraph$ampm[forcirclegraph$hour <= 11 & forcirclegraph$hour >= 0] <- "am"
forcirclegraph$ampm[forcirclegraph$hour <= 23 & forcirclegraph$hour >= 12] <- "pm"
forcirclegraph$hour12 <- forcirclegraph$hour
forcirclegraph$hour12[forcirclegraph$ampm=="pm"] <- forcirclegraph$hour12[forcirclegraph$ampm=="pm"] - 12
#forcirclegraph$degree <- as.factor(forcirclegraph$degree)

forcirclegraph$mm <- 0
forcirclegraph$mm[forcirclegraph$month == 1] <- "Jan"
forcirclegraph$mm[forcirclegraph$month == 2] <- "Feb"
forcirclegraph$mm[forcirclegraph$month == 3] <- "Mar"
forcirclegraph$mm[forcirclegraph$month == 4] <- "Apr"
forcirclegraph$mm[forcirclegraph$month == 5] <- "May"
forcirclegraph$mm[forcirclegraph$month == 6] <- "Jun"
forcirclegraph$mm[forcirclegraph$month == 7] <- "Jul"
forcirclegraph$mm[forcirclegraph$month == 8] <- "Aug"
forcirclegraph$mm[forcirclegraph$month == 9] <- "Sept"
forcirclegraph$mm[forcirclegraph$month == 10] <- "Oct"
forcirclegraph$mm[forcirclegraph$month == 11] <- "Nov"

forcirclegraph$mm <- ordered(forcirclegraph$mm, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov"))

averages <- forcirclegraph %>%
  group_by(hour) %>%
  summarise(n=mean(n))

require(ggplot2)
circle1 <- ggplot(data=forcirclegraph, aes(x=hour, y=n))+
  geom_area(data=averages,aes(x=hour,y=n), color="grey70", fill="grey70")+
  geom_line()+
  theme_bw()+
  facet_wrap(~mm, nrow=3)+
  geom_vline(xintercept=12, linetype="dashed", color="grey30")+
  theme(strip.background = element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        panel.border=element_rect(color="grey30"))+
  scale_x_continuous(breaks=c(0,3,6,9,12,15,18,21), labels=c("0","","6 am","","12","","6 pm",""))+
  scale_y_continuous(breaks=c(0,15000,30000,45000), labels=c("0","15,000", "30,000", "45,000"))+
  ggtitle("Number of steps I took at each hour of the day, Jan-Nov 2017")+
  ylab("")+
  xlab("")

print(circle1)

require(dplyr)
mydatamile <- mydata %>%
  filter(X_unit=="mi")

require(lubridate)
mydatamile$X_startDate <- as_datetime(mydatamile$X_startDate)
mydatamile$X_startDate <- floor_date(mydatamile$X_startDate, unit="day")

mydatamile <- mydatamile %>%
  group_by(X_startDate) %>%
  summarise(n=sum(as.numeric(as.character(X_value))))
  
  
