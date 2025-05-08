library(dplyr)
library(ggplot2)

setwd("C:\\Users\\Agata Krawczyk\\Desktop\\all\\dataverse_files (1)")

r1987 <- read.csv("1987.csv.bz2")
r1988 <- read.csv("1988.csv.bz2")
r1989 <- read.csv("1989.csv.bz2")
r1990 <- read.csv("1990.csv.bz2")
r1991 <- read.csv("1991.csv.bz2")
r1992 <- read.csv("1992.csv.bz2")
r1993 <- read.csv("1993.csv.bz2")
r1994 <- read.csv("1994.csv.bz2")
r1995 <- read.csv("1995.csv.bz2")
r1996 <- read.csv("1996.csv.bz2")
r1997 <- read.csv("1997.csv.bz2")
r1998 <- read.csv("1998.csv.bz2")
r1999 <- read.csv("1999.csv.bz2")
r2000 <- read.csv("2000.csv.bz2")
r2001 <- read.csv("2001.csv.bz2")
r2002 <- read.csv("2002.csv.bz2")
r2003 <- read.csv("2003.csv.bz2")
r2004 <- read.csv("2004.csv.bz2")
r2005 <- read.csv("2005.csv.bz2")
r2006 <- read.csv("2006.csv.bz2")
names(r2001)
r2001
cancelled <- function(df){
  ilosc_lotow_odwolanych <- select(df,Cancelled,Month,DayofMonth,CancellationCode)%>%#wybranie kolumn
    filter(Cancelled==1 )%>%
    group_by(Month,DayofMonth)%>%
    summarise(Cancelled=n())
  ilosc_lotow_ogolnie <- select(df,Cancelled,Month,DayofMonth)%>%
    group_by(Month,DayofMonth)%>%
    summarise(No=n())
  fin <- cbind(ilosc_lotow_odwolanych,ilosc_lotow_ogolnie)
  
  
  fin <- fin%>%mutate(prop=100*Cancelled/No)
  
  fin <- fin[c(1,2,7)]
  colnames(fin) <- c("Month","DayofMonth","prop")
  fin
}
fin <- cancelled(r1999)
fin <- rbind(fin,cancelled(r2000))
fin <- rbind(fin,cancelled(r2001))
fin <- rbind(fin,cancelled(r2002))
fin <- rbind(fin,cancelled(r2003))
fin <- rbind(fin,cancelled(r2004))
typeof(fin)
fin
fin <- fin%>%
  mutate(id=1:2192)
ggplot(data=fin,aes(x=id,y=prop))+geom_line(color="darkred")+scale_x_continuous(breaks =c(0, 365,730,1095,1460,1825),labels = expression(1999,2000,2001,2002,2003,2004)) + ylab("procent lotów odwołanych")+xlab("rok")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=10),panel.background = element_rect(fill="white"),panel.grid.major = element_line("gray"),
        axis.title = element_text(size = 15,margin = margin(t = 10)))
fin <- cancelled(r2001)
fin <- fin%>%
  mutate(id=1:365)
ggplot(data=fin,aes(x=id,y=prop))+geom_line(color="darkblue")+scale_x_continuous(breaks =c(0, 31,59,90,120,151,181,212,243,272,304,334),labels = expression(1,2,3,4,5,6,7,8,9,10,11,12)) + ylab("procent lotów odwołanych")+xlab("miesiąc")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=10),panel.background = element_rect(fill="white"),panel.grid.major = element_line("gray"),
        axis.title = element_text(size = 15,margin = margin(t = 10)))

noOfFlights <- function(df){
  monthandflights <- select(df,Month,Year)%>%
    group_by(Month,Year)%>%
    summarise(no=n())
  

}
fin <- noOfFlights(r1999)
fin <- rbind(fin,noOfFlights(r2000))
fin <- rbind(fin,noOfFlights(r2001))
fin <- rbind(fin,noOfFlights(r2002))
fin <- rbind(fin,noOfFlights(r2003))
fin <- rbind(fin,noOfFlights(r2004))
fin <- fin%>%
  ungroup()%>%
  mutate(id=1:72)

ggplot(data=fin,aes(x=id,y=no))+geom_line(color="darkred")+scale_x_continuous(breaks =c(0, 12,24,36,48,60),labels = expression(1999,2000,2001,2002,2003,2004))+
  ylab("ilość lotów")+xlab("rok")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=10),panel.background = element_rect(fill="white"),panel.grid.major = element_line("gray"),
        axis.title = element_text(size = 15,margin = margin(t = 10)))
r2001
fin <- select(r2001, Origin,DepDelay,Month,DayofMonth)%>%
  filter(DepDelay>0)%>%
  group_by(Month,DayofMonth)%>%
  summarise(delay=sum(DepDelay),no=n())
fin
fin <- fin%>%
  ungroup()%>%
  mutate(id=1:364)%>%
  mutate(prop=delay/no)

ggplot(data=fin,aes(x=id,y=prop))+geom_line()+scale_x_continuous(breaks =c(0, 31,59,90,120,151,181,212,243,272,304,334),labels = expression(1,2,3,4,5,6,7,8,9,10,11,12) )
fin <- select(r2001, Origin,DepDelay,Month,DayofMonth)%>%
  filter(DepDelay>0,Month==9)%>%
  group_by(Month,DayofMonth)%>%
  summarise(delay=sum(DepDelay),no=n())
fin
fin <- fin%>%
  ungroup()%>%
  mutate(id=1:29)%>%
  mutate(prop=delay/no)
ggplot(data=fin,aes(x=id,y=prop))+geom_line()#+scale_x_continuous(breaks =c(0, 31,59,90,120,151,181,212,243,272,304,334),labels = expression(1,2,3,4,5,6,7,8,9,10,11,12) )
