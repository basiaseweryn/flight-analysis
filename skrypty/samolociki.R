library(readxl)
library(dplyr)
library(ggplot2)
library(beepr)
setwd("C:\\Users\\Agata Krawczyk\\Desktop\\all\\dataverse_files (1)")
samolociki <- read.csv("plane-data.csv")
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
r2007 <- read.csv("2007.csv.bz2")
r2008 <- read.csv("2008.csv.bz2")
beep()
airports <- read.csv("airports1.csv")
r1995
#samolociki które zrobiły najwiecej lotów
ile_lotow <- function(df){
  tmp <- select(df,TailNum)%>%
    group_by(TailNum)%>%
    summarise(noOfFlights=n())
  
}
a <- ile_lotow(r1995)
a <- rbind(a,ile_lotow(r1996))
a <- rbind(a,ile_lotow(r1997))
a <- rbind(a,ile_lotow(r1998))
a <- rbind(a,ile_lotow(r1999))
a <- rbind(a,ile_lotow(r2000))
a <- rbind(a,ile_lotow(r2001))
a <- rbind(a,ile_lotow(r2002))
a <- rbind(a,ile_lotow(r2003))
a <- rbind(a,ile_lotow(r2004))
a <- rbind(a,ile_lotow(r2005))
a <- rbind(a,ile_lotow(r2006))
a <- rbind(a,ile_lotow(r2007))
a <- rbind(a,ile_lotow(r2008))
safety <- a
a <- safety
beep()
a <- a%>%
  arrange(desc(noOfFlights))
a <- a[23:33,]
a <- left_join(a,samolociki, by=join_by("TailNum"=="tailnum"))
najw <- a[1,]
#plot ilosci lotow tego najwiecej latajacego
najwiecej <- function(df){
  
    tmp <- select(df,TailNum,Year)%>%
      filter(TailNum=="N308SW")%>%
      group_by(TailNum,Year)%>%
      summarise(noOfFlights=n())
     
  
}
i <- najwiecej(r2003)
i <- rbind(i,najwiecej(r2004))
i <- rbind(i,najwiecej(r2005))
i <- rbind(i,najwiecej(r2006))
i <- rbind(i,najwiecej(r2007))
i <- rbind(i,najwiecej(r2008))
help <- i
i

ggplot(data=i,aes(x=Year,y=noOfFlights))+geom_line()
write.csv(a, "C:\\Users\\Agata Krawczyk\\Desktop\\all\\dataverse_files (1)\\tabelka.csv", row.names=FALSE)

#ilość samolocików w obiegu danej firmy
tmp <- select(samolociki,manufacturer)%>%
  filter(manufacturer!="")%>%
  mutate(man=substr( manufacturer ,1,6))%>%
  group_by(man)%>%
  summarise(no=n())%>%
  arrange(desc(no))%>%
  slice(1:10)%>%
  mutate(man=c("BOEING",  "AIRBUS"  , "EMBRAER" ,"BOMBARDIER INC" ,"MCDONNELL DOUGLAS" , "CANADAIR","DOUGLAS" ,"SAAB-SCANIA","DEHAVILLAND","AEROSPATIALE" ))
cbind <- c("BOEING",  "AIRBUS"  , "EMBRAER" ,"BOMBARDIER INC" ,"MCDONNELL DOUGLAS" , "CANADAIR","DOUGLAS" ,"SAAB-SCANIA","DEHAVILLAND","AEROSPATIALE" )
ggplot(data=tmp,aes(x = reorder(man, -no), y = no))+
  geom_col(fill="darkred")+  xlab("producent")+ylab("ilość jednostek")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=10),panel.background = element_rect(fill="white"),panel.grid.major = element_line("gray"),
        axis.title = element_text(size = 15,margin = margin(t = 10)))


?theme
tmp <- select(samolociki,model)%>%
  filter(model!="")%>%
  group_by(model)%>%
  summarise(no=n())%>%
  arrange(desc(no))%>%
  slice(1:10)
tmp
  ggplot(data=tmp,aes(x = reorder(model, -no), y = no))+
    geom_col(fill="darkblue")+  xlab("model")+ylab("ilość jednostek")+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=10),panel.background = element_rect(fill="white"),panel.grid.major = element_line("gray"),
          axis.title = element_text(size = 15))
                                                                                                                  

years <-(substr( samolociki$issue_date ,7,10))
tmp <- select(samolociki, tailnum, issue_date)%>%
  mutate(year=substr( samolociki$issue_date ,7,10))%>%
  filter(year!="")%>%
  arrange(year)
  
head(tmp)
samolociki1 <- samolociki
old <- samolociki1%>%
  filter(tailnum=="N227AA")
old
balloon <- function(df){
  tmp <- select(df,Year,TailNum)%>%
    filter(TailNum=="N227AA")%>%
    summarise(no=n())
}
a <- balloon(r1987)
a <- rbind(a,balloon(r1988))
a <- rbind(a,balloon(r1989))
a <- rbind(a,balloon(r1990))
a <- rbind(a,balloon(r1991))
a <- rbind(a,balloon(r1992))
a <- rbind(a,balloon(r1993))
a <- rbind(a,balloon(r1994))
a
r1994
state_delays <- function(df){
  fin <- select(airports,IATA,STATE)
  colnames(fin) <- c("code","state")
  fin1 <- select(df,Origin,DepDelay)%>%
    na.omit()%>%
    group_by(Origin)%>%
    summarise(delays=sum(DepDelay),no=n())%>%
    mutate(prop=delays/no)%>%
    arrange(desc(prop))
  fin1 <- left_join(fin1,fin,by=join_by(Origin==code))
 
}
a <- state_delays(r1987)
a <- rbind(a,state_delays(r1988))
a <- rbind(a,state_delays(r1989))
a <- rbind(a,state_delays(r1990))
a <- rbind(a,state_delays(r1991))
a <- rbind(a,state_delays(r1992))
a <- rbind(a,state_delays(r1993))
a <- rbind(a,state_delays(r1994))
a <- rbind(a,state_delays(r1995))
a <- rbind(a,state_delays(r1996))
a <- rbind(a,state_delays(r1997))
a <- rbind(a,state_delays(r1998))
a <- rbind(a,state_delays(r1999))
a <- rbind(a,state_delays(r2000))
a <- rbind(a,state_delays(r2001))
a <- rbind(a,state_delays(r2002))
a <- rbind(a,state_delays(r2003))
a <- rbind(a,state_delays(r2004))
a <- rbind(a,state_delays(r2005))
a <- rbind(a,state_delays(r2006))
beep()
tmp <- a
a
a <- a%>%
  group_by(state)%>%
  summarise(no=sum(prop))%>%
  arrange(desc(no))%>%
  na.omit()
a

write.csv(a, "C:\\Users\\Agata Krawczyk\\Desktop\\all\\dataverse_files (1)\\tabelka1.csv", row.names=FALSE)






