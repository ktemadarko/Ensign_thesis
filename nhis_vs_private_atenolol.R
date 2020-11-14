#important for git
#Setting remote 'origin' to 'git@github.com:ktemadarko/Ensign_thesis.git'

library(tidyverse)
Private_Pharmacies=tibble(Number_of_years=c(0:20),Price=114*Number_of_years)
NHIS=tibble(Number_of_years=c(0:20),Price=67.2*Number_of_years)

Combo1=tibble(Number_of_years=c(0:20),Private_Price=114*Number_of_years,
              NHIS_Price=67.2*Number_of_years, Difference=Private_Price-NHIS_Price)

fabre=c("Private_Price"="red",NHIS_Price="blue")

Merge1=NHIS%>% mutate(Type="NHIS")%>%
  bind_rows(Private_Pharmacies%>%mutate(Type="Private Pharmacies"))

b=ggplot(Merge1,aes(x=Number_of_years,y=Price, color=Type))+
  geom_line(size=1)+
  geom_ribbon(data=Merge1,aes(ymin=0, ymax=2280, x=Number_of_years),
              fill="light green")+
  xlab("Number of Years")+
  ylab("Price of Atenolol")+
  ggtitle("Trend in the cost of Atenolol NHIS versus Private Pharmacies",
          subtitle="Assuming prices remain constant, NHIS= 5.60 Ghana cedis 
          and Private Pharmacies= 9.50 Ghana cedis")
b

plot(Private_Pharmacies)
plot(NHIS)

a=ggplot(data=Combo1,aes(x=Number_of_years))+
  geom_line(aes(y=Private_Price,color= "Private_Price"), size= 1.5)+
  geom_line(aes(y=NHIS_Price, color= "NHIS_Price"),size= 1.5)+
  scale_color_manual(values=fabre)+
  geom_ribbon(data=Combo1,aes(ymin=NHIS_Price, ymax=Private_Price, x=Number_of_years),
              fill="light blue")+
  labs(x="Number of Years",y="Price of Atenolol", color="Legend")+
  ggtitle("Trend in the cost of Atenolol NHIS versus Private Pharmacies",
          subtitle="Assuming prices for 28 tablets remain constant, NHIS= 5.60 Ghana cedis 
          and Private Pharmacies= 9.50 Ghana cedis")
a



library(reshape2)
c2=merge(NHIS,Private_Pharmacies, by="Number_of_years")
test_data<-melt(c2, id="Number_of_years")

test_dat<-melt(Combo1, id="Number_of_years")

ggplot(test_dat,aes(x=Number_of_years,y=value, col=variable))+geom_line(size=1)

c=ggplot(test_data,aes(x=Number_of_years,y=value, col=variable))+geom_line(size=1)+
  geom_ribbon(data=test_data,aes(ymin=0, ymax=value, x=Number_of_years),
              fill="light blue")+
  xlab("Number of Years")+
  ylab("Price of Atenolol")+
  ggtitle("Trend in the cost of Atenolol NHIS versus Private Pharmacies",
          subtitle="Assuming prices remain constant, NHIS= 5.60 Ghana cedis 
          and Private Pharmacies= 9.50 Ghana cedis")
c
