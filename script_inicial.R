options(scipen=999)
#importando o arquivo
caixa <- read.csv("05-10-21.csv", sep=",")
#transformando o timestamp em data
library(lubridate)
#apenas a data
caixa$timestamp <-  ymd_hms(caixa$timestamp)

caixa$data <- substr(caixa$timestamp, 1, 10)

caixa$data <- ymd(caixa$data)
#apenas a hora
caixa$hora <- substr(caixa$timestamp, 12, 19)

caixa$hora <- hms(caixa$hora)
#summary do airquality
summary(caixa$airquality)

#gráfico
library(ggplot2)
ggplot(caixa, aes(data, airquality))+
  geom_point()+
  geom_smooth()+
  geom_hline(yintercept = 50, colour= "red", linetype="dotted")

#tabela
library(dplyr)
tbair <- caixa%>%group_by(data)%>% summarise(media=mean(airquality), desvio=sd(airquality),
                                             maximo=max(airquality), minimo=min(airquality))
write.csv(tbair, "tab_airquality.csv")

#summary do dust
summary(caixa$dust)

#gráfico do dust
ggplot(caixa, aes(data, dust))+
  geom_point()+
  geom_smooth()+
  geom_hline(yintercept = 120, colour= "red", linetype="dotted")

#tabela
library(dplyr)
tbdust <- caixa%>%group_by(data)%>% summarise(media=mean(dust), desvio=sd(dust),
                                             maximo=max(dust), minimo=min(dust))
write.csv(tbdust, "tab_dust.csv")

#summary do temperature
summary(caixa$temperature)

#gráfico do temperature
ggplot(caixa, aes(data, temperature))+
  geom_point()+
  geom_smooth()+
  geom_hline(yintercept = 35, colour= "red", linetype="dotted")

#tabela
library(dplyr)
tbtemp <- caixa%>%group_by(data)%>% summarise(media=mean(temperature), desvio=sd(temperature),
                                             maximo=max(temperature), minimo=min(temperature))
write.csv(tbtemp, "tab_temp.csv")

#summary do ozone
summary(caixa$ozone)

#gráfico do temperature
ggplot(caixa, aes(data, ozone))+
  geom_point()+
  geom_smooth()+
  geom_hline(yintercept = 240, colour= "red", linetype="dotted")

#tabela
library(dplyr)
tbozone <- caixa%>%group_by(data)%>% summarise(media=mean(ozone), desvio=sd(ozone),
                                             maximo=max(ozone), minimo=min(ozone))
write.csv(tbozone, "tab_ozone.csv")
