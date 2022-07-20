#################################################################
################## UN YOUNTH HACKATON Script ####################
#################################################################


# packages necessary
require(readxl)
require(factoextra)
require(FactoMineR)
require(ggplot2)
require(tidyverse)
require(dplyr)
require(knitr)
require(RColorBrewer)
require(viridis)

# Databases importing
# covid19_symptom <- read.csv("Docs/SDG3_good_health/covid-19_symptom.csv", stringsAsFactors=TRUE)
# lawatlas.emergency <- read.csv("Docs/SDG3_good_health/lawatlas-emergency-declarations.csv", stringsAsFactors=TRUE)
vaccination.data <- read.csv("Docs/SDG3_good_health/vaccination-data.csv", stringsAsFactors=TRUE)
who_covid_global <- read.csv("Docs/SDG3_good_health/WHO-COVID-19-global-data.csv", stringsAsFactors = TRUE)
social_protection <- read_excel("Docs/SDG3_good_health/social_protection.xlsx",range = "A6:M1714")

# SDG 3
## Death due to covid  19 in the world

names(who_covid_global)
levels(who_covid_global$ï..Date_reported)
who_covid_global$ï..Date_reported<-as.Date(who_covid_global$ï..Date_reported)
whoReg <- levels(who_covid_global$WHO_region)
datawhoReg <- as.data.frame(cbind(Region = NA, Death = NA))

for (i in 1:length(whoReg)) {
  datawhoReg[i,] <- c(whoReg[i],sum(who_covid_global$New_deaths[who_covid_global$WHO_region == whoReg[i]]))
}
datawhoReg
# Plotting the total number of deaths due to covid19
ggplot(data = datawhoReg, aes(x = Region, y = Death )) +
  geom_bar(stat = 'identity', fill = c("#CCFF00","#FF6600","#FF2200","#FFCC00","#00F900","#FF0000","#00FF66")) +
    ylab('Total number of Deaths')

deathCur <- as.data.frame(cbind(DateC = NA,Death = NA, New_Cases = NA))
DateCov <- levels(factor(who_covid_global$ï..Date_reported))
s = 0 # cum death
t <- 0 # cum new cases
for (i in 1:length(DateCov)) {
  t = t + sum(who_covid_global$New_cases[who_covid_global$ï..Date_reported == DateCov[i]])
  s = s + sum(who_covid_global$New_deaths[who_covid_global$ï..Date_reported == DateCov[i]])
  deathCur[i,] <- c(DateCov[i], s, t)
}
head(deathCur,35)
deathCur$DateC <- as.Date(deathCur$DateC)
deathCur$Death <- as.numeric(deathCur$Death)
deathCur$New_Cases <- as.numeric(deathCur$New_Cases)

deathCur %>%
  ggplot(aes(x = DateC, y = New_Cases))+
    geom_line(size = 1, col = '#FF3300') + ggtitle('Cummulative Cases in Time')
deathCur %>%
  ggplot(aes(x = DateC, y = Death))+
    geom_line(size = 1, col = '#FF0000') + ggtitle('Total number of Deaths in Time') 
# 
# 
# 
# dated <- who_covid_global %>% group_by(ï..Date_reported) %>% mutate(sum_death = sum(New_deaths))
# aggregate(deathCur[,2], by = list(DateC), FUN = mean)


## Vaccination

vactemp <- vaccination.data
attach(vactemp)
names(vactemp)
# View(vaccination.data)
reg2 <- levels(WHO_REGION)
Peop_vac <- as.data.frame(0) # people vaccinated database creating
for (i in reg2) {
  reg_vac <- vactemp %>% filter(WHO_REGION == i)
  Peop_vac[which(reg2 == i),] <- sum(reg_vac$TOTAL_VACCINATIONS, na.rm = T)
  print(i)
}
Peop_vac <- cbind.data.frame(WHO_REGION = reg2, Peop_vac)
row.names(Peop_vac) <- reg2; names(Peop_vac)[2] <- "Vaccinations"

Peop_vac %>%
ggplot(aes(x = WHO_REGION, y = Vaccinations)) +
  geom_bar(stat = 'identity', fill = rainbow(12)[seq(12,6)]) +
  ylab('Total number of Vaccins administrated') + xlab('WHO REGION') + geom_point()

detach(vactemp)

# social protection
# How country take care of the social protection
# We use PCA 

socTemp <- social_protection
attach(socTemp)
names(socTemp)

soc_Tot <- socTemp[1:118,] %>% filter(Sex == "Total") %>% filter(Time == 2020)
soc_Tot_comp <- as.data.frame(soc_Tot)
soc_fem <- socTemp[1:118,] %>% filter(Sex == "Female") %>% filter(Time == 2020)
for (i in 1:39)
  for (j in 8:9)
    if (is.na(soc_Tot_comp[i,j]))
      soc_Tot_comp[i,j] = soc_fem[i,j]

soc_Tot_comp <- soc_Tot_comp[,-12]

row.names(soc_Tot_comp) <- soc_Tot_comp[,1]
PCA_soc <- na.omit(soc_Tot_comp[,5:12])
names(PCA_soc) <- c('Pop_cov_soc_Prot', "pers_retir_pens","pers_disab_soc_ben", "unemp_benef", "moth_born_benef",'Emp_work_inj', "child_benef","vul_pers_soc_assit")

# Principal Component Analysis (PCA)

PCA <- PCA(PCA_soc, scale.unit = T, ncp = 3, graph = F)
barplot(PCA$eig[,1], col = viridis(7)) # eigen values
kable(PCA$var$cor) 

plot.PCA(PCA, choix = "ind", axes = c(1:2),habillage = 1) # Population covered by at least one social protection benefit
plot.PCA(PCA, choix = 'var', axes = 1:2, col.var = viridis(8))
