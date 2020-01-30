setwd("D:/Material - Mestrado FEA-RP/Disciplinas/2º Semestre/Econometria II/Desafio/data")

#Setting our enviroment 
library('tidyr')
library('read.dbc')
library('readxl')
library('dplyr')
library('stringr')
library('plm')
library('stargazer')
library('reshape2')
library('pander')
library("dummies")
library('lmtest')

#loading the homicide and rate of homicide database

thom = read.csv(file = "taxa-homicidios.csv", head = TRUE, sep=";")
hom = read.csv(file = "homicidios.csv", head = TRUE, sep=";")

#making the two databases with the same name for years
thom = thom %>% 
  rename(periodo = perÃ.odo)

hom = hom%>% 
  rename(periodo = perÃ.odo)

#Constructing the same period database
yearth = pull(thom, var = "periodo")
yearth = unique(yearth)

yearh = pull(hom,var="periodo")
yearh = unique(yearh)


year1 = c()
for (i in yearth){
  year_final = c()
  for (j in yearh){
    if (i == j){
      year_final = c(j)
      year1 = c(year_final,year1)
      year1=sort(year1)   
    }
  }
}

hom = filter(hom,periodo %in% year1)

#Separating the database for SP and CE

codmunsp = read_xls("municipiosp.xls")
codmunsp = pull(codmunsp, var=2)
codmunsp=as.character(codmunsp)



thomsp = filter(thom,cod %in% codmunsp)

homsp = filter(hom,cod %in% codmunsp)

codmunce = read.csv("municipioce.csv")
codmunce = pull(codmunce, var = 1 )

thomce = filter(thom, cod %in% codmunce)
homce = filter(hom, cod %in% codmunce)

#Creating the DF with Municipal GDP

df = read.csv("pibmunicipal.csv")
df = df %>% 
  gather(year,pib, X2002:X2016) %>%
  rename(periodo = year)%>%
  rename(cod = Código)
  
df$periodo = str_replace(df$periodo,"X"," ")

df = df[order(df$periodo),]

#Creating the DF for SP
final_dfsp = data.frame()
final_dfsp = bind_rows(final_dfsp,df)
final_dfsp$periodo = as.numeric(final_dfsp$periodo)
final_dfsp = merge(final_dfsp, thomsp, by= c("cod","periodo"))
final_dfsp = final_dfsp[,-5]
final_dfsp =final_dfsp %>% rename(thom = valor)

#Creating the DF for CE
final_dfce = data.frame()
final_dfce = bind_rows(final_dfce,df)
final_dfce$periodo = as.numeric(final_dfce$periodo)
final_dfce = merge(final_dfce, thomce, by = c("cod","periodo"))
final_dfce = final_dfce[,-5]
final_dfce =final_dfce %>% rename(thom = valor)

#Another merge from Caiques DATABASE to mine
x = select(spdata,codigo,Ano,mortes_droga)

x = x %>% rename(periodo=Ano,cod=codigo)

final_dfsp = merge(final_dfsp,x, by= c("cod","periodo"),all.x = TRUE)
final_dfsp = final_dfsp %>% 
  mutate(cod=substr(cod,1,6))

pop = read.csv("populacao.csv") %>% 
  melt("Municipio") %>%
  rename(populacao = value,periodo = variable,cod=Municipio)

pop$periodo = as.character(pop$periodo)
pop$periodo = str_replace(pop$periodo, "X","")
pop$periodo = as.numeric(pop$periodo)
pop$populacao= as.numeric(pop$populacao)

final_dfsp = merge(final_dfsp,pop,by=c("cod","periodo"),all.x=TRUE)

##Preparing the dataframe to estimation

final_dfsp = final_dfsp %>%
  mutate(mortes_drogas100m = (mortes_droga/populacao)*100000) %>%
  mutate(pib_percapita = pib/populacao)
final_dfsp = cbind(final_dfsp,dummy(final_dfsp$periodo,sep = "_"))



#Merges from pop, Caiques DATABASE to CEARA
y = select(cedata,codigo,Ano,mortes_droga)
y = y %>% rename(periodo = Ano,cod = codigo)
final_dfce = merge(final_dfce,y, by= c("cod","periodo"),all.x = TRUE)
final_dfce = final_dfce %>%
  mutate(cod=substr(cod,1,6))
final_dfce = merge(final_dfce,pop,by=c("cod","periodo"),all.x=TRUE)

##Preparing the dataframe to estimation

final_dfce = final_dfce %>%
  mutate(mortes_drogas100m =(mortes_droga/populacao)*100000)%>%
  mutate(pib_percapita=pib/populacao)
final_dfce = cbind(final_dfce,dummy(final_dfce$periodo,sep = "_"))

#Estimation

Dynamic_SP = pgmm(thom~lag(thom,1) + lag(pib_percapita,0:1) + lag(mortes_drogas100m,0:1) + lag(I(mortes_drogas100m^2),0:1)|
                    lag(thom,2:99), data = final_dfsp, effect="twoways",model="twosteps")

mod_dinamicoAB<-pgmm(log(Apreensao)~lag(log(Apreensao),1:2)+lag(Produtividade_policia,0:1)+lag(Taxa_desemprego,0:1)|
                       lag(Apreensao,2:99),
                     data = Basepainel, effect = "twoways", model = "twosteps")

FE_SP = plm(thom ~ pib_percapita + mortes_drogas100m + I(mortes_drogas100m^2), data = final_dfsp, index = c("periodo","cod"), model = "within")

Pool_SP = plm(thom ~ pib_percapita + mortes_drogas100m + I(mortes_drogas100m^2), data = final_dfsp, index = c("periodo","cod"), model = "pooling")

RE_SP = plm(thom ~ pib_percapita + mortes_drogas100m + I(mortes_drogas100m^2),data=final_dfsp,index= c("periodo","cod"), model = "random")

FE_SP_Int = plm(thom ~ pib_percapita +mortes_drogas100m  + I(mortes_drogas100m)+ I(mortes_drogas100m*final_dfsp_2002) + I(mortes_drogas100m*final_dfsp_2003),data = final_dfsp,index = c("periodo","cod"),
            model = "within")


FE_SP_Int1 = plm(thom ~ pib_percapita +mortes_drogas100m  + I(mortes_drogas100m^2)+ I(mortes_drogas100m*final_dfsp_2002) 
                 + I(mortes_drogas100m*final_dfsp_2003) + I(mortes_drogas100m*final_dfsp_2004) + I(mortes_drogas100m*final_dfsp_2005)+ I(mortes_drogas100m*final_dfsp_2006)+ I(mortes_drogas100m*final_dfsp_2007)
                + I(mortes_drogas100m*final_dfsp_2008) + I(mortes_drogas100m*final_dfsp_2009) + 
                  I(mortes_drogas100m*final_dfsp_2010) + I(mortes_drogas100m*final_dfsp_2011)
                + I(mortes_drogas100m*final_dfsp_2012) + I(mortes_drogas100m*final_dfsp_2013)
                + I(mortes_drogas100m*final_dfsp_2014) + I(mortes_drogas100m*final_dfsp_2015)
                + I(mortes_drogas100m*final_dfsp_2016),data = final_dfsp,index = c("periodo","cod"),
                model = "within")
                                                                                                                                                                              
##Testing for SP
phtest(FE_SP,RE_SP)

pbgtest(FE_SP)
pander(testforsp)




###Estimation for CE
Dynamic_CE = pgmm(thom~lag(thom,1) + lag(pib_percapita,0:1) + lag(mortes_drogas100m,0:1) + lag(I(mortes_drogas100m^2),0:1)|
                    lag(thom,2:99), data = final_dfce, effect="twoways",model="twosteps")

FE_CE = plm(thom ~ pib_percapita + mortes_drogas100m+ I(mortes_drogas100m^2), data = final_dfce, index = c("periodo","cod"), model = "within")

Pool_CE = plm(thom ~pib_percapita + mortes_drogas100m+ I(mortes_drogas100m^2), data = final_dfce, index = c("periodo","cod"), model = "pooling")

RE_CE = plm(thom ~ pib_percapita + mortes_drogas100m+ I(mortes_drogas100m^2), data = final_dfce, index = c("periodo","cod"), model = "random")

FE_CE_Int1 = plm(thom ~ pib_percapita +mortes_drogas100m  + I(mortes_drogas100m^2)+ I(mortes_drogas100m^2*final_dfce_2002) 
                 + I(mortes_drogas100m^2*final_dfce_2003) + I(mortes_drogas100m^2*final_dfce_2004) + I(mortes_drogas100m^2*final_dfce_2005)
                 + I(mortes_drogas100m^2*final_dfce_2006)+ I(mortes_drogas100m^2*final_dfce_2007)
                 + I(mortes_drogas100m^2*final_dfce_2008) + I(mortes_drogas100m^2*final_dfce_2009) + 
                   I(mortes_drogas100m^2*final_dfce_2010) + I(mortes_drogas100m^2*final_dfce_2011)
                 + I(mortes_drogas100m*final_dfce_2012) + I(mortes_drogas100m^2*final_dfce_2013)
                 + I(mortes_drogas100m^2*final_dfce_2014) + I(mortes_drogas100m^2*final_dfce_2015)
                 + I(mortes_drogas100m^2*final_dfce_2016),data = final_dfce,index = c("periodo","cod"),
                 model = "within")

summary(FE_CE_Int1)

#Latex Tables:  
stargazer(Dynamic_SP,Dynamic_CE)
stargazer(Pool_SP,RE_SP,FE_SP)
stargazer(FE_SP_Int,FE_SP_Int1)
stargazer(Pool_CE,RE_CE,FE_CE)
stargazer(coeftest(RE_SP, vcovHC(RE_SP, method = "arellano")),coeftest(FE_SP, vcovHC(FE_SP, method = "arellano")), title = "Modelos robustos -São Paulo ", column.labels = c("EA", "EF"), align = TRUE)
stargazer(coeftest(RE_CE, vcovHC(RE_CE, method = "arellano")),coeftest(FE_CE, vcovHC(FE_CE, method = "arellano")), title = "Modelos robustos - Ceará", column.labels = c("EA", "EF"), align = TRUE)


#Creating the Graphic plot 
final_dfce1 = final_dfce %>% select(mortes_droga, populacao)
final_dfce2 = aggregate(final_dfce1,by=list(final_dfce$periodo), FUN="sum")
final_dfce2 %>% View()

saveRDS(final_dfce2,)

final_dfsp1 = final_dfsp %>% select(mortes_droga, populacao)
final_dfsp2 = aggregate(final_dfsp1,by=list(final_dfsp$periodo), FUN ="sum",na.rm=TRUE)
final_dfsp2 %>%View()


par(mfrow=c(1,1))
plot(x = (final_dfsp2$Group.1),
     y = (final_dfsp2$mortes_droga/final_dfsp2$populacao)*100000,
     type = "l",
     col = "blue",     
     main = "São Paulo"
     ,xlab = "Ano",ylab = "Mortes por Droga 100 mil habitantes")

plot(x = final_dfce2$Group.1,
     y =(final_dfce2$mortes_droga/final_dfce2$populacao)*100000 ,
     type = "l",
     main = "Ceará",
     col = "blue",
     xlab="Ano", ylab = "Mortes por Droga 100 mil habitantes")







saveRDS(final_dfsp2, file = "d_finalsp2.rds")
saveRDS(final_dfce2,file = "d_finalce2.rds")





#Creating the Graphics


library(ggplot2)

spce = read.csv("SP&CE.csv", dec = ",")%>%
  melt("X")%>%
  rename(periodo = variable,thom = value)

spce = spce[order(spce$X),]
spce$periodo = as.numeric(str_replace(spce$periodo,"X",""))

names(spce)

par(mfrow=c(1,1))
plot(x = spce[spce$X == "CE",]$periodo,
     y = spce[spce$X == "CE",]$thom,
     type = "l",
     col = "blue",     
     main = "Ceará"
     ,xlab = "Ano",ylab = "Taxa de Homicídio por 100 mil habitantes")
plot(x = spce[spce$X == "SP",]$periodo,
     y = spce[spce$X == "SP",]$thom,
     type = "l",
     main = "São Paulo",
     col = "blue",
     xlab="Ano", ylab = "Taxa de Homicídio por 100 mil habitantes")



par(mfrow=c(1,1))

ggplot(final_dfce,aes(x=periodo,y=thom,group=1)) +
  ggtitle("Taxa de homicídio por 100 mil habitantes - Ceará") +
  geom_line(colour = 'red', size = 1.5) +
  xlab("") +
  ylab("Taxa de homicídio")

ggplot(taxa_homicidios_sp,aes(x=ano,y=taxa_homicidio,group=1)) +
  ggtitle("Taxa de homicídio por 100 mil habitantes - São Paulo") +
  geom_line(colour = 'red', size = 1.5) +
  xlab("") +
  ylab("Taxa de homicídio") +
  geom_vline(xintercept = 2006,linetype="dashed")


















#options(scipen=999)(removing scientific notation)

#Estimating GMM Arellano Bond

GMM_SP = pgmm(thom ~ lag(thom,1) + lag(thom,2) + lag(pib_percapita, 0:1) +
              lag(I(mortes_drogas100m^2),0:1) + lag(mortes_drogas100m,0:1) |lag(thom,2:99), data = final_dfsp,effect = "twoways", model = "twosteps")

summary(GMM_SP)

z1 <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
           + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99),
           data = EmplUK, effect = "twoways", model = "twosteps")
summary(z1, robust = FALSE)
#Testing Hausman

phtest(FE_CE,RE_CE)



coeftest(FE_SP, vcovHC(FE_SP, method = "arellano"))





#######################################################################
#Merging Homcide Rate of SP and CE
#final_df$periodo = as.numeric(final_df$periodo)
#final_df = merge(final_df,thomsp,by = c("cod","periodo"),all.x=TRUE) 
#final_df = merge(final_df,thomce,by=c("cod","periodo"),all.x=TRUE)


#for (i in 1:length(final_df$nome.x)){
#  if (is.na(final_df$nome.x[i])){
#    final_df$nome.x[i] = final_df$nome.y[i]
#    }
#}

#for (i in 1:length(final_df$valor.x)){
#  if(is.na(final_df$valor.x[i])){
#    final_df$valor.x[i] = final_df$valor.y[i]
#  }
#}

#final_df = subset(final_df,select = 1:6)
#final_df = final_df %>% rename(nome = nome.x, thom = valor.x)
#final_df = final_df[,-5]

