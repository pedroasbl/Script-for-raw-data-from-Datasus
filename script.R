#Pacotes necessários para execução do script#
setwd("D:/Material - Mestrado FEA-RP/Disciplinas/2º Semestre/Econometria II/Desafio/database - sus")


#library(devtools)
#install_github("rpradosiqueira/datasus")


#Extraindo dados de mortalidade por UF:

#library(datasus)


 #x = sim_obt10_mun(linha = "Município",conteudo = 1,
 #                  coluna = "Capítulo CID-10"
#                   ,periodo=c(1997:2017))
library('dplyr')
library('read.dbc')
doac97 = read.dbc("DOAC1997.dbc")
doac98 = read.dbc("DOAC1998.dbc") 
dosp04 = read.dbc("DOSP2004.dbc")

#preparing loop
myfiles1 = list.files(pattern = "*.dbc")
myfiles2 = list.files(pattern = "*.DBC")


#constructing lists to read the data 
lista1 = list()

for (i in myfiles1){
  state = substr(i,1,4)
  for(j in myfiles1){
    if(substr(j,1,4)==state){
      lista1[[state]][[j]] = j
    }
  }
}

lista2 = list()

for (i in myfiles2){
  state = substr(i,1,4)
  for(j in myfiles2){
    if(substr(j,1,4)==state){
      lista2[[state]][[j]] = j
    }
  }
}




