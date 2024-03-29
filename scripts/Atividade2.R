##############################################
#################Atividade 2###################
###############################################

#Iremos utilizar um banco de dados composto pelo �ndice de desenvolvimento
#humano municipal(IDHM) de Minas Gerais dos anos de 1990, 2000 e 2010 acompanhado
#do resultado do 1o turno das elei��es para presidente no ano de 2014.

#################Dados IDHM elei�ao 1 turno Minas Gerais###################

#Carregando Pacotes

library(readxl)

#Importa��o do Conjunto de Dados

idhmMinasGerais=read_excel("C:\\Users\\alefr\\Desktop\\MESTRADO Alef\\An�lise de Dados\\M�todos Quantitativos com Auxilio de Software\\ModuloMetodosQuantitativos\\dados\\Banco_IDHM_elei�ao_1_turnoMinasGerais.xlsx",col_names=T)

###############Estat�stica Descritivas##############################

############Medidas de Posi��o#################

#####M�dia########

mean(idhmMinasGerais$IDHM_1991, na.rm = TRUE) #IDHM m�dio de Minas Gerais no ano de 1991
mean(idhmMinasGerais$IDHM_2000, na.rm = TRUE) #IDHM m�dio de Minas Gerais no ano de 2000
mean(idhmMinasGerais$IDHM_2010, na.rm = TRUE) #IDHM m�dio de Minas Gerais no ano de 2010

####Mediana#####

median(idhmMinasGerais$IDHM_1991, na.rm = TRUE) #Mediana do IDHM de Minas Geraisl em 1991
median(idhmMinasGerais$IDHM_2000, na.rm = TRUE) #Mediana do IDHM de Minas Gerais em 2000
median(idhmMinasGerais$IDHM_2010, na.rm = TRUE) #Mediana do IDHM de Minas Gerais em 2010

#########Quantil##################

quantile(idhmMinasGerais$IDHM_1991,c(0.25,0.5,0.75), na.rm = T) #Quartis do IDHM de Minas Gerais em 1991
quantile(idhmMinasGerais$IDHM_2000,c(0.25,0.5,0.75), na.rm = T) #Quartis do IDHM de Minas Gerais em 2000
quantile(idhmMinasGerais$IDHM_2010,c(0.25,0.5,0.75), na.rm = T) #Quartis do IDHM de Minas Gerais em 2010

quantile(idhmMinasGerais$IDHM_1991,seq(0.1,1,0.1), na.rm = T) #Decis do IDHM de Minas Gerais em 1991
quantile(idhmMinasGerais$IDHM_2000,seq(0.1,1,0.1), na.rm = T) #Decis do IDHM de Minas Gerais em 2000
quantile(idhmMinasGerais$IDHM_2010,seq(0.1,1,0.1), na.rm = T) #Decis do IDHM de Minas Gerais em 2010

########Maximo e M�nimos##########

max(idhmMinasGerais$IDHM_1991,na.rm = T) #Maior IDHM de Minas Gerais em 1991
max(idhmMinasGerais$IDHM_2000,na.rm = T) #Maior IDHM de Minas Gerais em 2000
max(idhmMinasGerais$IDHM_2010,na.rm = T) #Maior IDHM de Minas Gerais em 2010

min(idhmMinasGerais$IDHM_1991,na.rm = T) #Menor IDHM de Minas Gerais em 1991
min(idhmMinasGerais$IDHM_2000,na.rm = T) #Menor IDHM de Minas Gerais em 2000
min(idhmMinasGerais$IDHM_2010,na.rm = T) #Menor IDHM de Minas Gerais em 2010

idhmMinasGerais$Municipios[idhmMinasGerais$IDHM_1991==min(idhmMinasGerais$IDHM_1991,na.rm = T)] #Comando para Retornar o nome da Cidade com Menor IDHM de Minas em 1991

##############Sum�rio dos dados################

summary(idhmMinasGerais$IDHM_1991,na.rm = T) #Medidas de posi��o para o IDHM de Minas Gerais em 1991
summary(idhmMinasGerais$IDHM_2000,na.rm = T) #Medidas de posi��o para o IDHM de Minas Gerais em 2000
summary(idhmMinasGerais$IDHM_2010,na.rm = T) #Medidas de posi��o para o IDHM de Minas Gerais em 2010


############Medidas de Dispers�o#################

#####Vari�ncia e Desvio Padr�o########

var(idhmMinasGerais$IDHM_1991, na.rm = T) #Vari�ncia do IDHM MG em 1991
var(idhmMinasGerais$IDHM_2000, na.rm = T) #Vari�ncia do IDHM MG em 2000
var(idhmMinasGerais$IDHM_2010, na.rm = T) #Vari�ncia do IDHM MG em 2010

sd(idhmMinasGerais$IDHM_1991, na.rm = T) #Desvio padr�o do IDHM MG em 1991
sd(idhmMinasGerais$IDHM_2000, na.rm = T) #Desvio padr�o do IDHM MG em 2000
sd(idhmMinasGerais$IDHM_2010, na.rm = T) #Desvio padr�o do IDHM MG em 2010

###########Amplitude Total###############

AmplIDHMG1991=max(idhmMinasGerais$IDHM_1991, na.rm = T) - min(idhmMinasGerais$IDHM_1991, na.rm = T) #Amplitude do IDHM de MG no ano de 1991
AmplIDHMG2000=max(idhmMinasGerais$IDHM_2000, na.rm = T) - min(idhmMinasGerais$IDHM_2000, na.rm = T) #Amplitude do IDHM de MG no ano de 2000
AmplIDHMG2010=max(idhmMinasGerais$IDHM_2010, na.rm = T) - min(idhmMinasGerais$IDHM_2010, na.rm = T) #Amplitude do IDHM de MG no ano de 2010

AmplIDHMG1991
AmplIDHMG2000
AmplIDHMG2010

#########Coeficiente de Varia��o#############

CVIDHMG1991=(sd(idhmMinasGerais$IDHM_1991, na.rm = T)/mean(idhmMinasGerais$IDHM_1991, na.rm = T))*100 #Coef. var. do IDHM de MG no ano de 1991
CVIDHMG2000=(sd(idhmMinasGerais$IDHM_2000, na.rm = T)/mean(idhmMinasGerais$IDHM_2000, na.rm = T))*100 #Coef. var. do IDHM de MG no ano de 2000
CVIDHMG2010=(sd(idhmMinasGerais$IDHM_2010, na.rm = T)/mean(idhmMinasGerais$IDHM_2010, na.rm = T))*100 #Coef. var. do IDHM de MG no ano de 2010

CVIDHMG1991
CVIDHMG2000
CVIDHMG2010

############Explorando variaveis Quantitativas#################

#########Calculo propor��o de votos#############

###Primeiro encontro os 3 primeiros candidatos com mais votos###

#Crio vetor que armazena a soma de votos de cada candidato

candidatos=c(
  sum(idhmMinasGerais$AecioNeves),
  sum(idhmMinasGerais$`Dilma Rousseff`),
  sum(idhmMinasGerais$`Eduardo Jorge`),
  sum(idhmMinasGerais$Eymael),
  sum(idhmMinasGerais$`Levy Fidelyx`),
  sum(idhmMinasGerais$`Luciana Genro`),
  sum(idhmMinasGerais$`Marina Silva`),
  sum(idhmMinasGerais$`Mauro Iasi`),
  sum(idhmMinasGerais$`Pastor Everaldo`),
  sum(idhmMinasGerais$`Rui Costa Pimenta`),
  sum(idhmMinasGerais$`Z� Maria`))

#Esse vetor armazena o nome dos candidatos na mesma ordem em que ocorre a soma no vetor candidatos

nomesCandidatos=c("A�cio Neves","Dilma Rousseff","Eduardo Jorge","Eymael","Levy Fidelyx","Luciana Genro","Marina Silva","Mauro Iasi","Pastor Everaldo","Rui Costa Pimenta","Z� Maria")

posCand=character() #criei vetor que armazena candidatos conforme � ordem de sua posi��o
x=1 #crio uma variavel que funciona como contador 

#La�o for que me retornara os candidatos na ordem dos que tiveram mais votos para os que tiveram menos e os armazera em posCand por ordem de posi��o

for (i in 1:11) {
  for (i in 1:11) {
    if(candidatos[i]==max(candidatos)&&x<=11){
      posCand[x]=nomesCandidatos[i]
      candidatos[i]=0
      x=x+1
    }
  }
}

posCand[1] #Primeiro candidato com mais votos
posCand[2] #Segundo candidato com mais votos
posCand[3] #Terceiro candidato com mais votos

###Calculo propor��o de votos 3 Maiores colocados na elei��o de 2014 em MG:Dilma Rousseff,A�cio Neves e Marina Silva

cand=c(
  sum(idhmMinasGerais$`Dilma Rousseff`)/sum(idhmMinasGerais$Total), #Propor��o Dilma Rousseff
  sum(idhmMinasGerais$AecioNeves)/sum(idhmMinasGerais$Total), #Propor��o A�cio Neves
  sum(idhmMinasGerais$`Marina Silva`)/sum(idhmMinasGerais$Total)) #Propor��o Marina Silva

nomes=c("Dilma Rousseff","A�cio Neves","Marina")#Defino nomes dos 3 primeiros colocados num vetor
names(cand)=nomes #Insiro os nomes com base no vetor nomes
cand

#Criamos as vari�veis com a propor��o de votos v�lidos para os 3 canditados em Minas Gerais. O vetor nomes � utilizado para dar nome a cada elemento dos vetores. 

######Gr�fico de Barras Propor��o de Votos MG#######

barplot(cand, beside=TRUE,
        col =c("blue","yellow","green"),
        args.legend = list(x = 3.5, y = -0.1, bty = "n", ncol = 3),
        legend.text = nomes,
        main = "Propor��o de votos v�lidos para os 3 primeiros colocados
em Minas Gerais")

#Classificando os municipios de Minas Gerais segundo a Regi�o Metropolitana de Belo Horizonte

rm=rep(0,853)
for(i in 1:853){
  if(idhmMinasGerais$Municipio[i]=="BALDIM"|
     idhmMinasGerais$Municipio[i]=="BELO HORIZONTE"|
     idhmMinasGerais$Municipio[i]=="BETIM"|
     idhmMinasGerais$Municipio[i]=="BRUMADINHO"|
     idhmMinasGerais$Municipio[i]=="CAET�"|
     idhmMinasGerais$Municipio[i]=="CAPIM BRANCO"|
     idhmMinasGerais$Municipio[i]=="CONFINS"|
     idhmMinasGerais$Municipio[i]=="CONTAGEM"|
     idhmMinasGerais$Municipio[i]=="ESMERALDAS"|
     idhmMinasGerais$Municipio[i]=="FLORESTAL"|
     idhmMinasGerais$Municipio[i]=="IBIRIT�"|
     idhmMinasGerais$Municipio[i]=="IGARAP�"|
     idhmMinasGerais$Municipio[i]=="ITAGUARA"| 
     idhmMinasGerais$Municipio[i]=="ITATIAIU�U"|
     idhmMinasGerais$Municipio[i]=="JABOTICATUBAS"|
     idhmMinasGerais$Municipio[i]=="JUATUBA"|
     idhmMinasGerais$Municipio[i]=="LAGOA SANTA"|
     idhmMinasGerais$Municipio[i]=="M�RIO CAMPOS"|
     idhmMinasGerais$Municipio[i]=="MATEUS LEME"|
     idhmMinasGerais$Municipio[i]=="MATOZINHOS"|
     idhmMinasGerais$Municipio[i]=="NOVA LIMA"|
     idhmMinasGerais$Municipio[i]=="NOVA UNI�O"|
     idhmMinasGerais$Municipio[i]=="PEDRO LEOPOLDO"|
     idhmMinasGerais$Municipio[i]=="RAPOSOS"|
     idhmMinasGerais$Municipio[i]=="RIBEIR�O DAS NEVES"|
     idhmMinasGerais$Municipio[i]=="RIO ACIMA"|
     idhmMinasGerais$Municipio[i]=="RIO MANSO"|
     idhmMinasGerais$Municipio[i]=="SABAR�"|
     idhmMinasGerais$Municipio[i]=="SANTA LUZIA"|
     idhmMinasGerais$Municipio[i]=="S�O JOAQUIM DE BICAS"|
     idhmMinasGerais$Municipio[i]=="S�O JOS� DA LAPA"|
     idhmMinasGerais$Municipio[i]=="SARZEDO"|
     idhmMinasGerais$Municipio[i]=="TAQUARA�U DE MINAS"|
     idhmMinasGerais$Municipio[i]=="VESPASIANO"){
    rm[i]= "1"
  } else{
    rm[i] ="0"
  }
}

#Incluindo a informa��o se o municipio pertence a regi�o metropolitana de Belo Horizonte (rm=1), ou N�o (rm=0)

idhmMinasGerais = cbind(idhmMinasGerais,rm)

#Propor��o de votos regi�o metropolitana de Belo Horizonte

cand.rm = c(sum(idhmMinasGerais$`Dilma Rousseff`[rm=="1"])/sum(idhmMinasGerais$Total[rm=="1"]),
            sum(idhmMinasGerais$AecioNeves[rm=="1"])/sum(idhmMinasGerais$Total[rm=="1"]), 
            sum(idhmMinasGerais$`Marina Silva`[rm=="1"])/sum(idhmMinasGerais$Total[rm=="1"]))

#Propor��o de Votos no interior de Minas Gerais

cand.int = c(sum(idhmMinasGerais$`Dilma Rousseff`[rm=="0"])/sum(idhmMinasGerais$Total[rm=="0"]),
             sum(idhmMinasGerais$AecioNeves[rm=="0"])/sum(idhmMinasGerais$Total[rm=="0"]),
             sum(idhmMinasGerais$`Marina Silva`[rm=="0"])/sum(idhmMinasGerais$Total[rm=="0"]))
cand.rm
cand.int

#Defino os nomes com o Vetor nomes anteriormente utilizado

names(cand.rm) = nomes
names(cand.int) = nomes

cand.rm
cand.int

######Gr�fico de Barras Propor��o de Votos Regi�o Metropolitana de BH e Interior de MG#######

barplot(cbind(cand.rm,cand.int), beside = TRUE,
        names.arg = c("Regi�o Metropolitana","Demais cidades"),
        col =c("blue","yellow","green"),
        args.legend = list(x = 8, y = -0.1, bty = "n", ncol = 3),
        legend.text = nomes,
        main = "Propor��o de votos v�lidos para os 3 primeiros colocados
na regi�o metropolitana de Belo Horizonte e demais cidades")