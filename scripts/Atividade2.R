##############################################
#################Atividade 2###################
###############################################

#Iremos utilizar um banco de dados composto pelo índice de desenvolvimento
#humano municipal(IDHM) de Minas Gerais dos anos de 1990, 2000 e 2010 acompanhado
#do resultado do 1o turno das eleições para presidente no ano de 2014.

#################Dados IDHM eleiçao 1 turno Minas Gerais###################

#Carregando Pacotes

library(readxl)

#Importação do Conjunto de Dados

idhmMinasGerais=read_excel("C:\\Users\\alefr\\Desktop\\MESTRADO Alef\\Análise de Dados\\Métodos Quantitativos com Auxilio de Software\\ModuloMetodosQuantitativos\\dados\\Banco_IDHM_eleiçao_1_turnoMinasGerais.xlsx",col_names=T)

###############Estatística Descritivas##############################

############Medidas de Posição#################

#####Média########

mean(idhmMinasGerais$IDHM_1991, na.rm = TRUE) #IDHM médio de Minas Gerais no ano de 1991
mean(idhmMinasGerais$IDHM_2000, na.rm = TRUE) #IDHM médio de Minas Gerais no ano de 2000
mean(idhmMinasGerais$IDHM_2010, na.rm = TRUE) #IDHM médio de Minas Gerais no ano de 2010

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

########Maximo e Mínimos##########

max(idhmMinasGerais$IDHM_1991,na.rm = T) #Maior IDHM de Minas Gerais em 1991
max(idhmMinasGerais$IDHM_2000,na.rm = T) #Maior IDHM de Minas Gerais em 2000
max(idhmMinasGerais$IDHM_2010,na.rm = T) #Maior IDHM de Minas Gerais em 2010

min(idhmMinasGerais$IDHM_1991,na.rm = T) #Menor IDHM de Minas Gerais em 1991
min(idhmMinasGerais$IDHM_2000,na.rm = T) #Menor IDHM de Minas Gerais em 2000
min(idhmMinasGerais$IDHM_2010,na.rm = T) #Menor IDHM de Minas Gerais em 2010

idhmMinasGerais$Municipios[idhmMinasGerais$IDHM_1991==min(idhmMinasGerais$IDHM_1991,na.rm = T)] #Comando para Retornar o nome da Cidade com Menor IDHM de Minas em 1991

##############Sumário dos dados################

summary(idhmMinasGerais$IDHM_1991,na.rm = T) #Medidas de posição para o IDHM de Minas Gerais em 1991
summary(idhmMinasGerais$IDHM_2000,na.rm = T) #Medidas de posição para o IDHM de Minas Gerais em 2000
summary(idhmMinasGerais$IDHM_2010,na.rm = T) #Medidas de posição para o IDHM de Minas Gerais em 2010


############Medidas de Dispersão#################

#####Variância e Desvio Padrão########

var(idhmMinasGerais$IDHM_1991, na.rm = T) #Variância do IDHM MG em 1991
var(idhmMinasGerais$IDHM_2000, na.rm = T) #Variância do IDHM MG em 2000
var(idhmMinasGerais$IDHM_2010, na.rm = T) #Variância do IDHM MG em 2010

sd(idhmMinasGerais$IDHM_1991, na.rm = T) #Desvio padrão do IDHM MG em 1991
sd(idhmMinasGerais$IDHM_2000, na.rm = T) #Desvio padrão do IDHM MG em 2000
sd(idhmMinasGerais$IDHM_2010, na.rm = T) #Desvio padrão do IDHM MG em 2010

###########Amplitude Total###############

AmplIDHMG1991=max(idhmMinasGerais$IDHM_1991, na.rm = T) - min(idhmMinasGerais$IDHM_1991, na.rm = T) #Amplitude do IDHM de MG no ano de 1991
AmplIDHMG2000=max(idhmMinasGerais$IDHM_2000, na.rm = T) - min(idhmMinasGerais$IDHM_2000, na.rm = T) #Amplitude do IDHM de MG no ano de 2000
AmplIDHMG2010=max(idhmMinasGerais$IDHM_2010, na.rm = T) - min(idhmMinasGerais$IDHM_2010, na.rm = T) #Amplitude do IDHM de MG no ano de 2010

AmplIDHMG1991
AmplIDHMG2000
AmplIDHMG2010

#########Coeficiente de Variação#############

CVIDHMG1991=(sd(idhmMinasGerais$IDHM_1991, na.rm = T)/mean(idhmMinasGerais$IDHM_1991, na.rm = T))*100 #Coef. var. do IDHM de MG no ano de 1991
CVIDHMG2000=(sd(idhmMinasGerais$IDHM_2000, na.rm = T)/mean(idhmMinasGerais$IDHM_2000, na.rm = T))*100 #Coef. var. do IDHM de MG no ano de 2000
CVIDHMG2010=(sd(idhmMinasGerais$IDHM_2010, na.rm = T)/mean(idhmMinasGerais$IDHM_2010, na.rm = T))*100 #Coef. var. do IDHM de MG no ano de 2010

CVIDHMG1991
CVIDHMG2000
CVIDHMG2010

############Explorando variaveis Quantitativas#################

#########Calculo proporção de votos#############

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
  sum(idhmMinasGerais$`Zé Maria`))

#Esse vetor armazena o nome dos candidatos na mesma ordem em que ocorre a soma no vetor candidatos

nomesCandidatos=c("Aécio Neves","Dilma Rousseff","Eduardo Jorge","Eymael","Levy Fidelyx","Luciana Genro","Marina Silva","Mauro Iasi","Pastor Everaldo","Rui Costa Pimenta","Zé Maria")

posCand=character() #criei vetor que armazena candidatos conforme à ordem de sua posição
x=1 #crio uma variavel que funciona como contador 

#Laço for que me retornara os candidatos na ordem dos que tiveram mais votos para os que tiveram menos e os armazera em posCand por ordem de posição

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

###Calculo proporção de votos 3 Maiores colocados na eleição de 2014 em MG:Dilma Rousseff,Aécio Neves e Marina Silva

cand=c(
  sum(idhmMinasGerais$`Dilma Rousseff`)/sum(idhmMinasGerais$Total), #Proporção Dilma Rousseff
  sum(idhmMinasGerais$AecioNeves)/sum(idhmMinasGerais$Total), #Proporção Aécio Neves
  sum(idhmMinasGerais$`Marina Silva`)/sum(idhmMinasGerais$Total)) #Proporção Marina Silva

nomes=c("Dilma Rousseff","Aécio Neves","Marina")#Defino nomes dos 3 primeiros colocados num vetor
names(cand)=nomes #Insiro os nomes com base no vetor nomes
cand

#Criamos as variáveis com a proporção de votos válidos para os 3 canditados em Minas Gerais. O vetor nomes é utilizado para dar nome a cada elemento dos vetores. 

######Gráfico de Barras Proporção de Votos MG#######

barplot(cand, beside=TRUE,
        col =c("blue","yellow","green"),
        args.legend = list(x = 3.5, y = -0.1, bty = "n", ncol = 3),
        legend.text = nomes,
        main = "Proporção de votos válidos para os 3 primeiros colocados
em Minas Gerais")

#Classificando os municipios de Minas Gerais segundo a Região Metropolitana de Belo Horizonte

rm=rep(0,853)
for(i in 1:853){
  if(idhmMinasGerais$Municipio[i]=="BALDIM"|
     idhmMinasGerais$Municipio[i]=="BELO HORIZONTE"|
     idhmMinasGerais$Municipio[i]=="BETIM"|
     idhmMinasGerais$Municipio[i]=="BRUMADINHO"|
     idhmMinasGerais$Municipio[i]=="CAETÉ"|
     idhmMinasGerais$Municipio[i]=="CAPIM BRANCO"|
     idhmMinasGerais$Municipio[i]=="CONFINS"|
     idhmMinasGerais$Municipio[i]=="CONTAGEM"|
     idhmMinasGerais$Municipio[i]=="ESMERALDAS"|
     idhmMinasGerais$Municipio[i]=="FLORESTAL"|
     idhmMinasGerais$Municipio[i]=="IBIRITÉ"|
     idhmMinasGerais$Municipio[i]=="IGARAPÉ"|
     idhmMinasGerais$Municipio[i]=="ITAGUARA"| 
     idhmMinasGerais$Municipio[i]=="ITATIAIUÇU"|
     idhmMinasGerais$Municipio[i]=="JABOTICATUBAS"|
     idhmMinasGerais$Municipio[i]=="JUATUBA"|
     idhmMinasGerais$Municipio[i]=="LAGOA SANTA"|
     idhmMinasGerais$Municipio[i]=="MÁRIO CAMPOS"|
     idhmMinasGerais$Municipio[i]=="MATEUS LEME"|
     idhmMinasGerais$Municipio[i]=="MATOZINHOS"|
     idhmMinasGerais$Municipio[i]=="NOVA LIMA"|
     idhmMinasGerais$Municipio[i]=="NOVA UNIÃO"|
     idhmMinasGerais$Municipio[i]=="PEDRO LEOPOLDO"|
     idhmMinasGerais$Municipio[i]=="RAPOSOS"|
     idhmMinasGerais$Municipio[i]=="RIBEIRÃO DAS NEVES"|
     idhmMinasGerais$Municipio[i]=="RIO ACIMA"|
     idhmMinasGerais$Municipio[i]=="RIO MANSO"|
     idhmMinasGerais$Municipio[i]=="SABARÁ"|
     idhmMinasGerais$Municipio[i]=="SANTA LUZIA"|
     idhmMinasGerais$Municipio[i]=="SÃO JOAQUIM DE BICAS"|
     idhmMinasGerais$Municipio[i]=="SÃO JOSÉ DA LAPA"|
     idhmMinasGerais$Municipio[i]=="SARZEDO"|
     idhmMinasGerais$Municipio[i]=="TAQUARAÇU DE MINAS"|
     idhmMinasGerais$Municipio[i]=="VESPASIANO"){
    rm[i]= "1"
  } else{
    rm[i] ="0"
  }
}

#Incluindo a informação se o municipio pertence a região metropolitana de Belo Horizonte (rm=1), ou Não (rm=0)

idhmMinasGerais = cbind(idhmMinasGerais,rm)

#Proporção de votos região metropolitana de Belo Horizonte

cand.rm = c(sum(idhmMinasGerais$`Dilma Rousseff`[rm=="1"])/sum(idhmMinasGerais$Total[rm=="1"]),
            sum(idhmMinasGerais$AecioNeves[rm=="1"])/sum(idhmMinasGerais$Total[rm=="1"]), 
            sum(idhmMinasGerais$`Marina Silva`[rm=="1"])/sum(idhmMinasGerais$Total[rm=="1"]))

#Proporção de Votos no interior de Minas Gerais

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

######Gráfico de Barras Proporção de Votos Região Metropolitana de BH e Interior de MG#######

barplot(cbind(cand.rm,cand.int), beside = TRUE,
        names.arg = c("Região Metropolitana","Demais cidades"),
        col =c("blue","yellow","green"),
        args.legend = list(x = 8, y = -0.1, bty = "n", ncol = 3),
        legend.text = nomes,
        main = "Proporção de votos válidos para os 3 primeiros colocados
na região metropolitana de Belo Horizonte e demais cidades")