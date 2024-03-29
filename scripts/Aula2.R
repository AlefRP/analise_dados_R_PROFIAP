#########################################################################
#############################Aula 02####################################
########################################################################

#Carregando Pacote

library(readxl)

#Importa��o do Conjunto de Dados

tabela1=read_excel("C:\\Users\\alefr\\Desktop\\MESTRADO Alef\\An�lise de Dados\\M�todos Quantitativos com Auxilio de Software\\ModuloMetodosQuantitativos\\dados\\Tabela1_1.xlsx",col_names=TRUE)

################An�lise Estat�stica Descritiva##########################

#Explora��o de Variaveis Categoricas

#Usar a fun��o table para explorar as variaveis Ra�a e G�nero

raca=table(tabela1$nonwhite) #N�o Branco=1, Branco=0
raca
raca=sort(raca,decreasing=T)

#Gr�fico de barras

barplot(raca, names.arg=c("Branco","N�o Branco"))

#G�nero

genero=sort(table(tabela1$female),decreasing=T) #Feminino=1, Masculino=0
genero

barplot(genero,names.arg=c("Maculino","Feminino"),col=c("blue","pink"))

###Explorando Variaveis Quantitativas

summary(tabela1$wage)
summary(tabela1$education)
summary(tabela1$exper)

sd(tabela1$wage) #Desvio Padr�o Sal�rio

#Histograma e boxplot

hist(tabela1$wage,xlab="Sal�rio",ylab="Frequ�ncia")

boxplot(tabela1$wage)


#Iremos utilizar um banco de dados composto pelo �ndice de desenvolvimento
#humano municipal(IDHM) do Brasil dos anos de 1990, 2000 e 2010 acompanhado
#do resultado do 1o turno das elei��es para presidente no ano de 2014.

#IREMOS ANALISAR O ESTADO DE PERNANBUCO SEGUNDO A REGI�O METROPOLITANA

idhmPernambuco=read_excel("C:\\Users\\alefr\\Desktop\\MESTRADO Alef\\An�lise de Dados\\M�todos Quantitativos com Auxilio de Software\\ModuloMetodosQuantitativos\\dados\\Banco_IDHM_elei�ao_1_turnoPernambuco.xlsx",col_names=T)

#Regi�o Metropolitana de Recife: OLINDA,PAULISTA,RECIFE,JABOAT�O DOS GUARARAPE,IGARASSU,
#ABREU E LIMA,CAMARAGIBE,CABO DE SANTO AGOSTINHO,IPOJUCA,S�O LOUREN�O DA MATA
#ARA�OIABA,ILHA DE ITAMARAC�,ITAPISSUMA,MORENO

#Classificando os municipios de Pernambunco segundo a Regi�o Metropolitana de Recife

rm=rep(0,185)
for(i in 1:185){
  if(idhmPernambuco$Municipio[i]=="OLINDA"|
     idhmPernambuco$Municipio[i]=="PAULISTA"|
     idhmPernambuco$Municipio[i]=="RECIFE"|
     idhmPernambuco$Municipio[i]=="JABOAT�O DOS GUARARAPES"|
     idhmPernambuco$Municipio[i]=="IGARASSU"|
     idhmPernambuco$Municipio[i]=="ABREU E LIMA"|
     idhmPernambuco$Municipio[i]=="CAMARAGIBE"|
     idhmPernambuco$Municipio[i]=="CABO DE SANTO AGOSTINHO"|
     idhmPernambuco$Municipio[i]=="IPOJUCA"|
     idhmPernambuco$Municipio[i]=="S�O LOUREN�O DA MATA"|
     idhmPernambuco$Municipio[i]=="ARA�OIABA"|
     idhmPernambuco$Municipio[i]=="ILHA DE ITAMARAC�"| 
     idhmPernambuco$Municipio[i]=="ITAPISSUMA"|
     idhmPernambuco$Municipio[i]=="MORENO"){
    rm[i]= "1"
  } else{
    rm[i] ="0"
  }
}

rm

#Incluindo a informa��o se o municipio pertence ou n�o a regi�o metropolitana de Recife

idhmPernambuco=cbind(idhmPernambuco,rm)

#Propor��o de votos para os 3 primeiros colocados na elei��o de 2014

#Regi�o Metropolitana de recife

cand.rm=c(
  sum(idhmPernambuco$AecioNeves[rm=="1"]/sum(idhmPernambuco$Total[rm=="1"])),
  sum(idhmPernambuco$`Dilma Rousseff`[rm=="1"]/sum(idhmPernambuco$Total[rm=="1"])),
  sum(idhmPernambuco$`Marina Silva`[rm=="1"]/sum(idhmPernambuco$Total[rm=="1"])))

cand.rm

#Regi�o Interior de recife

cand.int=c(
  sum(idhmPernambuco$AecioNeves[rm=="0"]/sum(idhmPernambuco$Total[rm=="0"])),
  sum(idhmPernambuco$`Dilma Rousseff`[rm=="0"]/sum(idhmPernambuco$Total[rm=="0"])),
  sum(idhmPernambuco$`Marina Silva`[rm=="0"]/sum(idhmPernambuco$Total[rm=="0"])))

cand.int

#Defino Nomes para os objetos que criei

nomes=c("A�cio Neves","Dilma Rousseff","Marina Silva")
names(cand.rm)=nomes
names(cand.int)=nomes

cand.rm
cand.int

#########Representa��o Gr�fica###############

barplot(cbind(cand.rm,cand.int),
        beside=TRUE,
        names.arg=c("Regi�o Metropolitana","Demais Cidades"),
        col=c("blue","red","green"),
        args.legend=list(x=7,y=-0.1,bty="n",ncol=3),
        legend.text=nomes,
        main="Propor��o de votos v�lidos para os 3 primeiros colocados na regi�o metropolitana de recife e demais cidades")
