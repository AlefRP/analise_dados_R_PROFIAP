#########################################################################
#############################Aula 02####################################
########################################################################

#Carregando Pacote

library(readxl)

#Importação do Conjunto de Dados

tabela1=read_excel("C:\\Users\\alefr\\Desktop\\MESTRADO Alef\\Análise de Dados\\Métodos Quantitativos com Auxilio de Software\\ModuloMetodosQuantitativos\\dados\\Tabela1_1.xlsx",col_names=TRUE)

################Análise Estatística Descritiva##########################

#Exploração de Variaveis Categoricas

#Usar a função table para explorar as variaveis Raça e Gênero

raca=table(tabela1$nonwhite) #Não Branco=1, Branco=0
raca
raca=sort(raca,decreasing=T)

#Gráfico de barras

barplot(raca, names.arg=c("Branco","Não Branco"))

#Gênero

genero=sort(table(tabela1$female),decreasing=T) #Feminino=1, Masculino=0
genero

barplot(genero,names.arg=c("Maculino","Feminino"),col=c("blue","pink"))

###Explorando Variaveis Quantitativas

summary(tabela1$wage)
summary(tabela1$education)
summary(tabela1$exper)

sd(tabela1$wage) #Desvio Padrão Salário

#Histograma e boxplot

hist(tabela1$wage,xlab="Salário",ylab="Frequência")

boxplot(tabela1$wage)


#Iremos utilizar um banco de dados composto pelo índice de desenvolvimento
#humano municipal(IDHM) do Brasil dos anos de 1990, 2000 e 2010 acompanhado
#do resultado do 1o turno das eleições para presidente no ano de 2014.

#IREMOS ANALISAR O ESTADO DE PERNANBUCO SEGUNDO A REGIÃO METROPOLITANA

idhmPernambuco=read_excel("C:\\Users\\alefr\\Desktop\\MESTRADO Alef\\Análise de Dados\\Métodos Quantitativos com Auxilio de Software\\ModuloMetodosQuantitativos\\dados\\Banco_IDHM_eleiçao_1_turnoPernambuco.xlsx",col_names=T)

#Região Metropolitana de Recife: OLINDA,PAULISTA,RECIFE,JABOATÃO DOS GUARARAPE,IGARASSU,
#ABREU E LIMA,CAMARAGIBE,CABO DE SANTO AGOSTINHO,IPOJUCA,SÃO LOURENÇO DA MATA
#ARAÇOIABA,ILHA DE ITAMARACÁ,ITAPISSUMA,MORENO

#Classificando os municipios de Pernambunco segundo a Região Metropolitana de Recife

rm=rep(0,185)
for(i in 1:185){
  if(idhmPernambuco$Municipio[i]=="OLINDA"|
     idhmPernambuco$Municipio[i]=="PAULISTA"|
     idhmPernambuco$Municipio[i]=="RECIFE"|
     idhmPernambuco$Municipio[i]=="JABOATÃO DOS GUARARAPES"|
     idhmPernambuco$Municipio[i]=="IGARASSU"|
     idhmPernambuco$Municipio[i]=="ABREU E LIMA"|
     idhmPernambuco$Municipio[i]=="CAMARAGIBE"|
     idhmPernambuco$Municipio[i]=="CABO DE SANTO AGOSTINHO"|
     idhmPernambuco$Municipio[i]=="IPOJUCA"|
     idhmPernambuco$Municipio[i]=="SÃO LOURENÇO DA MATA"|
     idhmPernambuco$Municipio[i]=="ARAÇOIABA"|
     idhmPernambuco$Municipio[i]=="ILHA DE ITAMARACÁ"| 
     idhmPernambuco$Municipio[i]=="ITAPISSUMA"|
     idhmPernambuco$Municipio[i]=="MORENO"){
    rm[i]= "1"
  } else{
    rm[i] ="0"
  }
}

rm

#Incluindo a informação se o municipio pertence ou não a região metropolitana de Recife

idhmPernambuco=cbind(idhmPernambuco,rm)

#Proporção de votos para os 3 primeiros colocados na eleição de 2014

#Região Metropolitana de recife

cand.rm=c(
  sum(idhmPernambuco$AecioNeves[rm=="1"]/sum(idhmPernambuco$Total[rm=="1"])),
  sum(idhmPernambuco$`Dilma Rousseff`[rm=="1"]/sum(idhmPernambuco$Total[rm=="1"])),
  sum(idhmPernambuco$`Marina Silva`[rm=="1"]/sum(idhmPernambuco$Total[rm=="1"])))

cand.rm

#Região Interior de recife

cand.int=c(
  sum(idhmPernambuco$AecioNeves[rm=="0"]/sum(idhmPernambuco$Total[rm=="0"])),
  sum(idhmPernambuco$`Dilma Rousseff`[rm=="0"]/sum(idhmPernambuco$Total[rm=="0"])),
  sum(idhmPernambuco$`Marina Silva`[rm=="0"]/sum(idhmPernambuco$Total[rm=="0"])))

cand.int

#Defino Nomes para os objetos que criei

nomes=c("Aécio Neves","Dilma Rousseff","Marina Silva")
names(cand.rm)=nomes
names(cand.int)=nomes

cand.rm
cand.int

#########Representação Gráfica###############

barplot(cbind(cand.rm,cand.int),
        beside=TRUE,
        names.arg=c("Região Metropolitana","Demais Cidades"),
        col=c("blue","red","green"),
        args.legend=list(x=7,y=-0.1,bty="n",ncol=3),
        legend.text=nomes,
        main="Proporção de votos válidos para os 3 primeiros colocados na região metropolitana de recife e demais cidades")
