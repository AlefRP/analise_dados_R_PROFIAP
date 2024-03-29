############################################################################################
##########M�dulo:M�todos Quantitativos com Aux�lio de Software Estat�stico###########
######################Profa.Dra.Gislene Araujo Pereira######################################

####Estat�stica Descritiva######
####Representa��o Gr�fica#######


#carregando os pacotes

library(readxl)

#Importando Dados
idhm=read_excel("C:\\Users\\Usuario\\Dropbox\\PosGest�oPublica\\MaterialdoModulo-UtilizadoNasAulas\\Script\\Banco_IDHM_elei�ao_1_turno.xlsx",              col_names = TRUE )

###############Estat�stica Descritivas##############################

############Medidas de Posi��o#################

#####M�dia########
mean(idhm$IDHM_2010, na.rm = TRUE) #IDHM m�dio do Brasil no ano de 2010

mean(idhm$`Dilma Rousseff`) #M�dia de votos por cidade de Dilma

mean(idhm$`Levy Fidelyx`) #M�dia de votos por cidade de Levy

mean(idhm$AecioNeves[idhm$IDHM_2010>0.55],na.rm = TRUE) #M�dia de votos de A�cio nas cidades com idhm acima de 0.55
mean(idhm$AecioNeves[idhm$IDHM_2010<=0.55],na.rm = TRUE)

mean(idhm$`Dilma Rousseff`[idhm$IDHM_2010>0.55],na.rm = TRUE)
mean(idhm$`Dilma Rousseff`[idhm$IDHM_2010<=0.55],na.rm = TRUE)

####Mediana#####
median(idhm$IDHM_2010, na.rm = TRUE) #Mediana do IDHM do Brasil em 2010

median(idhm$`Dilma Rousseff`) #Mediana dos votos de Dilma

median(idhm$AecioNeves[idhm$IDHM_2010>0.55], na.rm = TRUE) #Mediana dos votos de A�cio nas cidades com idhm acima de 0.55


#########Quantil##################
quantile(idhm$IDHM_2010,c(0.25,0.5,0.75), na.rm = T) #Quartis

quantile(idhm$IDHM_2010,seq(0.1,1,0.1), na.rm = T) #Decis

########Maximo e M�nimos##########
max(idhm$IDHM_2010,na.rm = T)

min(idhm$IDHM_2010, na.rm = T)


##############Sum�rio dos dados################
summary(idhm$IDHM_2010,na.rm = T) #Medidas de posi��o para o IDHM do Brasil em 2010

#########################################MEDIDAS DE DISPERS�O############################
###########vARI�NCIA E DESVIO pADR�O##############

var(idhm$IDHM_2010, na.rm = T)

sd(idhm$IDHM_2010, na.rm = T) #Desvio padr�o do IDHM BR em 2010

sd(idhm$IDHM_2010, na.rm = T)==sqrt(var(idhm$IDHM_2010, na.rm = T))

sd(idhm$IDHM_2000,na.rm = T) #Desvio padr�o do IDHM BR em 2000

###########Amplitude Total###############
AmplIDH2010=max(idhm$IDHM_2010, na.rm = T) - min(idhm$IDHM_2010, na.rm = T) #Amplitude do IDHM do Brasil no ano de 2010
AmplIDH2010

AmplIDH2000=max(idhm$IDHM_2000, na.rm = T) - min(idhm$IDHM_2000, na.rm = T) #Amplitude do IDHM do Brasil no ano de 2000
AmplIDH2000

#########Coeficiente de Varia��o#############

CVIDH2010=(sd(idhm$IDHM_2010, na.rm = T)/mean(idhm$IDHM_2010, na.rm = T))*100 #Coef. var. do IDHM do Brasil no ano de 2010
CVIDH2010

CVIDH2000=(sd(idhm$IDHM_2000,na.rm = T)/mean(idhm$IDHM_2000,na.rm = T))*100   #Coef. var. do IDHM do Brasil no ano de 2000
CVIDH2000


#####################################GR�FICOS#####################################################

#vamos criar a nova vari�vel prop.votos para os tr^es primeiros colocados

prop.votos.total.dilma = sum(idhm$`Dilma Rousseff`)/sum(idhm$Total) #Propor��o de votos v�lidos para Dilma

prop.votos.total.aecio = sum(idhm$AecioNeves)/sum(idhm$Total) #Propor��o de votos v�lidospara A�cio

prop.votos.total.marina = sum(idhm$`Marina Silva`)/sum(idhm$Total) #Propor��o de votos v�lidos para Marina

##grafico simples
barplot(c(prop.votos.total.aecio,prop.votos.total.dilma,prop.votos.total.marina))

#Note que o gr�fico ficou  muito feio e sem informa��o nenhuma. Contudo, podemos melhorar
#basta utilizar alguns argumentos na fun��o barplot:

#Grafico simples na horizontal
barplot(c(prop.votos.total.aecio,prop.votos.total.dilma,prop.votos.total.marina),horiz = TRUE)

#Se o argumento horiz = TRUE as barras do gr�fico ficam na horizontal, caso contr�ario,
#as barras ficam na vertical. Por pad�ao horiz = FALSE.

#Grafico simples na vertical com t�tulo
barplot(c(prop.votos.total.aecio,prop.votos.total.dilma,prop.votos.total.marina),
       main = "Propor��o de votos v�lidos dos 3 primeiros colocados
        nas elei��e para Presidente do Brasil o no ano de 2014")

#O argumento main cria um t�tulo para o gr�fico e deve ser sempre escrito entre aspas.

#Grafico na vertical, com titulo e cores para cada candidato
barplot(c(prop.votos.total.aecio,prop.votos.total.dilma,prop.votos.total.marina),
         main = "Propor��o de votos v�lidos dos 3 primeiros colocados
        nas elei��e para Presidente do Brasil o no ano de 2014",
        col = c("blue","red","green"))


#O argumento col simboliza as cores para os respectivos elementos do vetor dado como
#argumento, respeitando a ordem de cada elemento. Neste caso temos: azul, vermelho e
#verde representando respectivamente os candidatos A�cio, Dilma e Marina.


#Grafico na vertical, com titulo e cores para cada candidato e legenda para cada cor.

barplot(c(prop.votos.total.aecio,prop.votos.total.dilma,prop.votos.total.marina),
         main = "Propor��o de votos v�lidos dos 3 primeiros colocados
        nas elei��e para Presidente do Brasil o no ano de 2014",ylim=c(0,1),
        col = c("blue","red","green"),
        legend.text = c("Aecio Neves","Dilma Roussef","Marina Silva"))

#O argumento legend.text cria uma caixa de legendas utilizando um vetor de nomes, onde
#cada elemento representa o nome de um elemento do vetor utilizado para criar as barras.


#Grafico na vertical, com titulo e cores para cada candidato e define a posi��o da legenda.

barplot(c(prop.votos.total.aecio,prop.votos.total.dilma,prop.votos.total.marina),
         main = "Propor��o de votos v�lidos dos 3 primeiros colocados
        nas elei��e para Presidente do Brasil o no ano de 2014",ylim=c(0,1),
        col = c("blue","red","green"),args.legend= list(y=1,x=5, bty="n",cex=2/3),
        legend.text = c("A�cio Neves","Dilma Roussef","Marina Silva"))


#"topright" Legenda fica no topo a direita
#"topleft"  Legenda fica no topo a esquerda
#"bottomright" Legenda fica embaixo a direita
#"bottomleft" Legenda fica embaixo a esquerda




idhmPernambuco=read_excel("C:\\Users\\Usuario\\Dropbox\\PosGest�oPublica\\MaterialdoModulo-UtilizadoNasAulas\\Script\\Banco_IDHM_elei�ao_1_turnoPernambuco.xlsx",col_names = TRUE )

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

#Incluindo a informa��o se o municipio pertence a regi�o metropolitana de Recife (RMR=1), ou N�o (RMR=0)
idhmPernambuco = cbind(idhmPernambuco,rm) 

#Agora que j� temos a vari�vel que indica se uma cidade pertence ou n�o a RMR podemos
#calcular a propor��oo de votos v�lidos para os candidatos em cada parte do estado.

#Propor��o de votos regi�o metropolitana de Recife
cand.rm = c(sum(idhmPernambuco$AecioNeves[rm=="1"])/sum(idhmPernambuco$Total[rm=="1"]), 
            sum(idhmPernambuco$`Dilma Rousseff`[rm=="1"])/sum(idhmPernambuco$Total[rm=="1"]),
            sum(idhmPernambuco$`Marina Silva`[rm=="1"])/sum(idhmPernambuco$Total[rm=="1"]))

#Propor��o de Votos interior Pernambuco
cand.int = c(sum(idhmPernambuco$AecioNeves[rm=="0"])/sum(idhmPernambuco$Total[rm=="0"]),
             sum(idhmPernambuco$`Dilma Rousseff`[rm=="0"])/sum(idhmPernambuco$Total[rm=="0"]),
             sum(idhmPernambuco$`Marina Silva`[rm=="0"])/sum(idhmPernambuco$Total[rm=="0"]))
nomes = c("A�cio Neves","Dilma Rousseff", "Marina Silva")

names(cand.rm) = nomes
names(cand.int) = nomes


#Criamos as vari�veis com a propor��o de votos v�lidos para os 3 canditados em cada
#regi�o. O vetor nomes � utilizado para dar nome a cada elemento dos vetores. 
#Para dar os nomes utilizamos a fun��o names().

barplot(cbind(cand.rm,cand.int), beside = TRUE,
        names.arg = c("Regi�o Metropolitana","Demais cidades"),
        col =c("blue","red","green"),
        args.legend = list(x = 7, y = -0.1, bty = "n", ncol = 3),
        legend.text = nomes,
        main = "Propor��o de votos v�lidos para os 3 primeiros colocados
na regi�o metropolitana de Recife e demais cidades")

#Os argumento utilizados:
#beside: Indica se as categorias ser�o postas lado a lado. S� funciona caso o conjunto de
#       dados utilizado como argumento seja uma matriz.
#names.arg: Fornece o nome dos subconjuntos ou categorias que ficar�o por baixo das barras.
#ncol: Informa o n�mero de colunas da caixa de legendas. S� pode ser utilizado dentro do argumento args.legend.

################Gr�fico de Pizza############
#Iremos fazer um gr�fico de setores para a popula��o total do estado de Pernambuco
#no ano de 2010 categorizado entre RMR e as demais cidades do estado.

pie(c(sum(idhmPernambuco$Popula�ao_2010[rm=="1"]),
      sum(idhmPernambuco$Popula�ao_2010[rm=="0"])),
    labels = c("Regi�o metropolitana","Demais cidades"),
    main = "Popula��o total do estado de Pernambuco em 2010.",
    col = c("darkblue","lightblue"))



#Faremos o mesmo gr�fico anterior, mas agora mostraremos a % e criaremos uma
#legenda para cada setor. Para isso temos que criar um objeto com as respectivas porcentagens.


pop.per = c(sum(idhmPernambuco$Popula�ao_2010[rm=="1"])/sum(idhmPernambuco$Popula�ao_2010),
            sum(idhmPernambuco$Popula�ao_2010[rm=="0"])/sum(idhmPernambuco$Popula�ao_2010))
pop.per = round(pop.per*100,2)

pie(c(sum(idhmPernambuco$Popula�ao_2010[rm=="1"]),
      sum(idhmPernambuco$Popula�ao_2010[rm=="0"])),labels = pop.per,
    main = "Popula��o total do estado de Pernambuco em 2010.",
    col = c("darkblue","lightblue"))
legend("bottomleft",c("Regi�o metropolitana", "Demais cidades"),
       fill = c("darkblue","lightblue"), cex = 2/3,bty = "n")


#Note que agora tivemos que fazer a legenda fora da fun��o que faz o gr�fico.
#Isso se deve pelo fato de que na fun��o pie n�o temos a op��o de legenda. 
#A sintaxe da fun��o para criar legendas �:  legend(x, y, legenda, fill, bty, cex)

#x: Indica a coordenada do eixo x.
#y: Indica a coordenada do eixo y.
#legenda: � o vetor com o nome dos setores.
#ll: Objeto que ir� preencher as caixas das legendas com as cores indicadas.
#bty: Tipo de borda da caixa da legenda. Se bty = "n"a caixa � formada sem as bordas.
#cex: Tamanho da caixa da legenda dada em propor��o.

#Observa��o: Os argumentos x e y podem ser omitidos utilizando como argumento posi��e pr�-definidas
#tais como: "topleft", "topright", "center", "left"etc.


#Faremos o mesmo gr�fico mas agora em 3D. Para tal teremos que utilizar a fun��o pie3D do pacote plotrix. 
#A sintaxe da fun�aoo pie3D � muito parecida com a pie, tendo como novo somente o argumento explode
#que indica o quanto as "fatias"estar�o separadas.

library("plotrix")
pie3D(c(sum(idhmPernambuco$Popula�ao_2010[rm=="1"]),
        sum(idhmPernambuco$Popula�ao_2010[rm=="0"])),
      labels = c("Regi�o metropolitana","Demais cidades"),
      main = "Popula��o total do estado de Pernambuco em 2010.",
      col = c("darkblue","lightblue"), explode = 0.1)



###################### boxplot################
#Vamos an�lisar o �ndice de desenvolvimento humano municipal das cidades do estado de
#Pernambuco dos tR�S anos em que foram calculados.

boxplot(idhmPernambuco$IDHM_1991,idhmPernambuco$IDHM_2000,idhmPernambuco$IDHM_2010,
        names=c("1991","2000","2010"),
        main = "IDHM do estado de PE nos anos de 1991, 2000 e 2010")


#Vamos an�lisar o �ndice de desenvolvimento humano municipal das cidades do estado de
#Pernambuco categorizando pelos que pertencem ou n�o a regi�o metropolitana de Recife.

boxplot(idhmPernambuco$IDHM_2010[rm=="1"],idhmPernambuco$IDHM_2010[rm=="0"],
        names = c("Regi�o Metropolitana", "Demais cidades"),
        main = "IDHM do estado de PE em 2010")


#Note que no segundo boxplot existem alguns outliers.
#Para identific�-los vamos utilizar a seguinte fun��o: boxplot.stats(objeto) 
boxplot.stats(idhmPernambuco$IDHM_2010[rm=="0"])

# onde $out nos indica os outliers.

##################Diagrama de dispers�o e gr�fico de linhas##################

#Para os exemplos utilizaremos os seguintes dados:

escolaridade = c(8,5,6,2,4,3,8,6,7)
renda = c(8120,3666,4020,950,1100,1850,7525,3755,6100)
idade = c(50,32,34,18,22,23,42,28,38)
genero = c("F","M","M","F","F","M","M","M","F","F")
setor = c("privado","privado","publico","privado",
          "privado","privado","publico","publico","privado")


#Vamos observar como se comporta a renda em fun��o da idade.

plot(idade,renda)


#Vamos fazer o mesmo gr�fico anterior mas agora diferenciado entre os g�neros. 
#Paraisso precisaremos plotar dois gr�ficos, sendo que para adicionar pontos em um gr�fico j�
#existente utilizaremos a fun��o points que possui os mesmos argumentos da fun��o plot.

plot(idade[genero=="F"],renda[genero=="F"], col = "red", lwd = 2,xlab="Idade",ylab="Renda")
points(idade[genero=="M"],renda[genero=="M"], col = "blue", lwd = 2)


#Vamos averiguar se nesta amostra a escolaridade aumenta de acordo com a idade de cada individuo.
#Iremos utilizar o mesmo racioc�nio do exemplo anterior.
plot(idade[genero=="F"],escolaridade[genero=="F"], col = "red", lwd = 3,xlab="Idade",ylab="Escolaridade")
points(idade[genero=="M"],escolaridade[genero=="M"], col = "blue", lwd = 3)


#Agora, averiguaremos como se comporta a escoralidade em fun��o da idade sendo que
#agora obsevaremos o setor em que cada individuo trabalha.

plot(idade[setor=="publico"],escolaridade[setor=="publico"],
     col = "darkgreen", lwd = 3,xlab="Idade",ylab="Escolaridade")
points(idade[setor=="privado"],escolaridade[setor=="privado"],
       col = "gold", lwd = 3)

#Note que s� foram inseridos 4 pontos no gr�fico, isso ocorreu devido o intervalo utilizado para fazer o plot ser muito pequeno, 
#fazendo com que os pontos da fun��o points n�o aperececem no gr�fico inicial. Para resolver este problema temos que estipular
#limites para o eixo x e y. Para isso, faremos uma simples an�lise do sum�ario das vari�veis que compoem o eixo x e y afim de 
#identificar os valores de m�ximo e m�nimo, viabilizando a cria��o de um intervalo para que todas as informa��es sejam vis�veis.

summary(idade)
summary(escolaridade)

#Agora que j� sabemos o intervalo onde cada vari�vel se encontra podemos fazer o
#gr�fico proposto corretamente.

plot(idade[setor=="publico"],escolaridade[setor=="publico"],
     col = "darkgreen", lwd = 3,xlim = range(18:50),
     ylim = range(2:8),xlab="Idade",ylab="Escolaridade")
points(idade[setor=="privado"],escolaridade[setor=="privado"],
       col = "gold", lwd = 3)


############Gr�fico S�ries Temporais#############

#Faremos um gr�fico de s�rie temporal do PIB do Brasil e da �ndia. Para fazer um gr�fico t�pico de 
#s�rie temporal basta adicionar o argumento type e faze-lo receber "l":

pibbr = c(655400000000,559400000000,508000000000,558300000000,669300000000,
          891600000000,1108000000000,1397000000000,1696000000000,
          1667000000000,2209000000000,2616000000000,2465000000000,
          2473000000000,2456000000000,1804000000000,1796000000000)
pibin = c(462100000000,479000000000,508100000000,599600000000,699700000000,
          808900000000,920300000000,1201000000000,1187000000000,
          1324000000000,1657000000000,1823000000000,1828000000000,
          1857000000000,2035000000000,2112000000000,2264000000000)
ano = c(2000:2016)


summary(pibbr)
summary(pibin)

plot(ano,pibbr,type = "l",col = "darkgreen",
     main = "PIB do Brasil e �ndia nos anos de 2000 a 2016")
points(ano,pibin, type = "l", col = "gold")
legend("bottomright",c("Brasil","�ndia"),col= c("darkgreen","gold"),lty=(1:2),bty="n",cex=2/3)  
  
  
