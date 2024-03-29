##############################################
#################Atividade 3###################
###############################################

#Carregando Pacotes

library(MASS) #Pacote para An�lise de Regress�o (Modelagem Linear)
library(lmtest) #Pacote de Testes
library(car) #Pacote para realizar teste de multicolinearidade
library(forecast)#Pacote para utilizar fun��o Box.Cox

##############1-Dados Teor de Proteina e Taxa de Fertilidade#################

#Pa�s
Pais=c(
  "Formosa",
  "Malaia",
  "�ndia",
  "Jap�o",
  "Lugosl�via",
  "Gr�cia",
  "It�lia",
  "Bulgaria",
  "Alemanha",
  "Irlanda",
  "Dinamarca",
  "Austr�lia",
  "EUA",
  "Su�cia")

#Taxa de Natalidade
Natalidade=c(
  45.6,
  39.7,
  33.0,
  27.0,
  25.9,
  23.5,
  23.4,
  22.2,
  20.0,
  19.1,
  18.3,
  18.0,
  17.9,
  15.0)

#Teor de Proteina na Dieta
Proteina=c(
  4.7,
  7.5,
  8.7,
  9.7,
  11.2,
  15.2,
  15.2,
  16.8,
  37.3,
  46.7,
  56.1,
  59.9,
  61.4,
  62.6)

####Estat�sticas Descritivas###

summary(Natalidade)
sd(Natalidade)

summary(Proteina)
sd(Proteina)

#Verificar, sem calcular, o sinal do coeficiente de correla��o entre as 
#duas vari�veis (Fa�o isso vizualizando o Diagrama de Dispers�o)

#Diagrama de Dispers�o

plot(Proteina,Natalidade,xlab="Taxa de Natalidade",ylab="Teor de Proteina na Dieta")

#Parece haver uma rela��o linear (moderada negativa) pelo gr�fico de dispers�o, 
#se houver, ser� negativo, pois o grafico segue uma linha imagin�ria decrescente
#Apesar de haver rela��o linear, isso n�o significa que h� rela��o de causa
#entre o teor de proteina e fertilidade, tamb�m deve-se observar a natureza
#das variaveis

###########2-Dados Quantidade Produto (X) e Custo Total (Y)##############

#Quantidade Produzida de certo produto (Mil Unidades)

x=c(1,2,3,4,5,6,7,8,9,10)

#custo total de produ��o em milhares de Reais

y=c(7,11,15,14,18,21,23,30,32,34)

#Estat�sticas Descritivas

summary(x)
sd(x)

summary(y)
sd(y)

#Diagrama de Dispers�o

plot(x,y,xlab="Quantidade Produzida Produto X",ylab="custo Total de produ��o (R$ 1.000)")

#Pelo Gr�fico de Dispers�o, observa-se uma rela��o direta (ou positiva )
#pois quando os valores de X aumentam, y tamb�m tende a aumentar.

##Quantifico a rela��o do custo total (Y) e quantidade produzida (X)
#Fa�o isso calculando o Coeficiente de Correla��o

#Correla��o Linear

cor(x,y) #O coeficiente de correla��o � forte e Positivo (r=0.98679810)

################Regress�o Linear################

###Obtendo Estimadores de Minimos Quadrados###

modelo1=lm(y~x) #custo total (Y) e quantidade produzida (X)
modelo1

#Beta0(4) e Beta1(3). Nesse caso, a fun��o do Custo Total � dada abaixo
#CustoTotal = 4 + 3*QntProduzida (ou --> y=4+3x)
#Sendo CustoTotal(y), QntProduzida (X)

#Teste Para Validade da Regress�o

summary(modelo1)

#Ao que foi indicado em summary, os coeficientes s�o significativos a 1%.

#O Coeficiente de varia��o - R�  - (R�) � de 0.9738, desse modo, 
#aproximadamente 97% da varia��o do Custo Total(y) � explicada pela varia��o
#da quantidade produzida (x) (Muito relevante)

# QUALIDADE DO AJUSTE
#Verifico o qu�o distante est�o minhas observa��es Reais da Reta Ajustada

plot(x,y,xlab="Quantidade Produzida Produto X",ylab="custo Total de produ��o (R$ 1.000)")
abline(modelo1)

##########AN�LISE DE RES�DUO###############

#An�lise Gr�fica

par(mfrow=c(2,2))
plot(modelo1,which=c(1:4))

####Teste de Breusch-Pagan####
####Hipoteses Estat�sticas####
#H0:Res�duos Homocedasticos (vari�ncia Constante)
#H1:Res�duos N�O Homocedasticos 
# ao um nivel de signific�ncia de 5% vamos a pr� suposi��o ser� confirmada
#se o p-valor form maior que 0,05

bptest(y~x)

# A n�vel de 5% de significancia,
#como p-value=  1, Aceita-se H0, 
#ou seja os dados S�O homocedasticos


#Teste de Normalidade
#Teste Shapiro-Wilk

####Hipoteses Estat�sticas####
#H0:Res�duos Seguem uma distribui��o normal
#H1:Res�duos N�O Seguem uma distribui��o normal
# ao um nivel de signific�ncia de 5% vamos a pr� suposi��o ser� confirmada
#se o p-valor form maior que 0,05

shapiro.test(modelo1$residuals)
#como p-value  = 0,3411 Aceita-se H0, 
#ou seja os residuos S�O normais 

#TEste Independ�ncia dos Residuos

####Hipoteses Estat�sticas####
#H0:Res�duos S�O independentes
#H1:Res�duos N�O s�o independentes
#ao um nivel de signific�ncia de 5% vamos a pr� suposi��o ser� confirmada
#se o p-valor form maior que 0,05

Box.test(modelo1$residuals, type = c("Ljung-Box"))

#como p-value  = 0.8551 aceita-se H0, 
#ou seja os residuos S�O Independentes

#Gr�fico Observa��o individual

x0=x
p1 = predict(modelo1,interval="prediction",se=T)
matplot(x0,p1$fit,lty=c(1,2,2),type="l",xlab="Quantidade Produzida Produto X",ylab="custo Total de produ��o (R$ 1.000)")
points(x,y, type = "p")
p1

#Sendo CustoTotal(y), QntProduzida (X)
#Chamando p1, vizualizo que para x=10,y=34
#fun��o do Custo Total �  y=4+3x

###########3-Dados Distribuidor de Cerveja##############

####Regress�o Multipla#####

#Tentar prever o tempo requerido para atender um ponto de venda com base no
#n�mero de caixas de cerveja fornecidas e a dist�ncia do dep�sito ao posto
#de venda

#N�mero de Caixas
X1=c(10,15,10,20,25,18,12,14,16,22,24,17,13,30,24)

#Dist�ncia
X2=c(30,25,40,18,22,31,26,34,29,37,20,25,27,23,33)

#Tempo
Y=c(24,27,29,31,25,33,26,28,31,39,33,30,25,42,40)

#An�lisar se a vari�vel Tempo para Atender Ponto de Venda(Y) pode ser explicada pelas outras  vari�veis

Reg<- lm(Y~X1+X2) 
summary(Reg)
anova(Reg)

#R�=0.7368 (Muito significativa a Explica��o de Y pelas variaveis X1 e X2)

###########Verificando Pr� Suposi��es modelo Linear##############

#An�lise Grafica de Residuos

par(mfrow=c(2,2))
plot(Reg,which = c(1:4),pch=20)

#Pelo gr�fico, as pressuposi��es de normalidade, homocedasticidade e independ�ncia parecem ser atendidas

#Teste de Homocedasticidade
# Teste de Breush-Pagan

####Hipoteses Estat�sticas####
#H0:Res�duos Homocedasticos (vari�ncia Constante)
#H1:Res�duos N�O Homocedasticos 
# ao um nivel de signific�ncia de 5% vamos a pr� suposi��o ser� confirmada
#se o p-valor form maior que 0,05

bptest(Y~X1+X2)

#como p-value= 0.2669 aceita-se H0, ou seja os dados s�o homocedasticos

#Teste de Normalidade
#Teste Shapiro-Wilk

####Hipoteses Estat�sticas####
#H0:Res�duos Seguem uma distribui��o normal
#H1:Res�duos N�O Seguem uma distribui��o normal
# ao um nivel de signific�ncia de 5% vamos a pr� suposi��o ser� confirmada
#se o p-valor form maior que 0,05

shapiro.test(Reg$residuals)

#como p-value  = 0.0006439 rejeita-se H0, ou seja os residuos N�O s�o normais
#Suponho que esse resultado seja influencia do Outlier

#TEste Independ�ncia dos Residuos

####Hipoteses Estat�sticas####
#H0:Res�duos S�O independentes
#H1:Res�duos N�O s�o independentes
#ao um nivel de signific�ncia de 5% vamos a pr� suposi��o ser� confirmada
#se o p-valor form maior que 0,05


Box.test(Reg$residuals, type = c("Ljung-Box"))

#como p-value  = 0.1684 aceita-se H0, ou seja os residuos s�o Independentes


####Utilizando a Transforma��o de Box-Cox####

#transforma��o nos dados###
#Pois meus dados n�o atenderam o pressuposto de normalidade

#####Usei Segunda maneira##############
####Utilizando o comando  BoxCox.lambda para pegar
####automaticamente o valor de lambda#####

lambda=BoxCox.lambda(Reg$residuals)
YT=((Y^lambda)-1)/lambda


###Regress�o com dados transformados####

Reg2<- lm(YT~X1+X2)
summary(Reg2)
anova(Reg2)

#R�=0.7423 (Muito significativa a Explica��o de Y pelas variaveis X1 e X2)

##############vERIFICANDO PR� SUPOSI��ES DOS RES�DUOS#########

par(mfrow=c(2,2))
plot(Reg2,which = c(1:4),pch=20)

#Teste de Homocedasticidade
# Teste de Breush-Pagan

bptest(YT~X1+X2 )

#como p-value= 0.2589 aceita-se H0, ou seja os dados s�o homocedasticos

#Teste de Normalidade
#Teste Shapiro-Wilk

shapiro.test(Reg2$residuals)

#como p-value=0.0007014, rejeita-se H0, ou seja os residuos N�O s�o normais

#TEste Independ�ncia dos Residuos

#H0: Residuos independentes

Box.test(Reg2$residuals, type = c("Ljung-Box"))

#como p-value = 0.1955 aceita-se H0, ou seja os residuos s�o Independentes


###Verificando multicolinearidade###
#A multicolinearidade � um problema no ajuste do modelo que pode causar
#impactos na estimativa dos par�metros. 
#Podemos diagnosticar Multicolinearidade por meio do 
#VIF (Variance Inflation Factor).

vif(Reg2) # o VIF>10  � indicativo de problemas de multicolinearidade

#Como os Valores est�o abaixo de 10, n�o h� multicolinearidade

##Mesmo transformando os dados, n�o se obedeceu o pressuposto de normalidade,
#Ent�o, n�o cabe o uso de regress�o para essa amostra
