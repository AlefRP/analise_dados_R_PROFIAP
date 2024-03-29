#########################################################################
#############################Aula 03####################################
########################################################################

#Carregando Pacotes

library(readxl)#Pacote importa��o Excel
library(MASS) #Pacote para An�lise de Regress�o (Modelagem Linear)
library(lmtest) #Pacote de Testes
library(car) #Pacote para realizar teste de multicolinearidade
library(forecast)#Pacote para utilizar fun��o Box.Cox

#Importa��o Dados utilizando a op��o "Import Dataset" (PublicidadeVendas)
#Importa��o Dados PublicidadeVendas

PublicidadeVendas=read_excel("C:\\Users\\alefr\\Desktop\\MESTRADO Alef\\An�lise de Dados\\M�todos Quantitativos com Auxilio de Software\\ModuloMetodosQuantitativos\\dados\\PublicidadeVendas.xlsx")

attach(PublicidadeVendas) #comando para poder acessar cada coluna do DataFrame

#Estat�sticas Descritivas

summary(Publicidade)
sd(Publicidade)

summary(Vendas)
sd(Vendas)

#Diagrama de Dispers�o

plot(Publicidade,Vendas,xlab="Publicidade",ylab="Vendas")

#Correla��o Linear

cor(Publicidade,Vendas)

# O R retornou o valor de 0.9011918 indicando forte rela��o linear entre as variaveis

###Obtendo Estimadores de Minimos dos Quadrados###

modelo1=lm(Vendas~Publicidade) #lm(y~x)
modelo1

#Teste para a validade da Regress�o

summary(modelo1)

#No output observamos as estimativas dos par�metros, o erro padr�o
#associado a cada estimativa, uma estat�stica t e um p-valor associado.
# O resultado do teste t  � utilizado para saber se as estimativas
#s�o realmente diferentes de zero. Quanto mais asteriscos presentes 
#ao lado do efeito estimado, maior o n�vel de confian�a com que podemos
#afirmar que o efeito n�o � nulo.

#Quanto ao R�, ao utilizar apenas uma vari�vel � normal que o valor n�o 
#seja extremamente alto. Nesse caso o R�=0,8121 � extremamente relevante

# Qualidade do Ajuste
#Verificar o qu�o distantes est�o as observa��es reais da reta ajustada

plot(Publicidade,Vendas,xlab="Publicidades",ylab="Vendas")
abline(modelo1)

#Obtendo intervalos de confian�a para Beta0 e Beta1

confint(modelo1)

#####AN�LISE DE RES�DUOS########

par(mfrow=c(2,2))
plot(modelo1,wich=c(1:4))

####Teste de Breusch-Pagan#######
####Hipoteses Estat�sticas####
#H0:Res�duos Homocedasticos (vari�ncia Constante) p<0.05
#H1:Res�duos N�O Homocedasticos p=>0.05
# ao um nivel de signific�ncia de 5% vamos a pr� suposi��o ser� confirmada
#se o p-valor form maior que 0,05

bptest(Vendas~Publicidade)

# A n�vel de 5% de significancia,
#como p-value=  0.07492 Aceita-se H0 (adotamos aqui o crit�rio de 10% de significancia devido ao tamanho da amostra e tamb�m pelo valor de p estar pr�ximo de 0.05), 
#ou seja os dados S�O homocedasticos

#Teste de Normalidade
#Teste Shapiro-Wilk

shapiro.test(modelo1$residuals)

#como p-value  = 0,7731 Aceita-se H0, 
#ou seja os residuos S�O normais (se p<0.05 eu rejeitava H0, res�duos n�o seriam normais)

#Teste de Independ�ncia dos respiduos
#H0: Res�duos Independentes (p>0.05)

Box.test(modelo1$residuals,type=c("Ljung-Box"))

#como p-value  = 0.5036 ACEITA sE H0, 
#ou seja os residuos S�O Independentes


#Uma vez verificado todas as pr�-suposi��es podemos ent�o utilizar
#o modelo ajustado para fazer previs�es

#Vamos contruir gr�ficos com as estimativas pontuais e intervalares
#das respostas m�dias e individuais:

#Gr�fico Observa��o m�dia

x0=Publicidade
p1 = predict(modelo1,interval="confidence",se=T)
p1 #Valores previstos para Vendas (fit) juntamente como os limites inferior e superior
matplot(x0,p1$fit,lty=c(1,2,2),type="l",xlab="Publicidades",ylab="Vendas")
points(Publicidade,Vendas, type = "p")

##########################REGRESS�O MULTIPLA###################

#Dados sobre o produto bruto real(Y), trabalho(X1) e capital real(X2) 
#no setor industrial de Taiwan

#Importando Dados
#Taiwan.xlsx

Taiwan=read_excel("C:\\Users\\alefr\\Desktop\\MESTRADO Alef\\An�lise de Dados\\M�todos Quantitativos com Auxilio de Software\\ModuloMetodosQuantitativos\\dados\\Taiwan.xlsx")

#An�lisar se a vari�vel produto bruto real(Y) pode ser explicada pelas outras  vari�veis

attach(Taiwan)

Reg<- lm(Y~X1+X2) #Comando Regress�o Multipla (Mais de uma Variav�l Independente)
summary(Reg) #lm(y~x1)... Faria isso pois X2 n�o � significativo
anova(Reg)


#Teste de Homocedasticidade
# Teste de Breush-Pagan

####Hipoteses Estat�sticas####
#H0:Res�duos Homocedasticos (vari�ncia Constante)
#H1:Res�duos N�O Homocedasticos 
# ao um nivel de signific�ncia de 5% vamos a pr� suposi��o ser� confirmada
#se o p-valor form MAIOR que 0,05


bptest(Y~X1+X2 )

#como p-value=  0.05071 (apesar de muito proximo de 0,05) rejeita-se H0,
#ou seja os dados N�O s�o homocedasticos

#Teste de Normalidade
#Teste Shapiro-Wilk

####Hipoteses Estat�sticas####
#H0:Res�duos Seguem uma distribui��o normal
#H1:Res�duos N�O Seguem uma distribui��o normal
# ao um nivel de signific�ncia de 5% vamos a pr� suposi��o ser� confirmada
#se o p-valor form MAIOR que 0,05

shapiro.test(Reg$residuals)

#como p-value  = 0.3117  aceita sE H0, ou seja os residuos s�o normais

#TEste Independ�ncia dos Residuos

####Hipoteses Estat�sticas####
#H0:Res�duos S�O independentes
#H1:Res�duos N�O s�o independentes
#ao um nivel de signific�ncia de 5% vamos a pr� suposi��o ser� confirmada
#se o p-valor form MAIOR que 0,05


Box.test(Reg$residuals, type = c("Ljung-Box"))

#como p-value  = 0.03403  rejeita se H0, ou seja os residuos N�o s�o Independentes (tem que ser)

####Utilizando a Transforma��o de Box-Cox para verificar melhor
#transforma��o nos dados###

#Primeira maneira (encontrar o valor de lambda por meio do grafico)

boxcox(Reg, plotit=T, lam=seq(-1, 1, 1/10))

#O gr�fico mostra que a fun��o que maximiza a fun��o � aproximadamente 0.1, logo:

lambda=0.1
YT=((Y^lambda)-1)/lambda # ytransf=((y^lambda)-1)/lambda

#Regress�o com dados transformados####

Reg2<- lm(YT~X1+X2)
summary(Reg2)
anova(Reg2)

##############vERIFICANDO PR� SUPOSI��ES DOS RES�DUOS#########

par(mfrow=c(2,2))
plot(Reg2,which = c(1:4),pch=20)

#Teste de Homocedasticidade
# Teste de Breush-Pagan

bptest(YT~X1+X2 )

#como p-value= 0.5921 aceita-se H0, ou seja os dados s�o homocedasticos

#Teste de Normalidade
#Teste Shapiro-Wilk

shapiro.test(Reg2$residuals)

#como p-value=0.3546 Aceita SE H0, ou seja os residuos s�o normais

#TEste Independ�ncia dos Residuos

#H0: Residuos independentes

Box.test(Reg2$residuals, type = c("Ljung-Box"))

#como p-value  = 0.152  Aceita sE H0, ou seja os residuos s�o Independentes

###Verificando multicolinearidade###
#A multicolinearidade � um problema no ajuste do modelo que pode causar
#impactos na estimativa dos par�metros. 
#Podemos diagnosticar Multicolinearidade por meio do 
#VIF (Variance Inflation Factor).

vif(Reg2) # o VIF>10  � indicativo de problemas de multicolinearidade
