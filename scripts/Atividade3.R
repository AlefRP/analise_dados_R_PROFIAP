##############################################
#################Atividade 3###################
###############################################

#Carregando Pacotes

library(MASS) #Pacote para Análise de Regressão (Modelagem Linear)
library(lmtest) #Pacote de Testes
library(car) #Pacote para realizar teste de multicolinearidade
library(forecast)#Pacote para utilizar função Box.Cox

##############1-Dados Teor de Proteina e Taxa de Fertilidade#################

#País
Pais=c(
  "Formosa",
  "Malaia",
  "índia",
  "Japão",
  "Lugoslávia",
  "Grécia",
  "Itália",
  "Bulgaria",
  "Alemanha",
  "Irlanda",
  "Dinamarca",
  "Austrália",
  "EUA",
  "Suécia")

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

####Estatísticas Descritivas###

summary(Natalidade)
sd(Natalidade)

summary(Proteina)
sd(Proteina)

#Verificar, sem calcular, o sinal do coeficiente de correlação entre as 
#duas variáveis (Faço isso vizualizando o Diagrama de Dispersão)

#Diagrama de Dispersão

plot(Proteina,Natalidade,xlab="Taxa de Natalidade",ylab="Teor de Proteina na Dieta")

#Parece haver uma relação linear (moderada negativa) pelo gráfico de dispersão, 
#se houver, será negativo, pois o grafico segue uma linha imaginária decrescente
#Apesar de haver relação linear, isso não significa que há relação de causa
#entre o teor de proteina e fertilidade, também deve-se observar a natureza
#das variaveis

###########2-Dados Quantidade Produto (X) e Custo Total (Y)##############

#Quantidade Produzida de certo produto (Mil Unidades)

x=c(1,2,3,4,5,6,7,8,9,10)

#custo total de produção em milhares de Reais

y=c(7,11,15,14,18,21,23,30,32,34)

#Estatísticas Descritivas

summary(x)
sd(x)

summary(y)
sd(y)

#Diagrama de Dispersão

plot(x,y,xlab="Quantidade Produzida Produto X",ylab="custo Total de produção (R$ 1.000)")

#Pelo Gráfico de Dispersão, observa-se uma relação direta (ou positiva )
#pois quando os valores de X aumentam, y também tende a aumentar.

##Quantifico a relação do custo total (Y) e quantidade produzida (X)
#Faço isso calculando o Coeficiente de Correlação

#Correlação Linear

cor(x,y) #O coeficiente de correlação é forte e Positivo (r=0.98679810)

################Regressão Linear################

###Obtendo Estimadores de Minimos Quadrados###

modelo1=lm(y~x) #custo total (Y) e quantidade produzida (X)
modelo1

#Beta0(4) e Beta1(3). Nesse caso, a função do Custo Total é dada abaixo
#CustoTotal = 4 + 3*QntProduzida (ou --> y=4+3x)
#Sendo CustoTotal(y), QntProduzida (X)

#Teste Para Validade da Regressão

summary(modelo1)

#Ao que foi indicado em summary, os coeficientes são significativos a 1%.

#O Coeficiente de variação - R²  - (R²) é de 0.9738, desse modo, 
#aproximadamente 97% da variação do Custo Total(y) é explicada pela variação
#da quantidade produzida (x) (Muito relevante)

# QUALIDADE DO AJUSTE
#Verifico o quão distante estão minhas observações Reais da Reta Ajustada

plot(x,y,xlab="Quantidade Produzida Produto X",ylab="custo Total de produção (R$ 1.000)")
abline(modelo1)

##########ANÁLISE DE RESÍDUO###############

#Análise Gráfica

par(mfrow=c(2,2))
plot(modelo1,which=c(1:4))

####Teste de Breusch-Pagan####
####Hipoteses Estatísticas####
#H0:Resíduos Homocedasticos (variância Constante)
#H1:Resíduos NÃO Homocedasticos 
# ao um nivel de significância de 5% vamos a pré suposição será confirmada
#se o p-valor form maior que 0,05

bptest(y~x)

# A nível de 5% de significancia,
#como p-value=  1, Aceita-se H0, 
#ou seja os dados SÂO homocedasticos


#Teste de Normalidade
#Teste Shapiro-Wilk

####Hipoteses Estatísticas####
#H0:Resíduos Seguem uma distribuição normal
#H1:Resíduos NÃO Seguem uma distribuição normal
# ao um nivel de significância de 5% vamos a pré suposição será confirmada
#se o p-valor form maior que 0,05

shapiro.test(modelo1$residuals)
#como p-value  = 0,3411 Aceita-se H0, 
#ou seja os residuos SÃO normais 

#TEste Independência dos Residuos

####Hipoteses Estatísticas####
#H0:Resíduos SÃO independentes
#H1:Resíduos NÃO são independentes
#ao um nivel de significância de 5% vamos a pré suposição será confirmada
#se o p-valor form maior que 0,05

Box.test(modelo1$residuals, type = c("Ljung-Box"))

#como p-value  = 0.8551 aceita-se H0, 
#ou seja os residuos SÃO Independentes

#Gráfico Observação individual

x0=x
p1 = predict(modelo1,interval="prediction",se=T)
matplot(x0,p1$fit,lty=c(1,2,2),type="l",xlab="Quantidade Produzida Produto X",ylab="custo Total de produção (R$ 1.000)")
points(x,y, type = "p")
p1

#Sendo CustoTotal(y), QntProduzida (X)
#Chamando p1, vizualizo que para x=10,y=34
#função do Custo Total é  y=4+3x

###########3-Dados Distribuidor de Cerveja##############

####Regressão Multipla#####

#Tentar prever o tempo requerido para atender um ponto de venda com base no
#número de caixas de cerveja fornecidas e a distância do depósito ao posto
#de venda

#Número de Caixas
X1=c(10,15,10,20,25,18,12,14,16,22,24,17,13,30,24)

#Distância
X2=c(30,25,40,18,22,31,26,34,29,37,20,25,27,23,33)

#Tempo
Y=c(24,27,29,31,25,33,26,28,31,39,33,30,25,42,40)

#Análisar se a variável Tempo para Atender Ponto de Venda(Y) pode ser explicada pelas outras  variáveis

Reg<- lm(Y~X1+X2) 
summary(Reg)
anova(Reg)

#R²=0.7368 (Muito significativa a Explicação de Y pelas variaveis X1 e X2)

###########Verificando Pré Suposições modelo Linear##############

#Análise Grafica de Residuos

par(mfrow=c(2,2))
plot(Reg,which = c(1:4),pch=20)

#Pelo gráfico, as pressuposições de normalidade, homocedasticidade e independência parecem ser atendidas

#Teste de Homocedasticidade
# Teste de Breush-Pagan

####Hipoteses Estatísticas####
#H0:Resíduos Homocedasticos (variância Constante)
#H1:Resíduos NÃO Homocedasticos 
# ao um nivel de significância de 5% vamos a pré suposição será confirmada
#se o p-valor form maior que 0,05

bptest(Y~X1+X2)

#como p-value= 0.2669 aceita-se H0, ou seja os dados são homocedasticos

#Teste de Normalidade
#Teste Shapiro-Wilk

####Hipoteses Estatísticas####
#H0:Resíduos Seguem uma distribuição normal
#H1:Resíduos NÃO Seguem uma distribuição normal
# ao um nivel de significância de 5% vamos a pré suposição será confirmada
#se o p-valor form maior que 0,05

shapiro.test(Reg$residuals)

#como p-value  = 0.0006439 rejeita-se H0, ou seja os residuos NÃO são normais
#Suponho que esse resultado seja influencia do Outlier

#TEste Independência dos Residuos

####Hipoteses Estatísticas####
#H0:Resíduos SÃO independentes
#H1:Resíduos NÃO são independentes
#ao um nivel de significância de 5% vamos a pré suposição será confirmada
#se o p-valor form maior que 0,05


Box.test(Reg$residuals, type = c("Ljung-Box"))

#como p-value  = 0.1684 aceita-se H0, ou seja os residuos são Independentes


####Utilizando a Transformação de Box-Cox####

#transformação nos dados###
#Pois meus dados não atenderam o pressuposto de normalidade

#####Usei Segunda maneira##############
####Utilizando o comando  BoxCox.lambda para pegar
####automaticamente o valor de lambda#####

lambda=BoxCox.lambda(Reg$residuals)
YT=((Y^lambda)-1)/lambda


###Regressão com dados transformados####

Reg2<- lm(YT~X1+X2)
summary(Reg2)
anova(Reg2)

#R²=0.7423 (Muito significativa a Explicação de Y pelas variaveis X1 e X2)

##############vERIFICANDO PRÉ SUPOSIÇÕES DOS RESÍDUOS#########

par(mfrow=c(2,2))
plot(Reg2,which = c(1:4),pch=20)

#Teste de Homocedasticidade
# Teste de Breush-Pagan

bptest(YT~X1+X2 )

#como p-value= 0.2589 aceita-se H0, ou seja os dados são homocedasticos

#Teste de Normalidade
#Teste Shapiro-Wilk

shapiro.test(Reg2$residuals)

#como p-value=0.0007014, rejeita-se H0, ou seja os residuos NÃO são normais

#TEste Independência dos Residuos

#H0: Residuos independentes

Box.test(Reg2$residuals, type = c("Ljung-Box"))

#como p-value = 0.1955 aceita-se H0, ou seja os residuos são Independentes


###Verificando multicolinearidade###
#A multicolinearidade é um problema no ajuste do modelo que pode causar
#impactos na estimativa dos parâmetros. 
#Podemos diagnosticar Multicolinearidade por meio do 
#VIF (Variance Inflation Factor).

vif(Reg2) # o VIF>10  é indicativo de problemas de multicolinearidade

#Como os Valores estão abaixo de 10, não há multicolinearidade

##Mesmo transformando os dados, não se obedeceu o pressuposto de normalidade,
#Então, não cabe o uso de regressão para essa amostra
