############################################################################################
##########M�todos Quantitativos com Aux�lio de Softwares##################
######################Profa.Dra.Gislene Araujo Pereira######################################

####Regress�o com Vari�veis (indepedentes) Dummies######


#carregando os pacotes

library(readxl)
library(lmtest)
library(MASS)
library(car)

#Importando Dados
#SalarioGenero.xlsx

#Definindo a Vari�vel Dummy para o Genero


n=length(SalarioGenero$GENERO)

GeneroM=rep(0,n)
#1 masculino
#0 Feminino

for(i in 1:n){
  if(SalarioGenero$GENERO[i]=="M"){
    GeneroM[i]=1
  } else{
    GeneroM[i] =0
  }
}
#Incluindo a vari�vel Dummy no conjunto de dados
SalarioGenero = cbind(SalarioGenero,GeneroM) 

#Criando nova variavel com a intera��o XZ (tempo experi�ncia/Genero)
ExpGen=SalarioGenero$EXP*SalarioGenero$GeneroM

#Incluindo a vari�vel intera��o no conjunto de dados

SalarioGenero = cbind(SalarioGenero,ExpGen) 

attach(SalarioGenero)#comando para acessar as coluna do dataFrame

###########################1� Maneira######################
####Modelos diferentes para cada categoria#######
RegDummy1=lm(SALARIO~EXP+GeneroM)
summary(RegDummy1)
anova(RegDummy1)


p1=min(EXP)
p2=max(EXP)

plot(SALARIO~EXP,pch=as.integer(GENERO),col=as.integer(GENERO))

#Bolinha preta Feminino
#Tri�ngulo Vermelho Masculino

#reta modelo Feminino
lines(c(p1,p2),
      c(RegDummy1$coefficients%*%c(1,p1,0),
      RegDummy1$coefficients%*%c(1,p2,0)),col=1)

#Reta modelo Masculino      
lines(c(p1,p2),
      c(RegDummy1$coefficients%*%c(1,p1,1),
      RegDummy1$coefficients%*%c(1,p2,1)),col=2)


#Obs:A inclus�o da Vari�vel Dummy desloca o intercepto da regress�o
# o beta 2 ser� a diferen�a sal�rial entre homens e mulheres dada 
#a mesma experi�ncia

##############vERIFICANDO PR� SUPOSI��ES DOS RES�DUOS#########
par(mfrow=c(2,2))
plot(RegDummy1,which = c(1:4),pch=20)

#Teste de Homocedasticidade
# Teste de Breush-Pagan

bptest(SALARIO~EXP+GeneroM )

#H0:Homocedasticidade dos Residuos

#como p-value= 0.2846 aceita-se H0,
#ou seja os dados s�o homocedasticos

#Teste de Normalidade
#Teste Shapiro-Wilks

shapiro.test(RegDummy1$residuals)

#Ho: Residuos normais
#como p-value= 0.1852 Aceita SE H0, ou seja os residuos s�o normais

#TEste Independ�ncia dos Residuos

#H0: Residuos independentes

Box.test(RegDummy1$residuals, type = c("Ljung-Box"))

#como p-value  = 0.5327  Aceita sE H0, ou seja os residuos s�o Independentes


###Verificando multicolineariedade###
vif(RegDummy1)
#Como todos os Vif<10 n�o temos problema de multicolineariedade

#####Podemos utilizar os modelos para prever salario segundo exp e G�nero.


###############2�Maneira###############
#####Ajustar um �nico modelo para as duas categorias##########

RegDummy2=lm(SALARIO~EXP+GeneroM+ExpGen)
summary(RegDummy2)
anova(RegDummy2)

##############vERIFICANDO PR� SUPOSI��ES DOS RES�DUOS#########
par(mfrow=c(2,2))
plot(RegDummy2,which = c(1:4),pch=20)

#Teste de Homocedasticidade
# Teste de Breush-Pagan

bptest(SALARIO~EXP+GeneroM+ExpGen )

#como p-value= 0.5549 aceita-se H0, ou seja os dados s�o homocedasticos

#Teste de Normalidade
#Teste Shapiro-Wilks

shapiro.test(RegDummy2$residuals)

#como p-value= 0.8893 Aceita SE H0, ou seja os residuos s�o normais

#TEste Independ�ncia dos Residuos

#H0: Residuos independentes

Box.test(RegDummy2$residuals, type = c("Ljung-Box"))

#como p-value  = 0.9678  Aceita sE H0, ou seja os residuos s�o Independentes


###Verificando multicolinearidade###
vif(RegDummy2)
#Com certeza a variavel inteira��o daria um VIF grande
#Pois ela � um produto das outras vari�veis
#mas como deu menor que 10 n�o ocosionar� problemas matem�ticos

