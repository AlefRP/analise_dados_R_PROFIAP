############################################################################################
##########M�todos Quantitativos com Aux�lio de Softwares##################
######################Profa.Dra.Gislene Araujo Pereira######################################

####Regress�o Logistica######


#carregando os pacotes

library(readxl)
library(lmtest)
library(MASS)
library(car)

#Importando Dados
#Titanic 
#Banco de dados com 891 passageiros

Titanic
#Desses 891 passageiro falta informa��o de idade para 177 passageiro
#Vamos retirar do nosso data.frame essas observa��es

Titanic=subset(Titanic, !is.na(Age))

#idade- quantitativa
#sexo- categoria
#classe-categoria


#Definindo a Vari�vel Dummy para o Sexo
#(Como tem duas categoria ser� criado apenas uma dummy)


n=length(Titanic$Sex)

DFeminino=rep(0,n)
#0 masculino
#1 Feminino

for(i in 1:n){
  if(Titanic$Sex[i]=="female"){
    DFeminino[i]=1
  } else{
    DFeminino[i] =0
  }
}

#Definindo Vari�vel Dummy para Classe que viajou
#(Como tem tr�s categoria ser� criado duas vari�veis dummies)

D1Classe=rep(0,n)
#1 passageiro da 1�Classe
#0 caso contrario

for(i in 1:n){
  if(Titanic$Pclass[i]==1){
    D1Classe[i]=1
  } else{
    D1Classe[i] =0
  }
}

D2Classe=rep(0,n)
#1 passageiro da 2�Classe
#0 caso contrario

for(i in 1:n){
  if(Titanic$Pclass[i]==2){
    D2Classe[i]=1
  } else{
    D2Classe[i] =0
  }
}

#Passageiro da 3� Classe ser� quando D1Classe=0 e D2Classe=0

#Incluindo as vari�veis Dummies no conjunto de dados
Titanic = cbind(Titanic,DFeminino,D1Classe,D2Classe) 

attach(Titanic)


RegLogit=glm(Survived~Age+DFeminino+D1Classe+D2Classe,family=binomial(link="logit"))
summary(RegLogit)

#Raz�es de chance (Odds Ratio) e os seus respectivos IC(95%)

OR=exp(RegLogit$coefficients)
ICbeta=confint.default(RegLogit,level=0.95)
ICOR=exp(ICbeta)
round((cbind(OR, ICOR)),3)

#Avaliando Qualidade do Modelo

#Classifica��o do Modelo
Classif=table(Survived,predict(RegLogit,type = "response")>0.5)
Classif

VN=Classif[1,1]
FP=Classif[1,2]
FN=Classif[2,1]
VP=Classif[2,2]
Total=VN+FP+FN+VP

#ACUR�CIA:Raz�o das predi��es mais corretas do total de observa��es
#Taxa de acerto geral do modelo
acuracia=(VP+VN)/Total

#Sensibilidade:Quando a classifica��o � realmente "Sim" e o quanto ocorreu "Sim"
#Capacidade do modelo de prever que o �ndividuo iria sobreviver e realmente ele sobreviveu
Sensibilidade=VP/(VP+FN)

#Especificidade: Quando a classifica��o � realmente "N�o" e o quanto ocorreu "N�o" 
#Capacidade do modelo de prever que o �ndividuo N�o  sobreviveria e realmente ele  N�o sobreviveu
Especificidade=VN/(VN+FP)

round(acuracia,4)
round(Sensibilidade,4)
round(Especificidade,4)

#TEste Independ�ncia dos Residuos

#H0: Residuos independentes

Box.test(RegLogit$residuals, type = c("Ljung-Box"))

#como p-value  = 0.3059  Aceita sE H0, ou seja os residuos s�o Independentes

