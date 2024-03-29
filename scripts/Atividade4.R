##############################################
#################Atividade 4###################
###############################################

#Carregando Pacotes

library(readxl)#Pacote leitura base excel
library(MASS) #Pacote para An�lise de Regress�o (Modelagem Linear)
library(lmtest) #Pacote de Testes
library(car) #Pacote para realizar teste de multicolinearidade

#Importando Dados
#Status

status=read_excel("C:\\Users\\alefr\\Desktop\\MESTRADO Alef\\An�lise de Dados\\M�todos Quantitativos com Auxilio de Software\\ModuloMetodosQuantitativos\\dados\\status.xlsx")

#J� tenho Definida a Vari�vel Dummy para o V�nculo empregat�cio (VE)
#Como tem duas categoria, preciso apenas de uma dummy
#Variav�l Dummy VE que j� assume valores de 0 e 1 no conjunto de dados

#0 Sem v�nculo empregaticio
#1 Com v�nculo empregat�cio

attach(status)

#####Regres�o Log�stica#######

RegLogit=glm(ST~R+ND+VE,family=binomial(link="logit"))

#Lembrando que:
#ST: status do Cliente (Inadimplente=1, Adimplente=0)
#R: Renda mensal
#ND: N�meros de Dependentes
#VE: V�nculo empregat�cio (Com v�nculo=1, Sem v�nculo=0).

summary(RegLogit)

#Todos os coeficientes s�o significativos � 5% (p<0.05)
#A Renda mensal (R) apresentou coeficiente negativo, logo a Renda tem efeito negativo sobre a probavilidade de inadimpl�ncia
#Em s�ntese, quanto maior a renda, menor o risco da pessoa ser inadimplente (faz todo sentido)
#N�mero de Dependentes tem coeficiente positivo, ou seja, quanto maior o n�mero de dependentes, maior a probabilidade da pessoa ser inadimplente
#Sobre o v�nculo empregaticio,  a estat�stica mostra que uma pessoa com vinculo empregaticio tem uma probabilidade maior de se tornar inadimplente do que  uma pessoa sem vinculo

#Raz�es de chance (Odds Ratio) e os seus respectivos IC(95%)

OR=exp(RegLogit$coefficients)
ICbeta=confint.default(RegLogit,level=0.95)
ICOR=exp(ICbeta)
round((cbind(OR, ICOR)),3)

#Interpreta��o das raz�es de chance (Odds Ratio)
#A cada unidade de renda acrescida, diminiu a chance do individuo ser inadimplente ser�  0.152 vezes menor (pois o coeficiente � negativo)
#A cada dependente acrescido, a chance do individuo ser inadimplente ser�  2.362 vezes maior (pois o coeficiente � positivo)
#Pessoas com v�nculo empregat�cio tem chance 16.812 vezes maior de ser inadimplente do que pessoas com vinculo

#Avaliando Qualidade do Modelo

#Classifica��o do Modelo
Classif=table(ST,predict(RegLogit,type = "response")>0.5)
Classif

#Lembrando:
#ST: status do Cliente (Inadimplente=1, Adimplente(N�o inadimplente)=0)

VN=Classif[1,1]
FP=Classif[1,2]
FN=Classif[2,1]
VP=Classif[2,2]
Total=VN+FP+FN+VP

#ACUR�CIA:Raz�o das predi��es mais corretas do total de observa��es
#Taxa de acerto geral do modelo
acuracia=(VP+VN)/Total

#Sensibilidade:Quando a classifica��o � realmente "Sim" e o quanto ocorreu "Sim"
#Capacidade do modelo de prever que o �ndividuo iria ser inadimplente e realmente ele � inadimplente
Sensibilidade=VP/(VP+FN)

#Especificidade: Quando a classifica��o � realmente "N�o" e o quanto ocorreu "N�o" 
#Capacidade do modelo de prever que o �ndividuo N�o  seria inadimplente e realmente ele  N�o foi inadimplente
Especificidade=VN/(VN+FP)

round(acuracia,4)
round(Sensibilidade,4)
round(Especificidade,4)

#Teste Independ�ncia dos Residuos

#H0: Residuos independentes

Box.test(RegLogit$residuals, type = c("Ljung-Box"))

#como p-value  = 0.6459  Aceita-se H0, ou seja os residuos s�o Independentes