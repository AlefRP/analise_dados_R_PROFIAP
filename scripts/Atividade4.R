##############################################
#################Atividade 4###################
###############################################

#Carregando Pacotes

library(readxl)#Pacote leitura base excel
library(MASS) #Pacote para Análise de Regressão (Modelagem Linear)
library(lmtest) #Pacote de Testes
library(car) #Pacote para realizar teste de multicolinearidade

#Importando Dados
#Status

status=read_excel("C:\\Users\\alefr\\Desktop\\MESTRADO Alef\\Análise de Dados\\Métodos Quantitativos com Auxilio de Software\\ModuloMetodosQuantitativos\\dados\\status.xlsx")

#Já tenho Definida a Variável Dummy para o Vínculo empregatício (VE)
#Como tem duas categoria, preciso apenas de uma dummy
#Variavél Dummy VE que já assume valores de 0 e 1 no conjunto de dados

#0 Sem vínculo empregaticio
#1 Com vínculo empregatício

attach(status)

#####Regresão Logística#######

RegLogit=glm(ST~R+ND+VE,family=binomial(link="logit"))

#Lembrando que:
#ST: status do Cliente (Inadimplente=1, Adimplente=0)
#R: Renda mensal
#ND: Números de Dependentes
#VE: Vínculo empregatício (Com vínculo=1, Sem vínculo=0).

summary(RegLogit)

#Todos os coeficientes são significativos à 5% (p<0.05)
#A Renda mensal (R) apresentou coeficiente negativo, logo a Renda tem efeito negativo sobre a probavilidade de inadimplência
#Em síntese, quanto maior a renda, menor o risco da pessoa ser inadimplente (faz todo sentido)
#Número de Dependentes tem coeficiente positivo, ou seja, quanto maior o número de dependentes, maior a probabilidade da pessoa ser inadimplente
#Sobre o vínculo empregaticio,  a estatística mostra que uma pessoa com vinculo empregaticio tem uma probabilidade maior de se tornar inadimplente do que  uma pessoa sem vinculo

#Razões de chance (Odds Ratio) e os seus respectivos IC(95%)

OR=exp(RegLogit$coefficients)
ICbeta=confint.default(RegLogit,level=0.95)
ICOR=exp(ICbeta)
round((cbind(OR, ICOR)),3)

#Interpretação das razões de chance (Odds Ratio)
#A cada unidade de renda acrescida, diminiu a chance do individuo ser inadimplente será  0.152 vezes menor (pois o coeficiente é negativo)
#A cada dependente acrescido, a chance do individuo ser inadimplente será  2.362 vezes maior (pois o coeficiente é positivo)
#Pessoas com vínculo empregatício tem chance 16.812 vezes maior de ser inadimplente do que pessoas com vinculo

#Avaliando Qualidade do Modelo

#Classificação do Modelo
Classif=table(ST,predict(RegLogit,type = "response")>0.5)
Classif

#Lembrando:
#ST: status do Cliente (Inadimplente=1, Adimplente(Não inadimplente)=0)

VN=Classif[1,1]
FP=Classif[1,2]
FN=Classif[2,1]
VP=Classif[2,2]
Total=VN+FP+FN+VP

#ACURÁCIA:Razão das predições mais corretas do total de observações
#Taxa de acerto geral do modelo
acuracia=(VP+VN)/Total

#Sensibilidade:Quando a classificação é realmente "Sim" e o quanto ocorreu "Sim"
#Capacidade do modelo de prever que o índividuo iria ser inadimplente e realmente ele é inadimplente
Sensibilidade=VP/(VP+FN)

#Especificidade: Quando a classificação é realmente "Não" e o quanto ocorreu "Não" 
#Capacidade do modelo de prever que o índividuo Não  seria inadimplente e realmente ele  Não foi inadimplente
Especificidade=VN/(VN+FP)

round(acuracia,4)
round(Sensibilidade,4)
round(Especificidade,4)

#Teste Independência dos Residuos

#H0: Residuos independentes

Box.test(RegLogit$residuals, type = c("Ljung-Box"))

#como p-value  = 0.6459  Aceita-se H0, ou seja os residuos são Independentes