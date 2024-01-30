##############################################
#################Atividade 1###################
###############################################

#################Dados Alunos###################

Id=c(26,27,36,35,38,38)#Idade
Genero=c("masculino","feminino","masculino","masculino","masculino","masculino")#Gênero
Alt=c(1.87,1.66,1.74,1.82,1.7,1.69)#Altura
Renda=c(800,500,1000,1100,2000,1200)#Renda Média
GAlimentacao=c(350,250,400,350,600,400)#Média mensal de gastos com alimentação
GCombustivel=c(150,120,130,180,150,150)#Média Mensal de Gastos com Combustíveis
GInternet=c(70,70,70,70,70,70)#Média mensal de gastos com serviços de Internet
GTelefone=c(55,30,45,50,80,23)#Média mensal de gastos com serviços de Telefonia

################Classificação quanto à altura################

classAlt=character()#Criei resposta para armazenar os valores da classificacao

for(i in 1:6){
  if(Alt[i]>=1.7){
    classAlt[i]="alto"
  }else{
    classAlt[i]="baixo"
  }
}

classAlt #Armazenei a classificação  quanto a altura aqui para exibir no Data-Frame

################Percentuais################

###Cria vetores para os percentuais de Gastos em relação a Renda

PerGAlimentacao=round(GAlimentacao/Renda*100,2)
PerGCombustivel=round(GCombustivel/Renda*100,2)
PerGInternet=round(GInternet/Renda*100,2)
PerGTelefone=round(GTelefone/Renda*100,2)

#####Data Frame dos Dados#####

Dados=data.frame(Id,Genero,Alt,classAlt,Renda,GAlimentacao,GCombustivel,GInternet,GTelefone,PerGAlimentacao,PerGCombustivel,PerGInternet,PerGTelefone)

Dados