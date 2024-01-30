############Aula 1############

####Objetos####
###Vetores###
####Vou criar 2 vetores####

x=c(4,5,6,8,5.2)
letras=c("a", "b", "c", "d", "e")

x
letras

x[3]

#####usando função rep(repetição)######
y=rep(1,5) #repetir o número 1, 5 vezes
y

z=rep(x,3)
z

#######Usando Função seq(sequencia)

w=seq(0,12,2)
w

#######Matrizes######

matrix(x,5,1)
m2=matrix(c(x,1,2,3,10,11),5,2)
m=cbind(x,c(1,2,3,10,11))
ml=cbind(letras,c("f","g","h","i","j"))
m2[2,2]

#####Data Frame####

altura=c(1.75,1.5,1.85,1.55,1.7)
classificacao=c("alto","baixo", "alto", "baixo","alto")
dados=data.frame(altura,classificacao)
dados

dados$altura
dados$classificacao

######Listas#####

lista=list(altura, classificacao, dados)
lista

lista[[3]]

########Função########

soma2=function(a1,a2){
  resultado=a1+a2
  return(resultado)
}

soma2

soma2(1,2)

media=function(dados,n){
  resultado=sum(dados)/n
  return(resultado)
}

media(x,5)

#########Classes de Objeto#######

is.numeric(x)
is.character(x)

########Comandos de decisão##########

resposta=character()

if(dados$altura[1]>=1.7){
  resposta[1]="alto"
}else{
  resposta[1]="baixo"
}

resposta

if(dados$altura[2]>=1.7){
  resposta[2]="alto"
}else{
  resposta[2]="baixo"
}

resposta

dados$altura

###########Repetição########

resposta1=character()

for (i in 1:5) {
  if(dados$altura[i]>=1.7){
    resposta1[i]="alto"
  }else{
    resposta1[i]="baixo"
  }
}

resposta1

######Comando while##################

resposta2 = character()
i = 1 # aqui você tem que inicializar o contador

while(i<=5){
  if(dados$altura[i]>=1.70){
    resposta2[i] = "Alto"
  }
  else{
    resposta2[i] = "Baixo"
  }
  i = i + 1 #para dar sequencia ao contador
}


dados$altura
resposta2