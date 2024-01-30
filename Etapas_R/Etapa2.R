############################################################################################
##########Módulo:Métodos Quantitativos com Auxílio de Software Estatístico###########
######################Profa.Dra.Gislene Araujo Pereira######################################


#Vetores
x = c(4,5,6,8,5.2)
letras = c("a","b","c","d","e")

x
letras

y = rep(1,5) #Criamos um vetor de 1´s
y
z = rep(x,3) #Estamos repetindo o vetor x 3 vezes
z


seq(0,12,2); #Estamos criando uma sequência de 0 a 12 , aumentando de 2 em 2
seq(12,0,-2); #Estamos criando uma sequência de 12 a 0 , diminuindo de 2 em 2
seq(x[1],x[4],1) #Estamos criando uma sequência de x[1] a x[4] , aumentando de 1 em 1


x2 = c(1:7) #Estamos criando uma sequência de 1 a 7 , aumentando de 1 em 1
x2
x3 = c(7:1) #Estamos criando uma sequência de 7 a 1 , diminuindo de 1 em 1
x3

alfabeto = letters[1:26]
alfabeto
ALFABETO = LETTERS[1:26]
ALFABETO 

#########Matrizes##############
matrix(x,5,1)   #Estamos definindo uma matriz com 5 linhas e 1 coluna, composta pelo vetor x.

matrix(c(x,1,2,3,10,11),5,2) #Estamos definindo um vetor  diretamente na função matrix
m = cbind(x,c(1,2,3,10,11)) #Temos a mesma matriz do exemplo anterior.
m
ml = cbind(letras,c("f","g","h","i","j")) #Estamos definindo uma matriz com 5 linhas e 1 colunas,
                                          #onde a primeira coluna será composta pelo vetor letras,
                                          #e a segunda coluna pelo vetor ("f","g","h","i","j"))

ml


############Data-frames#######################
altura = c(1.75,1.5,1.85,1.55,1.7)
classificacao = c("alto","baixo","alto","baixo","alto")
dados = data.frame(altura,classificacao)
dados

dados$altura

dados$classificacao


########Listas#########################
lista= list(altura,classificacao,dados)
lista
lista[[1]]
lista[[2]]
lista[[3]]



#########Funções#############

######Função para calcular a Soma Dois Valores


somar2 = function(a1,a2)
{
  resultado = a1 + a2
  return(resultado)
}

somar2(1,2) #retornará o valor 3
somar2(1,5) #retornará o valor 6
somar2(1,5.2293) # retornará o valor 6.2293


###Função para calcular a media amostral


media = function(dados,n)
{
  resultado = sum(dados)/n #sum() e função do somatório
  return(resultado)
}

x
media(x,5) # retornará o valor 5.64

#######################TIPOS DE CLASSES################
###Números Inteiros##############
Z = c(-3:3)
Z

###Números reais##############
R = seq(-1,1,0.2)
R

###########Caracteres##############
c = letters[1:5]
c

############Fatores##############
cf = as.factor(c) #Transformamos o objeto c que era da classe "character" em um fator.
cf 

zf = as.factor(Z) #Transformamos o objeto z que era da classe "integer"  em um fator.
zf

dados$classificacao = as.factor(dados$classificacao) #Transformamos o objeto dados$classificacao que 
                                                    # era da classe "character" em um fator.
dados$classificacao


########Lógico################################
l = c(1,1,1,1,0,0,0,1)
l
l = as.logical(l)
l


#########################IDENTIFICANDO TIPO DE CLASSSES######################################

####Função class(objeto)###############

class(c) #retornará "character"

class(z) #retornará "integer"

class(R) #retornará "numeric"

class(l) #retornará "logical"


####Função is.tipodaclasse(objeto) ############### 
is.integer(z) #retornará False

is.integer(R) #retornará FALSE

is.integer(c) #retornará FALSE

is.integer(l) #retornará FALSE


####Função is.na(objeto) ############### 
na = c(1,2,3,NaN,5)
is.na(na)   #retornará FALSE FALSE FALSE TRUE FALSE



#########################COMANDOS LÓGICOS NO R##################################
dados

dados$altura>1.7 #retornará [1] TRUE FALSE TRUE FALSE FALSE

dados$altura>=1.7 #retornará [1] TRUE FALSE TRUE FALSE TRUE

dados$classificacao=="baixo" #retornará [1] FALSE TRUE FALSE TRUE FALSE

!(dados$altura>1.75) #retornará [1] TRUE TRUE FALSE TRUE TRUE (qual altura NÃO é maior que 1.75)

dados$classificacao==dados$altura #retornará [1] FALSE FALSE FALSE FALSE FALSE

dados$classificacao!=dados$altura  #retornará [1] TRUE TRUE TRUE TRUE TRUE

(dados$altura>1.75)||(dados$classificacao=="alto")  #retornará [1] TRUE

(dados$altura>1.75)&&(dados$classificacao=="alto")  #retornará [1] FALSE

(dados$altura>1.75)||(dados$classificacao!="alto")||(dados$altura<1.8)  #retornará [1] TRUE


#Note que ao utilizarmos os operadores || ou && eles só retornam uma resposta, ou
#seja, a comparação só está sendo feita no primeiro elemento dos dois vetores. Isto ocorre
#pois, para estes operadores devemos explicitar o  índice de cada elemento a ser testado.




############################Comandos de decisão - if e else######################################

resposta = character()

if(dados$altura[1]>=1.70){
  resposta[1] = "Alto"
} else{
  resposta[1] = "Baixo"
}

resposta

if(dados$altura[2]>=1.70){
  resposta[2] = "Alto"
} else{
  resposta[2] = "Baixo"
}

if(dados$altura[3]>=1.70){
  resposta[3] = "Alto"
} else{
  resposta[3] = "Baixo"
}

if(dados$altura[4]>=1.70){
  resposta[4] = "Alto"
} else{
  resposta[4] = "Baixo"
}

if(dados$altura[5]>=1.70){
  resposta[5] = "Alto"
} else{
  resposta[5] = "Baixo"
}


dados$altura
resposta



############################Comandos de repetição - for e while######################################

#########Comando for################
resposta2 = character()

for(i in 1:5){
  if(dados$altura[i]>=1.70){
    resposta2[i] = "Alto"
  }else{
    resposta2[i] = "Baixo"
  }
}    

dados$altura
resposta2    
    
######Comando while##################

resposta3 = character()
i = 1 # aqui você tem que inicializar o contador
while(i<=5){
  if(dados$altura[i]>=1.70){
    resposta3[i] = "Alto"
  }
  else{
    resposta3[i] = "Baixo"
  }
  i = i + 1 #para dar sequencia ao contador
}


dados$altura
resposta3 


