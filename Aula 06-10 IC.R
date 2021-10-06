#RESOLUÇÃO DO EXEMPLO 1

#DADOS DO PROBLEMA:

#Desvio-padrao = 5
#n = 100
#Media = 500
#Nivel de confianca: 1 - alpha = 95% => nivel de significancia (alpha): 1 - 0,95 = 0,05

#Pergunta: I.C. para Media?
#------------------------------------------------------------------------------------------

#MODO 1
xbarra = 500
n = 100
sigma = 5
alpha = 0.05

#Valor critico modo separado
esq = (1-alpha)/2
dir = 1-((1-alpha)/2)
vc1esq = qnorm(esq) #valor critico a esquerda
vc1dir = qnorm(dir) #valor critico a direita
c(vc1esq,vc1dir)

#Modo unificado
valor_critico = qnorm(c((1-alpha)/2, 1-((1-alpha)/2)));valor_critico #valores criticos

#Intervalo de Confianca para a Media
icm1 = xbarra + valor_critico * (sigma/sqrt(n));icm1
#------------------------------------------------------------------------------------------

#MODO 2 (Construindo funcao)
funcao_icm1 <- function(xbarra,sigma,n,alpha){
  valor_critico = qnorm(c((1-alpha)/2, 1-((1-alpha)/2)))
  icm1 = xbarra + valor_critico * (sigma/sqrt(n))
  return(icm1)
}
funcao_icm1(500,5,100,0.05)
#===================================================================================================


#===================================================================================================
#RESOLUÇÃO DO EXEMPLO 2
#===================================================================================================

#DADOS DO PROBLEMA:

#amostra = (9, 8, 12, 7, 9, 6, 11, 6, 10, 9)
#Nível de Confianca: 1 - alpha = 95%

#Pergunta: I.C. para Media?

#O valor de alpha ja foi passado no problema:
#Nível de Confianca: 1 - alpha = 95%
#Nivel de significancia (alpha) = 1 - 0,95 = 0,05
alpha = 0.05

#1) Obter a media da amostra
amostra = c(9, 8, 12, 7, 9, 6, 11, 6, 10, 9)
amostra = c(52,44,55,44,45,59,50,54,62,46,54,58,60,62,63)
xbarra = mean(amostra);xbarra

#2) Obter o desvio-padrao da amostra
s = sd(amostra);s

#3) Obter o tamanho da amostra
n = length(amostra);n

#------------------------------------------------------------------------------------------
#MODO 1

#Valor critico modo separado
esq = 1-(alpha/2)
dir = 1-(1-(alpha/2))
vc2esq = qt(esq,n-1,lower.tail = FALSE) #valor critico a esquerda
vc2dir = qt(dir,n-1,lower.tail = FALSE) #valor critico a direita
c(vc2esq,vc2dir)

#Modo unificado
vc2 = qt(c(1-(alpha/2),1-(1-(alpha/2))),n-1,lower.tail = FALSE);vc2 #valores criticos

#Intervalo de Confianca
icm2 = xbarra + vc2 * (s/sqrt(n));icm2
#------------------------------------------------------------------------------------------

#MODO 2 (Construindo funcao)
funcao_icm2 <- function(xbarra,s,n,alpha){
  vc2  = qt(c(1-(alpha/2),1-(1-(alpha/2))),n-1,lower.tail = FALSE)
  icm2 = xbarra + vc2 *(s/sqrt(n))
  return(icm2)
}
funcao_icm2(xbarra,s,n,alpha)
