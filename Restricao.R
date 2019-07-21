############ Função de Restrição ############

c1 <- function(x){
    sum(x) - 1
}

#####################
# testando
#matriz <- matrix(rep(0.2, 60), ncol = 5)
#matriz
#c1(matriz[1,])  
#c1(matriz[2,])
#c1(matriz[12,])
#matriz[1,] <- c(0.5, 0.2, 0.2, 0.2, 0.2)
#c1(matriz[1,])

# criando a penalização 
pen <- sqrt(.Machine$double.xmax)
#pen
# testes
#penalizacao1 <- max(c1(matriz[2,]), 0)*pen    
#penalizacao1
#penalizacao2 <- max(c1(matriz[1,]), 0)*pen    
#penalizacao2

# testando para o problema em questao com 12 meses e 5 ativos 
#w1 <- rep(0.2, 5)
#w <- c(0.6, 0.2, 0.4, 0.2, 0.7)
#matriz <- matrix(0, ncol = 5, nrow = 12)
#matriz[1:4,] <- w1 
#matriz[5:7,] <- w
#matriz[8:12,] <- w1 
#matriz

#max(c1(matriz[5,]), 0)*pen

# Loop para alocar os pesos   
#for(i in 1:12){
#  penalizacao[i] <- max(c1(matriz[i,]),0)*pen
#}
#-0.1 -sum(penalizacao)
