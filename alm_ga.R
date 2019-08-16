###### TCC - Eduardo
## Carregar Bibliotecas
library(GA)
library(scales)
library(dplyr)
library(tidyr)
library(magrittr)
library(lubridate)
library(ggplot2)
library(Hmisc)
source("Restricao.R")

### Importar dados
Retorno <- read.csv("fundos.csv", header = TRUE, sep = ";")
# Se estiver importando os retornos como factor, rode o código abaixo usando a função `read.csv2`:
# Retorno <- read.csv2("fundos.csv", header = TRUE, sep = ";")

head(Retorno, 10)

######### Análise Exploratória de Dados #########

# visualizando as 6 primeiras linhas
head(Retorno)

# visualizando a estrutura dos dados
str(Retorno)

# Alterando a coluna das data para `data type`
Retorno$data <- as.Date(Retorno$data, format = "%d/%m/%y")

# visualizando as 6 primeiras linhas
str(Retorno)

## Organizar tabela p/ Estatísticas
# Usando o pacote tidyr para empilhar os dados
Fundos_long <- Retorno %>% 
  gather("Ativos", "Retorno", -data)

str(Fundos_long)

# Visualizando as 6 primeiras linhas 
head(Fundos_long)

# Visualizando as 6 últimas linhas
tail(Fundos_long)

# Visualizando as primeiras linhas de todos os ativos
Fundos_long[c(1:10, 132:143, 264:275, 396:407, 528:540),]

# Usando o pacote dplyr para calcular as estatísticas por ativo
Fundos_stats <- Fundos_long %>% 
                group_by(Ativos) %>% 
                summarise(N = n(), Mínimo = min(Retorno), Q25 = quantile(Retorno, 0.25), 
                Média = mean(Retorno), Mediana = median(Retorno), 
                Q75 = quantile(Retorno, 0.75), Máximo = max(Retorno),
                Desvio_Padrão = sd(Retorno))

Fundos_stats

##### Estatística por Ano 
year(Fundos_long$data)

Fundos_stats_Ano <- Fundos_long %>% 
  group_by(Ativos, year(Fundos_long$data)) %>% 
  summarise(N = n(), Mínimo = min(Retorno), Q25 = quantile(Retorno, 0.25), 
            Média = mean(Retorno), Mediana = median(Retorno), 
            Q75 = quantile(Retorno, 0.75), Máximo = max(Retorno),
            Desvio_Padrão = sd(Retorno))

# Visualizar tudo
print(Fundos_stats_Ano, n=55)
####

## Calculando a matriz de correlação e o p-valor 
Retorno %>% 
  select(-data) %>% 
  as.matrix() %>% 
  rcorr()

## Gráfico Histograma
Fundos_long %>% 
  ggplot() +
  geom_histogram(aes(Retorno, fill = Ativos, color = Ativos), alpha = 0.5, bins = 10) +
  labs(x = "", y = "Frequência") +
  facet_wrap(~ Ativos, ncol = 3) +
  ggtitle("Histograma do Retorno dos Fundos") +
  theme_bw()

## Gráfico da Densidade
Fundos_long %>% 
  ggplot() + 
  geom_density(aes(Retorno, fill = Ativos, color = Ativos), alpha = 0.4, adjust = 30) +
  scale_y_continuous(name = "Densidade") +
  ggtitle("Densidade do Retorno dos Fundos")+
  theme_bw()

Fundos_long %>% 
  ggplot() + 
  geom_density(aes(Retorno, fill = Ativos, color = Ativos), alpha = 0.4) +
  labs(x = "", y = "Frequência") +
  facet_wrap(~ Ativos, ncol = 3) +
  ggtitle("Densidade do Retorno dos Fundos")+
  theme_bw()


## Gráfico Box-Plot
Fundos_long %>% 
  ggplot() +
  geom_boxplot(aes(x = Ativos ,y = Retorno, fill = Ativos, color = Ativos), alpha = 0.5) +
  ggtitle("Box-Plot do Retorno dos Fundos")+
  theme_bw()

## Gráfico múltiplas séries
Fundos_long %>% 
  ggplot(mapping = aes(x = data, y = Retorno)) +
  geom_line(mapping = aes(group = 1, color = Ativos)) +
  geom_hline(data = Fundos_stats, aes(yintercept = Média, color = Ativos), 
             linetype = "dashed") +
  facet_wrap(~ Ativos, ncol = 3) + 
  labs(title = "Retorno dos Fundos",
       subtitle = "",
       y = "Retorno", x = "") + 
  theme_bw()

## Gráfico de dispersão em que o eixo X representa a Volatilidade (desvio-padrão) e o eixo Y representa o Retorno
Fundos_stats %>% 
  ggplot(mapping = aes(x = Desvio_Padrão, y = Média, color = Ativos)) +
  geom_point(size = 5) +
  theme_bw() + ggtitle("Risco x Retorno") +
  xlab("Volatilidade") + ylab("Retorno") +
  scale_y_continuous(limits = c(0, 0.02)) +
  scale_x_continuous(limits = c(0, 0.07))

ggplot(matriz_medias_dp, aes(x = vetor_dp, y = vetor_medias, color = Ativos)) +
  geom_point(size = 5) +
  theme_bw() + ggtitle("Risco-Retorno") +
  xlab("Volatilidade") + ylab("Retorno") +
  scale_y_continuous(limits = c(0, 0.015)) +
  scale_x_continuous(limits = c(0, 0.08))



######## Retorno Acumulado ########
## Aplicando a função do retorno acumulado às colunas dos fundos
Retorno_Acumulado <- cbind(data = Retorno$data, 
                           as.data.frame(sapply(Retorno[,-1], function(x) cumprod(1 + x) - 1)))

# Visualizando a estrutura dos dados
str(Retorno_Acumulado)

# Organizar tabela p/ Estatísticas do acumulado 
# Usando o pacote tidyr para empilhar os dados
Retorno_Acumulado_long <- Retorno_Acumulado %>% 
  gather("Ativos", "Retorno", -data)

# visualizando a estrutura 
str(Retorno_Acumulado_long)

# 6 primeiras e 6 últimas linhas
head(Retorno_Acumulado_long)
tail(Retorno_Acumulado_long)

# Tabela com estatísticas
Retorno_Acumulado_stats <- Retorno_Acumulado_long %>% 
  group_by(Ativos) %>% 
  summarise(N = n(), Mínimo = min(Retorno), Q25 = quantile(Retorno, 0.25), 
            Média = mean(Retorno), Mediana = median(Retorno), 
            Q75 = quantile(Retorno, 0.75), Máximo = max(Retorno))

Retorno_Acumulado_stats

## Gráfico múltiplas séries

Retorno_Acumulado_long %>% 
  ggplot(mapping = aes(x = data, y = Retorno)) +
  geom_line(mapping = aes(group = 1, color = Ativos)) +
  geom_hline(data = Retorno_Acumulado_stats, aes(yintercept = Média, color = Ativos), 
             linetype = "dashed") +
  facet_wrap(~ Ativos, ncol = 3) + 
  labs(title = "Retorno Acumulado dos Fundos",
       subtitle = "",
       y = "Retorno", x = "") + 
  theme_bw()

## Gráfico Box-Plot
Retorno_Acumulado_long %>% 
  ggplot() +
  geom_boxplot(aes(x = Ativos ,y = Retorno, fill = Ativos, color = Ativos), alpha = 0.5) +
  ggtitle("Box-Plot do Retorno Acumulado dos Fundos")+
  theme_bw()





################################ ALM ########################################
#### Simulando ALM para todas os ativos usando a abordagem do trabalho de Amaral (2010)
# Usaremos 100 iterações para testar
# Função objetivo para minimizar a probabilidade de ruína
ALM <- function(X){

N_iter <- 1000 # quantidade de iterações simulações
Tam <- 12  # período anual
A0 <- 202714698 # Valor inicial do Ativo - colocar no mínimo o valor da RM0
FC_r <- c(515632, 515632, 515632, 515632, 515632, 515632, 515632, 515632, 515632, 515632, 515632, 515632) # Fluxo de Caixa financeiro esperado de contribuições futuras - benefícios futuros
RM_r_1 <- 232326306
RM_r <- A0
dif <- RM_r_1 - A0
vet <- rep(dif/12, 13) # teste
vet[1] <- A0
RM_r <- cumsum(vet) # Reserva Matemática dos FC_r
  
## Calculo do período (em meses) em que a carteira é revista
# como o tamanho do vetor 60, temos 12 ponderações e dividimos por 5 (quantidade de ativos)
#Xativo1 <- X[,1]
#Xativo2 <- X[,2]
#Xativo3 <- X[,3]
#Xativo4 <- X[,4]
#Xativo5 <- X[,5]

tam <- max(length(X))/5
periodo <- Tam/tam
reav <- data.frame(0)
reav[1,1] <- 1
for(t in seq(2,Tam)){
  reav[t,] <- round(reav[t-1,1]+periodo)
}
# Estrutura do ativo 1
Xativo1 <- rep(0,Tam)
Xativo1[1] <- X[1]  
v <- 1
for(t in seq(2,Tam)){
    v <- v + 1
    Xativo1[t] <- X[v]
}

# Estrutura do ativo 2
Xativo2 <- rep(0,Tam)
Xativo2[1] <- X[v+1]  
v <- v+1
for(t in seq(2,Tam)){
  v <- v + 1
  Xativo2[t] <- X[v]
}

# Estrutura do ativo 3
Xativo3 <- rep(0,Tam)
Xativo3[1] <- X[v+1]
v <- v+1
for(t in seq(2,Tam)){
  v <- v + 1
  Xativo3[t] <- X[v]
}

# Estrutura do ativo 4
Xativo4 <- rep(0,Tam)
Xativo4[1] <- X[v+1]
v <- v+1
for(t in seq(2,Tam)){
  v <- v + 1
  Xativo4[t] <- X[v]
}

# Estrutura do ativo 5
Xativo5 <- rep(0,Tam)
Xativo5[1] <- X[v+1]
v <- v+1
for(t in seq(2,Tam)){
  v <- v + 1
  Xativo5[t] <- X[v]
}

X <- round(cbind(Xativo1, Xativo2, Xativo3, Xativo4, Xativo5), 2) # arredondei para convergir
#X <- matrix(X, ncol = 5, nrow = 12, byrow = TRUE)
linhameses <- c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez")
rownames(X) <- linhameses
colnames(X) <- colnames(Retorno[-1])
cat("\n")
cat("---------------- ALTERANDO OS PESOS DA CARTEIRA ----------------------")
cat("\n")
cat("Pesos da carteira :", X)
cat("\n")
#### 2 parte
# Importando a matriz de correlação dos ativos 
COR <- cor(Retorno[,-1]) # Matriz de correlação do retorno dos ativos
COR_Chol <- chol(COR) # Decomposição de Cholesky
# Criando a array para realizar a simulação com 1000 iterações
# Vamos usar a média e o desvio-padrão amostral, pois em todos os ativos a distribuição que melhor se ajustou aos dados foi a Normal.
# Ainda, vamos captar a correlação entre os ativos e usar a Decomposição (Fatoração) de Cholesky.
# Para mais informações sobre a decomposição de Cholesky: http://prorum.com/?qa=2874/o-que-e-a-decomposicao-de-cholesky
# Criaremos uma distribuição normal multivariada com o vetor de médias e uma matriz de covariância.
arrayteste <- array(NA, dim = c(12, 5, N_iter), dimnames = list(linhameses, colnames(Retorno[-1])))
iter <- 1
Inad <- rep(0, N_iter)
# selecionando a seed para reproduzir
set.seed(1)
repeat{
  for (i in seq(1,12)){
    vetorRandn <- rnorm(5)
    vetorCorrRandn <- vetorRandn * COR_Chol
    arrayteste[i,1,iter] <- mean(Retorno$Renda.fixa.TPF) + vetorCorrRandn[1,1] * sd(Retorno$Renda.fixa.TPF)
    arrayteste[i,2,iter] <- mean(Retorno$Renda.fixa.ref.ind) + vetorCorrRandn[1,2] * sd(Retorno$Renda.fixa.ref.ind)
    arrayteste[i,3,iter] <- mean(Retorno$Renda.fixa.e.índice) + vetorCorrRandn[1,3] * sd(Retorno$Renda.fixa.e.índice)
    arrayteste[i,4,iter] <- mean(Retorno$Ações) + vetorCorrRandn[1,4] * sd(Retorno$Ações)
    arrayteste[i,5,iter] <- mean(Retorno$Multimercado) + vetorCorrRandn[1,5] * sd(Retorno$Multimercado)
  }
  # Matriz dos retornos
  R <- arrayteste[,,iter]
  # Retorno do portfólio
  RX <- R*X
  Rport <- apply(RX, 1, sum)
  ### 3 parte - Dinamica entre Ativos e passivos
  ## Fluxo de Caixa Atuarial
  #### Dinâmica entre Ativos e Passivos
  # Fluxo de caixa atuarial e Reserva Matemática na iter-ésima simulação
  FC <- FC_r 
  RM <- RM_r
  # Este exemplo serve para apenas 1 simulação 
  # Valor dos ativos A em cada instante do tempo e valor do superávit S em cada instante do tempo na iter-ésima simulação
  A <- rep(0, 12)
  A[1] <- A0 * (1+Rport[1]) + FC[1]
  S <- rep(0, 12)
  S[1] <- A[1] - RM[1]
  A[2] <- A[1] * (1+Rport[2]) + FC[2]
  S[2] <- A[2] - RM[2]
  for(t in 3:12){
    A[t] <- A[t-1]*(1+Rport[t]) + FC[t]
    S[t] <- A[t] - RM[t]
  }
  # Valor de resposta para a função objetivo
  # Vetor indicador Inad - Insolvência de ocorrência real de inadimplencia, em todas as 1000 iterações
  Inad[iter] <- sum(S<0)>0
  iter <- iter + 1
  if(iter == N_iter){
    break
  }
}
# Termina o loop das 1000 iterações
# Valor da Função Objetivo
# Percentual de ocorrências de insolvência nas 1.000 simulações
  cat("\n")
  cat("------------------------ PROBABILIDADE DE INSOLVÊNCIA ------------------------------")
  cat("\n")
  cat("iteração :", iter)
  cat("\n")
  prob_insolv <- sum(Inad)/N_iter
  cat("Probabilidade de Insolvência :", prob_insolv)
  cat("\n")
# Agora vamos calcular a função de penalização - X1 + X2 + X3 + X4 + X5 == 1
  cat("Calculando a função de restrição")
  cat("\n")
# Inicializando o vetor de Penalização
  penalizacao <- rep(0, 12)
# Loop para alocar os pesos   
  for(i in 1:12){
    penalizacao[i] <- max(c1(X[i,]), 0)*pen
  }
# 
  ### Funcao objetivo com a restricao 
  return(- prob_insolv - sum(penalizacao))
}
########## COLOQUEI NEGATIVA A PROB DE INSOLVÊNCIA PARA TENTAR MAXIMIZAR PELOS GA

# Testando com pesos somados = 1
X <- rep(0.2, 60)
obj <- ALM(X)
obj

# Testando com pesos somados != 1
X <- rep(0.262, 60)
obj <- ALM(X)
obj

# Testando com pesos somados = 1 apenas no arrendodamento - o arrendodamento serviu para ver se a combinação de pop gerada consegue convergir para 1.
X <- rep(0.5, 60)
obj <- ALM(X)
obj

X <- rep(0, 60)
obj <- ALM(X)
obj

X <- rep(0.1, 60)
obj <- ALM(X)
obj

X <- c(rep(0.2,36),rep(0.1, 24))
obj <- ALM(X)
obj
##### OK


# simulações de carteiras ingênuas
x1 <- rep(c(1,0,0,0,0), each = 12)
ALM(x1)
x2 <- rep(c(0.5,0.5,0,0,0), each = 12)
ALM(x2)
x3 <- rep(c(0.3334, 0.3333, 0.3333, 0, 0), each = 12)
ALM(x3)
x4 <- rep(c(0.3,0.3,0.3,0,0.1), each = 12)
ALM(x4)
x5 <- rep(c(0.2667, 0.26, 0.26, 0.2, 0), each = 12)
ALM(x5)


############## Algoritmos Genéticos ##############
### gerando a sugestão
#X <- c(rep(0.2,36),rep(0.1, 24))

### Implementando o algoritmo genético com o mínimo e o máximo de alocação para cada ativo
# máximo de iterações = 10 e tamanho da população = 600
otimizando <- ga(type = "real-valued", fitness = ALM, 
         lower = rep(c(0, 0, 0, 0, 0), each = 12), 
         upper = rep(c(1, 0.6, 0.4, 0.2, 0.1), each =  12), popSize = 200, 
         maxiter = 2, maxFitness = 0, names = rep(paste0('X', 1:5), each = 12),
         keepBest = TRUE, seed=12345)

# apresentando os resultados 
summary(otimizando)
otimizando@solution
otimizando@fitnessValue
otimizando@population
plot(otimizando)
plot(otimizando, log = "x")
otimizando@solution

x <- otimizando@solution 
obj <- ALM(x)
obj
solucao <- x
# escrevendo resultados em .csv
#solucao <- matrix(otimizando@solution, nrow = 12, byrow = TRUE)
#colnames(solucao) <- names(Retorno[,-1])
#linhameses <- c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez")
#rownames(solucao) <- linhameses
#solucao
#apply(round(solucao,1), 1, sum)
write(solucao, file = "solucao.csv")

## Testando com apenas dois ativos
#otimizando <- ga(type = "real-valued", fitness = ALM, 
#                 lower = rep(c(0, 0, 0, 0, 0), each = 12), 
#                 upper = rep(c(1, 0, 0, 0.3, 0), each =  12), popSize = 120, 
#                 maxiter = 1, maxFitness = 0, names = rep(paste0('X', 1:5), each = 12),
#                 keepBest = TRUE, seed=12345)

# apresentando os resultados 
#summary(otimizando)
#otimizando@solution
#otimizando@fitnessValue
#otimizando@population
#plot(otimizando)
#plot.ga(otimizando)
#otimizando@solution

## Testando com apenas 3 ativos
#otimizando <- ga(type = "real-valued", fitness = ALM, 
#                 lower = rep(c(0, 0, 0, 0, 0), each = 12), 
#                 upper = rep(c(1, 0.3, 0, 0.3, 0), each =  12), popSize = 120, 
#                 maxiter = 1, maxFitness = 0, names = rep(paste0('X', 1:5), each = 12),
#                 keepBest = TRUE, seed=12345)

# apresentando os resultados 
#summary(otimizando)
#otimizando@solution
#otimizando@fitnessValue
#otimizando@population
#plot(otimizando)
#plot.ga(otimizando)
#otimizando@solution

# Otimizando com 4 ativos
otimizando <- ga(type = "real-valued", fitness = ALM, 
                 lower = rep(c(0, 0, 0, 0, 0), each = 12), 
                 upper = rep(c(1, 0.6, 0.4, 0.2, 0.1), each =  12), popSize = 120, 
                 maxiter = 1, maxFitness = 0, names = rep(paste0('X', 1:5), each = 12),
                 keepBest = TRUE, seed=12345)

# apresentando os resultados 
summary(otimizando)
otimizando@solution
otimizando@fitnessValue
otimizando@population
#plot(otimizando)
#plot.ga(otimizando)
otimizando@solution



################ Resultados e Gráficos ###################

#### organizando os dados

### Criando uma tabela com o vetor de médias dos retornos e do desvio-padrão para os 5 ativos
vetor_medias <- apply(Retorno[,-1], 2, mean)
vetor_dp <- apply(Retorno[,-1], 2, sd)
matriz_medias_dp <- data.frame(cbind(vetor_medias, vetor_dp))
rownames(matriz_medias_dp) <- NULL
matriz_medias_dp
matriz_medias_dp <- cbind(Ativos = colnames(Retorno[-1]), matriz_medias_dp)
matriz_medias_dp

## Gráfico de dispersão em que o eixo X representa a Volatilidade (desvio-padrão) e o eixo Y representa o Retorno
ggplot(matriz_medias_dp, aes(x = vetor_dp, y = vetor_medias, color = Ativos)) +
  geom_point(size = 5) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Risco x Retorno") +
  xlab("Volatilidade") + ylab("Retorno") +
  scale_y_continuous(limits = c(0, 0.015)) +
  scale_x_continuous(limits = c(0, 0.08))

#### gráfico dos ativos para um ano e da carteira otimizada
### criando uma tabela com o modelo de projeção dos retornos por meio de simulação com os retornos médios para 12 meses
linhameses <- c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez")

# Matriz de correlação do retorno dos ativos
COR <- cor(Retorno[,-1]) 

# Decomposição de Cholesky
COR_Chol <- chol(COR) 

# array onde as linhas representam os meses, enquanto que as colunas são os ativos
arrayteste <- array(NA, dim = c(12, 5, 1000), dimnames = list(linhameses, colnames(Retorno[-1])))

# selecionando uma seed para manter a reprodutibilidade
set.seed(1)

# Simulação de Monte Carlo n=1000
for(j in seq(1,1000)){
  for (i in seq(1,12)){
  vetorRandn <- rnorm(5)
  vetorCorrRandn <- vetorRandn * COR_Chol
  arrayteste[i,1,j] <- mean(Retorno$Renda.fixa.TPF) + vetorCorrRandn[1,1] * sd(Retorno$Renda.fixa.TPF)
  arrayteste[i,2,j] <- mean(Retorno$Renda.fixa.ref.ind) + vetorCorrRandn[1,2] * sd(Retorno$Renda.fixa.ref.ind)
  arrayteste[i,3,j] <- mean(Retorno$Renda.fixa.e.índice) + vetorCorrRandn[1,3] * sd(Retorno$Renda.fixa.e.índice)
  arrayteste[i,4,j] <- mean(Retorno$Ações) + vetorCorrRandn[1,4] * sd(Retorno$Ações)
  arrayteste[i,5,j] <- mean(Retorno$Multimercado) + vetorCorrRandn[1,5] * sd(Retorno$Multimercado)
  }
  arrayteste[,,j]
}

### Matriz dos retornos médios a partir da simulação 
str(arrayteste)
# Conferindo se o método para extração da média está correto
mean(arrayteste[1,1,1:100]) == apply(arrayteste[,1,], 1, mean)[1]

R <- as.data.frame(matrix(NA, ncol = 5, nrow = 12))

for(i in 1:5){
  R[,i] <- apply(arrayteste[,i,], 1, mean)
}

print(R)

colnames(R) <- colnames(Retorno[-1])
# Visualizando
print(R)


# Matriz com os pesos otimizados pelo Algoritmos Genéticos com o objetivo de reduzir a Probabilidade de Insolvência (Ruína)

X <- read.csv("solucao.csv", sep = "", header = F)
print(X)
#X <- otimizando@solution[1,]
apply(X, 1, sum)

# Matriz com os pesos iguais 
X0_2 <- rep(0.2, 60)

## Retorno do carteira otimizado pelos GA
RX <- R*X
Rport <- apply(RX, 1, sum)
Rport <- unname(Rport)
Rport <- data.frame(mes = linhameses ,Retorno_Carteira_GA = Rport)
Rport
Rport$mes <- factor(Rport$mes, levels = linhameses)
Rport$mes
Rport

## Retorno da carteira com pesos iguais
RX0_2 <- R*X0_2
Rport0_2 <- apply(RX0_2, 1, sum)
Rport0_2 <- unname(Rport0_2)
Rport0_2

## Anexando a carteira com pesos iguais ao data frame com a carteira otimizada
Rport <- cbind(Rport, Retorno_Carteira_Pesos_Iguais = Rport0_2)
str(Rport)
Rport


## Transformando a tabela larga em longa
Ret_Carteiras <- Rport %>% 
  gather("Carteiras", "Retorno", - mes)
Ret_Carteiras

## Calculando as estatísticas descritivas das carteiras
Ret_Carteiras_stats <- Ret_Carteiras %>% 
  group_by(Carteiras) %>% 
  select(Carteiras, Retorno) %>% 
  summarise(N = n(), Média = mean(Retorno), DesvioPadrão = sd(Retorno), 
            Mínimo = min(Retorno), Máximo = max(Retorno))

Ret_Carteiras_stats

# Gráfico de linha da simulação do portfólio ao longo do ano
Ret_Carteiras %>% 
  ggplot(mapping = aes(x = mes, y = Retorno)) +
  geom_line(mapping = aes(group = 1, color = Carteiras)) +
  geom_hline(data=Ret_Carteiras, aes(yintercept=mean(Retorno), color=Carteiras), 
             linetype="dashed") +
  facet_wrap(~ Carteiras, ncol = 2) +
  labs(title = "Retorno da Carteira",
                  subtitle = "",
                  y = "", x = "") + 
  theme_bw()

# Gráfico das distribuições dos retornos das carteiras 
Ret_Carteiras %>% 
  ggplot() + 
  geom_density(aes(Retorno, fill = Carteiras, color = Carteiras), alpha = 0.5, adjust = 1/2) +
  scale_y_continuous(name = "Densidade") +
  ggtitle("Densidade do Retorno das Carteiras")+
  theme_bw()

### Criando uma tabela para os ativos 
tabela <- as.data.frame(R)
tabela
rownames(tabela) <- NULL
colnames(tabela) <- names(Retorno[,-1])
tabela <- cbind(mes = linhameses, tabela)
str(tabela)
tabela
tabela$mes <- factor(tabela$mes, levels = linhameses)
tabela$mes
tabela

## Estruturando a tabela
Ret_Ativos_Mes <- tabela %>% 
gather("Ativos", "Retorno", -mes)
Ret_Ativos_Mes

# Estatísticas Descritivas
Ret_Ativos_Mes_stats <- Ret_Ativos_Mes %>% 
  group_by(Ativos) %>% 
  select(Ativos, Retorno) %>% 
  summarise(N = n(), Mínimo = min(Retorno), Q25 = quantile(Retorno, 0.25), 
            Média = mean(Retorno), Mediana = median(Retorno), 
            Q75 = quantile(Retorno, 0.75), Máximo = max(Retorno),
            Desvio_Padrão = sd(Retorno))

Ret_Ativos_Mes_stats

## Gráfico para todos os ativos 
Ret_Ativos_Mes %>% 
  ggplot(mapping = aes(x = mes, y = Retorno)) +
  geom_line(mapping = aes(group = 1, color = Ativos)) +
  geom_hline(data = Ret_Ativos_Mes_stats, aes(yintercept = Média, color = Ativos), 
             linetype = "dashed") +
  facet_wrap(~ Ativos, ncol = 5) + 
  labs(title = "Retorno dos Fundos",
       subtitle = "",
       y = "Retorno", x = "") + 
  theme_bw()


######################## Retorno Acumulado da Carteira ###############
# Retorno Acumulado Carteira GA 
Retorno_Acumulado_Carteira_GA <- cumprod(1+Rport[,2])-1
Retorno_Acumulado_Carteira_GA

# Retorno Acumulado Carteira Pesos iguais
Retorno_Acumulado_Carteira_Pesos_Iguais <- cumprod(1+Rport0_2)-1
Retorno_Acumulado_Carteira_Pesos_Iguais

Retorno_Acumulado_Carteiras <- data.frame(mes = linhameses, 
                                          Retorno_Acumulado_Carteira_GA = Retorno_Acumulado_Carteira_GA, 
                                          Retorno_Acumulado_Carteira_Pesos_Iguais = Retorno_Acumulado_Carteira_Pesos_Iguais)
Retorno_Acumulado_Carteiras
str(Retorno_Acumulado_Carteiras)
Rport$mes <- factor(Rport$mes, levels = linhameses)
Rport$mes

Retorno_Acumulado_Carteiras[12,] # Retorno acumulado em dezembro

# transformando em longo
Retorno_Acumulado_Carteiras <- Retorno_Acumulado_Carteiras %>% 
  gather("Carteira", "Retorno_Acumulado",-mes) 

# Gráfico de linha do portfólio retorno acumulado ao longo do ano
Retorno_Acumulado_Carteiras %>% 
  ggplot(mapping = aes(x = mes, y = Retorno_Acumulado)) +
  geom_line(mapping = aes(group = 1, color = Carteira)) +
  geom_hline(data=Retorno_Acumulado_Carteiras, aes(yintercept=mean(Retorno_Acumulado), color=Carteira), 
             linetype="dashed") +
  facet_wrap(~ Carteira, ncol = 2) +
  labs(title = "Retorno Acumulado da Carteira",
       subtitle = "",
       y = "", x = "") + 
  theme_bw()




