# Exercício baseado na aula de Deep Learning do MBA USP ESALQ do professor Jeronimo
# Implementando a  busca automática do código da ação na bolsa, pacot quantmod
# E a impressão do gráfico predito contra o teste

##################################################################################
#                  INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS             #
##################################################################################
#Pacotes utilizados
pacotes <- c("MASS","neuralnet","ISLR","mlbench","neuralnet","rpart","dplyr", "rnn", "quantmod")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
  
# Buscando dados da ação
  
acao = "KLBN4.SA"
inicio <- "2018-12-05"
final <- "2023-12-05"
#final = Sys.Date()   #Poderia ver até a data atual.

dfAcao <- getSymbols(Symbols=acao, from=inicio, to=final, env = NULL, return.class="data.frame")

fechamento <- dfAcao$KLBN4.SA.Close

# Vamos treinar baseado no valor anterior   y(t) = f(y()t-1))
# Prevendo valor de hoje baseado no valor de ontem.

fechamento_anterior <- lead(fechamento, n=1L)

data_analise <- data.frame(fechamento)
data_analise$fechamento_anterior <- fechamento_anterior

summary(data_analise)

data_analise <- data_analise[15:1239,]    # para excluir ultima linha com NA, e deixar multiplo de 25
x <- data_analise[,1]
y <- data_analise[,2]

X <- matrix(x, nrow = 49)
Y <- matrix(y, nrow = 49)

#Aqui alterando os valores para valor de 0 a 1
Yscale <- (Y - min(Y)) / (max(Y) - min(Y))
Xscale <- (X - min(X)) / (max(X) - min(X))
Y <- Yscale
X <- Xscale

train=1:19
test=20:25

set.seed(12)

model <- trainr(Y = Y[,train],
                X = X[,train],
                learningrate = 0.05,
                hidden_dim = 20,
                numepochs = 1000,
                network_type = "rnn"
                )

Ytrain <- t(matrix(predictr(model, X[,train]), nrow=1))
Yreal <- t(matrix(Y[,train],nrow=1))

rsq <- function(y_actual,y_predict){
       cor(y_actual,y_predict)^2
}

rsq(Yreal,Ytrain)   


plot(Ytrain, type ="l", col = "darkred")
lines(Yreal, col = "darkblue", typer = "l")

#Acuracia
Ytest <- matrix(Y[,test],nrow=1)
Ytest <- t(Ytest)

Yp <- predictr(model, X[,test])
Ypredicted <- matrix(Yp, nrow =1)
Ypredicted <- t(Ypredicted)

result_data <- data_frame(Ytest)
result_data$Ypredicted <- Ypredicted

rsq(result_data$Ytest, result_data$Ypredicted)

mean(result_data$Ytest)
mean(result_data$Ypredicted)

rsq(result_data$Ytest, result_data$Ypredicted)


# Aqui plotando o teste com predito
plot(result_data$Ytest, type ="l", col = "darkred")
lines(result_data$Ypredicted, col = "darkgreen", type = "l")


