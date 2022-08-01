#séries temporais
install.packages('fpp2')
library(fpp2)


#ARIMA
rm(list = ls()) #limpa a memória do R
x = 1:10
length(x) #tamanho de x
length(diff(x)) #diff toma a diferença. O lenght da diferença é 9
#Diferença de séries não estacionarias torna a série 
#estacionária. A segunda diferença ajusta
#ainda mais o modelo

fit = auto.arima(WWWusage)
class(WWWusage) #o WWW é uma série temporal
plot(forecast(fit,h=20)) 
fit
#O modelo ARIMA(1,1,1) possui uma parte auto regressiva
#Quantos passos para tras vc olha para predizer quantos passos para frente
#O modelo se auto explica, olhando para tras
#A média móvel tem a ver com a média de acordo com o tempo 
plot(diff(WWWusage))
#O diff prova que o método funciona para estacionariedade
plot(forecast(fit,h=50))

#ARIMA é o método mais antigo, porém muito bem planejado

#############################################




#ETS - Modelo de espaço de estados 

#TBATS - Modelo de espaço de esstsados com 
#suavização exponencial com transformção de 
#box cox(modelo de decaimento de séries ...)
#com erros ARMA e componentes sazionais de 
#tendencias ---- SE AJUSTA BEM PARA SERIES COM ALTA VARIABILIDADE(SAZONALIDADE)

#NNETAR - Rede neural autoregressiva - 

#nao existe um modelo campeao 
# a função fits 
# pega 80% inicial da série e ajusta para cada 
#um desses 4 modelos
#é um treino teste

###########################################
#Exemplo da função fits
#Função retirada do pacote JURIMETRICS do prof. Filipe Zabala

#instalando e chamando o pacote
install.packages('devtools')
library(devtools)

devtools::install_github('filipezabala/jurimetrics',force=T)
library(jurimetrics)
#exemplo
par(mfrow = c(2,2)) #configurar a tela para exibir multiplos graficos

plot(livestock)
plot(diff(livestock))
fits(livestock, show.sec.graph=T)
# a melhor série temporal foi o ETS
#O arima fez uma predição 0 parte regressiva 1 parte diferença e 0 parte media movel. Não ficou ruim

#O modelo com o menor erro quadratico médio de predição
# é o que melhor se ajusta para os dados


# Exemplo de jurimetria
data(tjrs_year_month)
tail(tjrs_year_month)
fits(tjrs_year_month$count)


##########################################
#Exemplo: Considere o banco de dados do índice Dow Jones 
#disponível em
#https://archive.ics.uci.edu/ml/datasets/Dow+Jones+Index, 
#lido com o código abaixo
library(readr)
url1 <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/00312/dow_jones_index.zip'
download.file(url1, 'temp.zip', mode = 'wb')
dat <- read_csv('temp.zip')
st <- ts(dat$percent_change_next_weeks_price)
dat
st
?fits
fits(dat$percent_change_next_weeks_price,show.sec.graph = T,train = 0.90, max.points = 1200 )
fits(dat$percent_change_next_weeks_price,show.sec.graph = T,train = 0.50, max.points = 300 )

# modificando os valores para porcentagem testada e 
#quantidade de pontos mudamos os modelos de predição para as series temporais


summary(dat)


######################################################################
#IMPACTO CAUSAL
#quanto uma coisa causa a outra
#existe um pacote do R que utiliza inferencia baysiana para medir causalidade
#Se uma ação possui efeito, se valeu a pena ou não, aplicar a serie temporal


###########################################################



