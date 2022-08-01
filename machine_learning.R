#APRENDIZADO DE MÁQUINA
#Modelagem algorítmica 


#Atualmente poussimos muitas variáveis para os problemas
# Desta maneira, foi realizado um estudo para qualificar essas variaveis
#Iremos colocar pesos nas variaveis
#Através de algebra matricial
#Autovalores(variancia de cada uma das colunas criadas)
#e autovetores(colunas ponderas, com os pesos considerados)
#A partir dos autovalores, decidimos quais os autovetores
#mais importantes a partir deste método

df = iris[-5] #banco de dados das sépalas
(m = colMeans(df)) #vetor de médias

(S = cov(df)) #matriz de covariância

eigen(S) #autovalores(variancias) e autovetores de S
#As coluna apresentadas contem os pesos(autoveores)
#E os #values autovalores são a variancia de cada coluna
#Quanto maior a variancia maior a variabilidade capturada
#Ou seja, a primeira coluna já explica consideravelmente o bando de daods

(av=prcomp(df)) #essa função ja calcula diretamente
#ela retorna o desvio padrão.. raiz quadrada da variancia

par(mfrow = c(3,2)) # modifica a tela para caber mais gráfico 3 linhas e 2 colunas

require(gridExtra)
library(ggplot2)
install.packages('gridExtra')

p1 = ggplot(iris,aes(Sepal.Length,Sepal.Width, colour = Species)) +geom_point()
p2 = ggplot(iris,aes(Sepal.Length,Petal.Length, colour = Species)) +geom_point()
p3 = ggplot(iris,aes(Sepal.Length,Petal.Width, colour = Species)) +geom_point()
p4 = ggplot(iris,aes(Sepal.Width,Petal.Length, colour = Species)) +geom_point()
p5 = ggplot(iris,aes(Sepal.Width,Petal.Width, colour = Species)) +geom_point()
p6 = ggplot(iris,aes(Sepal.Length,Petal.Width, colour = Species)) +geom_point()
grid.arrange(p1,p2,p3,p4,p5,p6,ncol=2)


install.packages('ggfortify')
library(ggfortify)
autoplot(prcomp(df), data = iris, colour = 'Species', loadings = T, loadings.label = T, type = 'raw')
#Gráfico de componentes principais pr.. 
#Quantos porcento da variabilidade eu tenho em relação a todas 
#as variancias admitidas
#o eixo X explica praticamente todo o modelo
#É UMA MANEIRA DE SEGMENTAR O BANCO DE DADOS

########################################################
#TÉCNICAS DE AGRUPAMENTO
#útei para realizar agregação de objetos similares
#######################################################
#Medidas de similaridade e dissimilaridade
#é comoum utilizar padronização através da função base::scale

# criando data frame 'df' para os exemplos a seguir
df <- data.frame(V1=c(3,1), V2=c(2,4))  # vetores V1 e V2
rownames(df) <- c('x','y')  # rótulo das linhas
df
# padronizando os dados
df.s <- scale(df)
df.s

#Distancia euclicinada
sqrt(sum((df[1,]-df[2,])^2))  # distância euclidiana aplicando
dist(df, method = 'euclidean')  # distância euclidiana via 'dist'
dist(df.s, method = 'euclidean')  # distância euclidiana via 'dist' dos valores padronizados

#distancia de minkowski
sum((abs(df[1,]-df[2,]))^5)^(1/5)     # distância de Minkowski com p=5 aplicando a eq
dist(df, method = 'minkowski', p = 5) # distância de Minkowski com p=5 via 'dist'
dist(df.s, method = 'minkowski', p = 5) # dist. de Minkowski com p=5 via 'dist' dos valores padronizados




#MÉTODOS HIERÁRQUICOS
#estrutura de árvore onde as folhas se aglomeram em nós que são ligados por uma grande raiz

#Algoritmo de agrupamento hierárquico aglomerativo
#Utilizando a base de dados USArrests
# 1. padronizando os dados
USArrest.scale <- scale(USArrests)
# 2. calculando distâncias (utilizando o padrão: euclidean)
dUSA <- dist(USArrest.scale)
# 3. aplicando a função de ligação (utilizando o padrão: complete)
hc <- hclust(dUSA)
# 4. apresentando o gráfico
plot(hc)

#a distância no eixo y é a distancia cofenética
#a função cophenetic visa obter tais distâncias
#maior valor maior a dissimilaridade
#correlação elevada entre as distancias calculadas e as distancias 
#cofeneticas sugere um bom agrupamento
# Calcula a matriz de distâncias cofenéticas
coph <- cophenetic(hc)
sort(unique(coph))

# Correlação entre as distâncias cofenéticas e as distâncias originais (maior, melhor)
cor(coph,dUSA)

#melhorando a visualização do dendograma
install.packages('factoextra')
library(factoextra)
fviz_dend(hc, cex = 0.4)  # fonte com 60% do tamanho
#a fviz pode colorir um grupo 
fviz_dend(hc, k = 4,  # 4 grupos
          cex = 0.6,  # tamanho do texto/rótulo (label)
          rect = TRUE # adiciona retângulos ao redor dos grupos
)
#o grupamento é realizado de cima pra baixo 

#Considere as distâncias da Seção 7.1 aplicada às colunas numéricas do banco de 
#dados pib, obtido pelo código abaixo.
rm(list = ls()) #limpar a memória
pib <- read.table('http://www.filipezabala.com/data/pib.txt', head = T, sep = '\t')
library(dplyr)
pib.s = scale(pib[,-c(1:2)])
pib.s
rownames(pib.s) = pib$Country
dpib.s = dist(pib.s, method = 'minkowski', p = 4)
hc = hclust(dpib.s)
plot(hc)
library(factoextra)
fviz_dend(hc, k = 5,  # número de grupos desejado
          cex = 0.6,  # tamanho do texto/rótulo (label)
          rect = TRUE # adiciona retângulos ao redor dos grupos
)
###############################
#MÉTODOS NÃO HIERARQUICOS
#K-MÉDIAS
#aprendizado não supervisionado
#método converge para um certo tipo de agrupamento
#Realizar um ponta pé inicial de centróides do Hartigan 
#da maior controle do agrupamento


