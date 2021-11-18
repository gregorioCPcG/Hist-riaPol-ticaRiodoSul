#novos dados 
library(readxl)
novo <- read_excel("novo.xlsx", col_types = c("date", 
                                              "skip", "text", "numeric", "skip"))

library(ggplot2)
ggplot(novo, aes(x = ano, y =valid, group = GRUPO,
                      colour = GRUPO)) + geom_point() + geom_line() +  ylab("% Válidos") + xlab("Pleito Municipal")
lin1 <- aggregate (valid~GRUPO+ano, data = novo, FUN="sum") # preparação dos dados

lin <- ggplot(lin1, aes(ano, valid, group = GRUPO))
lin + geom_line(aes(colour = GRUPO), size = 1.5) +
  geom_point(aes(colour= GRUPO, shape = GRUPO), size = 6)+ 
  labs(title="Eleições por grupo ideológico", subtitle="1947-2020 Rio do Sul/SC", 
       x = "Ano", y = "% Votos Válidos", caption = "Fonte: TRE-SC e câmara de vereadores de Rio do Sul", color = "Sexo") + theme_bw() +
  theme(text = element_text(size = 12), legend.position = "bottom") + guides(linetype = F, shape = F)

library(coefplot)
library(sjPlot)
novo$grupo <- as.factor(novo$GRUPO)
novo$grupo<- relevel(novo$grupo,"direita")
mod46_20_1 <- lm(valid ~ grupo, data = novo)
summary(mod46_20_1)
tab_model(mod46_20_1, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
coefplot(mod46_20_1, intercept = FALSE)
tab_model(mod46_20_1)

ggplot(novo, aes(x=GRUPO, y=valid))+
  geom_boxplot()




# sem conservadorismo

novo2 <- read_excel("novo2.xlsx", col_types = c("date", 
                                              "skip", "text", "numeric", "skip"))
lin1 <- aggregate (valid~GRUPO+ano, data = novo2, FUN="sum") # preparação dos dados

lin <- ggplot(lin1, aes(ano, valid, group = GRUPO))
lin + geom_line(aes(colour = GRUPO), size = 1.5) +
  geom_point(aes(colour= GRUPO, shape = GRUPO), size = 6)+ 
  labs(title="Eleições por grupo ideológico", subtitle="1947-2020 Rio do Sul/SC", 
       x = "Ano", y = "% Votos Válidos", caption = "Fonte: TRE-SC e câmara de vereadores de Rio do Sul", color = "Sexo") + theme_bw() +
  theme(text = element_text(size = 12), legend.position = "bottom") + guides(linetype = F, shape = F)

novo2$grupo <- as.factor(novo2$GRUPO)
novo2$grupo<- relevel(novo2$grupo,"direita")
mod46_20_2 <- lm(valid ~ grupo, data = novo2)
summary(mod46_20_2)
tab_model(mod46_20_2, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
coefplot(mod46_20_2, intercept = FALSE)
tab_model(mod46_20_2, mod46_20_1)




# pleitos com competição - o que explica a vitória

competitivo <- read_excel("competitivo.xlsx", 
                          col_types = c("date", "text", "numeric", 
                                        "text"))
table(competitivo$ano)

table(competitivo$GRUPO, competitivo$resultado_)
competitivo$GRUPO <- as.factor(competitivo$GRUPO)
competitivo$GRUPO<- relevel(competitivo$GRUPO,"direita")
vitoria1 <- glm(resultado ~ GRUPO, data = competitivo, family=binomial(link=logit))

tab_model(vitoria1, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
0.17-1#centro
0.06-1#esquerda


library(Zelig)
z.out1 <- zelig(resultado ~  GRUPO, model = "logit", data = competitivo, cite = FALSE)
summary(z.out1, odds_ratios = TRUE) # para comparar
x.dir <- Zelig::setx(z.out1,GRUPO = "direita")
direita <- sim(z.out1, x = x.dir)
summary(direita)
plot(direita)
# 74.6 deu esse valor, cada simulaçao pode dar valores diferentes

x.esq <- Zelig::setx(z.out1,GRUPO = "esquerda")
esquerda <- sim(z.out1, x = x.esq)
summary(esquerda)
plot(esquerda)
# 18.7 deu esse valor, cada simulaçao pode dar valores diferentes

x.centro <- Zelig::setx(z.out1,GRUPO = "centro")
centro <- sim(z.out1, x = x.centro)
summary(centro)
plot(centro)
# 32.9 deu esse valor, cada simulaçao pode dar valores diferentes



direita 
esquerda
centro

# acrescentar tempo

vitoria2 <- glm(resultado ~ GRUPO + ano, data = competitivo, family=binomial(link=logit))

tab_model(vitoria2, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
tab_model(vitoria1, vitoria2, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")#data tem q recodif

competitivo$data <- as.numeric(competitivo$ano)
summary(competitivo)
table(competitivo$data, competitivo$ano)
#normalizar..
epa32 <- preProcess(epa2[,c(2:4)], method = c("range"))
norm2 <- predict(epa32,epa2[,c(2:4)])
summary(norm2)
library(caret)
epa32 <- preProcess(competitivo[,c(5)], method = c("center", "scale"))
norm2 <- predict(epa32,competitivo[,c(5)])
summary(norm2)
competitivo$datanorm <- norm2$data
summary(competitivo)


vitoria3 <- glm(resultado ~ GRUPO + GRUPO*datanorm, data = competitivo, family=binomial(link=logit))

tab_model(vitoria3, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
tab_model(vitoria1, vitoria3, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
#não muda muita coisa


direita 
esquerda
centro


summary(obj)
obj
summary(direita)

mod <- vitoria1

##Diagnóstico do Modelo vitoria 1##
#Importância das Variáveis Preditoras#
library(caret)
varImp(mod)

###Criando Amostra de Treino e Teste###
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(competitivo), replace=TRUE, prob=c(0.7,0.3))
treino <- competitivo[sample, ]
teste <- competitivo[!sample, ] 

##Estimação do modelo de treino
modt <- glm(resultado~GRUPO, data = treino, family = "binomial")
summary(modt)

##Calulando a probabilidade prevista do voto em Bolsonaro##
previsto <- predict(modt, teste, type="response")

#Calculando a melhor probabilidade de voto#
library(InformationValue)
otimo <- optimalCutoff(teste$resultado, previsto)[1] 
otimo # linha de corte mais adequada

#Matriz de Confusão
confusionMatrix(teste$resultado, previsto)
prop.table(confusionMatrix(teste$resultado, previsto))

#Calculando a sensibilidade (taxa de positivo verdadeira)#
sensitivity(teste$resultado, previsto)

#Calculando a Especificidade (taxa de negativo verdadeira)#
specificity(teste$resultado, previsto)

#Calculando o Erro de Classificação
misClassError(teste$resultado, previsto, threshold=otimo)


#Estimando a Curva de ROC (Características operação do receptor)
plotROC(teste$resultado, previsto)

#0.8 to 0.9 is considered excellent, and more than 0.9 is considered outstanding
#https://www.sciencedirect.com/science/article/pii/S1556086415306043


#Exibindo os resultados
library(sjPlot)
tab_model(mod)
plot_model(mod)# editável no ggplot
###Fim do Script###

