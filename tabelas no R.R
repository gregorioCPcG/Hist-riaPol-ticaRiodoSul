library(readxl)
tabelona1 <- read_excel("tabelona1.xlsx", col_types = c("text", "text", "numeric"))
library(ggplot2)
tabelona1 <- read_excel("tabelona1.xlsx", 
                        col_types = c("date", "text", "numeric"))
str(tabelona1)
options("scipen"=100, "digits"=4)

ggplot(tabelona1, aes(x = ANO, y =porc_v, group = GRUPO,
               colour = GRUPO)) + geom_point() + geom_line() +  ylab("% Válidos") + xlab("Pleito Municipal")

ggplot(tabelona1, aes(x = ANO, y = porc_v, group = GRUPO,
                      colour = GRUPO)) + geom_point() + geom_line() +  ylab("% Válidos") + 
  xlab("Pleito Municipal") + scale_y_continuous(limits = c(0, 80))

# outro jeito (abaixo)

# fonte : D:\ATUALIZA_PASTA_d\Prefeitura_Rio_do_Sul_Histor\ggplotbuni.html


lin1 <- aggregate (porc_v~GRUPO+ANO, data = tabelona1, FUN="sum") # preparação dos dados

lin <- ggplot(lin1, aes(ANO, porc_v, group = GRUPO))
lin + geom_line(aes(colour = GRUPO), size = 1.5) +
  geom_point(aes(colour= GRUPO, shape = GRUPO), size = 6)+ 
  labs(title="Votos válidos por grupo político", subtitle="1982-2020 Rio do Sul/SC", 
       x = "Ano", y = "% Votos Válidos", caption = "Fonte: TRE-SC", color = "Sexo") + theme_bw() +
  theme(text = element_text(size = 12), legend.position = "bottom") + guides(linetype = F, shape = F)
                                             
# tabelona 2
tabelona2 <- read_excel("tabelona2.xlsx", 
                        col_types = c("date", "text", "numeric"))


lin2 <- aggregate (valid~GRUPO+ANO, data = tabelona2, FUN="sum") # preparação dos dados

lin9 <- ggplot(lin2, aes(ANO, valid, group = GRUPO))
lin9 + geom_line(aes(colour = GRUPO, linetype = GRUPO), size = 1.5) +
  geom_point(aes(colour= GRUPO, shape = GRUPO), size = 3)+ 
  labs(title="Votos válidos por grupo político", subtitle="1982-2020 Rio do Sul/SC", 
       x = "Ano", y = "% Votos Válidos", caption = "Fonte: TRE-SC", color = "Sexo") + theme_light() +
  theme(text = element_text(size = 12), legend.position = "bottom") + guides(linetype = F, shape = F)

# desde 1947 - vitórias

desd47 <- read_excel("tabgeral1947.xlsx", 
                     col_types = c("numeric", "text", "text"))

desd47_ <-read_excel("errata.xlsx", 
                     col_types = c("numeric", "text", "text"))

desd47$pleito <- as.Date(as.character(desd47$ano), format = "%Y")# se for usar

desd47_$pleito <- as.Date(as.character(desd47$ano), format = "%Y")# se for usar
desd47$cont <- 1 #variável para servir de marcador - usa para fazer contagem de variáveis
desd47_$cont <- 1

bar <- ggplot(desd47_, aes(partidovitoria, cont))
bar + geom_bar(stat = "identity")


bar <- ggplot(desd47_, aes(partidovitoria, cont, fill= partidovitoria))
bar + geom_bar(stat = "identity") + labs(title = "", 
                                         subtitle = "Rio do Sul 1947-2020",
                                         x = "Partido", y= "Vitória cabeça de chapa", 
                                         caption = "Fonte = TRE-SC e prefeitura municipal de Rio do Sul") + theme_bw() + 
  theme(text = element_text(size = 12), legend.position = "none") + scale_fill_grey()

bar2 <- ggplot(desd47, aes(grupo, cont, fill= grupo))
bar2 + geom_bar(stat = "identity") + labs(title = "Vitória por grupo ideológico", 
                                         subtitle = "Rio do Sul 1947-2020",
                                         x = "Localização Ideológica", y= "Vitória cabeça de chapa", 
                                         caption = "Fonte = TRE-SC e prefeitura municipal de Rio do Sul") + theme_bw() + 
  theme(text = element_text(size = 12), legend.position = "none") + scale_fill_grey()


# REGRESSAO LINEAR

mod82_2020 <- lm(valid ~ GRUPO, data = tabelona2)
summary(mod82_2020)


# dias no poder por partido e grupo ideológico

# desde 1931 
#https://calculator-online.net/pt/date-calculator/ #para calculcar

d_1931 <- read_excel("allTimes.xlsx") 

d_1931$AnoInicial <- d_1931$anoinicio
d_1931$AnoFinal <- d_1931$anotermino
d_1931$'Localização Ideológica' <- d_1931$grupo
d_1931$Partido <- d_1931$partido

library(knitr)
library(kableExtra)
library(tidyverse)

b5 <- d_1931 %>% 
  dplyr::select(AnoInicial, AnoFinal, Partido, 'Localização Ideológica', tempopoder) %>% 
  arrange(AnoInicial)
b5 %>%
  kbl(caption = "Tempo no poder - prefeitura Rio do Sul-SC - 1931-2024") %>%
  kable_classic(full_width = F, html_font = "Garamond")



bar4 <- ggplot(d_1931, aes(partido, tempopoder, fill= partido))
bar4 + geom_bar(stat = "identity") + labs(title = "Tempo no poder por partido", 
                                         subtitle = "Rio do Sul 1931-2020",
                                         x = "Partido", y= "Dias no Poder", 
                                         caption = "Fonte: prefeitura municipal de Rio do Sul") + theme_bw() + 
  theme(text = element_text(size = 12), legend.position = "none") 



bar5 <- ggplot(d_1931, aes(grupo, tempopoder, fill= grupo))
bar5 + geom_bar(stat = "identity") + labs(title = "Tempo no poder por partido", 
                                          subtitle = "Rio do Sul 1931-2020",
                                          x = "Partido", y= "Dias no poder", 
                                          caption = "Fonte: prefeitura municipal de Rio do Sul") + theme_bw() + 
  theme(text = element_text(size = 12), legend.position = "none") + scale_fill_brewer(palette = "Dark2")


mod_tempo31<- lm(tempopoder ~ grupo, data = d_1931)
summary(mod_tempo31)






# desde 1947

alllfrom1946 <- read_excel("alllfrom1946.xlsx")
bar5 <- ggplot(alllfrom1946, aes(grupo, tempopoder, fill= grupo))
bar5 + geom_bar(stat = "identity") + labs(title = "Tempo no poder por partido", 
                                          subtitle = "Rio do Sul 1947-2020",
                                          x = "Partido", y= "Dias no poder", 
                                          caption = "Fonte: prefeitura municipal de Rio do Sul") + theme_bw() + 
  theme(text = element_text(size = 12), legend.position = "none") + scale_fill_brewer(palette = "Dark2")


mod_tempo47<- lm(tempopoder ~ grupo, data = alllfrom1946)
summary(mod_tempo47)
# não 


# média por grupo % válidos desde 1982
aggregate(tabelona2[, 3], list(tabelona2$GRUPO), mean)#categoriz 2
aggregate(tabelona1[, 3], list(tabelona1$GRUPO), mean)#categoriz 1



