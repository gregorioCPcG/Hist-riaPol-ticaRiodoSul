#tabela 1 - parte 1

library(readxl)

allTimes_retificada <- read_excel("allTimes - retificada.xlsx")
library(ggplot2)
bar4 <- ggplot(allTimes_retificada, aes(partido, tempopoder, fill= partido))
bar4 + geom_bar(stat = "identity") + labs(title = "Tempo no poder por partido", 
                                          subtitle = "Rio do Sul 1931-2020",
                                          x = "Partido", y= "Dias no Poder", 
                                          caption = "Fonte: prefeitura municipal de Rio do Sul") + theme_bw() + 
  theme(text = element_text(size = 12), legend.position = "none") 
