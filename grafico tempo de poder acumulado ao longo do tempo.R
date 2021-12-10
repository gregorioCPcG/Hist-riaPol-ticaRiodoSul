#gráfico tempo de poder acumulado ao longo do tempo
library(readxl)
dias_poder <- read_excel("dias poder acumulado ao longo do tempo.xlsx")

library(ggplot2)
lin1 <- aggregate (dias_poder~ideologia+data, data = dias_poder, FUN="sum") # preparação dos dados

lin <- ggplot(lin1, aes(data, dias_poder, group = ideologia))
lin + geom_line(aes(colour = ideologia), size = 1) +
  geom_point(aes(colour= ideologia, shape = ideologia), size = 2)+ 
  labs(title="Dias no poder - acumulado", subtitle="1931-2020 Rio do Sul/SC", 
       x = "Ano", y = "Dias", caption = "Fonte: TRE-SC", color = "Sexo") + theme_bw() +
  theme(text = element_text(size = 12), legend.position = "bottom") + guides(linetype = F, shape = F)

