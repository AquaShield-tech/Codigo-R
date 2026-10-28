#  install.packages(c("ggplot2", "dplyr"))
library(ggplot2)
library(dplyr)

arquivo_kaua  <- "C:/Users/ADMIN/Downloads/registroKaua.csv"
arquivo_pedro <- "C:/Users/ADMIN/Downloads/registro_pedro.csv"

#  Leitura
kaua  <- read.csv(arquivo_kaua,  stringsAsFactors = FALSE)
pedro <- read.csv(arquivo_pedro, stringsAsFactors = FALSE)

# Padronizacao 
mapa_kaua <- c("1" = "Uso da CPU",
               "4" = "Uso da Memoria",
               "5" = "Uso do Disco")

kaua_pad <- kaua %>%
  filter(as.character(configuracao_fk_componente) %in% names(mapa_kaua),
         fk_medida == 1) %>%
  transmute(pessoa     = "Kaua",
            componente = unname(mapa_kaua[as.character(configuracao_fk_componente)]),
            valor      = valor)

pedro_pad <- pedro %>%
  filter(componente %in% mapa_kaua) %>%       
  transmute(pessoa = "Pedro", componente, valor)

dados <- bind_rows(kaua_pad, pedro_pad)
dados$componente <- factor(dados$componente, levels = unname(mapa_kaua))

#  Boxplot 
grafico <- ggplot(dados, aes(x = pessoa, y = valor, fill = pessoa)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "red", outlier.shape = 16, width = 0.55) +
  facet_wrap(~ componente, scales = "free_y") +
  scale_fill_manual(values = c("Maquina 1" = "#2E86AB", "Maquina 2" = "#F18F01")) +
  labs(title    = "Comparativo de uso de recursos: Maquina 1 x Maquina 2",
       subtitle = "Pontos vermelhos = outliers",
       x = NULL, y = "Uso (%)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))

print(grafico)  

ggsave("boxplot_comparativo.png", grafico, width = 10, height = 5.5, dpi = 150)

#  Ranking 
# Criterio: MENOR mediana de uso = melhor posicao (1o lugar).
# Se quiser o contrario, troque rank(mediana) por rank(mediana).

resumo <- dados %>%
  group_by(componente, pessoa) %>%
  summarise(n       = n(),
            mediana = round(median(valor), 2),
            minimo  = min(valor),
            maximo  = max(valor),
            desvio  = round(sd(valor), 2),
            .groups = "drop") %>%
  group_by(componente) %>%
  mutate(posicao = rank(mediana, ties.method = "min")) %>%
  arrange(componente, posicao) %>%
  ungroup()

cat("\n RANKING POR COMPONENTE (menor mediana = melhor) \n")
print(as.data.frame(resumo), row.names = FALSE)

ranking_geral <- resumo %>%
  group_by(pessoa) %>%
  summarise(media_das_posicoes = mean(posicao),
            mediana_geral_uso  = round(median(mediana), 2),
            .groups = "drop") %>%
  arrange(media_das_posicoes, mediana_geral_uso) %>%
  mutate(posicao_geral = row_number())

cat("\n RANKING GERAL\n")
print(as.data.frame(ranking_geral), row.names = FALSE)

write.csv(resumo,        "ranking_por_componente.csv", row.names = FALSE)
write.csv(ranking_geral, "ranking_geral.csv",          row.names = FALSE)
