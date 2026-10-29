library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)

pasta <- "C:/Users/WIN 11/Downloads"

arquivo_kaua    <- file.path(pasta, "registroKaua.csv")
arquivo_pedro   <- file.path(pasta, "registro_pedro_certo.csv")
arquivo_julia   <- file.path(pasta, "Captura_JuliaLemos.csv")
arquivo_banco   <- file.path(pasta, "Banco-Captura.csv")
arquivo_gustavo <- file.path(pasta, "Gustavo.csv")

kaua    <- read.csv(arquivo_kaua,    stringsAsFactors = FALSE)
pedro   <- read.csv(arquivo_pedro,   stringsAsFactors = FALSE)
julia   <- read.csv(arquivo_julia,   stringsAsFactors = FALSE)
banco   <- read.csv(arquivo_banco,   stringsAsFactors = FALSE)
gustavo <- read.csv(arquivo_gustavo, stringsAsFactors = FALSE)

componentes <- c("Uso da CPU", "Uso da Memoria", "Uso do Disco", "Atividade do Disco")
nomes_maquinas <- paste("Maquina", 1:5)

limites <- data.frame(
  componente = factor(componentes, levels = componentes),
  atencao    = c(70, 75, 80, 70),
  critico    = c(90, 90, 90, 90)
)

metricas <- data.frame(
  Metrica   = componentes,
  Descricao = c("Percentual de uso do processador",
                "Percentual de memoria RAM em uso",
                "Percentual de ocupacao do disco",
                "Percentual de atividade (leitura/escrita) do disco"),
  Unidade   = "%",
  Atencao   = paste0("> ", limites$atencao),
  Critico   = paste0("> ", limites$critico),
  stringsAsFactors = FALSE
)

dimensoes <- data.frame(
  Dimensao  = c("maquina", "componente", "data_hora"),
  Descricao = c("Maquina monitorada (Maquina 1 a Maquina 5)",
                "Metrica monitorada (CPU, Memoria, Disco, Atividade do Disco)",
                "Momento da leitura"),
  stringsAsFactors = FALSE
)

mapa_kaua  <- c("1" = "Uso da CPU",
                "4" = "Uso da Memoria",
                "5" = "Uso do Disco")
mapa_julia <- c("2" = "Uso da CPU",
                "3" = "Uso da Memoria",
                "4" = "Uso do Disco",
                "5" = "Atividade do Disco")
mapa_banco <- c("1" = "Uso da CPU",
                "2" = "Uso da Memoria",
                "3" = "Uso do Disco",
                "4" = "Atividade do Disco")

padroniza_id <- function(df, nome, mapa) {
  df %>%
    filter(as.character(configuracao_fk_componente) %in% names(mapa)) %>%
    transmute(maquina    = nome,
              componente = unname(mapa[as.character(configuracao_fk_componente)]),
              data_hora  = data_hora,
              valor      = valor)
}

padroniza_nome <- function(df, nome) {
  df %>%
    filter(componente %in% componentes) %>%
    transmute(maquina = nome, componente = componente, data_hora = data_hora, valor = valor)
}

dados <- bind_rows(
  padroniza_id(kaua,  "Maquina 1", mapa_kaua),
  padroniza_nome(pedro, "Maquina 2"),
  padroniza_id(julia, "Maquina 3", mapa_julia),
  padroniza_id(banco, "Maquina 4", mapa_banco),
  padroniza_nome(gustavo, "Maquina 5")
)
dados$componente <- factor(dados$componente, levels = componentes)
dados$maquina    <- factor(dados$maquina, levels = nomes_maquinas)

cores <- c("Maquina 1" = "#2E86AB", "Maquina 2" = "#F18F01",
           "Maquina 3" = "#A23B72", "Maquina 4" = "#3BB273",
           "Maquina 5" = "#8E6C8A")

tema_tab <- ttheme_default(base_size = 10,
                           padding = unit(c(3, 2), "mm"))

secao <- function(txt) textGrob(txt, x = 0.02, hjust = 0,
                                gp = gpar(fontsize = 13, fontface = "bold"))

analisa_grupo <- function(maquinas, id, titulo) {
  
  d <- dados %>%
    filter(maquina %in% maquinas) %>%
    droplevels()
  
  grafico <- ggplot(d, aes(x = maquina, y = valor, fill = maquina)) +
    geom_boxplot(alpha = 0.75, outlier.colour = "red", outlier.shape = 16,
                 outlier.size = 2.5, width = 0.6) +
    geom_hline(data = limites, aes(yintercept = atencao),
               colour = "orange", linetype = "dashed", inherit.aes = FALSE) +
    geom_hline(data = limites, aes(yintercept = critico),
               colour = "red", linetype = "dashed", inherit.aes = FALSE) +
    facet_wrap(~ componente, nrow = 2, scales = "free_x") +
    coord_cartesian(ylim = c(0, 100)) +
    scale_fill_manual(values = cores) +
    labs(title    = titulo,
         subtitle = "Pontos vermelhos = outliers | Tracejado laranja = atencao | Tracejado vermelho = critico",
         x = NULL, y = "Uso (%)") +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold", size = 18),
          strip.text = element_text(face = "bold", size = 14),
          axis.text.x = element_text(size = 12),
          panel.spacing = unit(1.5, "lines"))
  
  print(grafico)
  
  ggsave(paste0("boxplot_comparacao_", id, ".png"), grafico,
         width = 11, height = 8, dpi = 150)
  
  resumo <- d %>%
    group_by(componente, maquina) %>%
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
  
  ranking_geral <- resumo %>%
    group_by(maquina) %>%
    summarise(componentes_avaliados = n(),
              media_das_posicoes    = round(mean(posicao), 2),
              mediana_geral_uso     = round(median(mediana), 2),
              .groups = "drop") %>%
    arrange(media_das_posicoes, mediana_geral_uso) %>%
    mutate(posicao_geral = row_number())
  
  alertas <- d %>%
    left_join(limites, by = "componente") %>%
    group_by(componente, maquina) %>%
    summarise(leituras   = n(),
              em_atencao = sum(valor > atencao[1] & valor <= critico[1]),
              em_critico = sum(valor > critico[1]),
              .groups = "drop") %>%
    mutate(pct_alerta = round(100 * (em_atencao + em_critico) / leituras, 1),
           status = case_when(em_critico > 0 ~ "CRITICO",
                              em_atencao > 0 ~ "ATENCAO",
                              TRUE           ~ "OK")) %>%
    arrange(componente, maquina)
  
  cat("\n==================== ", titulo, " ====================\n")
  cat("\n----- RANKING POR COMPONENTE (menor mediana = melhor) -----\n")
  print(as.data.frame(resumo), row.names = FALSE)
  cat("\n----- RANKING GERAL -----\n")
  print(as.data.frame(ranking_geral), row.names = FALSE)
  cat("\n----- ALERTAS -----\n")
  print(as.data.frame(alertas), row.names = FALSE)
  
  write.csv(resumo,        paste0("ranking_por_componente_comparacao_", id, ".csv"), row.names = FALSE)
  write.csv(ranking_geral, paste0("ranking_geral_comparacao_", id, ".csv"),          row.names = FALSE)
  write.csv(alertas,       paste0("alertas_comparacao_", id, ".csv"),                row.names = FALSE)
  
  titulo_grob <- textGrob(paste("DASHBOARD -", titulo),
                          gp = gpar(fontsize = 18, fontface = "bold"))
  
  t_geral   <- tableGrob(as.data.frame(ranking_geral), rows = NULL, theme = tema_tab)
  t_rank    <- tableGrob(as.data.frame(resumo),        rows = NULL, theme = tema_tab)
  t_alertas <- tableGrob(as.data.frame(alertas),       rows = NULL, theme = tema_tab)
  t_metric  <- tableGrob(metricas,                     rows = NULL, theme = tema_tab)
  
  alturas <- unit.c(unit(1.5, "cm"),  unit(15, "cm"),
                    unit(1, "cm"),    grobHeight(t_geral),
                    unit(1, "cm"),    grobHeight(t_rank),
                    unit(1, "cm"),    grobHeight(t_alertas),
                    unit(1, "cm"),    grobHeight(t_metric),
                    unit(0.5, "cm"))
  
  pdf(NULL)
  altura_cm <- convertHeight(sum(alturas), "cm", valueOnly = TRUE)
  dev.off()
  
  dashboard <- arrangeGrob(
    titulo_grob, grafico,
    secao("Ranking geral (menor mediana de uso = melhor)"), t_geral,
    secao("Ranking por componente"),                         t_rank,
    secao("Alertas (leituras acima dos limites)"),           t_alertas,
    secao("Definicao das metricas e limites de alerta"),     t_metric,
    nullGrob(),
    ncol = 1, heights = alturas
  )
  
  png(paste0("dashboard_comparacao_", id, ".png"),
      width = 26, height = altura_cm, units = "cm", res = 110)
  grid.newpage()
  grid.draw(dashboard)
  dev.off()
  
  invisible(list(grafico = grafico, resumo = resumo,
                 ranking_geral = ranking_geral, alertas = alertas))
}

cat("\n===== METRICAS =====\n");  print(metricas,  row.names = FALSE)
cat("\n===== DIMENSOES =====\n"); print(dimensoes, row.names = FALSE)
write.csv(metricas, "metricas.csv", row.names = FALSE)

comparacoes <- list(
  list(id = 1,
       maquinas = c("Maquina 1", "Maquina 2"),
       titulo   = "Comparacao 1: Maquina 1 x Maquina 2"),
  list(id = 2,
       maquinas = c("Maquina 3", "Maquina 4", "Maquina 5"),
       titulo   = "Comparacao 2: Maquina 3 x Maquina 4 x Maquina 5")
)

for (cmp in comparacoes) {
  analisa_grupo(cmp$maquinas, cmp$id, cmp$titulo)
}

