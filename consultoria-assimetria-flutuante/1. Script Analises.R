# Analise Exploratória Carol 

library(tidyverse)
library(rstatix)
library(stats)
library(ggpmisc)


# Carregando a planilha 
dados <- read.csv2("morfologia.csv", dec = "," , stringsAsFactors = TRUE, header = TRUE)

# Remover linhas com NA para garantir integridade dos dados
dados <- na.omit(dados)

# Corrigir vírgulas e converter colunas de medidas para numérico
dados <- dados %>%
  mutate(across(matches("(_d|_e)$"),
                ~ as.numeric(gsub(",", ".", gsub("[^0-9,.-]", "", .)))))

# Transformar dados para formato longo com colunas para lado direito e esquerdo
dadosl <- dados %>%
  dplyr::select(-ends_with("_dif")) %>%  # Remover colunas de diferenças já existentes
  pivot_longer(
    cols = matches("(_d|_e)$"),
    names_to = c("caractere", "lado"),
    names_pattern = "(.*)_(d|e)",
    values_to = "valor"
  ) %>%
  mutate(valor = as.numeric(valor)) %>%
  pivot_wider(
    names_from = lado,
    values_from = valor,
    names_prefix = "lado_"
  ) %>%
  mutate(
    valores_ai = lado_d - lado_e,      # Diferença entre lados (R - L)
    media_rl = (lado_d + lado_e) / 2   # Média dos lados para tamanho médio
  )

# Definir caractere como fator para análise
dadosl$caractere <- as.factor(dadosl$caractere)

# Visualizando
summary(dados)
summary(dadosl)
str(dadosl)

# Pedindo as estatísticas descritivas 
dadosl |> group_by(caractere) |> rstatix::get_summary_stats(valores_ai)
# Pedindo as estatísticas descritivas 
dadosl |> group_by(caractere) |> rstatix::get_summary_stats(media_rl)

# Checando a normalidade 
# Teste de normalidade (Shapiro-Wilk) por caráter
normalidade <- dadosl %>%
  group_by(caractere) %>%
  shapiro_test(valores_ai)

print(normalidade)

# Teste t para verificação estatística 
teste_t <- dadosl %>%
  group_by(caractere) %>%
  summarise(t_test = list(t.test(valores_ai, mu = 0))) %>%
  mutate(statistic = sapply(t_test, function(x) x$statistic),
         p_value = sapply(t_test, function(x) x$p.value)) %>%
  dplyr::select(caractere, statistic, p_value)

print(teste_t)

# Verificando a assimetria 
library(moments)

dadosl |> 
  dplyr::group_by(caractere) |> 
  summarise(skew = skewness(valores_ai, na.rm = TRUE))

# Verificando a curtoso 
library(e1071)

dadosl |> 
  dplyr::group_by(caractere) |> 
  summarise(curt = kurtosis(valores_ai, na.rm = TRUE))

# Para ser considerada uma distribuição platicurtica a curtose precisa ser maior menor que 3 


# Histograma por caractere, cada gráfico separado
dadosl %>%
  group_by(caractere) %>%
  group_map(~ {
    ggplot(.x, aes(x = valores_ai)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "grey", color = "black") +
      stat_function(
        fun = dnorm,
        args = list(mean = mean(.x$valores_ai),
                    sd = sd(.x$valores_ai)),
        color = "black",
        size = 1
      ) +
      labs(
        x = "Diferença entre os lados (D - E)",
        y = "Densidade"
      ) +
      ggtitle(unique(.x$caractere)) +
      theme_classic(base_size = 14) +
      theme(
        plot.title = element_text(
          hjust = 0.5,          # Centraliza o título
          face = "bold",        # Deixa em negrito
          size = 16             # Aumenta o tamanho do título
        )
      )
  })


# Histograma + curva normal teórica + Q-Q plot para cada caráter
ggplot(dadosl, aes(x = valores_ai)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "grey", color = "black") +
  stat_function(fun = dnorm,
                args = list(mean = mean(dadosl$valores_ai),
                            sd = sd(dadosl$valores_ai)),
                color = "black", size = 1) +
  facet_wrap(~ caractere, scales = "free") +
  theme_minimal()

## Q-Q plot
ggplot(dadosl, aes(sample = valores_ai)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(x = "Observado", 
       y = "Esperado") +
  facet_wrap(~ caractere, scales = "free") +
  theme_classic()

ggsave ("qqplot.png")

# Para a caracterização da assimetria flutuante, segundo os conceitos de Palmer, a diferença entre os valores morfológicos de cada caractere deve apresentar distribuição normal e média próxima de zero. Nos caracteres analisados, observou-se que as médias foram próximas de zero; entretanto, a distribuição dos dados não foi normal. Por esse motivo, não é possível classificar essas diferenças como uma AF pura, mas sim como uma variação bilateral não direcional. Diante disso, a utilização de índices de AF poderia enviesar as análises estatísticas. Assim, optou-se por utilizar, em todas as análises, a diferença entre os valores dos caracteres como parâmetro analítico. 

# Calcular correlações para cada caractere
correlacoes <- dadosl %>%
  group_by(caractere) %>%
  summarise(
    spearman = cor.test(valores_ai, media_rl, method = "spearman")$estimate,
    spearman_p = cor.test(valores_ai, media_rl, method = "spearman")$p.value,
    pearson = cor.test(valores_ai, media_rl, method = "pearson")$estimate,
    pearson_p = cor.test(valores_ai, media_rl, method = "pearson")$p.value
  )

print(correlacoes)

# Para avaliar se a assimetria flutuante dependia do tamanho das estruturas, foram calculadas as correlações de Spearman e Pearson entre a diferença dos lados direito e esquerdo (D–E) e o tamanho médio da estrutura ((D+E)/2) para cada caractere. Os resultados indicaram que a maioria dos caracteres (bpeit_ppelv, diam_olho, dob) não apresentou correlação significativa com o tamanho, sugerindo que a variação bilateral é independente da dimensão da estrutura. Em contrapartida, alguns caracteres (doop, raios_peitoral e raios_pelv) apresentaram correlação significativa, indicando que, para essas estruturas, a diferença entre os lados pode ser parcialmente influenciada pelo tamanho. O coeficiente de Spearman foi considerado mais confiável devido à presença de empates e à possibilidade de detectar relações não lineares, enquanto o coeficiente de Pearson evidenciou apenas as associações lineares.

# Gráfico de correlação

ggplot(dadosl, aes(x = media_rl, y = valores_ai)) +
  geom_point(alpha = 0.4, color = "grey70") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  facet_wrap(~caractere, scales = "free") +
  labs(
    x = "Tamanho médio da estrutura ((D+E)/2)",
    y = "Diferença entre os lados (D-E)") +
  theme_classic()

ggsave("correlacao.png")

  
####PAREI AQUI VERIFICAR PAGINA 28 DA DISSERTAÇAO SEIXAS PARA FAZER A CORREÇAO DO CARACTER QUE DEU ASSIMETRIA DIRECIONAL##################


# Correção do caracter que deu assimetria direcional
# Media da diferença 
media_dif <- mean(dadosl$valores_ai)


if(media_dif > 0){
  # lado direito maior (“tira um pouco do lado maior e adiciona no lado menor” → equilibrando)
  dados$direito_corr <- dados$doop_d - (media_dif/2)
  dados$esquerdo_corr <- dados$doop_e + (media_dif/2)
} else {
  # lado esquerdo maior
  dados$direito_corr <- dados$doop_d + (abs(media_dif)/2)
  dados$esquerdo_corr <- dados$doop_e - (abs(media_dif)/2)
}


dados$dif_corr <- dados$direito_corr - dados$esquerdo_corr

# Teste t novamente 
t.test(dados$dif_corr, mu = 0)
# Normalidade
shapiro.test(dados$dif_corr)

# Para avaliar a existência de diferenças significativas nas diferenças entre os lados dos indivíduos entre as três áreas amostradas (nome da área: rio, lago, etc.), foi aplicado o teste de variância multivariado não paramétrico (PERMANOVA). As análises foram baseadas na matriz de distâncias euclidianas. Quando diferenças significativas foram detectadas, testes post-hoc foram realizados para identificar quais zonas apresentaram diferenças significativas entre si.


# Selecionar todas as colunas que serão usadas como caracteres/métricas
# Exemplo: todas as colunas de diferenças e medidas
col_caracteres <- c("raios_peitoral_dif", "raios_pelv_dif","doop_dif","dob_dif","bpeit_ppelv_dif","diam_olho_dif")

# Criar a matriz numérica
matriz_dados <- as.matrix(dados[, col_caracteres])

# Criar o fator de grupos
ponto <- as.factor(dados$ponto)

# Conferir estrutura
str(matriz_dados)
str(ponto)
head(matriz_dados)

# Supondo que suas colunas de diferença sejam todas as *_dif
col_dif <- grep("_dif$", names(dados), value = TRUE)

# Substituir vírgula por ponto e transformar em numérico
dados[col_dif] <- lapply(dados[col_dif], function(x) as.numeric(gsub(",", ".", x)))

# Pacotes
library(vegan)
library(pairwiseAdonis)

# Selecionar colunas de diferença
col_dif <- grep("_dif$", names(dados), value = TRUE)

# Índice das linhas completas (sem NA nas colunas de diferença)
linhas_validas <- complete.cases(dados[col_dif])

# Criar data.frame das diferenças absolutas
matriz_dados_df <- as.data.frame(abs(dados[linhas_validas, col_dif]))

# Remover linhas que têm todos os valores iguais a zero
linhas_com_variacao <- rowSums(matriz_dados_df != 0) > 0
matriz_dados_df <- matriz_dados_df[linhas_com_variacao, ]

# Criar fator de grupos alinhado
zonas <- as.factor(dados$ponto[linhas_validas][linhas_com_variacao])

# Garantir IDs únicos (opcional)
rownames(matriz_dados_df) <- dados$ID[linhas_validas][linhas_com_variacao]

# PERMANOVA global usando distância Euclidiana
dist_euc <- dist(matriz_dados_df, method = "euclidean")
permanova <- adonis2(dist_euc ~ zonas, permutations = 999)
print(permanova)

# Comparações post-hoc par a par
ph <- pairwise.adonis(
  matriz_dados_df,
  zonas,
  p.adjust.m = "bonferroni",
  reduce = NULL,
  perm = 999,
)

print(ph)

# Grafico
pairwise_df <- ph  # seu resultado do pairwise.adonis
ggplot(pairwise_df, aes(x = pairs, y = as.numeric(F.Model))) +
  geom_col(fill = "grey80") +
  theme_classic (base_size = 14) +
  coord_flip() +
  labs(x = "Comparações", y = "Estatística F")

ggsave("permanovaph.png")

# Quanto maior F.Model → maior diferença entre os grupos

# Outro gráfico 
# Supondo que col_dif contenha todas as colunas *_dif
col_dif <- grep("_dif$", names(dados), value = TRUE)

# Selecionar apenas linhas válidas (sem NA) e com variação
linhas_validas <- complete.cases(dados[col_dif])
matriz_dados_df <- abs(dados[linhas_validas, col_dif])
linhas_com_variacao <- rowSums(matriz_dados_df != 0) > 0
matriz_dados_df <- matriz_dados_df[linhas_com_variacao, ]
zonas <- as.factor(dados$ponto[linhas_validas][linhas_com_variacao])

# Criar data.frame no formato “long” para ggplot
library(tidyverse)

# Selecionar apenas as colunas de diferença
df_dif <- matriz_dados_df

# Pivotar apenas as colunas numéricas
df_long <- df_dif %>%
  pivot_longer(
    cols = everything(),
    names_to = "Atributo",
    values_to = "Diferenca"
  )

# Adicionar a coluna de Zona
df_long$Zona <- rep(zonas, times = ncol(df_dif))

# Conferir
head(df_long)


# Calcular média e erro padrão por atributo e zona
df_summary <- df_long %>%
  group_by(Zona, Atributo) %>%
  summarise(
    media = mean(Diferenca),
    erro = sd(Diferenca)/sqrt(n()),
    .groups = "drop"
  )


# Ordenando 

unique(df_summary$Zona)

df_summary$Zona <- factor(df_summary$Zona,
                          levels = c("montante", "intermediario", "jusante"))

# Legenda
unique(df_summary$Atributo)

df_summary <- df_summary %>%
  mutate(Atributo = recode(Atributo,
                           "bpeit_ppelv_dif" = "Base NPeit a Base NPelv",
                           "diam_olho_dif" = "Diâmetro Olho",
                           "dob_dif" = "Distância Olho a Boca",
                           "doop_dif"  = "Distância Olho a Opérculo",
                           "raios_peitoral_dif"  = "Raio Peitoral",
                           "raios_pelv_dif"  = "Raio Pélvico"))


# Gráfico de barras com erro padrão
ggplot(df_summary, aes(x = Zona, y = media)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = media - erro, ymax = media + erro),
                position = position_dodge(width = 0.8), width = 0.2) +
  theme_classic(base_size = 14) +
  facet_wrap(~ Atributo) +
  labs(
    x = "Área Amostral",
    y = "Diferença entre lados"
  ) +
  scale_fill_brewer(palette = "Set1")

ggsave("permanova.png")


# Gráfico de violino + boxplot
# Ordenando 
dadosl$ponto <- factor(dadosl$ponto,
                          levels = c("montante", "intermediario", "jusante"))

# Legenda
dadosl <- dadosl %>%
  mutate(caractere = recode(caractere,
                           "bpeit_ppelv" = "Base NPeit a Base NPelv",
                           "diam_olho" = "Diâmetro Olho",
                           "dob" = "Distância Olho a Boca",
                           "doop"  = "Distância Olho a Opérculo",
                           "raios_peitoral"  = "Raio Peitoral",
                           "raios_pelv"  = "Raio Pélvico"))

# Grafico
ggplot(dadosl, aes(x = ponto, y = valores_ai)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.8) +
  facet_wrap(~ caractere, scales = "free_y") +
  theme_classic(base_size = 14) +
  labs(
    x = "Ponto amostrado",
    y = "Diferença entre os lados",
    fill = "Ponto"
  ) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("boxplot.png")

