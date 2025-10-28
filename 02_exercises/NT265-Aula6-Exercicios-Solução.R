# ---
# title: manipulacao de dados e programacao em R
# author: mauricio vancine
# date: 2025-10-29
# ---

# pacotes -----------------------------------------------------------------

# pacotes
library(tidyverse)
library(palmerpenguins)
library(ggpubr)
library(cowplot)
library(patchwork)
library(GGally)
library(plotly)
library(gganimate)

# dados -------------------------------------------------------------------

# remova os NAs
penguins <- tidyr::drop_na(penguins)
penguins

# exercicios --------------------------------------------------------------

## exercicio 01 ----
# Faça um histograma da massa corporal (body_mass_g) dos pinguins.
ggplot(penguins, aes(x = body_mass_g)) +
    geom_histogram(fill = "steelblue", color = "black") +
    labs(x = "Massa corporal (g)", y = "Frequência", 
         title = "Distribuição da massa corporal") +
    theme_bw(base_size = 20)

## exercicio 02 ----
# Crie um histograma de massa corporal colorido por espécie.
ggplot(penguins, aes(x = body_mass_g, fill = species)) +
    geom_histogram(alpha = 0.7, position = "identity") +
    scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
    labs(x = "Massa corporal (g)", y = "Frequência", 
         title = "Massa corporal por espécie") +
    theme_bw(base_size = 20)

## exercicio 03 ----
# Faça um gráfico de densidade do comprimento da nadadeira por espécie.
ggplot(penguins, aes(x = flipper_length_mm, fill = species)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
    labs(x = "Comprimento da nadadeira (mm)", y = "Densidade", 
         title = "Distribuição do comprimento da nadadeira") +
    theme_bw(base_size = 20)

## exercicio 04 ----
# Crie um gráfico de densidade facetado por sexo.
ggplot(penguins, aes(x = body_mass_g, fill = sex)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c("lightblue", "pink")) +
    facet_wrap(~species) +
    labs(x = "Massa corporal (g)", y = "Densidade", 
         title = "Distribuição da massa corporal por espécie e sexo") +
    theme_bw(base_size = 20)

## exercicio 05 ----
# Crie uma tabela de frequência mostrando a proporção de pinguins por ilhas e 
# depois use o ggpie para criar um gráfico de setores (pizza).

# calculo da proporcao
penguins_prop <- penguins %>%
    dplyr::count(island) %>% 
    dplyr::mutate(prop = round(n/sum(n), 4)*100)
penguins_prop

ggpie(penguins_prop,
      "prop", 
      label = "prop",
      lab.pos = "in", 
      lab.font = c(8, "white"),
      fill = "island", 
      color = "white",
      palette = c("darkorange", "purple", "cyan4"))

## exercicio 06 ----
# use os mesmos dados para criar um gráfico de dunuts usando o ggdonutchart.
ggdonutchart(penguins_prop,
             "prop", 
             label = "prop",
             lab.pos = "in", 
             lab.font = c(8, "white"),
             fill = "island", 
             color = "white",
             palette = c("darkorange", "purple", "cyan4"))

## exercicio 07 ----
# Mostre a proporção de individuos femeas e machos em cada ilha usando combinacao de graficos.
penguins_sex_prop <- penguins %>%
    dplyr::count(island, sex) %>% 
    dplyr::mutate(prop = round(n/sum(n), 4)*100)
penguins_sex_prop

penguins_sex_prop_female <- dplyr::filter(penguins_sex_prop, sex == "female")
penguins_sex_prop_male <- dplyr::filter(penguins_sex_prop, sex == "male")

donut_female <- ggdonutchart(
    penguins_sex_prop_female,
    "prop", 
    fill = "island", 
    color = "white", 
    lab.pos = "in", 
    lab.font = c(5, "white"),
    title = "Proporção de fêmeas por ilha",
    palette = c("darkorange", "purple", "cyan4")) +
    theme(legend.position = c(0.5, 0.5),
          legend.background = element_rect(fill = "transparent"),
          legend.key.size = unit(0.6, "cm"),
          legend.title = element_blank())

donut_male <- ggdonutchart(
    penguins_sex_prop_male,
    "prop", 
    fill = "island", 
    color = "white", 
    lab.font = c(5, "white"),
    lab.pos = "in", 
    title = "Proporção de machos por ilha",
    legend = "",
    palette = c("darkorange", "purple", "cyan4")) +
    theme(legend.position = c(0.5, 0.5),
          legend.background = element_rect(fill = "transparent"),
          legend.key.size = unit(0.6, "cm"),
          legend.title = element_blank())

donut_female + donut_male

## exercicio 08 ----
# Faça um gráfico de barras mostrando o número de indivíduos por ilha. Add
# os numeros nas barras.
penguins_count <- penguins %>%
    dplyr::count(island)
penguins_count

ggplot(data = penguins_count, 
       aes(x = island, y = n, fill = island)) +
    geom_bar(stat = "identity") +
    geom_label(aes(label = n), fill = "white", size = 10) +
    scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
    theme_bw(base_size = 15) +
    theme(legend.position = "none") +
    labs(x = "Ilhas", 
         y = "Número de indivíduos", 
         fill = "Espécie")

## exercicio 09 ----
# Faça um gráfico de barras mostrando o número de indivíduos por ilha para cada sexo. Add
# os numeros nas barras. Dicas: aes(x = island, y = n, fill = sex, group = sex) e 
# position = position_dodge(width = 0.9)
penguins_count <- penguins %>%
    dplyr::count(island, sex)
penguins_count

ggplot(data = penguins_count, 
       aes(x = island, y = n, fill = sex, group = sex)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_label(aes(label = n), fill = "white", size = 5, position = position_dodge(width = 0.9)) +
    scale_fill_manual(values = c("lightblue", "pink")) +
    theme_bw(base_size = 15) +
    labs(x = "Ilhas", y = "Número de indivíduos", fill = "Sexo") +
    theme(legend.position = "top")

## exercicio 10 ----
# Crie um gráfico de pontos por espécie. Use o shape para mudar sex. 
# dica: geom_jitter(size = 5, alpha = .3, width = .2)
ggplot(penguins, 
       aes(x = species, y = flipper_length_mm, color = species, shape = sex)) +
    geom_jitter(size = 5, alpha = .3, width = .2) +
    scale_color_manual(values = c("darkorange", "purple", "cyan4")) +
    labs(x = "Espécie", y = "Comprimento médio (mm)", 
         title = "Comprimento da nadadeira por sexo e espécie") +
    theme_bw(base_size = 20) +
    theme(legend.position = "none")

## exercicio 11 ----
# Crie um boxplot da massa corporal por espécie.
ggplot(penguins, aes(x = species, y = body_mass_g, fill = species)) +
    geom_boxplot() +
    scale_color_manual(values = c("darkorange", "purple", "cyan4")) +
    labs(x = "Espécie", y = "Massa corporal (g)",
         title = "Distribuição da massa por espécie") +
    theme_bw(base_size = 20) +
    theme(legend.position = "none")

## exercicio 12 ----
# Crie um boxplot da massa corporal por espécie e sexo.
ggplot(penguins, aes(x = species, y = body_mass_g, fill = sex)) +
    geom_boxplot() +
    scale_fill_manual(values = c("lightblue", "pink")) +
    labs(x = "Espécie", y = "Massa corporal (g)", 
         title = "Boxplot por espécie e sexo") +
    theme_bw(base_size = 20)

## exercicio 13 ----
# Adicione pontos individuais ao boxplot anterior.
ggplot(penguins, aes(x = species, y = body_mass_g, fill = sex)) +
    geom_boxplot(alpha = 0.6, position = position_dodge(width = 0.8)) +
    geom_jitter(alpha = .3, 
                position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8)) +
    scale_fill_manual(values = c("lightblue", "pink")) +
    labs(x = "Espécie", y = "Massa corporal (g)", 
         title = "Boxplot por espécie e sexo") +
    theme_bw(base_size = 20)

## exercicio 14 ----
# Crie um gráfico de dispersão entre comprimento do bico e da nadadeira.
ggplot(penguins, aes(x = bill_length_mm, y = flipper_length_mm)) +
    geom_point() +
    labs(x = "Comprimento do bico (mm)", 
         y = "Comprimento da nadadeira (mm)",
         title = "Dispersão entre bico e nadadeira") +
    theme_bw(base_size = 20)

## exercicio 15 ----
# Crie um gráfico de dispersão colorido por espécie e com formas por sexo.
ggplot(penguins, aes(x = bill_length_mm, y = flipper_length_mm,
                     color = species, shape = sex)) +
    geom_point(size = 3, alpha = 0.8) +
    labs(x = "Bico (mm)", y = "Nadadeira (mm)",
         title = "Dispersão por espécie e sexo") +
    theme_bw(base_size = 20)

## exercicio 16 ----
# Adicione uma linha de tendência linear ao gráfico anterior. 
# Dica: geom_smooth(method = "lm")
ggplot(penguins, aes(x = bill_length_mm, y = flipper_length_mm,
                     color = species)) +
    geom_point(size = 3, alpha = 0.7) +
    geom_smooth(method = "lm") +
    labs(x = "Comprimento do bico (mm)", 
         y = "Comprimento da nadadeira (mm)",
         title = "Relação entre bico e nadadeira com linha de tendência") +
    theme_bw(base_size = 20)

## exercicio 17 ----
# Crie uma matriz de dispersão entre variáveis morfológicas.
# dica: GGally::ggpairs
penguins %>%
    select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>%
    GGally::ggpairs(title = "Matriz de dispersão morfológica")

## exercicio 18 ----
# Combine dois gráficos (histograma e boxplot) lado a lado.
# dica: ggpubr::ggarrange(g1, g2, ncol = 2, labels = c("A", "B"))
g1 <- ggplot(penguins, aes(x = body_mass_g)) +
    geom_histogram(fill = "steelblue", color = "black") +
    labs(x = "Massa corporal (g)", y = "Frequência", 
         title = "Distribuição da massa corporal") +
    theme_bw(base_size = 20)

g2 <- ggplot(penguins, aes(x = species, y = body_mass_g, fill = species)) +
    geom_boxplot() +
    scale_color_manual(values = c("darkorange", "purple", "cyan4")) +
    labs(x = "Espécie", y = "Massa corporal (g)",
         title = "Distribuição da massa por espécie") +
    theme_bw(base_size = 20) +
    theme(legend.position = "none")

ggpubr::ggarrange(g1, g2, ncol = 2, labels = c("A", "B"))

## exercicio 19 ----
# deixe o gráfico do ex. 15 interativo com plotly.
# dica: atribua o grafico a um objeto e depois use a funcao plotly::ggplotly()
p_static <- ggplot(penguins, aes(x = bill_length_mm, y = flipper_length_mm,
                                 color = species, shape = sex)) +
    geom_point(size = 3, alpha = 0.8) +
    labs(x = "Bico (mm)", y = "Nadadeira (mm)",
         title = "Dispersão por espécie e sexo") +
    theme_bw(base_size = 20)

plotly::ggplotly(p_static)

## exercicio 20 ----
# Crie um grafico animado mostrando as espécies em sequência no gráfico do ex. 15
# dica: + transition_states(species) e animate(nframes = 60, fps = 10)
p_anim <- ggplot(penguins, 
                 aes(x = bill_length_mm, y = flipper_length_mm, color = species)) +
    geom_point(size = 3, alpha = 0.8) +
    labs(x = "Bico (mm)", y = "Nadadeira (mm)",
         title = "Dispersão por espécie e sexo") +
    theme_bw(base_size = 20) +
    transition_states(species)

animate(p_anim, nframes = 60, fps = 10)


# end ---------------------------------------------------------------------