# ---
# title: manipulacao de dados e programacao em R
# author: mauricio vancine
# date: 2025-10-29
# ---

# pacotes ----
library(tidyverse)
library(palmerpenguins)
library(ggpubr)
library(GGally)
library(plotly)
library(gganimate)

# remova os NAs
penguins <- drop_na(penguins)
penguins

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
# Mostre a proporção de sexos em cada ilha usando facet.
penguins_sex_prop <- penguins %>%
    dplyr::count(island, sex) %>% 
    dplyr::mutate(prop = round(n/sum(n), 4)*100)
penguins_sex_prop

penguins_sex_prop %>%
    ggdonutchart("prop", 
                 fill = "island", 
                 color = "white", 
                 facet.by = "sex",
                 title = "Proporção de sexos por espécie")

## exercicio 08 ----
# Faça um gráfico de barras mostrando o número de indivíduos por ilha. Add
# os numeros as barras.
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
# Crie um gráfico de barras com a média da massa corporal por espécie.
penguins %>%
    group_by(species) %>%
    summarise(media_massa = mean(body_mass_g, na.rm = TRUE)) %>%
    ggplot(aes(x = species, y = media_massa, fill = species)) +
    geom_col() +
    labs(x = "Espécie", y = "Massa média (g)", title = "Massa média por espécie") +
    theme_bw(base_size = 20)

## exercicio 10 ----
# Crie um gráfico de barras agrupadas por sexo e espécie (média da nadadeira).
penguins %>%
    drop_na(sex) %>%
    group_by(species, sex) %>%
    summarise(media_flipper = mean(flipper_length_mm, na.rm = TRUE)) %>%
    ggplot(aes(x = species, y = media_flipper, fill = sex)) +
    geom_col(position = "dodge") +
    labs(x = "Espécie", y = "Comprimento médio (mm)", 
         title = "Comprimento médio da nadadeira por sexo e espécie") +
    theme_bw(base_size = 20)

## exercicio 11 ----
# Crie um boxplot da massa corporal por espécie.
ggplot(penguins, aes(x = species, y = body_mass_g, fill = species)) +
    geom_boxplot() +
    labs(x = "Espécie", y = "Massa corporal (g)",
         title = "Distribuição da massa por espécie") +
    theme_bw(base_size = 20)

## exercicio 12 ----
# Crie um boxplot da massa corporal por espécie e sexo.
ggplot(penguins, aes(x = species, y = body_mass_g, fill = sex)) +
    geom_boxplot() +
    labs(x = "Espécie", y = "Massa corporal (g)", title = "Boxplot por espécie e sexo")

## exercicio 13 ----
# Adicione pontos individuais ao boxplot anterior.
ggplot(penguins, aes(x = species, y = body_mass_g, fill = sex)) +
    geom_boxplot(alpha = 0.6, outlier.shape = NA) +
    geom_jitter(width = 0.2, alpha = 0.5) +
    labs(x = "Espécie", y = "Massa corporal (g)", title = "Boxplot com pontos")

## exercicio 14 ----
# Crie um gráfico de dispersão entre comprimento do bico e da nadadeira.
ggplot(penguins, aes(x = bill_length_mm, y = flipper_length_mm)) +
    geom_point() +
    labs(x = "Comprimento do bico (mm)", y = "Comprimento da nadadeira (mm)",
         title = "Dispersão entre bico e nadadeira")

## exercicio 15 ----
# Crie um gráfico de dispersão colorido por espécie e com formas por sexo.
ggplot(penguins, aes(x = bill_length_mm, y = flipper_length_mm,
                     color = species, shape = sex)) +
    geom_point(size = 3, alpha = 0.8) +
    labs(x = "Bico (mm)", y = "Nadadeira (mm)",
         title = "Dispersão por espécie e sexo")

## exercicio 16 ----
# Adicione uma linha de tendência linear ao gráfico anterior.
ggplot(penguins, aes(x = bill_length_mm, y = flipper_length_mm,
                     color = species)) +
    geom_point(size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Comprimento do bico (mm)", y = "Comprimento da nadadeira (mm)",
         title = "Relação entre bico e nadadeira com linha de tendência")

## exercicio 17 ----
# Crie uma matriz de dispersão entre variáveis morfológicas.
penguins %>%
    select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>%
    ggpairs(title = "Matriz de dispersão morfológica")

## exercicio 18 ----
# Combine dois gráficos (histograma e boxplot) lado a lado.
g1 <- ggplot(penguins, aes(x = body_mass_g)) +
    geom_histogram(binwidth = 200, fill = "steelblue") +
    labs(title = "Histograma")

g2 <- ggplot(penguins, aes(x = species, y = body_mass_g, fill = species)) +
    geom_boxplot() +
    labs(title = "Boxplot")

ggarrange(g1, g2, ncol = 2, labels = c("A", "B"))

## exercicio 19 ----
# Crie um gráfico de dispersão interativo com Plotly.
p_static <- ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm,
                                 color = species)) +
    geom_point(size = 3) +
    labs(x = "Comprimento do bico (mm)", y = "Profundidade do bico (mm)",
         title = "Gráfico interativo com Plotly")

ggplotly(p_static)

## exercicio 20 ----
# Crie uma animação mostrando as espécies em sequência no gráfico de dispersão.

p_anim <- ggplot(penguins, aes(bill_length_mm, flipper_length_mm, color = species)) +
    geom_point(size = 3) +
    labs(title = "Espécie: {closest_state}",
         x = "Comprimento do bico (mm)", y = "Comprimento da nadadeira (mm)") +
    transition_states(species)

animate(p_anim, nframes = 60, fps = 10)


# end ---------------------------------------------------------------------