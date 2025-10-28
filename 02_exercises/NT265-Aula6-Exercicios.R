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


## exercicio 02 ----
# Crie um histograma de massa corporal colorido por espécie.


## exercicio 03 ----
# Faça um gráfico de densidade do comprimento da nadadeira por espécie.


## exercicio 04 ----
# Crie um gráfico de densidade facetado por sexo.


## exercicio 05 ----
# Crie uma tabela de frequência mostrando a proporção de pinguins por ilhas e 
# depois use o ggpie para criar um gráfico de setores (pizza).

# calculo da proporcao


## exercicio 06 ----
# use os mesmos dados para criar um gráfico de dunuts usando o ggdonutchart.


## exercicio 07 ----
# Mostre a proporção de individuos femeas e machos em cada ilha usando combinacao de graficos.


## exercicio 08 ----
# Faça um gráfico de barras mostrando o número de indivíduos por ilha. Add
# os numeros nas barras.

## exercicio 09 ----
# Faça um gráfico de barras mostrando o número de indivíduos por ilha para cada sexo. Add
# os numeros nas barras. Dicas: aes(x = island, y = n, fill = sex, group = sex) e 
# position = position_dodge(width = 0.9)


## exercicio 10 ----
# Crie um gráfico de pontos por espécie. Use o shape para mudar sex. 
# dica: geom_jitter(size = 5, alpha = .3, width = .2)


## exercicio 11 ----
# Crie um boxplot da massa corporal por espécie.


## exercicio 12 ----
# Crie um boxplot da massa corporal por espécie e sexo.


## exercicio 13 ----
# Adicione pontos individuais ao boxplot anterior.


## exercicio 14 ----
# Crie um gráfico de dispersão entre comprimento do bico e da nadadeira.


## exercicio 15 ----
# Crie um gráfico de dispersão colorido por espécie e com formas por sexo.

## exercicio 16 ----
# Adicione uma linha de tendência linear ao gráfico anterior. 
# Dica: geom_smooth(method = "lm")


## exercicio 17 ----
# Crie uma matriz de dispersão entre variáveis morfológicas.
# dica: GGally::ggpairs


## exercicio 18 ----
# Combine dois gráficos (histograma e boxplot) lado a lado.
# dica: ggpubr::ggarrange(g1, g2, ncol = 2, labels = c("A", "B"))


## exercicio 19 ----
# Crie um gráfico de dispersão interativo com Plotly.


## exercicio 20 ----
# Crie uma animação mostrando as espécies em sequência no gráfico de dispersão


# end ---------------------------------------------------------------------