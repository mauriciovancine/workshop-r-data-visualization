#' ----
#' title: visualizacao de dados em r
#' author: mauricio vancine
#' date: 2025-10-15
#' ----

# pacotes -----------------------------------------------------------------

library(tidyverse)
library(palmerpenguins)
library(datasauRus)
library(ggpubr)
library(ggdist)
library(ggtext)
library(colorspace)
library(ragg)
library(GGally)
library(psych)
library(cowplot)
library(patchwork)
library(gganimate)
library(plotly)
library(htmlwidgets)
library(shiny)
library(esquisse)

# topicos -----------------------------------------------------------------

# pacotes para graficos 
# gramatica dos graficos
# principais tipos de graficos 
# histograma e densidade
# grafico de setores
# grafico de barras
# grafico de caixas
# grafico de dispersao
# grafico pareado
# combinando graficos
# graficos animados
# graficos interativos
# graficos usando uma interface

# pacotes para graficos  ---------------------------------------------

# graphics
plot(flipper_length_mm ~ body_mass_g, data = penguins)

# ggplot2 
ggplot(data = penguins) + aes(x = body_mass_g, y = flipper_length_mm) + geom_point()

# ggpubr
ggscatter(penguins, x = "body_mass_g", y = "flipper_length_mm")

# paleta de cores ---------------------------------------------------------

# cols4all
cols4all::c4a_gui() # lembre=se apertar esc para continuar

# histograma e densidade -----------------------------------------------

# visualizar os dados
penguins

## base r ----
hist(penguins$flipper_length_mm)

hist(penguins$flipper_length_mm,
     col = "gray50",
     border = "gray")

hist(penguins$flipper_length_mm,
     col = "gray50",
     border = "gray",
     main = "Comprimento da nadadeira dos penguins")

hist(penguins$flipper_length_mm,
     col = "gray50",
     border = "gray",
     main = "Comprimento da nadadeira dos penguins",
     xlab = "Comprimento da nadadeira (mm)",
     ylab = "Frequência")

hist(penguins$flipper_length_mm,
     col = "gray50",
     border = "gray",
     main = "Comprimento da nadadeira dos penguins",
     xlab = "Comprimento da nadadeira (mm)",
     ylab = "Frequência",
     br = 50)

par(mar = c(5, 5, 5, 5))
hist(penguins$flipper_length_mm,
     col = "gray50",
     border = "gray",
     main = "Comprimento da nadadeira dos penguins",
     xlab = "Comprimento da nadadeira (mm)",
     ylab = "Frequência",
     br = 50,
     cex.main = 2,
     cex.lab = 2, 
     cex.axis = 1.5)

par(mar = c(5, 5, 5, 5))
hist(penguins$flipper_length_mm,
     col = "gray50",
     border = "gray",
     main = "Comprimento da nadadeira dos penguins",
     xlab = "Comprimento da nadadeira (mm)",
     ylab = "Densidade",
     br = 50,
     cex.main = 2,
     cex.lab = 2, 
     cex.axis = 1.5,
     prob = TRUE)
lines(density(na.omit(penguins$flipper_length_mm)))

par(mar = c(5, 5, 5, 5))
plot(density(na.omit(penguins$flipper_length_mm)),
     col = "gray50",
     main = "Comprimento da nadadeira dos penguins",
     xlab = "Comprimento da nadadeira (mm)",
     ylab = "Densidade",
     cex.main = 2,
     cex.lab = 2, 
     cex.axis = 1.5)
polygon(density(na.omit(penguins$flipper_length_mm)), 
        col = "gray50")

# exportar
# definir o diretorio de trabalho
setwd("/home/mude/data/github/course-intror/data") # mudem

png("plot_densidade.png", wi = 20, he = 20, un = "cm", res = 300)

par(mar = c(5, 5, 5, 5))
plot(density(na.omit(penguins$flipper_length_mm)),
     col = "gray50",
     main = "Comprimento da nadadeira dos penguins",
     xlab = "Comprimento da nadadeira (mm)",
     ylab = "Frequência",
     cex.main = 2,  cex.lab = 2, cex.axis = 1.5)
polygon(density(na.omit(penguins$flipper_length_mm)), col = "gray50")

dev.off()

## ggplot2 ----
ggplot(data = penguins)

ggplot(data = penguins, aes(x = flipper_length_mm))

ggplot(data = penguins, aes(x = flipper_length_mm)) +
  geom_histogram()

ggplot(data = penguins,  aes(x = flipper_length_mm)) +
  geom_histogram(color = "black", fill = "cyan4", bins = 10)

ggplot(data = penguins, aes(x = flipper_length_mm)) +
  geom_histogram(color = "black", fill = "cyan4", 
                 bins = 10, alpha = .5) +
  labs(title = "Comprimento da nadadeira dos penguins",
       x = "Comprimento da nadadeira (mm)", 
       y = "Frequência") +
  theme_bw(base_size = 16)

ggplot(data = penguins, 
       aes(x = flipper_length_mm, fill = species)) +
  geom_histogram(bins = 10, alpha = .5) +
  labs(title = "Comprimento da nadadeira dos penguins",
       fill = "Espécies",
       x = "Comprimento da nadadeira (mm)", 
       y = "Frequência") +
  theme_bw(base_size = 16)

ggplot(data = penguins, 
       aes(x = flipper_length_mm, fill = species)) +
  geom_histogram(bins = 10, alpha = .5, position = "dodge") +
  labs(title = "Comprimento da nadadeira dos penguins",
       fill = "Espécies",
       x = "Comprimento da nadadeira (mm)", 
       y = "Frequência") +
  theme_bw(base_size = 16)

ggplot(data = penguins, 
       aes(x = flipper_length_mm, fill = species)) +
  geom_histogram(alpha = .5, position = "identity") +
  scale_fill_manual(values = c("darkorange", "darkorchid", "cyan4")) +
  labs(title = "Comprimento da nadadeira dos penguins",
       fill = "Espécies",
       x = "Comprimento da nadadeira (mm)", 
       y = "Frequência") +
  theme_bw(base_size = 16)

ggplot(data = penguins, 
       aes(x = flipper_length_mm, fill = species)) +
  geom_histogram() +
  scale_fill_manual(values = c("darkorange", "darkorchid", "cyan4")) +
  facet_wrap(~ island, ncol = 2, scale = "free_y") +
  labs(title = "Comprimento da nadadeira dos penguins",
       fill = "Espécies",
       x = "Comprimento da nadadeira (mm)", 
       y = "Frequência") +
  theme_bw(base_size = 16)

ggplot(data = penguins, 
       aes(x = flipper_length_mm, fill = species)) +
  geom_histogram() +
  scale_fill_manual(values = c("darkorange", "darkorchid", "cyan4")) +
  facet_grid(species ~ .) +
  labs(title = "Comprimento da nadadeira dos penguins",
       fill = "Espécies",
       x = "Comprimento da nadadeira (mm)", 
       y = "Frequência") +
  theme_bw(base_size = 16)

ggplot(data = penguins) +
  aes(x = flipper_length_mm) +
  geom_density(color = "black", fill = "cyan4", alpha = .5) + 
  labs(title = "Comprimento da nadadeira dos penguins",
       x = "Comprimento da nadadeira (mm)", 
       y = "Densidade") +
  theme_bw(base_size = 16)

ggplot(data = penguins, 
       aes(x = flipper_length_mm, fill = species)) +
  geom_density(alpha = .5) +
  scale_fill_manual(values = c("darkorange", "darkorchid", "cyan4")) +
  labs(x = "Comprimento da nadadeira (mm)", 
       y = "Densidade", 
       fill = "Espécie") +
  theme_bw(base_size = 16)

ggplot_densidade <- ggplot(data = penguins, 
                           aes(x = flipper_length_mm, fill = species)) +
  geom_density(alpha = .5) +
  scale_fill_manual(values = c("darkorange", "darkorchid", "cyan4")) +
  labs(x = "Comprimento da nadadeira (mm)", 
       y = "Densidade", 
       fill = "Espécie") +
  theme_bw(base_size = 16)
ggplot_densidade

ggsave(filename = "density_ggplot2.png", 
       plot = ggplot_densidade, wi = 20, he = 15, 
       un = "cm", dpi = 300)

## ggpubr ----
gghistogram(data = penguins, 
            x = "flipper_length_mm",
            add = "median",
            color = "species",
            fill = "species",
            palette = c("darkorange", "darkorchid", "cyan4"),
            xlab = "Comprimento da nadadeira (mm)",
            ylab = "Densidade")

ggdensity(data = penguins, 
          x = "flipper_length_mm",
          add = "median",
          color = "species",
          fill = "species",
          palette = c("darkorange", "darkorchid", "cyan4"),
          xlab = "Comprimento da nadadeira (mm)",
          ylab = "Densidade")

ggpubr_densidade <- ggdensity(data = penguins, 
                              x = "flipper_length_mm",
                              add = "median",
                              color = "species",
                              fill = "species",
                              palette = c("darkorange", "darkorchid", "cyan4"),
                              xlab = "Comprimento da nadadeira (mm)",
                              ylab = "Densidade")

ggsave(filename = "densidade_ggpubr.png", 
       plot = ggpubr_densidade, wi = 20, he = 15, un = "cm", dpi = 300)

# grafico de setores ---------------------------------------------------

# classico
par(mar = c(0, 1, 0, 1))
pie(c(280, 60, 20),
    c("Sky", "Sunny side of pyramid", "Shady side of pyramid"),
    col = c("#0292D8", "#F7EA39", "#C4B632"),
    init.angle = -50, border = NA)

# calculo da proporcao
penguins_prop <- penguins %>%
  dplyr::count(species) %>% 
  dplyr::mutate(prop = round(n/sum(n), 4)*100)
penguins_prop

# graphics
par(mar = c(5, 5, 5, 5))
pie(penguins_prop$prop,
    labels = paste(penguins_prop$prop, "%"), 
    main = "Espécies",
    col = c("darkorange", "darkorchid", "cyan4"))
legend("topright", legend = penguins_prop$species, 
       fill = c("darkorange", "darkorchid", "cyan4"))

# ggplot2
ggplot(data = penguins_prop, aes(x = "", y = prop, fill = species)) + 
  geom_bar(stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(prop, "%")), color = "white", 
            position = position_stack(vjust = .5), size = 8) +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_void() +
  labs(fill = "Espécie")

# ggpubr
ggpie(penguins_prop,
      "prop", 
      label = "prop",
      lab.pos = "in", 
      lab.font = c(8, "white"),
      fill = "species", 
      color = "white",
      palette = c("darkorange", "purple", "cyan4"))

# ggplot2
ggplot(data = penguins_prop, aes(x = 2, y = prop, fill = species)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y", start = 0) +
  geom_text(aes(label = paste0(prop, "%")), color = "white",
            position = position_stack(vjust = .5), size = 5) +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  xlim(0, 2.5) +
  theme_void() +
  theme(legend.position = c(.5, .5),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15)) +
  labs(fill = "Espécie")

# ggpubr
ggdonutchart(penguins_prop,
             "prop", 
             label = "prop",
             lab.pos = "in", 
             lab.font = c(7, "white"),
             fill = "species", 
             color = "white",
             palette = c("darkorange", "purple", "cyan4"))

# grafico de barras ----------------------------------------------------

# numero de individuos coletados
penguins_count <- penguins %>%
  dplyr::count(species)
penguins_count

# graphics
barplot(n ~ species,
        data = penguins_count, 
        col = c("darkorange", "purple", "cyan4"),
        main = "Número de indivíduos coletados por espécie",
        xlab = "Espécies",
        ylab = "Frequência absoluta",
        cex.main = 1.5,
        cex.lab = 1.3,
        cex.axis = 1.2)

# ggplot2
ggplot(data = penguins_count, 
       aes(x = species, y = n, fill = species)) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = n), fill = "white") +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_bw(base_size = 15) +
  theme(legend.position = "none") +
  labs(x = "Espécie", 
       y = "Número de indivíduos", 
       fill = "Espécie")

ggplot(data = penguins_count, 
       aes(x = species, y = n, fill = species)) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = n), fill = "white") +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  coord_flip() +
  theme_bw(base_size = 15) +
  theme(legend.position = "none") +
  labs(x = "Espécie", 
       y = "Número de indivíduos", 
       fill = "Espécie")

# ggpubr
ggbarplot(penguins_count,
          x = "species",
          y = "n", 
          fill = "species", 
          color = "species",
          palette = c("darkorange", "purple", "cyan4"),
          label = TRUE, 
          lab.pos = "in", 
          lab.col = "white",
          lab.size = 8,
          main = "Número de indivíduos coletados por espécie",
          xlab = "Espécies",
          ylab = "Frequência absoluta",
          legend = "none")

ggbarplot(penguins_count,
          x = "species",
          y = "n", 
          fill = "species", 
          color = "species",
          palette = c("darkorange", "purple", "cyan4"),
          label = TRUE, 
          lab.pos = "out", 
          lab.col = "black",
          lab.size = 8,
          main = "Número de indivíduos coletados por espécie",
          xlab = "Espécies",
          ylab = "Frequência absoluta",
          legend = "none",
          orientation = "horiz")

# dynamit
penguins_flipper_length_mn_sd <- penguins %>%
  dplyr::group_by(species) %>% 
  dplyr::summarise(mean = mean(flipper_length_mm, na.rm = TRUE),
                   sd = sd(flipper_length_mm, na.rm = TRUE))
penguins_flipper_length_mn_sd

ggplot(data = penguins, 
       aes(x = species, y = flipper_length_mm, fill = species)) +
  stat_summary(fun = mean,
               geom = "col",
               color = "black",
               show.legend = FALSE) +
  stat_summary(fun.data = mean_se,
               geom = "errorbar",
               width = 0.15,
               linewidth = 0.8) +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_bw(base_size = 15) +
  labs(x = "Species", y = "Flipper length (mm)")

ggbarplot(penguins,
          x = "species",
          y = "flipper_length_mm",
          fill = "species",
          color = "gray30",              
          palette = c("darkorange", "purple", "cyan4"),
          add = "mean_se",
          add.params = list(size = 1),
          main = "Tamanho médio da barbatana por espécie (desvio padrão)",
          xlab = "Espécies",
          ylab = "Frequência absoluta",
          legend = "none")

# grafico de caixas ----------------------------------------------------

# graphics
boxplot(flipper_length_mm ~ as.factor(species),
        data = penguins,
        col = c("darkorange", "purple", "cyan4"),
        border = "black",
        main = "Espécies por amostragens",
        xlab = "Espécies",
        ylab = "Comprimento da nadadeira (mm)",
        cex.main = 1.5,
        cex.lab = 1.3,
        cex.axis = 1.2)

# ggplot2
ggplot(data = penguins, 
       aes(x = species, y = flipper_length_mm, fill = species)) +
  geom_boxplot(width = .3, 
               show.legend = FALSE) +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_bw(base_size = 15) +
  labs(x = "Species", y = "Flipper length (mm)")

ggplot(data = penguins, 
       aes(x = species, y = flipper_length_mm, fill = species)) +
  geom_boxplot(width = .3, 
               show.legend = FALSE) +
  geom_jitter(alpha = .5, 
              show.legend = FALSE, 
              position = position_jitter(width = .1, seed = 0)) +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_bw(base_size = 15) +
  labs(x = "Species", y = "Flipper length (mm)")

ggplot(data = penguins, 
       aes(x = species, y = flipper_length_mm, fill = species)) +
  geom_violin(width = .3, 
              show.legend = FALSE) +
  geom_jitter(alpha = .5, 
              show.legend = FALSE, 
              position = position_jitter(width = .1, seed = 0)) +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_bw(base_size = 15) +
  labs(title = "Pontos com jitter", x = "Species", y = "Flipper length (mm)")

# ggpubr
ggboxplot(data = penguins, 
          x = "species", 
          y = "flipper_length_mm",
          add = "jitter", 
          shape = "species",
          fill = "species",
          color = "black",
          palette = c("darkorange", "purple", "cyan4"),
          xlab = "Comprimento da nadadeira (mm)",
          ylab = "Frequência absoluta",
          legend = "none")

ggviolin(data = penguins, 
         x = "species", 
         y = "flipper_length_mm",
         add = "jitter", 
         shape = "species",
         fill = "species",
         color = "black",
         palette = c("darkorange", "purple", "cyan4"),
         xlab = "Comprimento da nadadeira (mm)",
         ylab = "Frequência absoluta",
         legend = "none")

# grafico de dispersao ------------------------------------------------

# graphics
par(mar = c(5, 5, 1, 1))
plot(bill_depth_mm ~ bill_length_mm,
     data = penguins,
     pch = 20,
     cex = 1.5,
     xlab = "Comprimento do bico (mm)", 
     ylab = "Profundidade do bico (mm)",
     cex.lab = 1.5,
     cex.axis = 1.3,
     bty = "l")

# ggplot2
ggplot(data = penguins, 
       aes(x = bill_length_mm, 
           y = bill_depth_mm,
           color = species,
           shape = species)) +
  geom_point(size = 3, alpha = .8) +
  scale_shape_manual(values = c(19, 15, 17))+
  scale_color_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_bw(base_size = 15) +
  labs(x = "Comprimento do bico (mm)", 
       y = "Profundidade do bico (mm)", 
       color = "Espécies", shape = "Espécies")

ggplot(data = penguins, 
       aes(x = bill_length_mm, 
           y = bill_depth_mm,
           color = species,
           shape = species)) +
  geom_point(size = 3, alpha = .8) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_shape_manual(values = c(19, 15, 17))+
  scale_color_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_bw(base_size = 15) +
  labs(x = "Comprimento do bico (mm)", 
       y = "Profundidade do bico (mm)", 
       color = "Espécies", shape = "Espécies")

# data + plot
datasaurus_dozen %>% 
  dplyr::filter(dataset == "dino") %>% 
  ggplot() +
  aes(x = x, y = y) +
  geom_point(colour = "black", fill = "black", 
             size = 5, alpha = .75, pch = 21) +
  theme_bw(base_size = 16)

# dados totais
datasaurus_dozen %>% 
  ggplot() +
  aes(x = x, y = y) +
  geom_point(colour = "black", fill = "black", 
             size = 1, alpha = .75, pch = 21) +
  facet_wrap(~dataset) +
  theme_bw(base_size = 16)

# ggpubr
ggscatter(data = penguins,
          x = "flipper_length_mm", 
          y = "bill_depth_mm",
          color = "species",
          fill = "species",
          palette = c("darkorange", "purple", "cyan4"),
          shape = "species", 
          size = 5,
          xlab = "Comprimento do bico (mm)", 
          ylab = "Profundidade do bico (mm)")

ggscatter(data = penguins,
          x = "flipper_length_mm", 
          y = "bill_depth_mm",
          color = "species",
          fill = "species",
          palette = c("darkorange", "purple", "cyan4"),
          shape = "species", 
          size = 5,
          xlab = "Comprimento do bico (mm)", 
          ylab = "Profundidade do bico (mm)",
          ellipse = TRUE, 
          mean.point = TRUE)

ggscatter(data = datasaurus_dozen %>% 
            dplyr::filter(dataset == "dino"),
          x = "x", 
          y = "y",
          size = 5,
          xlab = "X", 
          ylab = "Y")

ggscatter(data = datasaurus_dozen,
          x = "x", 
          y = "y",
          size = 1,
          xlab = "X", 
          ylab = "Y",
          facet.by = "dataset")


# grafico pareado -----------------------------------------------------

# graphics
penguins %>%
  dplyr::select(body_mass_g, ends_with("_mm")) %>%
  pairs(pch = 20,
        upper.panel = NULL)

# ggally
penguins %>%
  dplyr::select(body_mass_g, ends_with("_mm")) %>%
  ggpairs() +
  theme_bw(base_size = 16)

penguins %>%
  dplyr::select(species, sex, body_mass_g, ends_with("_mm")) %>%
  GGally::ggpairs(aes(color = species)) +
  scale_colour_manual(values = c("darkorange", "purple", "cyan4")) +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_bw(base_size = 16)

# psych
penguins %>%
  dplyr::select(body_mass_g, ends_with("_mm")) %>%
  pairs.panels(pch = 20, 
               ellipses = FALSE, 
               density = FALSE, 
               stars = TRUE, 
               hist.col = "gray",
               digits = 2,
               rug = FALSE,
               breaks = 10,
               ci = TRUE)

# combinando graficos -------------------------------------------------

# graphics
par(mfrow = c(1, 2))
boxplot(flipper_length_mm ~ as.factor(species),
        data = penguins,
        col = c("darkorange", "purple", "cyan4"),
        main = "Espécies por amostragens",
        xlab = "Espécies",
        ylab = "Comprimento da nadadeira (mm)",
        cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.2)
plot(bill_depth_mm ~ bill_length_mm,
     data = penguins,
     pch = 20,
     cex = 1.5,
     main = "Comprimento e profundidade do bico", 
     xlab = "Comprimento do bico (mm)", 
     ylab = "Profundidade do bico (mm)",
     cex.lab = 1.5, cex.axis = 1.3)


par(mfrow = c(2, 1))
boxplot(flipper_length_mm ~ as.factor(species),
        data = penguins,
        col = c("darkorange", "purple", "cyan4"),
        main = "Espécies por amostragens",
        xlab = "Espécies",
        ylab = "Comprimento da nadadeira (mm)",
        cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.2)
plot(bill_depth_mm ~ bill_length_mm,
     data = penguins,
     pch = 20,
     cex = 1.5,
     main = "Comprimento e profundidade do bico", 
     xlab = "Comprimento do bico (mm)", 
     ylab = "Profundidade do bico (mm)",
     cex.lab = 1.5, cex.axis = 1.3)


# ggplot2
ggplot_boxplot <- ggplot(data = penguins, 
                         aes(x = species, y = flipper_length_mm, fill = species)) +
  geom_boxplot(width = .3, 
               show.legend = FALSE) +
  geom_jitter(alpha = .5, 
              show.legend = FALSE, 
              position = position_jitter(width = .1, seed = 0)) +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_bw(base_size = 15) +
  labs(x = "Species", y = "Flipper length (mm)")
ggplot_boxplot

ggplot_scatterplot <- ggplot(data = penguins, 
                             aes(x = bill_length_mm, 
                                 y = bill_depth_mm,
                                 color = species,
                                 shape = species)) +
  geom_point(size = 3, alpha = .8) +
  scale_shape_manual(values = c(19, 15, 17))+
  scale_color_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_bw(base_size = 15) +
  labs(x = "Comprimento do bico (mm)", 
       y = "Profundidade do bico (mm)", 
       color = "Espécies", shape = "Espécies")
ggplot_scatterplot

# cowplot
# combinacao horizontal
plot_grid(ggplot_boxplot, ggplot_scatterplot, 
          align = "h", rel_widths = c(1, 1.5),
          labels = "AUTO")

# combinacao vertical
plot_grid(ggplot_boxplot, ggplot_scatterplot, 
          ncol = 1, align = "v", 
          labels = "AUTO")

# patchwork
# combinacao horizontal
ggplot_boxplot + ggplot_scatterplot

# combinacao vertical
ggplot_boxplot / ggplot_scatterplot

# graficos animados ---------------------------------------------------

# gganimate - demora uns 10 segundos!
plot_animate <- ggplot(data = penguins,
       aes(x = bill_length_mm, 
           y = bill_depth_mm, 
           color = species)) +
  geom_point() +
  scale_color_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_bw(base_size = 15) +
  labs(x = "Comprimento do bico (mm)", 
       y = "Profundidade do bico (mm)", 
       color = "Espécies", shape = "Espécies") +
  labs(title = "{closest_state}") +
  transition_states(species) +
  enter_grow() + 
  exit_fade()
plot_animate

# exportar
gganimate::anim_save(filename = "plot_animate.gif",
                     animation = plot_animate, wi = 700, he = 500, un = "px")

# graficos interativos ------------------------------------------------

# plotly
plot_ly(data = penguins,
        x = ~bill_length_mm, 
        y = ~bill_depth_mm, 
        type = "scatter",
        color = ~species,
        colors = c("darkorange", "purple", "cyan4")) %>% 
  layout(xaxis = list(title = "Comprimento do bico (mm)"),
         yaxis = list(title = "Profundidade do bico (mm)"))

plot_ly(data = penguins,
        x = ~bill_length_mm, 
        y = ~bill_depth_mm, 
        z = ~body_mass_g,
        type = "scatter3d",
        color = ~species,
        colors = c("darkorange", "purple", "cyan4")) %>% 
  layout(scene = list(xaxis = list(title = "Comprimento do bico (mm)"),
                      yaxis = list(title = "Profundidade do bico (mm)"),
                      zaxis = list(title = "Massa (g)")))

plot_penguins_scatter_int <- ggplotly(
  ggplot(data = penguins, 
         aes(x = bill_length_mm, 
             y = bill_depth_mm,
             color = species,
             shape = species)) +
    geom_point(size = 3, alpha = .8) +
    geom_smooth(method = "lm", se = FALSE) +
    scale_shape_manual(values = c(19, 15, 17)) +
    scale_color_manual(values = c("darkorange", "purple", "cyan4")) +
    theme_bw(base_size = 15) +
    labs(x = "Comprimento do bico (mm)", 
         y = "Profundidade do bico (mm)", 
         color = "Espécies", shape = "Espécies"))
plot_penguins_scatter_int

# export
htmlwidgets::saveWidget(widget = plot_penguins_scatter_int, 
                        file = "plot_penguins_scatter_int.html")

# graficos usando uma interface ---------------------------------------

# esquisse
palmerpenguins::penguins %>% 
  na.omit() %>% 
  esquisse::esquisser()

# end ---------------------------------------------------------------------
