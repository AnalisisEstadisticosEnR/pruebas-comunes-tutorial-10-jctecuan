#Tutorial 10
#Pruebas comunes
#Juan Carlos MArtinez Montes

##DOS MUESTRAS



setwd("G:/MAESTRIA-JC/CLASES/Analisis en Lenguaje R/04 Analisis estadisticos")

Mis_datos <- ToothGrowth

#Comparar dos varianzas (Prueba F de Fiseher)

#Buscar el valor critico de la razon de varianza

#Pruebe F es muy sensible a suposicion de normalidad
#prueba de normalidad con Shapiro-Wilk y grafica de QQplot

library(ggpubr)
shapiro.test(Mis_datos$len)
ggqqplot(Mis_datos$len)

#T10_01_prueba_f_ggqqplot

res.Ftest <- var.test(len~supp, data = Mis_datos)
res.Ftest

#Comparar dos medias, prueba t de Student

#datos en dos vectores

peso_mujer <- c(38.9, 61.2, 73.3, 21.8, 63.4, 64.6, 48.4, 48.8, 48.5)
peso_hombre <- c(67.8, 60, 63.4, 76, 89.4, 73.3, 67.3, 61.3, 62.4)

mi_data <- data.frame(grupo = rep(c("Mujer", "Hombre"), each = 9),
                      peso = c(peso_mujer, peso_hombre))
#visualizar

library(dplyr)
group_by(mi_data, grupo) %>% 
  summarise(count = n(),
            mean = mean(peso, na.rm = TRUE),
            sd = sd(peso, na.rm = TRUE))

library(ggpubr)

ggboxplot(mi_data, x = "grupo", y = "peso",
          color = "grupo", palette = c("#00AFBB", "#E7B800", "#"),
          ylab = "Peso", xlab = "Grupos")
#T10_02_ggboxplot_peso

#comprobar distribucion normal y varianzas iguales

shapiro.test(mi_data$peso)

res.ftest <- var.test(peso ~ grupo, data = mi_data)
res.ftest

#prueba t de muestras independientes

res <- t.test(peso ~ grupo, data = mi_data, var.equal = TRUE)
res

#probar si peso promedio es menor en hombres que mujeres

t.test(peso ~ grupo, data = mi_data, paired = TRUE, 
       alternative = "less")

#Muestras dependientes

#formato ancho

library(tidyr)
library(datarium)
data("mice2", package = "datarium")
head(mice2, 3)

mice2.long <- mice2 %>% gather(key = "group",
                               value = "weight",
                               before,
                               after)

head(mice2.long, 3)

#estadisticas basicas

mice2.long %>% group_by(group) %>% 
  get_summary_stats(weight, type = "mean_sd")

#prueba de t

res <- t.test(weight ~ group, data = mice2.long,
              paired = TRUE)
res

#visualizar los datos

bxp <- ggpaired(mice2.long, x = "group", y = "weight",
                order = c("before", "after"),
                ylab = "Weight", xlab = "Groups")

bxp

#T10_03_bxp_mice2

##COMPARAR DOS MEDIANAS

#muestras emparejadas, Prueba de Wilcoxon

dat2 <- data.frame(
  Beginning = c(16, 5, 15, 2, 14, 15, 4, 7, 15, 6, 7, 14),
  End = c(19, 18, 8, 17, 8, 7, 16, 19, 20, 9, 11, 18)
)
dat2

dat2 <- data.frame(Time = c(rep("Before", 12), rep("After", 12)),
                   Grade = c(dat2$Beginning, dat2$End))
dat2

#visualizar los datos
#reordenar tiempo

dat2$Time <- factor(dat2$Time, levels = c("Before", "After"))

ggplot(dat2) +
  aes(x = Time, y = Grade) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

test <- wilcox.test(dat2$Grade ~dat2$Time,
                    paired = TRUE)

test

#muestras independientes
#Prueba Mann-Withnney-Wilcoxon

dat <- data.frame(
  Sex = as.factor(c(rep("Girl", 12), rep("Boy", 12))),
                    Grade = c(
                      19, 18, 9, 17, 8, 7, 16, 19, 20, 9, 11, 18,
                      16, 5, 15, 2, 14, 15, 4, 7, 15, 6, 7, 14))

dat

#Visualizar

ggplot(dat) +
  aes(x = Sex, y = Grade) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
#T10_05_sex_grade
#ver normalidad o no

ggqqplot(dat$Grade)

#prueba no parametrica

test <- wilcox.test(dat$Grade ~ dat$Sex, 
                    alternative = "less")

test

#Prueba binomial de proporciones

#Ej1, prueba binomial de dos colas

binom.test(9, 24, 1/6)

#Ej2, prueba binomial de cola izquierda

binom.test(11, 30, 0.5, alternative = "less")

#Ej3, prueba binomial de cola derecha

binom.test(46, 50, 0.8, alternative = "greater")

#PEARSON

my_data <- mtcars
head(my_data, 6)
library(ggpubr)
ggscatter(my_data, x = "mpg", y = "wt",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xliab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")
#T10_07_pearson_mtcars

#prueba de normalidad con Shapiro-Wilk

shapiro.test(my_data$mpg)
shapiro.test(my_data$wt)

#visual

ggqqplot(my_data$mpg, ylab = "MPG")
#T10_11_norm_mpg
ggqqplot(my_data$wt, ylab = "WT")
#T10_12_norm_wt

#prueba de pearson

res <- cor.test(my_data$wt, my_data$mpg, 
                method = "pearson")
res

#SPEARMAN

res2 <- cor.test(my_data$wt, my_data$mpg, method ="spearman")
res2

#Chi-cuadrada

#H0 las variables son independientes, conocer valor de 1 no predice la 2
#H1 las variables son dependientes, predecir 1, predice 2

dat <- iris
head(dat)

dat$size <- ifelse(dat$Sepal.Length < median(dat$Sepal.Length), "small", "big")

#crear tabla de contingencia

table(dat$Species, dat$size)

#visual
library(ggplot2)
library(ggpubr)

ggplot(dat) +
  aes(x = Species, fill = size) +
  ylab("(n)") +
  xlab("Especies") +
  geom_bar(width = 0.4) +
  scale_fill_manual(values = c("#00667A", "#9D6A89")) +
  theme_classic2() +
  theme(axis.text.x = element_text(size = 12, face = "italic"),
        axis.title = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 10, face = "bold"))
  
#T10_12_bar_sepal
#chi cuadrada a Especie y tamaño

test <- chisq.test(table(dat$Species, dat$size))
test
