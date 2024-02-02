heavymetal <- read.csv('heavymetal_1.csv', header = TRUE, stringsAsFactors = TRUE)
heavymetal
levels(heavymetal$group)

library('dplyr')

group_by(heavymetal) %>%
  summarise(
    mean = mean(Zn, na.rm=TRUE),
    sd = sd(Zn, na.rm = TRUE),
    min = min(Zn, na.rm = TRUE),
    max = max(Zn, na.rm = TRUE)
  )





group_by(heavymetal, Group) %>%
  summarise(
    count = n(),
    mean = mean(Zn, na.rm=TRUE),
    sd = sd(Zn, na.rm = TRUE),
    min = min(Zn, na.rm = TRUE),
    max = max(Zn, na.rm = TRUE)
  )

Li <- read.csv('Li.csv', header = TRUE, stringsAsFactors = TRUE)
Cr <- read.csv('Cr.csv', header = TRUE, stringsAsFactors = TRUE)

kruskal.test(Zn ~ group, data = heavymetal)

library('FSA')

dunnTest(Cr~group,
         data=Cr,
         method = 'bh')




install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
library("ggpubr")
library(ggplot2)
ggboxplot(heavymetal, x = "group", y = "Li", 
          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("Aceh", "PO", "IN"),
          ylab = "Li", xlab = "Concentration")

