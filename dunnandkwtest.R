Aceh <- c(0.227,0.022,0.003,0.002,0.011,0.014,0.051)
IN <-c(0.010,0.010,0.006,0.008,0.015,0.003,0.004, 0.009,0.017,0.016)
PO <-c(0.015,0.017,0.021,0.024,0.151,0.069,0.016,0.066)

data <- data.frame(
  values = c(Aceh, IN, PO),
  group = factor(rep(c("Aceh", "IN", "PO"), 
                     times = c(length(Aceh), length(IN), length(PO))))
)
kruskal_test <- kruskal.test(values ~ group, data = data)
print(kruskal_test)

install.packages("dunn.test")
library(dunn.test)
dunn_result <- dunn.test(data$values, data$group, method="bonferroni")
print(dunn_result)

