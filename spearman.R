metal_6 <-read.csv('6metal.csv',header = TRUE)


variables <-c("As", "Co", "Cr", "Cu", "Ni", "Zn")
results <- data.frame() 
for (var in variables){
  spearman <- cor.test(metal_6$Length,metal_6[[var]], method = 'spearman')
  results <-rbind(results, data.frame(variable = var, 
                                      spearman_rho = spearman$estimate,
                                      p_value = spearman$p.value))
}
print(results)


data_2 <- read.csv('length.csv', header = TRUE)
variables<-c('Ag','Al','As','B','Ba','Ca','Co','Cr','Cu','Fe','Ga','K','Li','Mg','Mn','Na','Ni','Sr','Zn')
results_2 <- data.frame() 
for (var in variables){
  spearman <- cor.test(data_2$length,data_2[[var]], method = 'spearman')
  results_2 <-rbind(results_2, data.frame(variable = var, 
                                      spearman_rho = spearman$estimate,
                                      p_value = spearman$p.value))
}
print(results_2)




