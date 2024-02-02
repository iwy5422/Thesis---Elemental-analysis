metal_6 <-read.csv('6metal.csv',header = TRUE)
metal_6

for(var in c('As','Co','Cr','Cu','Ni','Zn')){
  formula <-as.formula(paste('Length ~', var))
  model <- lm(formula, data=metal_6)
  print(paste("Regression for", var))
  print(summary(model))
}

library('ggplot2')
par(mfrow=c(2,3))
variables<-c('As','Co','Cr','Cu','Ni','Zn')
for (var in variables){
  formula <- as.formula(paste('Length ~', var))
  fit <- lm(formula,data=metal_6)
  r2<-summary(fit)$r.squared
  p_value <-summary(fit)$coefficients[2,4]
  plot(metal_6[[var]], metal_6$Length, xlab=var, ylab='Length',
       main = paste('Length vs', var))
  abline(fit, col='red')
  legend_text <-paste("R^2 = ", round(r2,2),'\n','p =',format.pval(p_value,digits=2))
  legend('bottomright', legend=legend_text, bty='n',cex=0.8)
}

data_2 <- read.csv('length.csv', header = TRUE)
data_2
par(mfrow=c(4,5))
variables<-c('Ag','Al','As','B','Ba','Ca','Co','Cr','Cu','Fe','Ga','K','Li','Mg','Mn','Na','Ni','Sr','Zn')

for(var in variables){
  formula <-as.formula(paste('length ~', var))
  model <- lm(formula, data=data_2)
  print(paste("Regression for", var))
  print(summary(model))
}

for (var in variables){
  formula <- as.formula(paste('length ~', var))
  fit <- lm(formula,data=data_2)
  r2<-summary(fit)$r.squared
  p_value <-summary(fit)$coefficients[2,4]
  plot(data_2[[var]], data_2$length, xlab=var, ylab='Length',
       main = paste('Length vs', var))
  abline(fit, col='red')
  legend_text <-paste("R^2 = ", round(r2,2),'\n','p =',format.pval(p_value,digits=2))
  legend('bottom', legend=legend_text, bty='n',cex=0.8)
}

for(var in c('As','Co','Cr','Cu','Ni','Zn')){
  formula <-as.formula(paste('Length ~', var))
  model <- lm(formula, data=metal_6)
  print(paste("Regression for", var))
  print(summary(model))
}




