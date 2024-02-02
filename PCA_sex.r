library(tidyverse)
library(gridExtra)

heavymetal <- read.csv('sexmetal.csv', header = T)
head(heavymetal)

summary(heavymetal)


######進行主成份分析
pca <- prcomp(formula = ~Al+As+B+Ba+Ca+Fe+K+Mg+Mn+Na+Sr+Zn, 
              data=heavymetal,
              scale=TRUE, center=TRUE)
pca
summary(pca)


#Standard deviations:特徵值開根號
#Rotation: 特徵向量，也就是各個主成份，所對應的線性組合(linear combination)的係數

####畫陡坡圖 Scree plot-凱莎原則
plot(pca, 
     type='line', 
     main = 'Scree plot for heavymetal')

#加藍線飆出特徵值=1的地方
abline(h=1, col='blue')
#####特徵值大於1的主成份就可以選取

#step 1: 求出每個主成份的特徵值 (variance = std^2)
#從pca中取出標準差再平方，計算variance
vars <- (pca$sdev)^2
vars
#step 2: 計算每個主成份的解釋比例= 各個主成份的特徵值/總特徵值
props<- vars/sum(vars)
props
#step 3: 累加每個主成份的解釋比例(aggregated effects)
cumulative.props <-cumsum(props)
cumulative.props
#step 4: 把累積解釋比例畫成圖
cumulative.props[5]
plot(cumulative.props)

#pca$rotation
top4_pca.data <- pca$x[,1:6]
top4_pca.data

#特徵向量(原變數的線性組合)
pca$rotation
#前四個主成份的特徵向量
top4.pca.eigenvector <- pca$rotation[,1:6]
top4.pca.eigenvector

##主成份負荷圖，觀察原變數跟主成份之間的關係
first.pca <- top4.pca.eigenvector[,1]
second.pca <- top4.pca.eigenvector[,2]
third.pca <- top4.pca.eigenvector[,3]
forth.pca <- top4.pca.eigenvector[,4]
fifth.pca <- top4.pca.eigenvector[,5]
sixth.pca <- top4.pca.eigenvector[,6]
#第一主成份
first.pca[order(first.pca, decreasing=FALSE)]
#負荷圖繪製
dotchart(first.pca[order(first.pca, decreasing =FALSE)],
         main='Loading plot for PC1',
         xlab = 'Variable Loadings',
         color = '#264653', pch=16)
second.pca[order(second.pca, decreasing=FALSE)]
dotchart(second.pca[order(second.pca, decreasing =FALSE)],
         main='Loading plot for PC2',
         xlab = 'Variable Loadings',
         color = '#2A9D8F',pch=16)

third.pca[order(third.pca, decreasing=FALSE)]
dotchart(third.pca[order(third.pca, decreasing =FALSE)],
         main='Loading plot for PC3',
         xlab = 'Variable Loadings',
         color = '#E9C46A', pch = 16)

forth.pca[order(forth.pca, decreasing=FALSE)]
dotchart(forth.pca[order(forth.pca, decreasing =FALSE)],
         main='Loading plot for PC4',
         xlab = 'Variable Loadings',
         color = '#E76F51', pch=16)

biplot(pca, choices =c(1,2))
biplot(pca, choices = 2:3)

#### https://wangcc.me/LSHTMlearningnote/PCA.html
library('FactoMineR')
library('factoextra')

silky.pca <- PCA(heavymetal[,3:14], ncp=5, graph = FALSE)
eig.val <-get_eigenvalue(silky.pca)
eig.val #eigenvalue (variances of each principal components)
#eigen vectors
silky.pca$svd$V
summary(silky.pca)

##cluster analysis/PCA practical 
cor(heavymetal[,3:14])
hc <- hclust(dist(heavymetal), 'ave')
plot(hc, cex =0.8, hang = -1, 
     main = '', ylab = 'L2 dissimilarity measure', 
     xlab = 'no of specimen')

hc <- hclust(dist(heavymetal)^2)
plot(hc, cex = 0.8, hang = -1, 
     main = "", ylab = "L2squared dissimilarity measure", 
     xlab = "No. of specimen", sub = "")



###ggplot ploting 
library(usethis)
library(devtools)
library(grid)
library(ggbiplot)
ggbiplot(pca, ellipse= TRUE,labels = heavymetal$Sample, var.axes= TRUE, groups = heavymetal$sex,,choices =c(1,2))+
  scale_color_manual(values = c('#E76F51','#2A9D8F' ))+
  ggtitle('PCA of heavymetal')+
  theme(legend.position = 'bottom')
ggbiplot(pca, ellipse= TRUE,labels = heavymetal$Sample,groups = heavymetal$sex, choices =c(1,3))+
  scale_color_manual(values = c('#E76F51','#2A9D8F'))+
  ggtitle('PCA of heavymetal')+
  theme(legend.position = 'bottom')
ggbiplot(pca, ellipse= TRUE,labels = heavymetal$Sample,groups = heavymetal$sex, choices =c(1,4))+
  scale_color_manual(values = c('#E76F51','#2A9D8F'))+
  ggtitle('PCA of heavymetal')+
  theme(legend.position = 'bottom')
ggbiplot(pca, ellipse= TRUE,labels = heavymetal$Sample,groups = heavymetal$Group, choices =c(1,5))+
  scale_color_manual(values = c('#E76F51','#2A9D8F', '#E9C46A'))+
  ggtitle('PCA of heavymetal')+
  theme(legend.position = 'bottom')
ggbiplot(pca, ellipse= TRUE,labels = heavymetal$Sample,groups = heavymetal$Group, choices =c(1,6))+
  scale_color_manual(values = c('#E76F51','#2A9D8F', '#E9C46A'))+
  ggtitle('PCA of heavymetal')+
  theme(legend.position = 'bottom')
ggbiplot(pca, ellipse= TRUE,labels = heavymetal$Sample,groups = heavymetal$Group, choices =c(2,3))+
  scale_color_manual(values = c('#E76F51','#2A9D8F', '#E9C46A'))+
  ggtitle('PCA of heavymetal')+
  theme(legend.position = 'bottom')
ggbiplot(pca, ellipse= TRUE,labels = heavymetal$Sample,groups = heavymetal$Group, choices =c(2,4))+
  scale_color_manual(values = c('#E76F51','#2A9D8F', '#E9C46A'))+
  ggtitle('PCA of heavymetal')+
  theme(legend.position = 'bottom')
ggbiplot(pca, ellipse= TRUE,labels = heavymetal$Sample,groups = heavymetal$Group, choices =c(2,5))+
  scale_color_manual(values = c('#E76F51','#2A9D8F', '#E9C46A'))+
  ggtitle('PCA of heavymetal')+
  theme(legend.position = 'bottom')
ggbiplot(pca, ellipse= TRUE,labels = heavymetal$Sample,groups = heavymetal$Group, choices =c(2,6))+
  scale_color_manual(values = c('#E76F51','#2A9D8F', '#E9C46A'))+
  ggtitle('PCA of heavymetal')+
  theme(legend.position = 'bottom')
ggbiplot(pca, ellipse= TRUE,labels = heavymetal$Sample,groups = heavymetal$Group, choices =c(3,4))+
  scale_color_manual(values = c('#E76F51','#2A9D8F', '#E9C46A'))+
  ggtitle('PCA of heavymetal')+
  theme(legend.position = 'bottom')








