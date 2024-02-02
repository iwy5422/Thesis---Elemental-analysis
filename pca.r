library('FactoMineR')
library('factoextra')
heavy.pca <- PCA(heavymetal_2[,-1], graph = FALSE)
eig.val<-get_eigenvalue(heavy.pca)
eig.val

#plotting Scree plot
fviz_eig(heavy.pca, addlabels = TRUE)
#plotting variables-PCA
fviz_pca_var(heavy.pca,col.var = 'black')

library('corrplot')
var<-get_pca_var(heavy.pca)
corrplot(var$contrib,is.corr = FALSE)


#plotting pca graph
fviz_pca_ind(heavy.pca,
             geom.ind='point',
             col.ind = heavymetal_2$Group,
             palette = c('#44C5CB', '#FCE315','#F53D52','#FF9200'),
             addEllipses = TRUE,
             mean.point= FALSE,
             legend.title = 'Groups',
             ellipse.type = 'confidence')


# https://rstudio-pubs-static.s3.amazonaws.com/323416_ab58ad22d9e64ba2831569cf3d14a609.html