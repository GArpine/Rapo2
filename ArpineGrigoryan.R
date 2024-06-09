data <- read.csv(file.choose())
head(data)

dim(data)
summary(data)

str(data)

install.packages("car")
library(car)
scatterplotMatrix(~ TV + Billboards + Social_Media + Influencer_Marketing + Affiliate_Marketing + Product_Sold, data = data)


install.packages (c("FactoMineR" , "factoextra" ))
library ("FactoMineR")
library ("factoextra")

lm =  lm(formula = Product_Sold ~  TV + Billboards + Social_Media + Influencer_Marketing + Affiliate_Marketing, data = data)
summary(lm)

res.pca = PCA(data, scale.unit = TRUE)
print(res.pca)

eig.val <- get_eigenvalue (res.pca)
eig.val


fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))


fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("lightblue", "blue", "darkblue"),
             repel = TRUE)

fviz_pca_ind(res.pca, col.ind = "cos2", gradient.cols=c("blue", "purple3", "red"), 
             geom.ind = "point", pointsize = 1)
