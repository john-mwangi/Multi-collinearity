getwd()

wages <- read.csv('cps_85_wages_2.csv')

head(wages)

summary(wages)

# Perform linear regression with multicollinearity
model_mc <- lm(data = wages, formula = log(WAGE) ~ .)
summary(model_mc)

# Check if the model assumptions are met
par(mfrow = c(2,2)) #This plots the assumptions on a 2x2 matrix
plot(model_mc)

install.packages('GGally')

library(GGally)

# Check pair-wise correlations
ggpairs(wages)

# APPLYING F-G TEST
install.packages('mctest')

library(mctest)

# omcdiag() & imcdiag() test for overall and individual multicollinearity 
omcdiag(x = wages[-c(6)], y = wages[c(6)])

imcdiag(x = wages[-c(6)], y = wages[c(6)])

install.packages('ppcor')

library(ppcor)

pcor(x = wages[-c(6)], method = 'pearson')

names(wages)

# Run the model having removed multi-collinearity
model_wmc <- lm(data = wages[-c(4)], formula = log(WAGE)~.)

summary(model_wmc)

imcdiag(x = wages[-c(6,4)], y = wages[c(6)])
# There is now no multicollinearity (Klein=0)

set.seed(0)

wages.z <- scale(wages)
#Scale the data

head(wages.z)

install.packages('Hmisc')

library(Hmisc)
describe(wages)

# Better way to obtain summary statistics
library(pastecs)

round(x = stat.desc(wages),digits = 2)

class(stat.desc(wages))
typeof(stat.desc(wages))

plot(wages$WAGE, type = 'hist')

# Here we check if the variance in values (scale) is constant across the variables
# Raw values
plot(sapply(wages, var))

# Variance in scaled values is constant (good)
plot(sapply(X = as.data.frame(scale(wages)), FUN = var))

library(stats)

# prcomp is superior to pricomp
# Perfom PCA without response var according to package documentation
pc <- prcomp(wages[-c(6)], scale. = TRUE)

plot(pc)
plot(pc, type = 'l')

summary(pc)

# This plot is better because it reflects the actual proportion of variance
plot(pc$sdev^2/sum(pc$sdev^2))
#The square of the sdev is the variance

plot(cumsum(pc$sdev^2/sum(pc$sdev^2)))

head(pc$x)

dim(x = pc$x)
dim(wages)

comp_k <- pc$x[,c(1:6)]

clusters_k <- kmeans(x = comp_k, centers = 3)

summary(clusters_k)

# Cluster sizes
table(clusters_k$cluster)

# Add clusters to original data
wages$cluster=clusters_k$cluster

head(wages)

# Check NA values
colSums(is.na(wages))

 install.packages('factoextra')

library(factoextra)

par(mfrow = c(2,2))
fviz_nbclust(comp_k, kmeans, method = "silhouette")
fviz_nbclust(comp_k, kmeans, method = "wss")
fviz_nbclust(comp_k, kmeans, method = "gap_stat")

fviz_cluster(object = clusters_k, data = wages)

fviz_cluster(object = clusters_k, data = wages, ellipse.type = "norm")

clusters_h <- hclust(d = dist(x = scale(wages[-c(6)]), method = 'euclidean'), method = 'ward.D2')

clusters_h

plot(clusters_h)
abline(h=31, col='red')
# Optimal number of clusters is 3

h_clusters <- cutree(tree = clusters_h, k = 3)

h_clusters

table(h_clusters,wages$SOUTH)

fviz_nbclust(x = wages[-c(6)], FUNcluster = hcut, method = 'wss')
fviz_nbclust(x = wages[-c(6)], FUNcluster = hcut, method = 'silhouette')
fviz_nbclust(x = wages[-c(6)], FUNcluster = hcut, method = 'gap_stat')

# Use hcut() which compute hclust and cut the tree
# We use hcut because fviz_nbclust FUNcluster=hcut

hc.cut <- hcut(scale(wages[-c(6)]), k = 3, hc_method = "ward.D2")
fviz_cluster(hc.cut, ellipse.type = "norm")

#hclust has better segmentation. The clusters are better demarcated
# Use hcut() which compute hclust and cut the tree
hc.cut <- hcut(scale(wages[-c(6)]), k = 3, hc_method = "ward.D2")
# Visualize dendrogram
#fviz_dend(hc.cut, show_labels = FALSE, rect = TRUE)
# Visualize cluster
fviz_cluster(hc.cut, ellipse.type = "norm")
wages$clusters_h = h_clusters

head(wages)

names(wages)[names(wages)=='cluster'] <- 'cluster_k'
names(wages)[names(wages)=='clusters_h'] <- 'cluster_h'

head(wages)


