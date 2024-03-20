# Multivariate-Analysis-of-Metal-Chemical-Constituents-in-Well-Water-Samples-
![image](https://github.com/Pavanirt/Multivariate-Analysis-of-Metal-Chemical-Constituents-using-RStudio/assets/160448544/e698de76-8a2c-4444-96b6-8e55650a3831)![image](https://github.com/Pavanirt/Multivariate-Analysis-of-Metal-Chemical-Constituents-using-RStudio/assets/160448544/1b0e92dc-28e0-4722-a4a8-585fd90dce7f)


A multivariate analysis on well water samples from Maine  and New Hampshire, USA


#loading the libraries
library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(reshape2)

#Setted the working directory 
#loading the dataset
data<-read_excel("C:\\KDU\\Semester 6\\MDA.xlsx")


head(data)




######################## EDA ##################################


# summary statistics
summary(data)


# Correlation matrix
print("Correlation Matrix:")
print(cor_matrix)

cor_matrix <- cor(data)
cor_matrix_long <- melt(cor_matrix)

# Calculate the correlation matrix
correlation_matrix <- cor(data)

# Creating a heatmap
heatmap(correlation_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(50),
        main = "Correlation Heatmap",
        xlab = "Variables",
        ylab = "Variables")

# Scatterplot matrix
pairs(data[-1], main = "Scatterplot Matrix")




########################## Objective 1 #########################


#removing the first column which is well water sample_no
data_components<-data[-1]

# Performing of PCA
pca_result <- prcomp(data_components,scale=TRUE)
pca_result





######################## Objective 2 ########################

# Converting data to a matrix
data_matrix <- as.matrix(data_components)

library(factoextra)
library(cluster)


# Determining the optimal number of clusters
wss <- c()
for (i in 1:10) {
  kmeans_model <- kmeans(data_matrix, centers = i)
  wss[i] <- kmeans_model$tot.withinss
}

# Plotting the scree plot
plot(1:10, wss, type = "b", xlab = "Number of Clusters", ylab = "Within-cluster Sum of Squares")

library(NbClust)

#obtain no of optimal clusters
NbClust(data_components, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 15, 
        method = "ward.D2", index = "all", alphaBeale = 0.1)

#optimal no of clusters =3
d_df <- dist(data_components)
d_df
wardse <- hclust(d_df,"ward.D2")

#dendrogram  
plot(wardse, hang = -1)

#data frame with sample no and cluster no
clusters <- cutree(wardse,k=3)
df2 <- data.frame(data$`well water sample_No`,clusters)
head(df2)





########################### objective 3 ########################


standard_values <- c(Be = 4, Cr = 100, Fe = 300, Ni = 20 , Cu = 1300, As = 10, Cd =5, Ba = 2000, Tl = 0.5, Pb = 15, U = 30)
standard_values

mean_values <- colMeans(df[, c("Be", "Cr", "Fe", "Ni", "Cu", "As", "Cd", "Ba", "Tl", "Pb", "U")])
mean_values



# Comparing of mean values with standard values
n=92 #no of samples
p=11 #no of variables
dset <- cbind(data_components)
S <- cov(dset)
S
x_bar = matrix(mean_values,c(11,1))
x_bar

mu_note =matrix(standard_values,c(11,1))
mu_note

#Test statistics 
T2_cal <- n*t(x_bar-mu_note)%*%solve(S)%*% (x_bar-mu_note)
T2_cal

Table_value =(n-1)*p/(n-p)*qf(0.95,p,n-p)
Table_value

#Below are the resulted tables

![image](https://github.com/Pavanirt/Multivariate-Analysis-of-Metal-Chemical-Constituents-using-RStudio/assets/160448544/bbc962ba-1949-4b62-b13c-ac22f56206af)
![image](https://github.com/Pavanirt/Multivariate-Analysis-of-Metal-Chemical-Constituents-using-RStudio/assets/160448544/966c2545-7857-44ac-b5ab-c0c522ff5568)
![image](https://github.com/Pavanirt/Multivariate-Analysis-of-Metal-Chemical-Constituents-using-RStudio/assets/160448544/0bfdda88-2ad3-4e0d-8cdf-a08e1b2b4ab9)


