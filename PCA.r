#setting the appropriate directory
setwd("D:/Documents/UNE/Trimester-4/STAT430 - Statistical
Learning/Assignment-4")
#reading the plants dataset
plants_data <- read.csv("STAT330430_plants_update.csv")
#get the row names of the dataset
plants = row.names(plants_data)
#get the variable names of the dataset
names(plants_data)
#convert the site column from character to numeric
plants_data$Site=as.numeric(plants_data$Site)
#remove the site column from the dataset while keeping only the quantitive
variable
plants <- plants_data[,-1]
#calculate the means of the variable
means <- apply(plants, 2, mean)
#format the value to remove the scientific notation
format(means, scientific = F)
#calculate the variance of the variable
variance <- apply(plants, 2, var)
#format the value to remove the scientific notation
format(variance, scientific = F)
#performing PCA on the plants dataset
pr.out=prcomp(plants_data, scale=TRUE)
#function to assign distinct color to each variable
Cols=function (vec ){
 cols=rainbow (length (unique (vec )))
 return (cols[as.numeric (as.factor (vec))])
}
#plotting the principal components
par(mfrow =c(1,2))
plot(pr.out$x [,1:2], col=Cols(plants),pch =19, xlab ="Z1",ylab="Z2")
plot(pr.out$x [,1:3], col=Cols(plants),pch =19, xlab ="Z1",ylab="Z3")
#getting the summary of the proportion of variance explained by principal
component
summary(pr.out)
#plotting the variance explained by the first few principal components
par(mfrow =c(1,1))
plot(pr.out)
#calculating the proportion of variance explained
pve=100*pr.out$sdev^2/sum(pr.out$sdev ^2)
#plotting the PVE of each principal component
par(mfrow =c(1,2))
plot(pve, type ="o", ylab="PVE", xlab="Principal Component",col="blue")
#plotting the cummulative PVE of each principal component
plot(cumsum(pve ), type="o", ylab ="Cumulative PVE", xlab="Principal
Component", col="brown3")
#Plotting the biplot
par(mfrow=c(1,1))
biplot(pr.out, scale=TRUE)