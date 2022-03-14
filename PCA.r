# Reading the plants dataset
plants_data <- read_xlsx(file.choose())
# Get the row names of the dataset
plants = row.names(plants_data)
names(plants_data)
#Convert the site column from character to numeric
plants_data$Site=as.numeric(plants_data$Site)
# Remove the site column from the dataset while keeping only the quantitive variable
plants <- plants_data[,-1]
# Calculate the means of the variable
means <- apply(plants, 2, mean)
# Format the value to remove the scientific notation
format(means, scientific = F)
# Calculate the variance of the variable
variance <- apply(plants, 2, var)
# Format the value to remove the scientific notation
format(variance, scientific = F)
# Performing PCA on the plants dataset
pr.out=prcomp(plants_data, scale=TRUE)
# Function to assign distinct color to each variable
Cols=function (vec ){
 cols=rainbow (length (unique (vec )))
 return (cols[as.numeric (as.factor (vec))])
}
# Plotting the principal components
par(mfrow =c(1,2))
plot(pr.out$x [,1:2], col=Cols(plants),pch =19, xlab ="Z1",ylab="Z2")
plot(pr.out$x [,1:3], col=Cols(plants),pch =19, xlab ="Z1",ylab="Z3")
# Getting the summary of the proportion of variance explained by principal component
summary(pr.out)
# Plotting the variance explained by the first few principal components
par(mfrow =c(1,1))
plot(pr.out)
# Calculating the proportion of variance explained
pve=100*pr.out$sdev^2/sum(pr.out$sdev ^2)

# Plotting the PVE of each principal component
par(mfrow =c(1,2))
plot(pve, type ="o", ylab="PVE", xlab="Principal Component",col="blue")

#Plotting the cummulative PVE of each principal component
plot(cumsum(pve ), type="o", ylab ="Cumulative PVE", xlab="Principal Component", col="brown3")

#Biplot
par(mfrow=c(1,1))
biplot(pr.out, scale=TRUE)
