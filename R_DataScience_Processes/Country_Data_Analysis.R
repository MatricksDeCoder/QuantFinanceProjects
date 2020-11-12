# import and require libraries
require(gdata)
library(readxl)
require(corrplot)
library(caret)

# import the excel data for countries
df <- read_excel("CountryData.xlsx")
head(df, 5)

# import variables names
namesDF <- read_excel("CountryData.xlsx", sheet = 'seriesLabels')
givenNames <- namesDF[,c(3)]
givenNames <- unlist(givenNames)
givenNames

# Removing blank rows generated
df<- df[c(1:21)]
head(df,5)

# view structure of the data 
str(df) #192 obs and 21 variables with type double and one char

#1. Renaming column variables names to their friendly names 
colnames(df)
names(df)[3:21] <- givenNames
head(df,5)
colnames(df)

#2. Exploratory data analysis

#2.1 correlations 
cor_matrix <- cor(df[,c(-1,-2)],use="complete.obs")
cor_matrix
corrplot(cor_matrix,method ="color",type="upper",tl.cex=0.7)

#2.2 boxplots all variables 
par(cex.axis=0.52)
boxplot(df[,c(-1,-2)],las=2)

#2.3 boxplots without GDP with scale that dominates others
boxplot(df[,c(-1,-2, -12)],las=2)

#2.4 boxplots without GDP, Mortality Male, Mortality Female -12)],las=2)
boxplot(df[,c(-1,-2, -12,-13, -14)],las=2)

#2.5 Boxplot of standardised data
boxplot(scale(df[,c(-1,-2)]),las=2)

#3. Handle null values, missing data using imputation

#count the number of null values in rows
nullsRows <- apply(df, 1, function(x) sum(is.na(x)))
nullsRows

#we add the country names to view by country 
rowNulls <- data.frame(df$CountryName,nullsRows)
rowNulls

#view only countries with few nulls
#we select where not 0
rowNulls[as.numeric(rowNulls[,2])>0,]

# fromabove we see few nulls in country data but many 
# countries with nulls imputation to handle null values 
# instead of deleting rows entries with null values

# nulls in columns
nullsColumns <- apply(df, 2, function(x) sum(is.na(x)))
nullsColumns

# almost every column has null values we cant drop some

#3.1 Data imputation to handle nulls

#set seed
set.seed(0)
#we impute missing values with a random forest

imputationModel <- preProcess(x = df[,-c(1,2)],method = "bagImpute")
imputatedData <- predict(object = imputationModel,newdata=df[,-c(1,2)])
imputatedData
length(imputatedData)

# Adding country names to the rows
countryNames <- df[,c(2)]
countryNames
newData  <- data.frame(countryNames,imputatedData)
newData

#Checking out this fresh imputated data
head(newData)
str(newData)

# check if there are no nulls in new data 
apply(newData, 2, function(x) sum(is.na(x)))

# above showsall values n columns = 0 nulls


#4. Mean, Max, Mode and other data analysis
dataColumns <- data.frame(newData[,c(-1)])
head(dataColumns,5)

means <- sapply(dataColumns,mean)
medians <- sapply(dataColumns, median)
means
medians

#5. Model Building 

#5.1 Principal Component Analysis biplot
pca.out<-prcomp(dataColumns,scale=TRUE)
pca.out
biplot(pca.out,scale = 0, cex=0.75)


#5.2 Principal Component Analysis Explained Variance
# ..of eachof the 19 variables
# Creating a datatable to store and plot the

# No of Principal Components vs Cumulative Variance Explained
vexplained <- as.data.frame(pca.out$sdev^2/sum(pca.out$sdev^2))
vexplained <- cbind(c(1:19),vexplained,cumsum(vexplained[,1]))
colnames(vexplained) <- c("No_of_Principal_Components","Individual_Variance_Explained",
                          
                          "Cumulative_Variance_Explained")
vexplained

#5.3 Principal Component Analysis, show contribution of
# ..each of the variables to the principal components
# will use package factoextra to visualize contributions variable
# ..to each of the components

if(!require(devtools)) install.packages("devtools")
# devtools::install_github("kassambara/factoextra")
library(factoextra) 

plot1 <- fviz_contrib(pca.out, choice="var", axes = 1, top = 19)
plot2 <- fviz_contrib(pca.out, choice="var", axes = 2, top = 19, color = "lightgrey")
library(gridExtra)
grid.arrange(plot1, plot2, nrow=2)

#5.4 Scaling the data
scaledData = scale(dataColumns)

#6. Clustering k means 

#6.1 visualize dissimilarity matrix answers if we
#.. can cluster the data using some distance metric
fviz_dist(dist(scaledData), show_labels = FALSE)+ labs(title = "Euclidean distance")

#plotting just 20 countires :) 
randomplot <- sample(1:183, 20)
mydata.sample<- scaledData[randomplot,]
fviz_dist(dist(mydata.sample), show_labels = TRUE)+ labs(title = "Euclidean distance")

#6.2 Install packages for k-means clustering
# install.packages(c("nloptr", "seriation", "pbkrtest", "NbClust", "cluster", "car", "scales", "fpc", "mclust", "apcluster", "vegan"))

# install package to help choose k
if(!require(NbClust)) install.packages("NbClust")
library(NbClust)
res<- NbClust(scaledData, distance = "euclidean", min.nc=2, max.nc=10, 
              
              method = "kmeans", index = "all")  
# Can access details of the tests here if you like
res

# results above will have D Index graph method
# we choose a significant knee in plot to select k
# results will also show proposed k among indices
# .. e.g 8 proposed 2, 9 proposed 3..etc 
# majority e.g 3

#6.3 k-means clustering 
km.res <- kmeans(scaledData, 3, nstart = 50)
# trying several random starts has been seen as ideal

data <- data.frame(scaledData)
row.names(data) <- unlist(df[,c(2)])
data
# visualizing the clusters
fviz_cluster(km.res, 
             
             data = data,
             
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             
             ellipse.type = "euclid", 
             
             star.plot = TRUE, # Add segments from centroids to items
             
             repel = TRUE, # Avoid label overplotting (slow)
             
             ggtheme = theme_minimal()
             
)

# Finding the averages for each cluster
aggregate(newData, by=list(cluster=km.res$cluster), mean)

#6.1 World mapping plot
#Required libraries for world map plotting
if(!require(rworldmap)) install.packages("rworldmap")
library(rworldmap)
if(!require(rworldxtra)) install.packages("rworldxtra")
library(rworldxtra)

# we plot to map
cluster = as.numeric(km.res$cluster)
par(mfrow=c(1,1))
spdf = joinCountryData2Map(data.frame(cluster,df$CountryName), joinCode="NAME", nameJoinColumn="df.CountryName",verbose = TRUE,mapResolution = "low")
mapCountryData(spdf, nameColumnToPlot="cluster", catMethod="fixedWidth",colourPalette=c("#2E9FDF","#00AFBB","#E7B800"), addLegend = FALSE, lwd = 0.5)




