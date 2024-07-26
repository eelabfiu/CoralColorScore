#### Script for Quantifying Color Score from RGB Intensity ####


####Load Required R Packages####
##Install Packages if Needed
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("vegan")) install.packages("vegan")

##Load Packages
library("ggplot2")
library("vegan")


#### Load and Organize Data ####

##Load Data
#Read in the Color Data file completed following the Example Data file
Color<-read.csv("ColorExampleData.csv", header=TRUE)

##Check the structure of the data file
str(Color)

#ID: ID's for each coral sample. Should be unique per sample and contain no spaces
#Treatment: If conducting an experiment with heated and control treatments, include a variable with that information
#GroupVar1, 2, etc: Include any additional grouping variables of interest, including Genotypes, Sites, etc.
#Red.Standard: Column with the Mean intensity of the Red color standard (tape) on the Red channel measured in ImageJ
#Green.Standard: Column with the Mean intensity of the Green color standard (tape) on the Green channel measured in ImageJ
#Blue.Standard: Column with the Mean intensity of the Blue color standard (tape) on the Blue channel measured in ImageJ
#Red.Coral: Column with the Mean intensity of the Coral sample on the Red channel measured in ImageJ
#Green.Coral: Column with the Mean intensity of the Coral sample on the Green channel measured in ImageJ
#Blue.Coral: Column with the Mean intensity of the Coral sample on the Blue channel measured in ImageJ

##Set rownames to ID
rownames(Color)<-Color$ID


#### Standardize Colors ####
##Standardize RGB colors by dividing Coral color by color standards. 
#Repeat for Red, Green, and Blue
Color$Red.Norm.Coral <- Color$Red.Coral/Color$Red.Standard
Color$Green.Norm.Coral <- Color$Green.Coral/Color$Green.Standard
Color$Blue.Norm.Coral <- Color$Blue.Coral/Color$Blue.Standard


#### Calculate Color Score ####

##Create matrix of standardized colors
Color.mat <- as.matrix(cbind(Color$Red.Norm.Coral,Color$Green.Norm.Coral,Color$Blue.Norm.Coral))

##Set row names to sample ID
rownames(Color.mat) <- Color$ID

##Create a Distance Matrix for PCA
Color.dist <- vegdist(Color.mat, method="euclidean", na.rm=TRUE)

##Run Principal Components Analysis
Color.PCA <- princomp(Color.dist) 

##Extract PC1 as Color Score
Color$Score<-Color.PCA$scores[,1]


#### Color Score QC ####

##Initial Visual Check
ggplot(Color, aes(x=ID, y=Score)) + 
  geom_boxplot(alpha=0.5, shape=2, outlier.shape = NA)+
  geom_jitter(shape=16, position=position_jitter(0.1))+
  theme(axis.text.x = element_text(angle = 90))

##If needed, invert signs by multiplying by -1 for intuitive interpretation of Control > Heated
#Example: Invert signs for Control > Heated
#If not needed, add "#" before the line of code below to skip this step
Color$Score<-Color$Score*(-1)

##If needed, add a constant value to make all scores positive for more straight forward interpretation
#Example: Add 20 to make all score values positive 
#If not needed, add "#" before the line of code below to skip this step
Color$Score<- Color$Score +20

##Repeat Visual Check
ggplot(Color, aes(x=ID, y=Score)) + 
  geom_boxplot(alpha=0.5, shape=2, outlier.shape = NA)+
  geom_jitter(shape=16, position=position_jitter(0.1))+
  theme(axis.text.x = element_text(angle = 90))

##Plot by Treatment
ggplot(Color, aes(x=Treatment, y=Score)) + 
  geom_boxplot(alpha=0.5, shape=2, outlier.shape = NA)+
  geom_jitter(shape=16, position=position_jitter(0.1))

