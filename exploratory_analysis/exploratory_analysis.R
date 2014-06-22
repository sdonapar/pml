library(caret)

check_nulls <- function(data,colnames) {
        for (i in seq(1,length(colnames))) {
                print(paste(i,":",colnames[i]))
                print(table(is.na(data[,c(i)])))
        }
}
pml_write_files <- function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}

setwd("~/coursera/pml")
pml <- read.csv("pml-training.csv", na.string="#DIV/0!")
pml_test <- read.csv("pml-testing.csv", na.string="#DIV/0!")
pml_names <- names(pml)
check_nulls(pml,pml_names)
## extracting the original data

x <- grep("_x$",pml_names)
pml_names[x]
y <- grep("_y$",pml_names)
pml_names[y]
z <- grep("_z$",pml_names)
pml_names[z]

myvar = c(3:11,37:45,46:49,60:68,84:86,102,113:121,122:124,140,151:159,160)

#mydata <- data.frame(pml[,x],pml[,y],pml[,z])
mydata <- pml[,myvar]
#mydata$classe <- pml$classe
mydata_names <- names(mydata)
#check_nulls(mydata,mydata_names)
m <- na.omit(mydata)

mydata_test <- data.frame(pml_test[,x],pml_test[,y],pml_test[,z])
mydata_test$classe <- pml_test$classe
#check_nulls(mydata,mydata_names)


belt <- grep("belt",mydata_names)
belt_data <- mydata[,belt]
belt_data$classe <- mydata$classe
plot(belt_data[,c(1,4,7)])
plot(belt_data[,c(2,5,8)])
plot(belt_data[,c(3,6,9)])
boxplot(belt_data)
qplot(classe, data=mydata, geom="density",fill=classe)
qplot(classe, data=mydata, geom="density",fill=classe, alpha=I(0.5))

#modFit <- train(classe ~ ., method="rpart",data=mydata)
#answers <- predict(modFit,newdata=pml_test)
#A A A A A D D D A A A D D A D A A D A A
modFit1 = train(classe ~ . , data = mydata, method = 'rf', trControl = trainControl(method='oob'))
answers <- predict(modFit,newdata=pml_test)
fit <- modFit1$finalModel
        
confusion=fit$confusion
sensitivity=(confusion[2,2]/(confusion[2,2]+confusion[2,1]))*100
specificity=(confusion[1,1]/(confusion[1,1]+confusion[1,2]))*100
overall_error=fit$err.rate[length(fit$err.rate[,1]),1]*100
overall_accuracy=1-overall_error
class1_error=paste(rownames(confusion)[1]," error rate= ",confusion[1,3], sep="")
class2_error=paste(rownames(confusion)[2]," error rate= ",confusion[2,3], sep="")
overall_accuracy=100-overall_error


varImpPlot(fit, type=2, n.var=30, scale=FALSE, main="Variable Importance (Gini) for top 30 predictors")

