### importing dataset :
data <- read.csv("Desktop/training_small_final_pc.csv", header = TRUE) ### for the training dataset 
#data <- read.csv("Desktop/test_final_pc.csv", header = TRUE) #### for the real dataset 
library(tree)
library(randomForest)
set.seed(100)
train<- sample(1:nrow(data),500)
data$sellingprice<- log(data$sellingprice)

data.test = data[-train, ]
sp.test<- data[-train,"sellingprice"]

### Using random forest and bagging in order to see the MSE and R^2
### to compare the final value. RF helps gives a better variable importance.
rf.sp <- randomForest(sellingprice~.-mmr, data = data, subset = train,
                      importance = TRUE,na.action=na.omit,mtry=6)
yhat.rf <- predict(rf.sp,newdata = data.test)
yhat.rf <- na.roughfix(yhat.rf)
mse <- mean((yhat.rf - sp.test)^2)
mse
rss<- mean((yhat.rf - sp.test)^2)
tss <- mean((data$sellingprice-mean(sp.test))^2)
rsq <- 1 - rss/tss
rsq
importance(rf.sp)

varImpPlot(rf.sp)

## To get a better MSE and R^2, using regression trees which is giving a better value.
tree.sp <- tree(sellingprice~.-mmr, data = data , subset = train)
plot(tree.sp)
text(tree.sp, pretty = 0)
yhat.tree <-  predict(tree.sp, newdata = data.test)
prune.sp<- prune.tree(tree.sp)
mse <- mean((yhat.tree-sp.test)^2)
mse
rss<- mean((yhat.tree-sp.test)^2)
tss <- mean((data$sellingprice-mean(sp.test))^2)
rsq<- 1 - rss/tss
rsq

