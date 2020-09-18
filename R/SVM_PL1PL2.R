library(e1071)
library(doParallel)

args <- commandArgs(TRUE)
prefix <- args[1]
npcs <- args[2]
kernel.opt <- args[3]

pc <- read.table(paste0(prefix, "pc.txt"), header = TRUE)
phe <- read.table(paste0(prefix, "_popref_asn.txt"), header = TRUE)
print(paste("Prepare the PC file and the reference file, starts at ",date()))
pop <- phe[, c("IID", "Population")]
train.cols <- c("IID", paste0("PC", seq(1:npcs)))
train.data <- pc[pc$AFF == 1, train.cols]
train.phe <- merge(train.data, pop, by = "IID")
test.cols <- c("FID","IID", paste0("PC", seq(1:npcs)))
test.data <- pc[pc$AFF == 2, test.cols]

train.x <- train.phe[, !colnames(train.phe) %in% c("Population", "IID")]
train.y <- train.phe[, "Population"]

numCores <- detectCores()
registerDoParallel(cores = min(round(numCores/2),41))

if (kernel.opt == "linear") {
  # For Linear
  tuneresult <- foreach(cost = 2^(seq(-10, 10, by = 0.5)), .combine = rbind) %dopar% {
    set.seed(123)
    mod = tune(svm, train.x, as.factor(train.y), kernel = kernel.opt, 
               cost = cost, probability = TRUE)
    performance <- mod$performances[, c("error", "dispersion")]
    data.frame(cost = cost, performance = performance)
  }
  best.setting <- tuneresult[tuneresult$performance.error == min(tuneresult$performance.error), ]
  best.cost <- min(best.setting$cost)
  
  print(paste("The best cost is ", round(best.cost,6), " after the wide grid search", sep=""))
  print("Grid search with a small range")
  
  more.cost <- 2^seq(log2(best.cost) - 0.5, log2(best.cost) + 0.5, by = 0.05)
  tune.more <- foreach(cost = more.cost, .combine = rbind) %dopar% {
    set.seed(123)
    mod = tune(svm, train.x, as.factor(train.y), kernel = kernel.opt, 
               cost = cost, probability = TRUE)
    performance <- mod$performances[, c("error", "dispersion")]
    data.frame(cost = cost, performance = performance)
  }
  best.setting <- tune.more[tune.more$performance.error == min(tune.more$performance.error), ]
  best.cost <- min(best.setting$cost)
  print(paste("The best cost is ", round(best.cost,6), " after the small grid search", sep=""))
  # my final model
  set.seed(123)
  mymod <- svm(train.x, as.factor(train.y), cost = best.cost, kernel = kernel.opt, probability=TRUE)
} else {
  # For radial
  tuneresult <- foreach(gamma = 2^(seq(-15, 3, by = 0.5)), .combine = rbind) %dopar% {
    registerDoParallel(cores = 1)
    foreach(cost = 2^(seq(-10, 10, by = 0.5)), .combine = rbind) %dopar% 
      {
        set.seed(123)
        mod = tune(svm, train.x, as.factor(train.y), 
                   kernel = kernel.opt, ranges = list(cost = cost, gamma = gamma), 
                   probability = TRUE)
        performance <- mod$performances[, c("error", "dispersion")]
        data.frame(cost = cost, gamma = gamma, performance = performance)
      }
  }
  best.setting <- tuneresult[tuneresult$performance.error == min(tuneresult$performance.error), ]
  best.cost <- best.setting$cost[1]
  best.gamma <- best.setting$gamma[1]
  print(paste("The best cost is ", round(best.cost,6), " after the wide grid search", sep=""))
  print(paste("The best gamma is ", round(best.gamma,6), " after the wide grid search", sep=""))
  print("Grid search with a small range")
  more.cost <- 2^seq(log2(best.cost) - 0.5, log2(best.cost) + 0.5, by = 0.05)
  more.gamma <- 2^seq(log2(best.gamma) - 0.5, log2(best.gamma) + 0.5, 
                      by = 0.05)
  registerDoParallel(cores = round((numCores/2)))  
  tune.more <- foreach(gamma= more.gamma, .combine = rbind) %dopar% {
    registerDoParallel(cores = 1)
    foreach(cost = more.cost, .combine = rbind) %dopar% {
      set.seed(123)
      mod = tune(svm, train.x, as.factor(train.y), kernel = kernel.opt, 
                 ranges = list(cost = cost, gamma = gamma), probability = TRUE)
      performance <- mod$performances[, c("error", "dispersion")]
      data.frame(cost = cost, gamma = gamma, performance = performance)
    }
  }
  best.setting <- tune.more[tune.more$performance.error == min(tune.more$performance.error), ]
  best.cost <- best.setting$cost[1]
  best.gamma <- best.setting$gamma[1]
  print(paste("The best cost is ", round(best.cost,6), " after the wide grid search", sep=""))
  print(paste("The best gamma is ", round(best.gamma,6), " after the wide grid search", sep=""))
  set.seed(123)
  mymod <- svm(train.x, as.factor(train.y), cost = best.cost, gamma = best.gamma, 
               kernel = kernel.opt, probability=TRUE)
}

pred.pop <- predict(mymod, test.data[, !colnames(test.data) %in%c("FID","IID")],probability=TRUE)

test.data$PRED <- pred.pop
class.prob <- attr(pred.pop, "probabilities")
print(paste("Prepare the summary file, starts at", date()))
orders <- t(apply(class.prob, 1, function(x) order(x,decreasing=T)))
orders.class <- t(apply(orders, 1, function(x) colnames(class.prob)[x]))
orders.probs <- t(sapply(1:nrow(class.prob), function(x) class.prob[x, orders[x,]]))
PL1PL2 <- orders.probs[,1]/orders.probs[,2]
test.data$Ancestry <- as.character(test.data$PRED)
test.data$Ancestry[PL1PL2 <=20] <- "MISSING"
write.table(test.data[,c("FID","IID","PC1","PC2","Ancestry")], paste0("KING_PL1PL2_ASN_Explore/",prefix, "_PL1PL2_asn_",npcs,"pcs_",kernel.opt, ".txt"), sep = "\t", quote = FALSE, row.names = FALSE)

