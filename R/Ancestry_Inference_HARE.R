library(optparse)
library(e1071)
library(doParallel)
library(dplyr)

option_list = list(
  make_option(c("--prefix"), type="character", default="king", 
              help="input file name", metavar="character"),
  make_option(c("--numfolds"), type="integer", default=5, 
              help="number of folds for cross validation", metavar="integer"),
  make_option(c("--rangevalue"), type="integer", default=3, 
              help="range value", metavar="integer"),
  make_option(c("-n","--numberpc"), type="integer", default=30,
              help="number of PCs to be selected", metavar="integer")
) 
opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

if (is.null(opt$prefix)){
  print_help(opt_parser)
  stop("prefix must be supplied", call.=FALSE)
}

prefix <- opt$prefix
npcs <- opt$numberpc
k <- opt$numfolds
rangeval <- opt$rangevalue


pc <- read.table(paste0(prefix, "pc.txt"), header = TRUE)
phe <- read.table(paste0(prefix, "_popref_asn.txt"), header = TRUE)
dat <- merge(pc, phe, all.x=TRUE, by=c("FID","IID"))
dat$Population <- as.factor(dat$Population)
dat$fold <- 0
set.seed(123)
dat$fold[!is.na(dat$Population)] <- sample(1:k, sum(!is.na(dat$Population)), replace=T)
svm.mod <- as.formula(paste0("Population~", paste0("PC", 1:npcs, collapse = "+")))

para_best = c(-2, 0)
step_cv0 = c(rangeval, rangeval)
step_cv1st = c(2, 2)
step_cv2nd = c(1, 1)
step_mat <- cbind(step_cv0, step_cv1st, step_cv2nd)

for (j in 1:2) {
  index <- dat$fold!=0
  perf_best <- 0
  mark_para <- NULL
  for(k0 in 2:ncol(step_mat)) {
    a0 <- para_best - step_mat[, k0 - 1]
    b0 <- para_best + step_mat[, k0 - 1];
    gamma0 <- seq(a0[1], b0[1], by = step_mat[1, k0]);
    cost0 <- seq(a0[2], b0[2], by = step_mat[2, k0]);
    para0 <- cbind(gamma = gamma0, cost = rep(cost0, each = length(gamma0)));
    mark0 <- paste0(para0[, 1], "_", para0[, 2]);
    para0 <- para0[!mark0 %in% mark_para, ];
    mark_para <- mark0;
    
    para = cbind(fold=rep(1:k,nrow(para0)),gamma=rep(para0[,"gamma"],k), cost=rep(para0[,"cost"],k),count=NA)
    para <- as.data.frame(para)
    para <- para[order(para$fold,para$cost, para$gamma),]
    para_one <- function(x) {
      fold.index <- para[x,"fold"]
      mod.svm <- svm(svm.mod, data=dat[dat$fold!=fold.index,], kernel = "radial",
                     gamma=10^para[x,"gamma"], cost=10^para[x,"cost"], fitted=F)
      pred.svm <- predict(mod.svm, newdata = dat[dat$fold ==fold.index, ])
      return(sum(pred.svm==dat[dat$fold == fold.index, "Population"]))
    }
    numCores <- detectCores()
    options(mc.cores=as.integer(numCores/2))
    results <- mclapply(1:nrow(para), para_one)
    para[,"count"] <- unlist(results)
    para <- as.data.frame(para)
    results <- para %>% 
      group_by(cost,gamma) %>% 
      summarise(Frequency = sum(count)) 
    best.para <- results[which.max(results$Frequency),]
    if(max(results$Frequency) > perf_best) {
      para_best <- unlist(best.para[,c("gamma", "cost")])
      perf_best <- max(results$Frequency)
    }
  }
  # SVM
  mod_svm <- svm(formula = svm.mod, data = dat[index, ], kernel = "radial", gamma = 10^para_best[1], cost = 10^para_best[2], probability = T, fitted = T);
  pred_svm <- predict(mod_svm, newdata = dat[, paste0("PC", 1:npcs)], probability = T);
  suspi_pop <- index & pred_svm != dat$Population;
  # Information for best parameters and number of suspicious individuals
  cat("Best parameters for (gamma, cost) in Round ", j, " (log10 scale): (", para_best[1], ", ", para_best[2], ").\n", sep = "");
  cat("Suspicious individuals in Round ", j, ": ", sum(suspi_pop), ".\n", sep = "");
  if(sum(suspi_pop) == 0) {
    break;
  } else {		
    dat$fold[suspi_pop] <- 0;
    set.seed(123)
    dat$fold[dat$fold != 0] <- sample(1:k, sum(dat$fold != 0), replace = T);
    step_cv0 <- step_cv1st;
  }
}

dat$Ancestry <- as.character(pred_svm)
class.prob <- attr(pred_svm, "probabilities")
print(paste("Prepare the summary file, starts at", date()))
orders <- t(apply(class.prob, 1, function(x) order(x,decreasing=T)))
orders.class <- t(apply(orders, 1, function(x) colnames(class.prob)[x]))
orders.probs <- t(sapply(1:nrow(class.prob), function(x) class.prob[x, orders[x,]]))
dat$Ancestry[orders.probs[,1] < 0.9] <- "Missing"

check.cumsum <- t(apply(orders.probs, 1, cumsum))
temp <- apply(check.cumsum, 1, function(x) which(x > 0.9)[1])
pred.class <- sapply(1:length(temp), function(x) paste(orders.class[x, 1:as.numeric(temp[x])], collapse = ";"))
pred.prob <- sapply(1:length(temp), function(x) paste(round(orders.probs[x, 1:as.numeric(temp[x])], 3), collapse = ";"))
pred.out <- cbind(dat[, c("FID", "IID", "PC1", "PC2","AFF","Ancestry")], pred.class, pred.prob)
test.data <- pred.out[pred.out$AFF==2,]
colnames(test.data) <- c("FID","IID","PC1","PC2","AFF","Ancestry","Ancestry_more","Ancestry_prob")
write.table(test.data[,c("FID","IID","PC1","PC2","Ancestry","Ancestry_more","Ancestry_prob")], 
            paste0(prefix, "_",npcs,"pcs_",k,"folds_", rangeval,"_king_hareimplement.txt"), 
            sep = "\t", quote = FALSE, row.names = FALSE)
