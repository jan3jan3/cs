install.packages("rjson")
install.packages("tidyverse")
install.packages("NLP")
library("tidyverse")
library("rjson")
library("NLP")
json_file <- "C:\\Users\\aadra\\Desktop\\TVs-all-merged.json"
df <- fromJSON(file=json_file)
# Collect data
productID <- c()
modelID <- c()
titleA <- c()
for (i in 1:length(df)){
  for (j in 1:length(df[[i]])){
    modelID <- c(modelID, tolower(toString(df[[i]][[j]]$modelID)))
    titleA <- c(titleA, tolower(toString(df[[i]][[j]]$title)))
  }
}
#clean data
M = length(modelID)
for (i in 1:M){
  #titleA[i] = str_remove(titleA[i],modelID[i])
  titleA[i] = str_remove_all(titleA[i],"-")
  #titleA[i] = str_remove_all(titleA[i],"/")
  titleA[i] = str_remove_all(titleA[i],"\\(")
  titleA[i] = str_remove_all(titleA[i],"\\)")
  titleA[i] = str_replace_all(titleA[i], "  ", " ")
  titleA[i] = str_replace_all(titleA[i], "   ", " ")
  titleA[i] = str_replace_all(titleA[i], "    ", " ")
  titleA[i] = str_replace_all(titleA[i], "     ", " ")
  titleA[i] = str_replace_all(titleA[i], "      ", " ")
  modelID[i] = str_remove_all(modelID[i], "-")
  modelID[i] = str_remove_all(modelID[i], "/")
  #titleA[i] = str_remove(titleA[i],modelID[i])
  productID = c(productID,i)
}
#find duplicates
data <- cbind(productID, modelID, titleA)
duplicate = matrix(0, M, M)
for (i in 1:M){
  for (j in 1:M){
    if(i < j){
      if(length(intersect(modelID[i], modelID[j]))==1){
        duplicate[i,j] <- 1
      }  
    }
  }
}
# brands as title words
titlew <- c()
titlew <- c(titlew, "lg")
titlew <- c(titlew, "samsung")
titlew <- c(titlew, "sony")
titlew <- c(titlew, "toshiba")
titlew <- c(titlew, "panasonic")
titlew <- c(titlew, "nec")
titlew <- c(titlew, "sharp")
titlew <- c(titlew, "coby")
titlew <- c(titlew, "vizio")
titlew <- c(titlew, "insignia")
titlew <- c(titlew, "haier")
titlew <- c(titlew, "sansui")
titlew <- c(titlew, "rca")
titlew <- c(titlew, "sunbritetv")
# titlew <- c(titlew, "jvc")
# titlew <- c(titlew, "lcd")
# titlew <- c(titlew, "led")
# titlew <- c(titlew, "plasma")
# titlew <- c(titlew, "smart")
# titlew <- c(titlew, "internet")
titlew <- c(titlew, "philips")
#find titlewords
for (i in 1:M){
  aux <- str_split(titleA[i]," ")
  for (j in 1:length(aux[[1]])){
    if ((grepl("\\d",aux[[1]][j]))&&grepl("[a-z]",aux[[1]][j])){
      titlew <- union(titlew, aux[[1]][j])
    } 
    else if ((grepl("\\d",aux[[1]][j]))&&grepl("[\":]",aux[[1]][j])){
      titlew <- union(titlew, aux[[1]][j])
    }
    else if ((grepl("[a-z]",aux[[1]][j]))&&grepl("[\":]",aux[[1]][j])){
      titlew <- union(titlew, aux[[1]][j])
    }
  }
}
#create binary vectors
onehot = matrix(0,length(titlew),M)
  for (i in 1:length(titlew)){
    for (j in 1:M){
      if(grepl(titlew[i],titleA[j])){
        onehot[i,j] <- 1
      }
    }
  }

#create explanatory brand variables
#1 if pair j,k from same brand i
z = 0
brands <- matrix(0,15*M,M)
for (i in 1:15){
  print(i)
  for (j in 1:M){
    for (k in 1:M){
      if(onehot[i,j]*onehot[i,k]==1){
        brands[z + j,k] <- 1
      }
    }
  }
  z = z + M
}

# brandvector
for (i in 1:M){
  print(i)
  brandvector1 <- as.matrix(onehot[1:15,i])
  for (j in 1:M){
    brandvector2 <- as.matrix(onehot[1:15,j])
    if(sum(t(brandvector1)%*%brandvector2)==1){
        brandmatrix[i,j] <- 1
    }
  }
}

# create 5 random bootstraps 
shuffle <- matrix(1:length(titlew),length(titlew),1)
#create signature matrix
signature <- signatureM(100,onehot,titlew,M)

#BOOTSTRAP
#create test and training sets
shuffle <- matrix(1:M,M,1)
#1
rand <- sample(shuffle)
testset1 <- testset(M,rand)
trainset1 <- trainset(M,rand)
#2
rand <- sample(shuffle)
testset2 <- testset(M,rand)
trainset2 <- trainset(M,rand)
#3
rand <- sample(shuffle)
testset3 <- testset(M,rand)
trainset3 <- trainset(M,rand)
#4
rand <- sample(shuffle)
testset4 <- testset(M,rand)
trainset4 <- trainset(M,rand)
#5
rand <- sample(shuffle)
testset5 <- testset(M,rand)
trainset5 <- trainset(M,rand)

# LSH

#0.05
b = 90
r = 1
#split matrix in parts
subvector <- splitmatrix(signature,M,r,b)
#divide into buckets
buckets <- hash2bucket(subvector,1000,M)
#find Candidate pairs
cp05 <- findCP(buckets,subvector)

#0.1
b = 50
r = 2
subvector <- splitmatrix(signature,M,r,b)
buckets <- hash2bucket(subvector,1000,M)
cp10 <- findCP(buckets,1000)

#0.15
b = 48
r = 2
subvector <- splitmatrix(signature,M,r,b)
buckets <- hash2bucket(subvector,R,M)
cp15 <- c()
for (i in 1:R){
  if(i%%100==0){
    print(i)
  }
  if (length(mybuckets[[i]]>1)){
    cp15 <- rbind(cp15,t(combn(mybuckets[[i]],2)))
  }
}

#0.2
b = 42
r = 2
subvector <- splitmatrix(signature,M,r,b)
buckets <- hash2bucket(subvector,R,M)
#cp20 <- findCP(buckets,subvector)
cp20 <- c()
for (i in 1:R){
  if(i%%100==0){
    print(i)
  }
  if (length(mybuckets[[i]]>1)){
    cp20 <- rbind(cp20,t(combn(mybuckets[[i]],2)))
  }
}

#0.25
b = 38
r = 2
subvector <- splitmatrix(signature,M,r,b)
buckets <- hash2bucket(subvector,R,M)
#cp25 <- findCP(buckets,subvector)
cp25 <- c()
for (i in 1:R){
  if(i%%100==0){
    print(i)
  }
  if (length(mybuckets[[i]]>1)){
    cp25 <- rbind(cp25,t(combn(mybuckets[[i]],2)))
  }
}

#0.3
b = 33
r = 3
subvector <- splitmatrix(signature,M,r,b)
buckets <- hash2bucket2(subvector,M)
cp30 <- findCP2(buckets,subvector)

#0.35
b = 30
r = 3
subvector <- splitmatrix(signature,M,r,b)
buckets <- hash2bucket2(subvector,M)
cp35 <- findCP2(buckets,subvector)

#0.4
b = 27
r = 3
subvector <- splitmatrix(signature,M,r,b)
buckets <- hash2bucket2(subvector,M)
cp40 <- findCP2(buckets,subvector)

#0.45
b = 24
r = 4
subvector <- splitmatrix(signature,M,r,b)
buckets <- hash2bucket2(subvector,M)
cp45 <- findCP2(buckets,subvector)

#0.5
b = 22
r = 4
subvector <- splitmatrix(signature,M,r,b)
buckets <- hash2bucket2(subvector,M)
cp50 <- findCP2(buckets,subvector)

#0.55
b = 19
r = 5
subvector <- splitmatrix(signature,M,r,b)
buckets <- hash2bucket2(subvector,M)
cp55 <- findCP2(buckets,subvector)

#0.6
b = 17
r = 5
subvector <- splitmatrix(signature,M,r,b)
buckets <- hash2bucket2(subvector,M)
cp60 <- findCP2(buckets,subvector)

#0.65
b = 15
r = 6
subvector <- splitmatrix(signature,M,r,b)
buckets <- hash2bucket2(subvector,M)
cp65 <- findCP2(buckets,subvector)

#0.7
b = 13
r = 7
subvector <- splitmatrix(signature,M,r,b)
buckets <- hash2bucket2(subvector,M)
cp70 <- findCP2(buckets,subvector)

#0.75
b = 11
r = 8
subvector <- splitmatrix(signature,M,r,b)
buckets <- hash2bucket2(subvector,M)
cp75 <- findCP2(buckets,subvector)

#0.80
b = 9
r = 10
subvector <- splitmatrix(signature,M,r,b)
buckets <- hash2bucket2(subvector,M)
cp80 <- findCP2(buckets,subvector)

#0.85
b = 7
r = 12
subvector <- splitmatrix(signature,M,r,b)
buckets <- hash2bucket2(subvector,R,M)
cp85 <- findCP2(buckets,subvector)

#0.90
b = 5
r = 16
subvector <- splitmatrix(signature,M,r,b)
buckets <- hash2bucket2(subvector,M)
cp90 <- findCP2(buckets,subvector)

#0.95
b = 3
r = 26
subvector <- splitmatrix(signature,M,r,b)
buckets <- hash2bucket2(subvector,M)
cp95 <- findCP2(buckets,subvector)

#find candidate duplicates
matrixcp30 <- matrixcp(cp30,M)
matrixcp35 <- matrixcp(cp35,M)
matrixcp40 <- matrixcp(cp40,M)
matrixcp45 <- matrixcp(cp45,M)
matrixcp50 <- matrixcp(cp50,M)
matrixcp55 <- matrixcp(cp55,M)
matrixcp60 <- matrixcp(cp60,M)
matrixcp65 <- matrixcp(cp65,M)
matrixcp70 <- matrixcp(cp70,M)
matrixcp75 <- matrixcp(cp75,M)
matrixcp80 <- matrixcp(cp80,M)
matrixcp85 <- matrixcp(cp85,M)
matrixcp90 <- matrixcp(cp90,M)
matrixcp95 <- matrixcp(cp95,M)

#create trainingset + testset for all 5 bootstraps
trainsetA <- traindf(matrixcp95,trainset1,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetB <- traindf(matrixcp95,trainset2,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetC <- traindf(matrixcp95,trainset3,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetD <- traindf(matrixcp95,trainset4,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetE <- traindf(matrixcp95,trainset5,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetA <- testdf(matrixcp95,testset1,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetB <- testdf(matrixcp95,testset2,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetC <- testdf(matrixcp95,testset3,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetD <- testdf(matrixcp95,testset4,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetE <- testdf(matrixcp95,testset5,onehot,jaccard,duplicate,brandmatrix,brands,M)
# calculate performance measures LSH 
NC95 <- (dim(testsetA)[1] + dim(testsetB)[1] + dim(testsetC)[1] + dim(testsetD)[1] + dim(testsetE)[1])/5
PQ95 <- (sum(testsetA[,1])/dim(testsetA)[1] + sum(testsetB[,1])/dim(testsetB)[1]
+ sum(testsetC[,1])/dim(testsetC)[1] + sum(testsetD[,1])/dim(testsetD)[1]
+ sum(testsetE[,1])/dim(testsetE)[1])/5
PC95 <- (sum(testsetA[,1])/duptest(duplicate,testset1,M) + sum(testsetB[,1])/duptest(duplicate,testset2,M)
+ sum(testsetC[,1])/duptest(duplicate,testset3,M) + sum(testsetD[,1])/duptest(duplicate,testset4,M)
+ sum(testsetE[,1])/duptest(duplicate,testset5,M))/5
log1 <- logistic(trainsetA,testsetA)
log2 <- logistic(trainsetB,testsetB)
log3 <- logistic(trainsetC,testsetC)
log4 <- logistic(trainsetD,testsetD)
log5 <- logistic(trainsetE,testsetE)
# calculate performance measures logistic regression
LNC95 <- ( sum(log1) + sum(log2) + sum(log3) + sum(log4) + sum(log5) )/5
LPQ95 <- (acc(testsetA,log1)/sum(log1) + acc(testsetB,log2)/sum(log2) + acc(testsetC,log3)/sum(log3) +
# + acc(testsetD,log4)/sum(log4) 
+ acc(testsetE,log5)/sum(log5))/5
LPC95 <- (acc(testsetA,log1)/duptest(duplicate,testset1,M) + acc(testsetB,log2)/duptest(duplicate,testset2,M) + 
acc(testsetC,log3)/duptest(duplicate,testset3,M) + acc(testsetD,log4)/duptest(duplicate,testset4,M)
+ acc(testsetE,log5)/duptest(duplicate,testset5,M))/5

#90
trainsetA <- traindf(matrixcp90,trainset1,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetB <- traindf(matrixcp90,trainset2,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetC <- traindf(matrixcp90,trainset3,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetD <- traindf(matrixcp90,trainset4,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetE <- traindf(matrixcp90,trainset5,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetA <- testdf(matrixcp90,testset1,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetB <- testdf(matrixcp90,testset2,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetC <- testdf(matrixcp90,testset3,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetD <- testdf(matrixcp90,testset4,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetE <- testdf(matrixcp90,testset5,onehot,jaccard,duplicate,brandmatrix,brands,M)
NC90 <- (dim(testsetA)[1] + dim(testsetB)[1] + dim(testsetC)[1] + dim(testsetD)[1] + dim(testsetE)[1])/5
PQ90 <- (sum(testsetA[,1])/dim(testsetA)[1] + sum(testsetB[,1])/dim(testsetB)[1]
         + sum(testsetC[,1])/dim(testsetC)[1] + sum(testsetD[,1])/dim(testsetD)[1]
         + sum(testsetE[,1])/dim(testsetE)[1])/5
PC90 <- (sum(testsetA[,1])/duptest(duplicate,testset1,M) + sum(testsetB[,1])/duptest(duplicate,testset2,M)
         + sum(testsetC[,1])/duptest(duplicate,testset3,M) + sum(testsetD[,1])/duptest(duplicate,testset4,M)
         + sum(testsetE[,1])/duptest(duplicate,testset5,M))/5
log1 <- logistic(trainsetA,testsetA)
log2 <- logistic(trainsetB,testsetB)
log3 <- logistic(trainsetC,testsetC)
log4 <- logistic(trainsetD,testsetD)
log5 <- logistic(trainsetE,testsetE)
LNC90 <- ( sum(log1) + sum(log2) + sum(log3) + sum(log4) + sum(log5) )/5
LPQ90 <- (acc(testsetA,log1)/sum(log1) + acc(testsetB,log2)/sum(log2) + acc(testsetC,log3)/sum(log3) +
            # + acc(testsetD,log4)/sum(log4) 
            + acc(testsetE,log5)/sum(log5))/5
LPC90 <- (acc(testsetA,log1)/duptest(duplicate,testset1,M) + acc(testsetB,log2)/duptest(duplicate,testset2,M) + 
            acc(testsetC,log3)/duptest(duplicate,testset3,M) + acc(testsetD,log4)/duptest(duplicate,testset4,M)
          + acc(testsetE,log5)/duptest(duplicate,testset5,M))/5


#85
trainsetA <- traindf(matrixcp85,trainset1,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetB <- traindf(matrixcp85,trainset2,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetC <- traindf(matrixcp85,trainset3,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetD <- traindf(matrixcp85,trainset4,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetE <- traindf(matrixcp85,trainset5,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetA <- testdf(matrixcp85,testset1,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetB <- testdf(matrixcp85,testset2,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetC <- testdf(matrixcp85,testset3,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetD <- testdf(matrixcp85,testset4,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetE <- testdf(matrixcp85,testset5,onehot,jaccard,duplicate,brandmatrix,brands,M)
NC85 <- (dim(testsetA)[1] + dim(testsetB)[1] + dim(testsetC)[1] + dim(testsetD)[1] + dim(testsetE)[1])/5
PQ85 <- (sum(testsetA[,1])/dim(testsetA)[1] + sum(testsetB[,1])/dim(testsetB)[1]
         + sum(testsetC[,1])/dim(testsetC)[1] + sum(testsetD[,1])/dim(testsetD)[1]
         + sum(testsetE[,1])/dim(testsetE)[1])/5
PC85 <- (sum(testsetA[,1])/duptest(duplicate,testset1,M) + sum(testsetB[,1])/duptest(duplicate,testset2,M)
         + sum(testsetC[,1])/duptest(duplicate,testset3,M) + sum(testsetD[,1])/duptest(duplicate,testset4,M)
         + sum(testsetE[,1])/duptest(duplicate,testset5,M))/5
log1 <- logistic(trainsetA,testsetA)
log2 <- logistic(trainsetB,testsetB)
log3 <- logistic(trainsetC,testsetC)
log4 <- logistic(trainsetD,testsetD)
log5 <- logistic(trainsetE,testsetE)
LNC85 <- ( sum(log1) + sum(log2) + sum(log3) + sum(log4) + sum(log5) )/5
LPQ85 <- (acc(testsetA,log1)/sum(log1) + acc(testsetB,log2)/sum(log2) + acc(testsetC,log3)/sum(log3) +
            + acc(testsetD,log4)/sum(log4) 
            + acc(testsetE,log5)/sum(log5))/5
LPC85 <- (acc(testsetA,log1)/duptest(duplicate,testset1,M) + acc(testsetB,log2)/duptest(duplicate,testset2,M) + 
            acc(testsetC,log3)/duptest(duplicate,testset3,M) + acc(testsetD,log4)/duptest(duplicate,testset4,M)
          + acc(testsetE,log5)/duptest(duplicate,testset5,M))/5

#80
trainsetA <- traindf(matrixcp80,trainset1,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetB <- traindf(matrixcp80,trainset2,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetC <- traindf(matrixcp80,trainset3,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetD <- traindf(matrixcp80,trainset4,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetE <- traindf(matrixcp80,trainset5,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetA <- testdf(matrixcp80,testset1,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetB <- testdf(matrixcp80,testset2,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetC <- testdf(matrixcp80,testset3,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetD <- testdf(matrixcp80,testset4,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetE <- testdf(matrixcp80,testset5,onehot,jaccard,duplicate,brandmatrix,brands,M)
NC80 <- (dim(testsetA)[1] + dim(testsetB)[1] + dim(testsetC)[1] + dim(testsetD)[1] + dim(testsetE)[1])/5
PQ80 <- (sum(testsetA[,1])/dim(testsetA)[1] + sum(testsetB[,1])/dim(testsetB)[1]
         + sum(testsetC[,1])/dim(testsetC)[1] + sum(testsetD[,1])/dim(testsetD)[1]
         + sum(testsetE[,1])/dim(testsetE)[1])/5
PC80 <- (sum(testsetA[,1])/duptest(duplicate,testset1,M) + sum(testsetB[,1])/duptest(duplicate,testset2,M)
         + sum(testsetC[,1])/duptest(duplicate,testset3,M) + sum(testsetD[,1])/duptest(duplicate,testset4,M)
         + sum(testsetE[,1])/duptest(duplicate,testset5,M))/5
log1 <- logistic(trainsetA,testsetA)
log2 <- logistic(trainsetB,testsetB)
log3 <- logistic(trainsetC,testsetC)
log4 <- logistic(trainsetD,testsetD)
log5 <- logistic(trainsetE,testsetE)
LNC80 <- ( sum(log1) + sum(log2) + sum(log3) + sum(log4) + sum(log5) )/5
LPQ80 <- (acc(testsetA,log1)/sum(log1) + acc(testsetB,log2)/sum(log2) + acc(testsetC,log3)/sum(log3) +
            # + acc(testsetD,log4)/sum(log4) 
            + acc(testsetE,log5)/sum(log5))/5
LPC80 <- (acc(testsetA,log1)/duptest(duplicate,testset1,M) + acc(testsetB,log2)/duptest(duplicate,testset2,M) + 
            acc(testsetC,log3)/duptest(duplicate,testset3,M) + acc(testsetD,log4)/duptest(duplicate,testset4,M)
          + acc(testsetE,log5)/duptest(duplicate,testset5,M))/5


#75
trainsetA <- traindf(matrixcp75,trainset1,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetB <- traindf(matrixcp75,trainset2,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetC <- traindf(matrixcp75,trainset3,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetD <- traindf(matrixcp75,trainset4,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetE <- traindf(matrixcp75,trainset5,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetA <- testdf(matrixcp75,testset1,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetB <- testdf(matrixcp75,testset2,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetC <- testdf(matrixcp75,testset3,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetD <- testdf(matrixcp75,testset4,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetE <- testdf(matrixcp75,testset5,onehot,jaccard,duplicate,brandmatrix,brands,M)
NC75 <- (dim(testsetA)[1] + dim(testsetB)[1] + dim(testsetC)[1] + dim(testsetD)[1] + dim(testsetE)[1])/5
PQ75 <- (sum(testsetA[,1])/dim(testsetA)[1] + sum(testsetB[,1])/dim(testsetB)[1]
         + sum(testsetC[,1])/dim(testsetC)[1] + sum(testsetD[,1])/dim(testsetD)[1]
         + sum(testsetE[,1])/dim(testsetE)[1])/5
PC75 <- (sum(testsetA[,1])/duptest(duplicate,testset1,M) + sum(testsetB[,1])/duptest(duplicate,testset2,M)
         + sum(testsetC[,1])/duptest(duplicate,testset3,M) + sum(testsetD[,1])/duptest(duplicate,testset4,M)
         + sum(testsetE[,1])/duptest(duplicate,testset5,M))/5
log1 <- logistic(trainsetA,testsetA)
log2 <- logistic(trainsetB,testsetB)
log3 <- logistic(trainsetC,testsetC)
log4 <- logistic(trainsetD,testsetD)
log5 <- logistic(trainsetE,testsetE)
LNC75 <- ( sum(log1) + sum(log2) + sum(log3) + sum(log4) + sum(log5) )/5
LPQ75 <- (acc(testsetA,log1)/sum(log1) + acc(testsetB,log2)/sum(log2) + acc(testsetC,log3)/sum(log3) +
            + acc(testsetD,log4)/sum(log4) 
            + acc(testsetE,log5)/sum(log5))/5
LPC75 <- (acc(testsetA,log1)/duptest(duplicate,testset1,M) + acc(testsetB,log2)/duptest(duplicate,testset2,M) + 
            acc(testsetC,log3)/duptest(duplicate,testset3,M) + acc(testsetD,log4)/duptest(duplicate,testset4,M)
          + acc(testsetE,log5)/duptest(duplicate,testset5,M))/5

#70
trainsetA <- traindf(matrixcp70,trainset1,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetB <- traindf(matrixcp70,trainset2,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetC <- traindf(matrixcp70,trainset3,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetD <- traindf(matrixcp70,trainset4,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetE <- traindf(matrixcp70,trainset5,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetA <- testdf(matrixcp70,testset1,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetB <- testdf(matrixcp70,testset2,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetC <- testdf(matrixcp70,testset3,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetD <- testdf(matrixcp70,testset4,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetE <- testdf(matrixcp70,testset5,onehot,jaccard,duplicate,brandmatrix,brands,M)
NC70 <- (dim(testsetA)[1] + dim(testsetB)[1] + dim(testsetC)[1] + dim(testsetD)[1] + dim(testsetE)[1])/5
PQ70 <- (sum(testsetA[,1])/dim(testsetA)[1] + sum(testsetB[,1])/dim(testsetB)[1]
         + sum(testsetC[,1])/dim(testsetC)[1] + sum(testsetD[,1])/dim(testsetD)[1]
         + sum(testsetE[,1])/dim(testsetE)[1])/5
PC70 <- (sum(testsetA[,1])/duptest(duplicate,testset1,M) + sum(testsetB[,1])/duptest(duplicate,testset2,M)
         + sum(testsetC[,1])/duptest(duplicate,testset3,M) + sum(testsetD[,1])/duptest(duplicate,testset4,M)
         + sum(testsetE[,1])/duptest(duplicate,testset5,M))/5
log1 <- logistic(trainsetA,testsetA)
log2 <- logistic(trainsetB,testsetB)
log3 <- logistic(trainsetC,testsetC)
log4 <- logistic(trainsetD,testsetD)
log5 <- logistic(trainsetE,testsetE)
LNC70 <- ( sum(log1) + sum(log2) + sum(log3) + sum(log4) + sum(log5) )/5
LPQ70 <- (acc(testsetA,log1)/sum(log1) + acc(testsetB,log2)/sum(log2) + acc(testsetC,log3)/sum(log3) +
            # + acc(testsetD,log4)/sum(log4) 
            + acc(testsetE,log5)/sum(log5))/5
LPC70 <- (acc(testsetA,log1)/duptest(duplicate,testset1,M) + acc(testsetB,log2)/duptest(duplicate,testset2,M) + 
            acc(testsetC,log3)/duptest(duplicate,testset3,M) + acc(testsetD,log4)/duptest(duplicate,testset4,M)
          + acc(testsetE,log5)/duptest(duplicate,testset5,M))/5

#65
trainsetA <- traindf(matrixcp65,trainset1,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetB <- traindf(matrixcp65,trainset2,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetC <- traindf(matrixcp65,trainset3,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetD <- traindf(matrixcp65,trainset4,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetE <- traindf(matrixcp65,trainset5,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetA <- testdf(matrixcp65,testset1,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetB <- testdf(matrixcp65,testset2,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetC <- testdf(matrixcp65,testset3,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetD <- testdf(matrixcp65,testset4,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetE <- testdf(matrixcp65,testset5,onehot,jaccard,duplicate,brandmatrix,brands,M)
NC65 <- (dim(testsetA)[1] + dim(testsetB)[1] + dim(testsetC)[1] + dim(testsetD)[1] + dim(testsetE)[1])/5
PQ65 <- (sum(testsetA[,1])/dim(testsetA)[1] + sum(testsetB[,1])/dim(testsetB)[1]
         + sum(testsetC[,1])/dim(testsetC)[1] + sum(testsetD[,1])/dim(testsetD)[1]
         + sum(testsetE[,1])/dim(testsetE)[1])/5
PC65 <- (sum(testsetA[,1])/duptest(duplicate,testset1,M) + sum(testsetB[,1])/duptest(duplicate,testset2,M)
         + sum(testsetC[,1])/duptest(duplicate,testset3,M) + sum(testsetD[,1])/duptest(duplicate,testset4,M)
         + sum(testsetE[,1])/duptest(duplicate,testset5,M))/5
log1 <- logistic(trainsetA,testsetA)
log2 <- logistic(trainsetB,testsetB)
log3 <- logistic(trainsetC,testsetC)
log4 <- logistic(trainsetD,testsetD)
log5 <- logistic(trainsetE,testsetE)
LNC65 <- ( sum(log1) + sum(log2) + sum(log3) + sum(log4) + sum(log5) )/5
LPQ65 <- (acc(testsetA,log1)/sum(log1) + acc(testsetB,log2)/sum(log2) + acc(testsetC,log3)/sum(log3) +
            # + acc(testsetD,log4)/sum(log4) 
            + acc(testsetE,log5)/sum(log5))/5
LPC65 <- (acc(testsetA,log1)/duptest(duplicate,testset1,M) + acc(testsetB,log2)/duptest(duplicate,testset2,M) + 
            acc(testsetC,log3)/duptest(duplicate,testset3,M) + acc(testsetD,log4)/duptest(duplicate,testset4,M)
          + acc(testsetE,log5)/duptest(duplicate,testset5,M))/5

#60
trainsetA <- traindf(matrixcp60,trainset1,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetB <- traindf(matrixcp60,trainset2,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetC <- traindf(matrixcp60,trainset3,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetD <- traindf(matrixcp60,trainset4,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetE <- traindf(matrixcp60,trainset5,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetA <- testdf(matrixcp60,testset1,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetB <- testdf(matrixcp60,testset2,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetC <- testdf(matrixcp60,testset3,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetD <- testdf(matrixcp60,testset4,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetE <- testdf(matrixcp60,testset5,onehot,jaccard,duplicate,brandmatrix,brands,M)
NC60 <- (dim(testsetA)[1] + dim(testsetB)[1] + dim(testsetC)[1] + dim(testsetD)[1] + dim(testsetE)[1])/5
PQ60 <- (sum(testsetA[,1])/dim(testsetA)[1] + sum(testsetB[,1])/dim(testsetB)[1]
         + sum(testsetC[,1])/dim(testsetC)[1] + sum(testsetD[,1])/dim(testsetD)[1]
         + sum(testsetE[,1])/dim(testsetE)[1])/5
PC60 <- (sum(testsetA[,1])/duptest(duplicate,testset1,M) + sum(testsetB[,1])/duptest(duplicate,testset2,M)
         + sum(testsetC[,1])/duptest(duplicate,testset3,M) + sum(testsetD[,1])/duptest(duplicate,testset4,M)
         + sum(testsetE[,1])/duptest(duplicate,testset5,M))/5
log1 <- logistic(trainsetA,testsetA)
log2 <- logistic(trainsetB,testsetB)
log3 <- logistic(trainsetC,testsetC)
log4 <- logistic(trainsetD,testsetD)
log5 <- logistic(trainsetE,testsetE)
LNC60 <- ( sum(log1) + sum(log2) + sum(log3) + sum(log4) + sum(log5) )/5
LPQ60 <- (acc(testsetA,log1)/sum(log1) + acc(testsetB,log2)/sum(log2) + acc(testsetC,log3)/sum(log3) +
            # + acc(testsetD,log4)/sum(log4) 
            + acc(testsetE,log5)/sum(log5))/5
LPC60 <- (acc(testsetA,log1)/duptest(duplicate,testset1,M) + acc(testsetB,log2)/duptest(duplicate,testset2,M) + 
            acc(testsetC,log3)/duptest(duplicate,testset3,M) + acc(testsetD,log4)/duptest(duplicate,testset4,M)
          + acc(testsetE,log5)/duptest(duplicate,testset5,M))/5

#55
trainsetA <- traindf(matrixcp55,trainset1,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetB <- traindf(matrixcp55,trainset2,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetC <- traindf(matrixcp55,trainset3,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetD <- traindf(matrixcp55,trainset4,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetE <- traindf(matrixcp55,trainset5,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetA <- testdf(matrixcp55,testset1,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetB <- testdf(matrixcp55,testset2,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetC <- testdf(matrixcp55,testset3,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetD <- testdf(matrixcp55,testset4,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetE <- testdf(matrixcp55,testset5,onehot,jaccard,duplicate,brandmatrix,brands,M)
NC55 <- (dim(testsetA)[1] + dim(testsetB)[1] + dim(testsetC)[1] + dim(testsetD)[1] + dim(testsetE)[1])/5
PQ55 <- (sum(testsetA[,1])/dim(testsetA)[1] + sum(testsetB[,1])/dim(testsetB)[1]
         + sum(testsetC[,1])/dim(testsetC)[1] + sum(testsetD[,1])/dim(testsetD)[1]
         + sum(testsetE[,1])/dim(testsetE)[1])/5
PC55 <- (sum(testsetA[,1])/duptest(duplicate,testset1,M) + sum(testsetB[,1])/duptest(duplicate,testset2,M)
         + sum(testsetC[,1])/duptest(duplicate,testset3,M) + sum(testsetD[,1])/duptest(duplicate,testset4,M)
         + sum(testsetE[,1])/duptest(duplicate,testset5,M))/5
log1 <- logistic(trainsetA,testsetA)
log2 <- logistic(trainsetB,testsetB)
log3 <- logistic(trainsetC,testsetC)
log4 <- logistic(trainsetD,testsetD)
log5 <- logistic(trainsetE,testsetE)
LNC55 <- ( sum(log1) + sum(log2) + sum(log3) + sum(log4) + sum(log5) )/5
LPQ55 <- (acc(testsetA,log1)/sum(log1) + acc(testsetB,log2)/sum(log2) + acc(testsetC,log3)/sum(log3) +
            # + acc(testsetD,log4)/sum(log4) 
            + acc(testsetE,log5)/sum(log5))/5
LPC55 <- (acc(testsetA,log1)/duptest(duplicate,testset1,M) + acc(testsetB,log2)/duptest(duplicate,testset2,M) + 
            acc(testsetC,log3)/duptest(duplicate,testset3,M) + acc(testsetD,log4)/duptest(duplicate,testset4,M)
          + acc(testsetE,log5)/duptest(duplicate,testset5,M))/5
#50
trainsetA <- traindf(matrixcp50,trainset1,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetB <- traindf(matrixcp50,trainset2,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetC <- traindf(matrixcp50,trainset3,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetD <- traindf(matrixcp50,trainset4,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetE <- traindf(matrixcp50,trainset5,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetA <- testdf(matrixcp50,testset1,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetB <- testdf(matrixcp50,testset2,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetC <- testdf(matrixcp50,testset3,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetD <- testdf(matrixcp50,testset4,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetE <- testdf(matrixcp50,testset5,onehot,jaccard,duplicate,brandmatrix,brands,M)
NC50 <- (dim(testsetA)[1] + dim(testsetB)[1] + dim(testsetC)[1] + dim(testsetD)[1] + dim(testsetE)[1])/5
PQ50 <- (sum(testsetA[,1])/dim(testsetA)[1] + sum(testsetB[,1])/dim(testsetB)[1]
         + sum(testsetC[,1])/dim(testsetC)[1] + sum(testsetD[,1])/dim(testsetD)[1]
         + sum(testsetE[,1])/dim(testsetE)[1])/5
PC50 <- (sum(testsetA[,1])/duptest(duplicate,testset1,M) + sum(testsetB[,1])/duptest(duplicate,testset2,M)
         + sum(testsetC[,1])/duptest(duplicate,testset3,M) + sum(testsetD[,1])/duptest(duplicate,testset4,M)
         + sum(testsetE[,1])/duptest(duplicate,testset5,M))/5
log1 <- logistic(trainsetA,testsetA)
log2 <- logistic(trainsetB,testsetB)
log3 <- logistic(trainsetC,testsetC)
log4 <- logistic(trainsetD,testsetD)
log5 <- logistic(trainsetE,testsetE)
LNC50 <- ( sum(log1) + sum(log2) + sum(log3) + sum(log4) + sum(log5) )/5
LPQ50 <- (acc(testsetA,log1)/sum(log1) + acc(testsetB,log2)/sum(log2) + acc(testsetC,log3)/sum(log3) +
            # + acc(testsetD,log4)/sum(log4) 
            + acc(testsetE,log5)/sum(log5))/5
LPC50 <- (acc(testsetA,log1)/duptest(duplicate,testset1,M) + acc(testsetB,log2)/duptest(duplicate,testset2,M) + 
            acc(testsetC,log3)/duptest(duplicate,testset3,M) + acc(testsetD,log4)/duptest(duplicate,testset4,M)
          + acc(testsetE,log5)/duptest(duplicate,testset5,M))/5
#45
trainsetA <- traindf(matrixcp45,trainset1,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetB <- traindf(matrixcp45,trainset2,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetC <- traindf(matrixcp45,trainset3,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetD <- traindf(matrixcp45,trainset4,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetE <- traindf(matrixcp45,trainset5,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetA <- testdf(matrixcp45,testset1,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetB <- testdf(matrixcp45,testset2,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetC <- testdf(matrixcp45,testset3,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetD <- testdf(matrixcp45,testset4,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetE <- testdf(matrixcp45,testset5,onehot,jaccard,duplicate,brandmatrix,brands,M)
NC45 <- (dim(testsetA)[1] + dim(testsetB)[1] + dim(testsetC)[1] + dim(testsetD)[1] + dim(testsetE)[1])/5
PQ45 <- (sum(testsetA[,1])/dim(testsetA)[1] + sum(testsetB[,1])/dim(testsetB)[1]
         + sum(testsetC[,1])/dim(testsetC)[1] + sum(testsetD[,1])/dim(testsetD)[1]
         + sum(testsetE[,1])/dim(testsetE)[1])/5
PC45 <- (sum(testsetA[,1])/duptest(duplicate,testset1,M) + sum(testsetB[,1])/duptest(duplicate,testset2,M)
         + sum(testsetC[,1])/duptest(duplicate,testset3,M) + sum(testsetD[,1])/duptest(duplicate,testset4,M)
         + sum(testsetE[,1])/duptest(duplicate,testset5,M))/5
log1 <- logistic(trainsetA,testsetA)
log2 <- logistic(trainsetB,testsetB)
log3 <- logistic(trainsetC,testsetC)
log4 <- logistic(trainsetD,testsetD)
log5 <- logistic(trainsetE,testsetE)
LNC45 <- ( sum(log1) + sum(log2) + sum(log3) + sum(log4) + sum(log5) )/5
LPQ45 <- (acc(testsetA,log1)/sum(log1) + acc(testsetB,log2)/sum(log2) + acc(testsetC,log3)/sum(log3) +
            # + acc(testsetD,log4)/sum(log4) 
            + acc(testsetE,log5)/sum(log5))/5
LPC45 <- (acc(testsetA,log1)/duptest(duplicate,testset1,M) + acc(testsetB,log2)/duptest(duplicate,testset2,M) + 
            acc(testsetC,log3)/duptest(duplicate,testset3,M) + acc(testsetD,log4)/duptest(duplicate,testset4,M)
          + acc(testsetE,log5)/duptest(duplicate,testset5,M))/5
#40
trainsetA <- traindf(matrixcp40,trainset1,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetB <- traindf(matrixcp40,trainset2,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetC <- traindf(matrixcp40,trainset3,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetD <- traindf(matrixcp40,trainset4,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetE <- traindf(matrixcp40,trainset5,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetA <- testdf(matrixcp40,testset1,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetB <- testdf(matrixcp40,testset2,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetC <- testdf(matrixcp40,testset3,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetD <- testdf(matrixcp40,testset4,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetE <- testdf(matrixcp40,testset5,onehot,jaccard,duplicate,brandmatrix,brands,M)
NC40 <- (dim(testsetA)[1] + dim(testsetB)[1] + dim(testsetC)[1] + dim(testsetD)[1] + dim(testsetE)[1])/5
PQ40 <- (sum(testsetA[,1])/dim(testsetA)[1] + sum(testsetB[,1])/dim(testsetB)[1]
         + sum(testsetC[,1])/dim(testsetC)[1] + sum(testsetD[,1])/dim(testsetD)[1]
         + sum(testsetE[,1])/dim(testsetE)[1])/5
PC40 <- (sum(testsetA[,1])/duptest(duplicate,testset1,M) + sum(testsetB[,1])/duptest(duplicate,testset2,M)
         + sum(testsetC[,1])/duptest(duplicate,testset3,M) + sum(testsetD[,1])/duptest(duplicate,testset4,M)
         + sum(testsetE[,1])/duptest(duplicate,testset5,M))/5
log1 <- logistic(trainsetA,testsetA)
log2 <- logistic(trainsetB,testsetB)
log3 <- logistic(trainsetC,testsetC)
log4 <- logistic(trainsetD,testsetD)
log5 <- logistic(trainsetE,testsetE)
LNC40 <- ( sum(log1) + sum(log2) + sum(log3) + sum(log4) + sum(log5) )/5
LPQ40 <- (acc(testsetA,log1)/sum(log1) + acc(testsetB,log2)/sum(log2) + acc(testsetC,log3)/sum(log3) +
            # + acc(testsetD,log4)/sum(log4) 
            + acc(testsetE,log5)/sum(log5))/5
LPC40 <- (acc(testsetA,log1)/duptest(duplicate,testset1,M) + acc(testsetB,log2)/duptest(duplicate,testset2,M) + 
            acc(testsetC,log3)/duptest(duplicate,testset3,M) + acc(testsetD,log4)/duptest(duplicate,testset4,M)
          + acc(testsetE,log5)/duptest(duplicate,testset5,M))/5
#35
trainsetA <- traindf(matrixcp35,trainset1,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetB <- traindf(matrixcp35,trainset2,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetC <- traindf(matrixcp35,trainset3,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetD <- traindf(matrixcp35,trainset4,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetE <- traindf(matrixcp35,trainset5,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetA <- testdf(matrixcp35,testset1,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetB <- testdf(matrixcp35,testset2,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetC <- testdf(matrixcp35,testset3,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetD <- testdf(matrixcp35,testset4,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetE <- testdf(matrixcp35,testset5,onehot,jaccard,duplicate,brandmatrix,brands,M)
NC35 <- (dim(testsetA)[1] + dim(testsetB)[1] + dim(testsetC)[1] + dim(testsetD)[1] + dim(testsetE)[1])/5
PQ35 <- (sum(testsetA[,1])/dim(testsetA)[1] + sum(testsetB[,1])/dim(testsetB)[1]
         + sum(testsetC[,1])/dim(testsetC)[1] + sum(testsetD[,1])/dim(testsetD)[1]
         + sum(testsetE[,1])/dim(testsetE)[1])/5
PC35 <- (sum(testsetA[,1])/duptest(duplicate,testset1,M) + sum(testsetB[,1])/duptest(duplicate,testset2,M)
         + sum(testsetC[,1])/duptest(duplicate,testset3,M) + sum(testsetD[,1])/duptest(duplicate,testset4,M)
         + sum(testsetE[,1])/duptest(duplicate,testset5,M))/5
log1 <- logistic(trainsetA,testsetA)
log2 <- logistic(trainsetB,testsetB)
log3 <- logistic(trainsetC,testsetC)
log4 <- logistic(trainsetD,testsetD)
log5 <- logistic(trainsetE,testsetE)
LNC35 <- ( sum(log1) + sum(log2) + sum(log3) + sum(log4) + sum(log5) )/5
LPQ35 <- (acc(testsetA,log1)/sum(log1) + acc(testsetB,log2)/sum(log2) + acc(testsetC,log3)/sum(log3) +
            # + acc(testsetD,log4)/sum(log4) 
            + acc(testsetE,log5)/sum(log5))/5
LPC35 <- (acc(testsetA,log1)/duptest(duplicate,testset1,M) + acc(testsetB,log2)/duptest(duplicate,testset2,M) + 
            acc(testsetC,log3)/duptest(duplicate,testset3,M) + acc(testsetD,log4)/duptest(duplicate,testset4,M)
          + acc(testsetE,log5)/duptest(duplicate,testset5,M))/5

#30

trainsetA <- traindf(matrixcp30,trainset1,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetB <- traindf(matrixcp30,trainset2,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetC <- traindf(matrixcp30,trainset3,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetD <- traindf(matrixcp30,trainset4,onehot,jaccard,duplicate,brandmatrix,brands,M)
trainsetE <- traindf(matrixcp30,trainset5,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetA <- testdf(matrixcp30,testset1,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetB <- testdf(matrixcp30,testset2,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetC <- testdf(matrixcp30,testset3,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetD <- testdf(matrixcp30,testset4,onehot,jaccard,duplicate,brandmatrix,brands,M)
testsetE <- testdf(matrixcp30,testset5,onehot,jaccard,duplicate,brandmatrix,brands,M)
NC30 <- (dim(testsetA)[1] + dim(testsetB)[1] + dim(testsetC)[1] + dim(testsetD)[1] + dim(testsetE)[1])/5
PQ30 <- (sum(testsetA[,1])/dim(testsetA)[1] + sum(testsetB[,1])/dim(testsetB)[1]
         + sum(testsetC[,1])/dim(testsetC)[1] + sum(testsetD[,1])/dim(testsetD)[1]
         + sum(testsetE[,1])/dim(testsetE)[1])/5
PC30 <- (sum(testsetA[,1])/duptest(duplicate,testset1,M) + sum(testsetB[,1])/duptest(duplicate,testset2,M)
         + sum(testsetC[,1])/duptest(duplicate,testset3,M) + sum(testsetD[,1])/duptest(duplicate,testset4,M)
         + sum(testsetE[,1])/duptest(duplicate,testset5,M))/5
log1 <- logistic(trainsetA,testsetA)
log2 <- logistic(trainsetB,testsetB)
log3 <- logistic(trainsetC,testsetC)
log4 <- logistic(trainsetD,testsetD)
log5 <- logistic(trainsetE,testsetE)
LNC30 <- ( sum(log1) + sum(log2) + sum(log3) + sum(log4) + sum(log5) )/5
LPQ30 <- (acc(testsetA,log1)/sum(log1) + acc(testsetB,log2)/sum(log2) + acc(testsetC,log3)/sum(log3) +
            # + acc(testsetD,log4)/sum(log4) 
            + acc(testsetE,log5)/sum(log5))/5
LPC30 <- (acc(testsetA,log1)/sum(testsetA[,1]) + acc(testsetB,log2)/sum(testsetB[,1]) + 
            acc(testsetC,log3)/sum(testsetC[,1]) + acc(testsetD,log4)/sum(testsetD[,1]) 
          + acc(testsetE,log5)/sum(testsetE[,1]))/5

#PLOT PC + PQ
PC <- c()
NC <- c()
PQ <- c()
PC <- c(PC,PC95,PC90,PC85,PC80,PC75,PC70,PC65,PC60,PC55,PC50,PC45,1)
NC <- c(NC,NC95,NC90,NC85,NC80,NC75,NC70,NC65,NC60,NC55,NC50,NC45,NC30)
PQ <- c(PQ,PQ95,PQ90,PQ85,PQ80,PQ75,PQ70,PQ65,PQ60,PQ55,PQ50,PQ45,0.000001)
NC <- NC/NC30
plot(NC, PQ,
     xlab="Fraction of comparisions",
     type= "b",
     ylab="Pair quality")
NC <- c()
LPQ <- c()
LPC <- c()
LPQ <- c(LPQ,LPQ95,LPQ90,LPQ85,LPQ80,LPQ75,LPQ65,LPQ60,LPQ55,LPQ50,LPQ45,LPQ40)
LPC <- c(LPC,LPC95,LPC90,LPC85,LPC80,LPC75,LPC65,LPC60,LPC55,LPC50,LPC45,LPC40)
F1 <- 2*PQ*PC/(PQ+PC)
F1 <- 2*LPQ*LPC/(LPQ+LPC)
# PLOT F1
plot(NC, F1,
     xlab="Fraction of comparisions",
     type= "b",
     ylab="F1-measure")

#measure accuracy found duplicates
fitted.results <- ifelse(fitted.results > 0.5,1,0) 
aux = 0
for (i in 1:dim(testset)[1]){
  if(testset$dup1[i] == 1 && fitted.results[i] == 1){
    aux <- aux + 1
  }
}

#identify pairs in trainingset
trainset <- function(M,rand){
  temp1 <- rand[1:1000]
  temp3 <- matrix(0,M,1)
  for (i in 1:1000){
    temp3[temp1[i]] <- 1
  }
  B <- temp3%*%t(temp3)
  B
}

#identify pairs in testset
testset <- function(M,rand){
  temp2 <- rand[1001:M]
  temp4 <- matrix(0,M,1)
  for (i in 1:624){
    temp4[temp2[i]] <- 1
  }
  B <- temp4%*%t(temp4)
  B
}

# bootstrappijng
bootstrap <- function(M){
  rand <- sample(1:M,M,1)
  temp1 <- rand[1:1000]
  temp2 <- rand[1001:M]
  temp3 <- matrix(0,M,1)
  temp4 <- matrix(0,M,1)
  for (i in 1:1000){
    temp3[temp1[i]] <- 1
    temp4[temp2[i]] <- 1
  }
  B1 <-
  # B1 <- matrix(0,M,M)
  # for (i in 1:1000){
  #   for (j in 1:1000){
  #     if(train1[i]<train1[j]){
  #       B1[train1[i],train1[j]] <- 1
  #     }
  #   }
  # }
  # for (i in 1:624){
  #   for (j in 1:624){
  #     if(test1[i]<test1[j]){
  #       B1[test1[i],test1[j]] <- 0
  #     }
  #   }
  # }
  B1
}

# get signature matrix
signatureM <- function(N,onehot,vocab,M){
  shuffle <- matrix(1:length(vocab),length(vocab),1)
  minhash <- matrix(0,length(vocab),N)
  signature <- matrix(0,N,M) # size N
  for(i in 1:N){ 
    rand <- sample(shuffle) # permutate n times
    for (j in 1:length(vocab)){
      minhash[j,i] <- rand[j] # minhashing
    }
  }
  for(i in 1:N){
    for (j in 1:M){
      for(k in 1:length(vocab)){
        if(onehot[minhash[k,i],j]==1){ # find first one
          signature[i,j] <- k #signature part
          break
        }
      }
    }
  }
  signature
}

# split into b bands
splitmatrix <- function(signature,M,r,b){
  subvector <- matrix(0,r,M*b)
  z = 1
  x = 1
  for (i in 1:b){ #amount of bands
    temp1 = z
    temp2 = x
    z = z + r 
    x = x + M
    for (j in 1:r){ #amount of rows
      subvector[,temp2:(x-1)] <- signature[temp1:(z-1),] #split signature matrix 
    }
  }
  subvector
}

#hash bands into buckets
hash2bucket2 <- function(subvector,M){ # hash 2 buckets
  stringvector <- matrix("",1,dim(subvector)[2])
  array <- c()
  product <- c()
  for (i in 1:dim(subvector)[2]){
    if(i%%1000 == 0){
      print(i) #progress bar
    }
    for(j in 1:dim(subvector)[1]){
      if(nchar(subvector[j,i])==3){ 
        stringvector[i] = paste(stringvector[i],as.character(subvector[j,i]),sep ="")
      }
      else if(nchar(subvector[j,i])==2){ # add zero
        stringvector[i] = paste(stringvector[i],paste("0",as.character(subvector[j,i]),sep = ""),sep = "")
      }
      else if(nchar(subvector[j,i])==1){ #add two zeros
        stringvector[i] = paste(stringvector[i],paste("00",as.character(subvector[j,i]),sep = ""),sep = "")
      }
    }
    stringvector[i] = paste("1",stringvector[i],sep = "") #paste signature as string
    array <- c(array,as.double(stringvector[i])) #convert back to double
    t = i
    while (t > M){
      t = t - M #identify product
    }
    product <- c(product,t)
  }
  buckets = rbind(product, array) #associate product with buckets
  buckets <- buckets[,order(buckets[2,], decreasing = T)] #sort bucket array
  buckets
}

#hash 2 buckets function for large b
hash2bucket <- function(subvector,R,M){
mybuckets <- c()
for (i in 1:R){ #create R buckets
  mybuckets[[i]] <- 0
  mybuckets[[i]] <- mybuckets[[i]][-1]
}
stringvector <- matrix("",1,dim(subvector)[2]) 
array <- c()
product <- c()
for (i in 1:dim(subvector)[2]){ #same procedure as above
  for(j in 1:dim(subvector)[1]){
    if(nchar(subvector[j,i])==3){
      stringvector[i] = paste(stringvector[i],as.character(subvector[j,i]),sep ="")
    }
    else if(nchar(subvector[j,i])==2){
      stringvector[i] = paste(stringvector[i],paste("0",as.character(subvector[j,i]),sep = ""),sep = "")
    }
    else if(nchar(subvector[j,i])==1){
      stringvector[i] = paste(stringvector[i],paste("00",as.character(subvector[j,i]),sep = ""),sep = "")
    }
  }
  stringvector[i] = paste("1",stringvector[i],sep = "")
  numberb <- as.double(stringvector[i])%%R #use modulus to reduce amount of buckets
  t = i
  while (t > M){
    t = t - M
  }
  mybuckets[[numberb]] <- union(mybuckets[[numberb]],t) #do not store duplicate products
  
}
mybuckets
}

# look for candidate pairs
findCP <- function(mybuckets,R){
cp <- c()
  for (i in 1:R){
    if(i%%100==0){
      print(i)
    }
    if (length(mybuckets[[i]]>1)){
      for(i in 1:length(mybuckets))
      cp <- rbind(cp,t(combn(mybuckets[[i]],2))) # get all possible pairs from bucket
    }
  }
}

findCP2 <- function (buckets,subvector){
  cp1 = c()
  cp2 = c()
  c = 0
  for (i in 2:dim(subvector)[2]){
    if(i%%1000 == 0){
      print(i)
    }
    if(buckets[2,i]==buckets[2,i-1]){ #if the same _ candidate pair
      c = c + 1
      for (j in 1:c){
        cp1 <- c(cp1, buckets[1,i-j]) #unique lower product
        cp2 <- c(cp2 ,buckets[1,i]) #unique higher product
      }
    }
    else {
      c = 0
    }
  }
  cp <- cbind(cp1,cp2)
}

jaccard = function (x, y) { #jaccard similarity function
  M.11 = sum(x == 1 & y == 1)
  M.10 = sum(x == 1 & y == 0)
  M.01 = sum(x == 0 & y == 1)
  return (M.11 / (M.11 + M.10 + M.01))
}

#identify candidate pairs
matrixcp<- function(cp,M){
matrixcp <- matrix(0,M,M)
for (i in 1:dim(cp)[1]){
  if (i%%1000000==0){
    print(i)
  }
  if(cp[i,1]<cp[i,2]){
    matrixcp[cp[i,1],cp[i,2]] = 1 #upper diagonal matrix!!!
  }
  else{
    matrixcp[cp[i,2],cp[i,1]] = 1
  }

}
matrixcp
}

#find trainig set
traindf <- function(matrixcp,trainset,onehot,jaccard,duplicate,brandmatrix,brands,M){
AB <- c()
BC <- c()
for (i in 1:M){
  for (j in 1:M){
    if(matrixcp[i,j]==1 && trainset[i,j]==1){
      AB <- c(AB,i)
      BC <- c(BC,j)
    }
  }
}
#add explanatory variables
cptest55 <- as.matrix(cbind(AB,BC))
jsim1 <- c()
dup1 <- c()
brand1 <- c()
brand2 <- c()
brand3 <- c()
brand4 <- c()
brand5 <- c()
brand6 <- c()
brand7 <- c()
brand8 <- c()
brand9 <- c()
brand10 <- c()
brand11 <- c()
brand12 <- c()
brand13 <- c()
brand14 <- c()
brand15 <- c()
AA <- brands[1:1624,]
BB <- brands[1625:3248,]
CC <- brands[3249:4872,]
DD <- brands[4873:6496,]
EE <- brands[6497:8120,]
FF <- brands[8121:9744,]
GG <- brands[9745:11368,]
HH <- brands[11369:12992,]
II <- brands[12993:14616,]
JJ <- brands[14617:16240,]
KK <- brands[16241:17864,]
LL <- brands[17865:19488,]
MM <- brands[19489:21112,]
NN <- brands[21113:22736,]
OO <- brands[22737:24360,]
for(i in 1:length(AB)){
  jsim1 <- c(jsim1,jaccard((onehot[,cptest55[i,1]]),(onehot[,cptest55[i,2]])))
  dup1 <- c(dup1,duplicate[cptest55[i,1],cptest55[i,2]])
  brand1 <- c(brand1, OO[1:1624,][cptest55[i,1],cptest55[i,2]])
  brand2 <- c(brand2, AA[cptest55[i,1],cptest55[i,2]])
  brand3 <- c(brand3, BB[cptest55[i,1],cptest55[i,2]])
  brand4 <- c(brand4, CC[cptest55[i,1],cptest55[i,2]])
  brand5 <- c(brand5, DD[cptest55[i,1],cptest55[i,2]])
  brand6 <- c(brand6, EE[cptest55[i,1],cptest55[i,2]])
  brand7 <- c(brand7, FF[cptest55[i,1],cptest55[i,2]])
  brand8 <- c(brand8, GG[cptest55[i,1],cptest55[i,2]])
  brand9 <- c(brand9, HH[cptest55[i,1],cptest55[i,2]])
  brand10 <- c(brand10, II[cptest55[i,1],cptest55[i,2]])
  brand11 <- c(brand11, JJ[cptest55[i,1],cptest55[i,2]])
  brand12 <- c(brand12, KK[cptest55[i,1],cptest55[i,2]])
  brand13 <- c(brand13, LL[cptest55[i,1],cptest55[i,2]])
  brand14 <- c(brand14, MM[cptest55[i,1],cptest55[i,2]])
  brand15 <- c(brand15, NN[cptest55[i,1],cptest55[i,2]])
}
#combine into dataframe
traindf <- cbind(dup1,jsim1,brand1,brand2,brand3,brand4,brand5,brand6,brand7,brand8,brand9,brand10,
                 brand11,brand12,brand13,brand14,brand15)
traindf
}

#make testset same procedure as training
testdf <- function(matrixcp,testset,onehot,jaccard,duplicate,brandmatrix,brandS,M){
CD <- c()
DE <- c()
for (i in 1:M){
  for (j in 1:M){
    if(matrixcp[i,j]==1 && testset[i,j]==1){
      CD <- c(CD,i)
      DE <- c(DE,j)
    }
  }
}
cptest55 <- as.matrix(cbind(CD,DE))
jsim1 <- c()
dup1 <- c()
brand1 <- c()
brand2 <- c()
brand3 <- c()
brand4 <- c()
brand5 <- c()
brand6 <- c()
brand7 <- c()
brand8 <- c()
brand9 <- c()
brand10 <- c()
brand11 <- c()
brand12 <- c()
brand13 <- c()
brand14 <- c()
brand15 <- c()
AA <- brands[1:1624,]
BB <- brands[1625:3248,]
CC <- brands[3249:4872,]
DD <- brands[4873:6496,]
EE <- brands[6497:8120,]
FF <- brands[8121:9744,]
GG <- brands[9745:11368,]
HH <- brands[11369:12992,]
II <- brands[12993:14616,]
JJ <- brands[14617:16240,]
KK <- brands[16241:17864,]
LL <- brands[17865:19488,]
MM <- brands[19489:21112,]
NN <- brands[21113:22736,]
OO <- brands[22737:24360,]
for(i in 1:length(DE)){
  jsim1 <- c(jsim1,jaccard((onehot[,cptest55[i,1]]),(onehot[,cptest55[i,2]])))
  dup1 <- c(dup1,duplicate[cptest55[i,1],cptest55[i,2]])
  brand1 <- c(brand1, OO[1:1624,][cptest55[i,1],cptest55[i,2]])
  brand2 <- c(brand2, AA[cptest55[i,1],cptest55[i,2]])
  brand3 <- c(brand3, BB[cptest55[i,1],cptest55[i,2]])
  brand4 <- c(brand4, CC[cptest55[i,1],cptest55[i,2]])
  brand5 <- c(brand5, DD[cptest55[i,1],cptest55[i,2]])
  brand6 <- c(brand6, EE[cptest55[i,1],cptest55[i,2]])
  brand7 <- c(brand7, FF[cptest55[i,1],cptest55[i,2]])
  brand8 <- c(brand8, GG[cptest55[i,1],cptest55[i,2]])
  brand9 <- c(brand9, HH[cptest55[i,1],cptest55[i,2]])
  brand10 <- c(brand10, II[cptest55[i,1],cptest55[i,2]])
  brand11 <- c(brand11, JJ[cptest55[i,1],cptest55[i,2]])
  brand12 <- c(brand12, KK[cptest55[i,1],cptest55[i,2]])
  brand13 <- c(brand13, LL[cptest55[i,1],cptest55[i,2]])
  brand14 <- c(brand14, MM[cptest55[i,1],cptest55[i,2]])
  brand15 <- c(brand15, NN[cptest55[i,1],cptest55[i,2]])
}
traindf <- cbind(dup1,jsim1,brand1,brand2,brand3,brand4,brand5,brand6,brand7,brand8,brand9,brand10,
                 brand11,brand12,brand13,brand14,brand15)
traindf
}

#find all duplicates
duptest <- function(duplicate,testset1,M){
jz = 0
for (i in 1:M){
  for (j in 1:M){
    if(duplicate[i,j]==1 && testset1[i,j]==1){
      jz = jz + 1
    }
  }

}  
jz
}

#logit regression
logistic <- function(trainset, testset){
  trainset <- as.data.frame(trainset)
  testset <- as.data.frame(testset)
  #trainset <- subset(trainset, select = c(-jsim1))
  #testset <- subset(testset, select = c(-jsim1))
  model <- glm(dup1 ~.,family=binomial(link='logit'),data=trainset)
  fitted.results <- predict(model,newdata=subset(testset,select=c(jsim1,brand1,brand2,brand3,brand4,brand5,brand6,brand7
                                                                  ,brand8,brand9,brand10,brand11
                                                                  ,brand12,brand13,brand14,brand15)),type='response')
  fitted.results <- ifelse(fitted.results > 0.5,1,0) 
  fitted.results
}

acc <- function(testset,fitted.results){
testset <- as.data.frame(testset)
aux = 0
for (i in 1:dim(testset)[1]){
  if(testset$dup1[i] == 1 && fitted.results[i] == 1){
    aux <- aux + 1
  }

}
aux
}