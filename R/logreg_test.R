#'@title Evaluation of \code{ensemble_fs} via logistic
#'  regression
#'@description Evaluates the accuracy
#'  of the ouput of \code{\link{ensemble_fs}} 
#'  using logistic regression analysis.
#'  It selects features which have an importance value
#'  above average and generates a logistic regression
#'  model. The function compares that model with
#'  a logistic regression model generated from all
#'  features (without \code{\link{ensemble_fs}}) and
#'  shows the results in a ROC-curve. For further
#'  evaluation the function also shows the p-value
#'  as result from \code{\link{roc.test}} in the pdf-file.
#'@param efs_table table object of class matrix (retrieved
#'  from \code{ensemble_fs})
#'@param file_name name of the file, wil be saved as
#'  \code{file_name} + "-ROC.pdf"
#'@inheritParams ensemble_fs
#'@return ROC-curve and p-value of \code{\link{roc.test}}
#'  in a pdf-File
#'@author Ursula Neumann
#'@examples
#'  ##loading dataset in Environment
#'  data(efsdata)
#'  ##Generate a ranking based on importance (with default
#'  ##NA_threshold = 0.7,cor_threshold = 0.2)
#'  efs<-ensemble_fs(efsdata,5,runs=2)
#'  ##Create a ROC Curve based on the output from ensemble_fs
#'  logreg_test(efsdata,efs,"test",5)
#'@seealso \link[stats]{glm}, \link[pROC]{roc}
#'@importFrom ROCR performance prediction plot
#'@importFrom pROC roc roc.test
#'@export
#'
logreg_test <- function(data ,efs_table,file_name,classnumber,
                        NA_threshold){
  data = data.frame(data)

  if(missing(efs_table))
    stop("efs_table argument missing")
  if(missing(file_name))
    stop("file_name argument missing")
  if(missing(classnumber))
    stop("classnumber argument missing")
  # if(missing(sep))
  #  stop("seperator argument missing")
  if(missing(NA_threshold)){
    NA_threshold=0.2
    print("default value for NA_threshold = 0.2")}
  if(!is.numeric(NA_threshold) | NA_threshold > 1 |
     NA_threshold < 0)
    stop("invalid argument:
         NA_threshold is required to be in [0,1]")


  # Retrieve EFS features
  e.features = which(colSums(efs_table)>
                       mean(colSums(efs_table)))

  # data = read.table(file_name, header=T, sep=sep)
  classname = colnames(data)[classnumber]

  # Delete parameters with too many NAs
  NrNA= c()
  for(i in 1:length(data[1,])){
    NrNA[i] = length(which(is.na(data[,i])))
  }
  NmNA = which(NrNA/length(data[,1])>NA_threshold)
  if(length(NmNA) != 0) data=data[,-NmNA]

  data=data[,which(colSums(data,na.rm=T)!=0)]

  data = na.omit(data, row.names=F)

  klasse = data[,classname]
  clnr= which(colnames(data)==classname)
  data = data[,-clnr]
  # k-fold cross-validation
  k=length(data[,1])
  prob1=c()
  prob2=c()
  prob3=c()
  for(i in 1:k){
    Train = seq(i,to=nrow(data),by=k)
    training <- data[-Train, ]
    training.cl <- klasse[-Train]
    testing <- data[ Train, ]
    testing.cl <- klasse[Train]
    logreg1 = glm(as.factor(training.cl)~.,
                  data = training, family = binomial,control = list(maxit = 50))
    logreg3 = glm(as.factor(training.cl)~.,
                  data = training[,e.features],
                  family = binomial,control = list(maxit = 50))
    prob1= (c(prob1,predict(logreg1, newdata=testing)))
    prob3= (c(prob3,predict(logreg3, newdata=testing)))
  }

  # Compute AUC for predicting klasse with the model
  prob1=prob1[order(as.numeric(names(prob1)))]
  roc1=roc(klasse, prob1, ci=T)
  ci1=roc1$ci
  ci1= gsub('95% CI:', '',ci1)
  ci1=round(as.numeric(ci1),1)*100
  pred1 <- prediction(prob1, klasse)
  perf1 <- performance(pred1, measure = "tpr",
                       x.measure = "fpr")
  auc1 <- performance(pred1, measure = "auc")
  auc1 <- auc1@y.values[[1]]


  prob3=prob3[order(as.numeric(names(prob3)))]
  roc3=roc(klasse, prob3, ci=T)
  ci3=roc3$ci
  ci3= gsub('95% CI:', '',ci3)
  ci3=round(as.numeric(ci3),3)*100
  pred3 <- prediction(prob3, klasse)
  perf3 <- performance(pred3, measure = "tpr",
                       x.measure = "fpr")
  auc3 <- performance(pred3, measure = "auc")
  auc3 <- auc3@y.values[[1]]
  ## Printing p value on plot
  r=roc.test(roc1,roc3)
  p = as.numeric(r$p.value)
  P = paste("p = ",round(p,3),sep ="")
  if(p<0.001){P = "p < 0.001"}

  # ROC Kurve
  d=c(0,1)
  pdf(paste(file_name,"-ROC.pdf", sep=""))
  plot(perf1, avg="vertical", spread.estimate="boxplot",
       main="")
  plot(perf3, avg="vertical", spread.estimate="boxplot",
       main="",add=TRUE, col = 'blue')
  abline(d, lty=2)
  text(0.15, 0.9, cex=1, paste("All: ",
                               round(mean(auc1),3)*100,
                               "% (", format(ci1[1], nsmall=1),
                               '...',format(ci1[3], nsmall=1),
                               ")", sep=""))
  text(0.15, 1,   cex=1, paste("EFS: ",round(mean(auc3),3)*100,
                               "% (",format(ci3[1], nsmall=1),
                               '...',format(ci3[3], nsmall=1),
                               ")", sep=""), col='blue')
  text(0.8, 0.1, cex=1, paste("ROC-test: ",P,sep=""))
  dev.off()

}
