#'@title Visualization of \code{ensemble_fs} in barplot
#'
#'@description Generates a barplot from
#'  the output of \code{\link{ensemble_fs}} and produces
#'  a pdf-file. This file will be located in the working
#'  directory. A barplot will only be provided, when the number
#'  of features does not exceed 100.
#'  \cr x-axis: sum of all normed importance values of each 
#'  feature ranging from 0 to 1 
#'  \cr y-axis: names of features
#'@author Ursula Neumann
#'@param name a character string giving the name of the file.
#'  If it is NULL, then no external file is created
#'  (effectively, no drawing occurs),
#'  but the device may still be queried.
#'@param efs_table table object of class matrix (retrieved
#'  from \code{ensemble_fs})
#'@examples
#'  ##loading dataset in Environment
#'  data(efsdata)
#'  ##Generate a ranking based on inportance (with default
#'  ##NA_threshold = 0.7,cor_threshold = 0.2)
#'  efs<-ensemble_fs(efsdata,5,runs=2)
#'  ##Create a cumulative barplot based on the output from efs 
#'  barplot_fs("test",efs)
#'@seealso \link{barplot}, \link{pdf}
#'@importFrom grDevices pdf dev.off
#'@importFrom graphics abline legend par segments text 
#'@importFrom stats glm predict na.omit binomial cor
#'  wilcox.test var
#'@export
barplot_fs <- function(name, efs_table){

  paranr = length(efs_table[1,])  
  if(paranr>100)
    stop("Too many variables for barplot, maximum is 100")
  if(paranr<35) h=10
  else h=(paranr/5)

  names=colnames(efs_table)
  cols = c('goldenrod1','navy',
           'royalblue','indianred3',
           'darkolivegreen1','darkgreen',
           'darkolivegreen3','chartreuse4')

  pdf(paste(name,'.pdf', sep=""),
      width= 12,
      height= h)
  par(mar=c(5, 4, 4, 10), xpd=TRUE)
  barplot= barplot(efs_table,
                   xlim=c(0,1),
                   main= 'Ensemble Feature Selection',
                   horiz=T,
                   las=2,
                   names.arg=abbreviate(names),
                   col=cols)
  legend("topright", inset=c(-0.2,0), legend=row.names(efs_table),col=cols, lty=1, lwd=12 )
  text(colSums(efs_table)+0.035,barplot,
       format(round(colSums(efs_table), 2),T))
  segments(1, 0, 1, 1.25*paranr, lty = 3, col = "gray40")
  dev.off()
}
