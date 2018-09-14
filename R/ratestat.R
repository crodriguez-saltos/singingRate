#' Statistics on singing rate
#' 
#' Get an estimate of the mean singing rate of a bird and the uncertainty of the estimate.
#' @keywords singing rate
#' @param folder Folder containing the labels.
#' @param nlb Maximum level of file label to analyze.
#' @export

ratestat <- function(folder, nlb){
  ##### Import data----
  txtfiles <- dir(folder, pattern = ".txt", recursive = T, full.names = T)
  txtfiles <- txtfiles[grep("label", txtfiles)]
  
  d <- lapply(txtfiles, read.table)
  d <- lapply(d, function(x) cbind(x, x[,2] - x[,1]))
  
  # Test
  
  fi <- sapply(1:nlb, function(x) grep(paste0("labels", x), txtfiles))
  fi <- do.call("c", fi); fi <- sort(fi)
  f <- d[fi]
  txtfiles <- txtfiles[fi]
  
  any(sapply(f, is.null))
  f <- f[!sapply(f, is.null)]
  f <- lapply(f, function(x) aggregate(x[,4], list(x[,3]), sum))
  f <- do.call("rbind", f)
  
  # Get stats----
  A <- f[f[,1] == "A",2]
  B <- f[f[,1] == "B",2]
  C <- f[f[,1] == "C",2]
  D <- f[f[,1] == "D",2]
  
  bA <- replicate(10000, sample(A, length(A), replace= T))
  bB <- replicate(10000, sample(B, length(B), replace= T))
  bC <- replicate(10000, sample(C, length(C), replace= T))
  bD <- replicate(10000, sample(D, length(D), replace= T))
  
  mA <- colMeans(bA)
  mB <- colMeans(bB)
  mC <- colMeans(bC)
  mD <- colMeans(bD)
  
  plot(x= range(density(c(mA, mB, mC))$x),
       y= range(density(mA)$y, density(mB)$y, density(mC)$y), type= "n")
  lines(density(mA), col= "red")
  lines(density(mB), col= "green")
  lines(density(mC), col= "blue")
  lines(density(mD))
}