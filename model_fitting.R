
library(MASS)
library(magic)
library(psych)
library(coda)

sourceEntireFolder <- function(folderName, verbose=FALSE, showWarnings=TRUE) {
  files <- list.files(folderName, full.names=TRUE)
  
  # Grab only R files
  files <- files[ grepl("\\.[rR]$", files) ]
  
  if (!length(files) && showWarnings)
    warning("No R files in ", folderName)
  
  for (f in files) {
    if (verbose)
      cat("sourcing: ", f, "\n")
    ## TODO:  add caught whether error or not and return that
    try(source(f, local=FALSE, echo=FALSE), silent=!verbose)
  }
  return(invisible(NULL))
}

## set directory of code
sourceEntireFolder("/gpfs/gibbs/project/zhao_yize/sw2384/latentSNA_code", verbose=FALSE, showWarnings=TRUE)




library(MASS)

#### N is the sample size/number of subjects
N<-nn
## can replace ids with actual subject IDs
ids=seq(1,N)
## V is the number of brain regions
V<-nv
## P is the number of variables for behavior/pathology
P<-1               


theoretical.str=matrix(1, nrow = P, ncol = 1)







##############  data

## X is a list of length N, and each element of the list is a V by V connectivity matrix
names(X)=ids
## Y is a matrix of N by P, N subjects' P behavior/pathology information
rownames(Y)=ids

## randomly sample subject ids to keep as training and test data, here we use 10% test data
sampled.id=sample(ids, round(length(ids)*.1), replace = FALSE)

train.id=ids[!ids %in% sampled.id]

## full data Y_full

Y_full=Y

## training data Y

Y[sampled.id,]=NA

## family_behavior indicates the distribution of behavior,"nrm" represents normal
## nscan = number of iterations, longer nscan takes longer
## burn = number of iterations before model converges


model1=jbc_scaleAlpha_new_abcd_multi(X, Y, W=NULL, H=NULL, D=1, theoretical.str=theoretical.str, family_behavior="nrm", indices = NULL, indices_irt = NULL,
                                     seed = 1, nscan = 5000, burn = 100, odens = 1,
                                     print = TRUE, gof=TRUE, plot=TRUE,
                                     prior=list())

res=list("model"=model1, "X_full"=X_full, "sampled.id"=sampled.id)

saveRDS(res,"res.rds")


