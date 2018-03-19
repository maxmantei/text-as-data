

convertSlamToMatrix <- function(DTM) {
  if (slam::is.simple_triplet_matrix(DTM)) {
    DTM <- Matrix::sparseMatrix(i=DTM$i, j=DTM$j, x=DTM$v, dims=c(DTM$nrow, DTM$ncol), dimnames = dimnames(DTM))
  }
  return(DTM)
}


convertMatrixToSparseM <- function(X) {
  X.csc <- new("matrix.csc", ra = X@x,
               ja = X@i + 1L,
               ia = X@p + 1L,
               dimension = X@Dim)
  return(as.matrix.csr(X.csc))
}


convertSlamToSparseM <- function(DTM) {
  return(convertMatrixToSparseM(convertSlamToMatrix(DTM)))
}