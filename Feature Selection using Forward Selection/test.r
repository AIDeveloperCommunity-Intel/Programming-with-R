tb <- read.csv("/home/donglee/Desktop/R_IIITS/Feature Selection using Forward Selection/Datasets/Ionosphere.csv", FALSE)
no_object <- nrow(tb)
nc <- ncol(tb)
no_col <- nc - 1
deltasquare <- 0.1^2

sigma <- apply(tb[ , 1:no_col], 2, sd)
smatrix <- list()
rrmat <- matrix()
all_matrix <- matrix()


GasKernel <- function(xy, sig){
  expval <- (xy * xy) / (2 * sig)
  return(exp(-expval))
}


creatematrix <- function(v, sg){
  r1 <- matrix(v, no_object, no_object, 2)
  r2 <- matrix(v, no_object, no_object)
  temp <- (abs(r1 - r2)/(4*sg))^2
  return(GasKernel(sqrt(temp), deltasquare))
}


FRC_GK <- function(){
  for(i in 1:no_col){
    smatrix[i] <<- list(creatematrix(tb[,i], sigma[i]))
  }
}


DGKA <- function(rrg){
  no_dclass <- unique(tb[,nc])
  l_dclass <- length(no_dclass)
  u <- 1:no_object
  gama <- 0
  for(i in 1:l_dclass){
    g <- which(tb[,nc] == no_dclass[i])
    ng <- setdiff(u, g)
    rm <- rrg[g, ng]
    rv<-apply(rm, 1, max)
    gama <- gama + sum(sqrt(1-rv^2))
  }
  return(gama/no_object)
}


mat_all_attb <- function(noc = no_col){
  all_matrix <<- smatrix[[1]]
  for(i in 2:noc){
   all_matrix <<- all_matrix * smatrix[[i]]
  }
  #return(DGKA(rmat))
}

mat_red <- function(rd){
  rmat <- smatrix[[rd[1]]]
  rd <- setdiff(rd, rd[1])
  for(i in rd){
    rmat <- rmat * smatrix[[i]]
  }
  rrmat <<- rmat
 # return(DGKA(rmat))
}

mat_division <- function(red, attb){
  rmat <- rrmat
  attb_mat <- smatrix[[attb]]
  temp_mat <- rmat / attb_mat
  red1 <- setdiff(red, attb)
  xi <- is.infinite(temp_mat)
  temp_mat[xi] <- smatrix[[red1[1]]][xi]
  red1 <- setdiff(red1, red1[1])
  for(i in red1){
    temp_mat[xi] <- temp_mat[xi] * smatrix[[i]][xi]
  }
  return(temp_mat)
}



mat_division1 <- function(red, attb){
  rmat <- rrmat
  
  attb_mat <- smatrix[[attb]]
  temp_mat <- rmat
  attb_mat_nz <- attb_mat != 0
  attb_mat_ze <- attb_mat == 0
  temp_mat[attb_mat_nz] <- rmat[attb_mat_nz] / attb_mat[attb_mat_nz]
  red1 <- setdiff(red, attb)
  
  temp_mat[attb_mat_ze] <- smatrix[[red1[1]]][attb_mat_ze]
  red1 <- setdiff(red1, red1[1])
  for(i in red1){
    temp_mat[attb_mat_ze] <- temp_mat[attb_mat_ze] * smatrix[[i]][attb_mat_ze]
  }
  return(DGKA(temp_mat))
}