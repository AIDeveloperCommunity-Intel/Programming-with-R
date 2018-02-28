reduct_global <- 0
gamma_ds <- c(0)
gamma_red <- c(0)

FRSA_NFS <- function(){
  start.time <- Sys.time()
  
  FRC_GK()
  mat_all_attb()
  gamma_ds <<- DGKA(all_matrix)
  print(paste("Gamma of the DS: ", gamma_ds))
  
  gamma_all <- rep(0, length(no_col))
  for(i in 1:no_col){
    gamma_all[i] <- DGKA(smatrix[[i]])
  }
  gamma_max <- max(gamma_all)
  index <- which.max(gamma_all)
  rrmat <<- smatrix[[index]]
  
  red <- index
  red_size <- length(red)
  gamma_red <<- gamma_max
  
  u <- c(1:no_col)
 
  while(red_size < no_col && round(gamma_red, 4) < round(gamma_ds, 4)){
    left_attb <- setdiff(u, red)
    gamma_all <- rep(0, length(left_attb))
    for(i in 1:length(left_attb)){
      gamma_all[i] <- DGKA(rrmat * smatrix[[left_attb[i]]])
    }
    gamma_max <- max(gamma_all)
    index <- which.max(gamma_all)
    rrmat <<- rrmat * smatrix[[left_attb[index]]]
   
    red <- c(red, left_attb[index])
    gamma_red <<- gamma_max
    red_size <- length(red)
  }
  print(red)
  ReducedTable <- tb[c(red,nc)]
  print(paste("Reduct size:", length(red)))
  reduct_global <<- red
  print(paste("No of Conditional attributes: ", no_col))
  print(paste("Gamma of the Reduct: ", gamma_red))
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  write.csv(ReducedTable, file = "ReducedDataset.csv", row.names = FALSE)
}

