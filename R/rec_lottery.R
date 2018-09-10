filelottery <- function(x, y, n, fs){
  # Arguments
  # x - Input folder
  # y - Output folder
  # n - total number of files to draw from list.
  # fs- Sections for file sorting
  
  f <- dir(x)
  f <- f[sample(1:length(f), n)]
  f <- sort(f)
  
  d <- as.list(1:fs)
  names(d) <- paste0('f', 1:fs)
  
  d <- lapply(d, function(z) f[seq(z, n, fs)])
  
  d2 <- d
  for (i in 1:length(d)){
    d2[[i]] <- paste(names(d)[i], d[[i]], sep= "/")
  }
  
  sapply(names(d), function(z) dir.create(paste(y, z, sep= "/")))
  
  lapply(d2, function(z) sapply(
    z, function(ff) dir.create(paste(y, ff, sep= "/"))
  )
  )
  
  for (i in 1:length(d)){
    for (j in 1:length(d[[i]])){
      sapply(dir(paste(x, d[[i]][[j]], sep= "/")), function(z) file.copy(
        from= paste(x, d[[i]][[j]], z, sep= "/"),
        to= paste(y, d2[[i]][[j]], z, sep= "/")
      ))
    }
  }
  
  d <- do.call('rbind', d)
  write.table(d, paste(y, "files.txt", sep= "/"))
}

filelot <- function(x, y, n){
  # Arguments
  # x - Input folder
  # y - Output folder
  # n - total number of files to draw from list

  f <- dir(x)
  f <- f[sample(1:length(f), n)]
  f <- sort(f)
  
  d <- as.list(1:fs)
  names(d) <- paste0('f', 1:fs)
  
  d <- lapply(d, function(z) f[seq(z, n, fs)])
  
  d2 <- d
  for (i in 1:length(d)){
    d2[[i]] <- paste(names(d)[i], d[[i]], sep= "/")
  }
  
  sapply(names(d), function(z) dir.create(paste(y, z, sep= "/")))
  
  lapply(d2, function(z) sapply(
    z, function(ff) dir.create(paste(y, ff, sep= "/"))
  )
  )
  
  for (i in 1:length(d)){
    for (j in 1:length(d[[i]])){
      sapply(dir(paste(x, d[[i]][[j]], sep= "/")), function(z) file.copy(
        from= paste(x, d[[i]][[j]], z, sep= "/"),
        to= paste(y, d2[[i]][[j]], z, sep= "/")
      ))
    }
  }
  
  d <- do.call('rbind', d)
  write.table(d, paste(y, "files.txt", sep= "/"))
}