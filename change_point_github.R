#-------------------------------------------------------
#Code for detecting change points in driving data
#-------------------------------------------------------
#AT

#multivariate bcp for LDA
x_right = read.csv("D:/Google Drive/UVA_PhD/Projects/Change Point - Extension/08232021_with_gmm_for_bcp.csv")

x_right_final = data.frame()
attrbs <- c('YLine','ZGyro','XLine')#'XGyro','YGyro','ZGyro','XLine','YLine','ZLine')

for (val1 in (0:as.integer(nrow(x_right)/5000))){ 
  if (val1 == 0){
    a <- val1*5000
    b <- (val1+1) * 5000
    x <- x_right[a:b,]
    x <- x %>% select(c('YLine','ZGyro','XLine'))
    x1 <- as.matrix(x)
    bcp.1a <- bcp(x1,p0=0.000000001)
    for (val in 1:length(attrbs))
    { 
      temp_mean = paste(attrbs[val],"_segment_bcp_mean",sep="")
      x[[paste(temp_mean)]] = bcp.1a$posterior.mean[,val]
    }
    temp_prob = paste("segment_bcp_prob",sep="")
    x[[paste(temp_prob)]] <- bcp.1a$posterior.prob
  }
  else if (val1 == (as.integer(nrow(x_right)/5000))){
    a <- val1*5000 +1
    b <- nrow(x_right)
    x <- x_right[a:b,]
    x <- x %>% select(c('YLine','ZGyro','XLine'))
    x1 <- as.matrix(x)
    bcp.1a <- bcp(x1,p0=0.000000001)
    for (val in 1:length(attrbs))
    { 
      temp_mean = paste(attrbs[val],"_segment_bcp_mean",sep="")
      x[[paste(temp_mean)]] = bcp.1a$posterior.mean[,val]
    }
    temp_prob = paste("segment_bcp_prob",sep="")
    x[[paste(temp_prob)]] <- bcp.1a$posterior.prob
  }
  
  else {
    a <- val1*5000 + 1
    b <- (val1+1) * 5000
    x <- x_right[a:b,]
    x <- x %>% select(c('YLine','ZGyro','XLine'))
    x1 <- as.matrix(x)
    bcp.1a <- bcp(x1,p0=0.0000000001)
    for (val in 1:length(attrbs))
    { 
      temp_mean = paste(attrbs[val],"_segment_bcp_mean",sep="")
      x[[paste(temp_mean)]] = bcp.1a$posterior.mean[,val]
    }
    temp_prob = paste("segment_bcp_prob",sep="")
    x[[paste(temp_prob)]] <- bcp.1a$posterior.prob
    
    
  }
  x_right_final <- rbind(x_right_final,x)
}

x_right_final = x_right_final %>% select(c("segment_bcp_prob","YLine_segment_bcp_mean","ZGyro_segment_bcp_mean","XLine_segment_bcp_mean"))
x_right <- cbind(x_right,x_right_final)

par(mfrow=c(3,1))

plot.ts(x_right_final$segment_bcp_prob)
plot.ts(x_right_final$ZGyro)
plot.ts(x_right_final$YLine)

write.csv(x_right,"D:/Google Drive/UVA_PhD/Projects/Change Point - Extension/08232021_with_gmm_with_bcp.csv")

