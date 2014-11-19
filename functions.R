allData <- read.csv("~/Desktop/Desktop Crap/Toby's Rainfall project/riv_rain.csv")
#
# params:
#   data - the dataframe to start with
#   offset - the offset you are trying to predict
#             ( ie 1,4, 12 hours out, etc)
#   hours - the hours to add to the matrix.
#           ie. 6, 12 and 48 hours of prior data
#             must be a list of 6 numbers
create_data_matrix <- function( data, offset, hours.rain, hours.cfs){
  shift.rain <- hours.rain # * 4
  shift.cfs <- hours.cfs # * 4
  pred_dist <- offset # * 4
  totalLength = nrow( data )
  mat <- matrix (0, nrow = totalLength, ncol = (length(hours.rain) + length( hours.cfs ) )
  for(i in (pred_dist + max(shift.rain) + 1): totalLength){
    i.1 <- i - pred_dist
    for( j in 1:(length(shift.rain)){
      mat[i,j] <- sum(data$qpcp[i.1:i.1 - shift.rain[j]])
    }
    for( j in  1: length( shift.cfs )){
      mat[i,j+length( shift.rain)] <- data$cfs[i.1 - shift.cfs[j]]
    }
  }
  cfs <- data$cfs
  ret <- data.frame( mat ) 

  for( i in 1:length(shift.rain)){
      names(ret)[i] <- "rain"+shift.rain[i]
  }
  for( i in 1:length( shift.cfs )){
      names(ret)[i + length( shift.rain )] <- "cfs"+shift[i]
  }
  return( cbind( ret, cfs ) )
}

create_formula <- function( rain, cfs ){
  str1 <- sprintf("rain%d",rain[1])
  str2 <- sprintf("cfs%d", cfs[1])
  for( i in 2:length( rain ){
    str1 <- sprintf("%s + rain%d", str1, rain[i])
  }
  for( i in 2:length( cfs ){
    str2 <- sprintf("%s + cfs%d", str2, cfs[i])
  }
  ret <- sprintf("cfs ~ %s + %s", str1, str2)
  return( ret )
}

dat <- allData[1:1000,]
offset <- 1
hours.rain <- 1:(48 *4 )
hours.cfs <- 1:(48 *4 )
#hours.rain <- c(1,2,4,6,8,10,12,18,24,48)
new_dat <- create_data_matrix( dat, offset, hours.rain, hours.cfs)
formula <- create_formula(hours.rain, hours.cfs)
head( new_dat )


train <- new_dat[1000:50000,]
train <- train[(train$cfs > 100) | ( train$V10 > 0),]


test <- new_dat[90000:92000,]
# test <- test[test$cfs > 100,]

fit <- lm( formula, train )
summary( fit )
pred <- predict.lm( fit, test )
plot( pred , type='l', col= 'red')
lines( test$cfs, type='l', col = 'blue')
plot( pred, test$cfs, type='p')

train.1 <- sample( allData, 100000)


#  start messing with arima
ar.dat <- allData[1:100000,]
ar.test <- allData[100001:200000,]
ar.ts <- ts(ar.dat$cfs)
ar.fit <- arima( ar.ts, xreg = ar.dat$qpcp )
summary(ar.fit )


head( new_dat )
max( new_dat$V3 )

plot( new_dat$cfs, type='l', col = 'blue')
lines( new_dat$V6, type='l', col = 'red')
lines(new_dat$V1 * 10 , type='p', col= 'green')
####
##  Notes:
#  1. sum of previous months rain  = 2880 observations

# currently set up to handle 6 intervals
pred_dist = 1   # 1 hour
hours <- c(1,6,12,18,24,48)
# observations are every 15 mins
shift <- hours * 4
pred_dist <- pred_dist * 4

totalLength <- length( allData$qpcp )
#totalLength <- 50000
sum <- numeric( totalLength )
sum1 <- numeric( totalLength )
sum2 <- numeric( totalLength )
sum3 <- numeric( totalLength )
sum4 <- numeric( totalLength )
sum5 <- numeric( totalLength )
sum6 <- numeric( totalLength )


cfs1 <- numeric( totalLength )
cfs2 <- numeric( totalLength )
cfs3 <- numeric( totalLength )
cfs4 <- numeric( totalLength )
cfs5 <- numeric( totalLength )
cfs6 <- numeric( totalLength )



for ( i in (pred_dist + shift[6] + 1):totalLength ){
  if( i-2880 < 1 ) sumIndex = 1 else sumIndex = (i - pred_dist) - 2876
  if( i-shift[1] < 1 ) i1 = 1 else i1 = (i - pred_dist) - shift[1]  
  if( i-shift[2] < 1 ) i2 = 1 else i2 = (i - pred_dist) - shift[2]  
  if( i-shift[3] < 1 ) i3 = 1 else i3 = (i - pred_dist) - shift[3]  
  if( i-shift[4] < 1 ) i4 = 1 else i4 = (i - pred_dist) - shift[4]
  if( i-shift[5] < 1 ) i5 = 1 else i5 = (i - pred_dist) - shift[5]
  if( i-shift[6] < 1 ) i6 = 1 else i6 = (i - pred_dist) - shift[6]
  
  cfs1[i] <- allData$cfs[[i1]]
  cfs2[i] <- allData$cfs[[i2]]
  cfs3[i] <- allData$cfs[[i3]]
  cfs4[i] <- allData$cfs[[i4]]
  cfs5[i] <- allData$cfs[[i5]]
  cfs6[i] <- allData$cfs[[i6]]
  
  
  sum[i] = sum( allData$qpcp[sumIndex:i])
  sum1[i] = sum( allData$qpcp[i1: (i - pred_dist)] )
  sum2[i] = sum( allData$qpcp[i2: (i - pred_dist)] )
  sum3[i] = sum( allData$qpcp[i3: (i - pred_dist)] )
  sum4[i] = sum( allData$qpcp[i4: (i - pred_dist)] )
  sum5[i] = sum( allData$qpcp[i5: (i - pred_dist)] )
  sum6[i] = sum( allData$qpcp[i6: (i - pred_dist)] )
}
# bind up the rows into a data frame
cfs <- allData[1:totalLength,'cfs']
starting_data <-data.frame( cbind( cfs , sum, sum1, sum2, sum3, sum4, sum5, sum6, cfs1, cfs2, cfs3, cfs4, cfs5, cfs6) )
# filter out everything less than 100
#head( starting_data )
high_data <- starting_data[cfs > 100 & cfs < 1000, ]
head( high_data )
# grab 1/3 of the data to train on
train_length = length( high_data$cfs ) / 3
# grab another chunk to test
test_length = train_length * 2 

# subset the data
length( high_data$cfs )
training_data <- high_data[1:train_length,]

test_data <- high_data[test_length: length( high_data$cfs), ]
typeof( high_data)

#fit the model
summary( high_data )
length( training_data$sum2 )
#fit <- lm( cfs ~ sum + sum1 + sum2 + sum3 + sum4 + sum5 + sum6 + cfs1 + cfs2 + cfs3 + cfs4 + cfs5 + cfs6, data = training_data)
fit <- lm( "cfs ~ sum + sum1 + sum2 + sum3 + sum4 + sum5 + sum6 + cfs1 + cfs2 + cfs3 + cfs4 + cfs5 + cfs6", data = training_data)

pred <- predict.lm( fit, test_data[3000:5000,] )
plot( pred , type = 'l', col='blue')
lines( test_data$cfs[3000:5000] , type='l', col="red")

plot( test_data$cfs[3000:5000], pred, type = 'p', col='blue')
