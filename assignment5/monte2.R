func_rand <- function(val, m){
	return(val/m);
}

func_val <- function(val, a, b, m){
	next_val <- ((a * val) + b) %% m;
	return(next_val);
}

func <- function(a, b, m, end, lambda){
	arr1 <- array(end);
	arr2 <- array(end);
	arr3 <- array(end);
	arr4 <- array(end);
	arr5 <- array(end);
	arr <- array(end);

	val1 <- 3452;
	val2 <- 997;
	val3 <- 34;
	val4 <- 269;
	val5 <- 5;

	for(count in 1:end){
		val1 <- func_val(val1, a, b, m);
		arr1[count] <- func_rand(val1, m);
		arr1[count] <- -log(1 - arr1[count])/lambda;
		
		val2 <- func_val(val2, a, b, m);
		arr2[count] <- func_rand(val2, m);
		arr2[count] <- -log(1 - arr2[count])/lambda;
		
		val3 <- func_val(val3, a, b, m);
		arr3[count] <- func_rand(val3, m);
		arr3[count] <- -log(1 - arr3[count])/lambda;
		
		val4 <- func_val(val4, a, b, m);
		arr4[count] <- func_rand(val4, m);
		arr4[count] <- -log(1 - arr4[count])/lambda;
		
		val5 <- func_val(val5, a, b, m);
		arr5[count] <- func_rand(val5, m);
		arr5[count] <- -log(1 - arr5[count])/lambda;
		
		val <- arr1[count] + arr2[count] + arr3[count] + arr4[count] + arr5[count];
		arr[count] <- val;
		print(arr[count])
	}
	
	sum <- 0.0;
	min <- arr[1];
	max <- arr[1];
	
	for(index in 1:end){
		sum <- sum + arr[index];
		
		if(arr[index] < min){
			min <- arr1[index];
		}
			
		if(arr[index] > max){
			max <- arr1[index];
		}
	}
	
	mean <- sum/end;

	cat(sprintf("Min=%g\nMax=%g\nMean=%g\n", min, max, mean));

	hist(arr, 100,freq = TRUE, include.lowest = TRUE, right = TRUE, density = NULL,
		angle = 45, col = '#1E90FF', border = NULL,
		main = paste("Histogram of frequencies and total values
		generated=",end), xlim=range(0,3.5), ylim=range(0,(300)), xlab = 'Random Values' ,
		axes = TRUE, plot = TRUE, labels = FALSE);

	dev.copy(jpeg,"hist1.jpg");
	dev.off();	
}

main<-function(){
	bound <- 5000;
	lambda <- 5;

	func(4696, 0, 5003, bound, lambda);
}
