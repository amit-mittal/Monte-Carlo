func_rand <- function(val, m){
	return(val/m);
}

func_val <- function(val, a, b, m){
	next_val <- ((a * val) + b) %% m;
	return(next_val);
}

func <- function(initial, a, b, m, end, lambda){
	arr <- array(end);
	cou <- array(end);

	val <- initial;
	random <- func_rand(val, m);

	for(count in 1:end){
		val <- func_val(val, a, b, m);
		random <- func_rand(val, m);
		arr[count] <- random;
	}
	
	sum <- 0.0;
	min <- arr[1];
	max <- arr[1];
	
	for(index in 1:end){
		arr[index] <- -log(1 - arr[index])/lambda;
		cat(sprintf("%g\n", arr[index]));
		
		sum <- sum + arr[index];
		
		if(arr[index] < min){
			min <- arr[index];
		}
			
		if(arr[index] > max){
			max <- arr[index];
		}
	}
	
	mean <- sum/end;

	cat(sprintf("Min=%g\nMax=%g\nMean=%g\n", min, max, mean));
	
	hist(arr, 100,freq = TRUE, include.lowest = TRUE, right = TRUE, density = NULL,
		angle = 45, col = '#1E90FF', border = NULL,
		main = paste("Histogram of frequencies and total values
		generated=",end), xlim=range(0,max), ylim=range(0,(500)), xlab = 'Random Values' ,
		axes = TRUE, plot = TRUE, labels = FALSE);

	dev.copy(jpeg,"hist.jpg");
	dev.off();	
}

main<-function(){
	
	init_value <- 3452;
	bound <- 5000;
	lambda <- 0.2;

	func(init_value, 4696, 0, 5003, bound, lambda);
}
