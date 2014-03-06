func_rand <- function(val, m){
	return(val/m);
}

func_val <- function(val, a, b, m){
	next_val <- ((a * val) + b) %% m;
	return(next_val);
}

func_f <- function(val){
	ans = exp(-0.5*((abs(val)-1)**2));
	return(ans);
}

func <- function(initial, a, b, m, end, initial_2){
	count<-0;
	total<-0;
	arr<-array(end+2);
	
	val <- func_val(initial, a, b, m);
	random <- func_rand(val, m);
	val_2 <- func_val(initial_2, a, b, m);
	random_2 <- func_rand(val_2, m);

	while(count<=end){
		y = -log(1-random);
		
		if(random_2<=func_f(y)){
			count = count+1;
			arr[count] = y;
		}
		
		val <- func_val(val, a, b, m);
		random <- func_rand(val, m);

		val_2 <- func_val(val_2, a, b, m);
		random_2 <- func_rand(val_2, m);
		total=total+1;
	}

	max<-0;
	min<-1;
	sum<-0;
	for(i in 1:end){
		cat(sprintf("arr[%g]\t%g\n", i, arr[i]));
		
		if(arr[i]>max){
			max = arr[i];
		}

		if(arr[i]<min){
			min = arr[i];
		}
		sum = sum + arr[i];
	}
	mean = sum/end;
	cat(sprintf("min = %g\nmax = %g\nmean = %g\nVariance = %g\nPractical Acceptance Probability = %g\n", min, max, mean, var(arr), 1000/total));
	theory_probab = 1/sqrt(2*exp(1)/pi);
	cat(sprintf("Theoritical Acceptance Probability = %g\n", theory_probab));

	hist(arr, 100,freq = TRUE, include.lowest = TRUE, right = TRUE, density = NULL,
		angle = 45, col = '#1E90FF', border = NULL,
		main = paste("Histogram of frequencies and total values
		generated=",end), xlab = "Random Values" ,
		axes = TRUE, plot = TRUE, labels = FALSE);

	dev.copy(jpeg,'img.jpg');
	dev.off();	
}	

init_value <- 113;
init_value_2 <- 1031;
bound <- 1000;

func(init_value, 16807, 0, 2**31-1, bound, init_value_2);

