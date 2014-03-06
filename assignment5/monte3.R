func_rand <- function(val, m){
	return(val/m);
}

func_val <- function(val, a, b, m){
	return(((a*val)+b)%%m);
}

func_f <- function(val){
	c=2.109375;
	ans = (20*val*((1-val)**3))/c;
	return(ans);
}

func <- function(initial, a, b, m, end, initial_2){
	count<-0;
	arr<-array(1002);
	
	val <- func_val(initial, a, b, m);
	random <- func_rand(val, m);
	val_2 <- func_val(initial_2, a, b, m);
	random_2 <- func_rand(val_2, m);

	while(count<=end){
		if(random_2<=func_f(random)){
			count = count+1;
			arr[count] = random;
		}
		val <- func_val(val, a, b, m);
		random <- func_rand(val, m);

		val_2 <- func_val(val_2, a, b, m);
		random_2 <- func_rand(val_2, m);
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
	cat(sprintf("min = %g\nmax = %g\nmean = %g\n", min, max, mean));

	hist(arr, 100,freq = TRUE, include.lowest = TRUE, right = TRUE, density = NULL,
		angle = 45, col = '#1E90FF', border = NULL,
		main = paste("Histogram of frequencies and total values
		generated=",end), xlim=range(0,max), ylim=range(0,(30)), xlab = "Random Values" ,
		axes = TRUE, plot = TRUE, labels = FALSE);

	dev.copy(jpeg,'img.jpg');
	dev.off();	
}

initial = 1853;
a = 5301;
b = 0;
m = (2**31)-1;
bound = 1000;
initial_2 = 12451;

func(initial, a, b, m, bound, initial_2);