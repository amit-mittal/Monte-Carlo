generate_LCG <- function(a, m, x){
	x_new = (a*x)%%m
	return(x_new)
}

generate_rand <- function(x, m){
	return(x/m)
}

generate_exp <- function(lambda, random){
	val = -log(random)/lambda
	return(val)
}

func <- function(lambda_1, lambda_2, lambda_3){
	a_1 = 16807
	a_2 = 950706376
	a_3 = 742938285
	m = (2**31) - 1
	seed = 3
	x_1 = x_2 = x_3 = seed
	bound = 10000
	count = 1
	X = array(bound)
	Y = array(bound)

	while(count<=bound){
		x_1 = generate_LCG(a_1, m, x_1)
		rand_1 = generate_rand(x_1, m)
		exp_1 = generate_exp(lambda_1, rand_1)

		x_2 = generate_LCG(a_2, m, x_2)
		rand_2 = generate_rand(x_2, m)
		exp_2 = generate_exp(lambda_2, rand_2)

		x_3 = generate_LCG(a_3, m, x_3)
		rand_3 = generate_rand(x_3, m);
		exp_3 = generate_exp(lambda_3, rand_3)

		X[count] = min(exp_1, exp_2);
		Y[count] = min(exp_1, exp_3);

		cat(sprintf("X = %g\tU1 = %g\tU2 = %g\n", X[count], exp_1, exp_2))
		cat(sprintf("Y = %g\tU1 = %g\tU3 = %g\n", Y[count], exp_1, exp_3))

		count = count + 1;
	}

	plot(X,Y)
	dev.copy(jpeg,paste("X,Y.jpg"));
	dev.off();

#	hist(Y, 100,freq = TRUE, include.lowest = TRUE, right = TRUE, density = NULL,
#		angle = 45, col = '#1E90FF', border = NULL,
#		main = paste("Histogram of frequencies for Y and total values
#		generated=",bound), xlab = "Random Numbers" ,
#		axes = TRUE, plot = TRUE, labels = FALSE);
#	dev.copy(jpeg,paste("Y.jpg"));
#	dev.off();	
}


lambda_1 = 1
lambda_2 = 2
lambda_3 = 3

func(lambda_1, lambda_2, lambda_3)