#include <iostream>
#include <cstdio>
#include <cmath>

#define LL long long

using namespace std;

double func_rand(LL int val, LL int m){
	return (double)val/m;
}

LL int func_val(LL int val, LL int a, LL int b, LL int m){
	LL next_val;
	next_val = ((a * val) + b) % m;

	return next_val;
}

void function(LL int initial, LL int a, LL int b, LL int m, int end, double array[]){
	LL int val, count = 0;
	LL int index, quot;
	double random, prev;

	val = initial;
	random = func_rand(val, m);

	for(count=0;count<end;++count){
		val = func_val(val, a, b, m);
		random = func_rand(val, m);
		
		array[count] = random;
	}
}

void exponential(double lambda, double array[], LL int bound){
	int index;
	
	for(index=0; index<bound; ++index){
		array[index] = -(double)log(1.0 - array[index])/lambda;
	}
}

int main(){
	LL int index;
	double array1[5005];
	double array2[5005];
	double array3[5005];
	double array4[5005];
	double array5[5005];
	double array[5005];
	LL int init_value[5] = {3452, 997, 34, 269, 5};
	LL int bound = 5000;
	double lambda = 5;
	double min, max, mean, sum = 0.0;

	function(init_value[0], 4696, 0, 5003, bound, array1);
	exponential(lambda, array1, bound);
	function(init_value[1], 4696, 0, 5003, bound, array2);
	exponential(lambda, array2, bound);
	function(init_value[2], 4696, 0, 5003, bound, array3);
	exponential(lambda, array3, bound);
	function(init_value[3], 4696, 0, 5003, bound, array4);
	exponential(lambda, array4, bound);
	function(init_value[4], 4696, 0, 5003, bound, array5);
	exponential(lambda, array5, bound);

	for(index=0; index<bound; ++index){
		array[index] = array1[index] + array2[index] + array3[index] + array4[index] + array5[index];
		printf("%lf\n", array[index]);
	}

	min = max = array[0];
	for(index=0; index<bound; ++index){
		sum+=array[index];
		
		if(array[index] < min)
			min = array[index];
			
		if(array[index] > max)
			max = array[index];
	}
	mean = (double)sum/bound;
	
	printf("Min=%lf\nMax=%lf\nMean=%lf\n", min, max, mean);
	
	return 0;
}
