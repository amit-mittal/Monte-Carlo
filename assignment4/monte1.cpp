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

	while(count<end){
		val = func_val(val, a, b, m);
		random = func_rand(val, m);
		
		array[count] = random;
		++count;
	}
	
	for(index = 0 ; index<20 ; ++index){
		printf("\"%.2lf\", %lld\n", 0.05*(index+1), array[index]);
	}
}

void exponential(double lambda, double array[], LL int bound){
	int index;
	double quot, min, max, mean, sum = 0.0;
	LL int count[100] = {0};
	
	min = max = array[0];
	for(index=0; index<bound; ++index){
		array[index] = -(double)log(1.0 - array[index])/lambda;
		printf("%lf\n", array[index]);
		
		++count[(int)array[index]];
		sum+=array[index];
		
		if(array[index] < min)
			min = array[index];
			
		if(array[index] > max)
			max = array[index];
	}
	printf("\n");
	
	mean = (double)sum/bound;

	printf("Min=%lf\nMax=%lf\nMean=%lf\n", min, max, mean);
	
	for(index = 0; index<=(int)max; ++index){
		printf("\"%d\" , %lld\n", index, count[index]);
	}
}

int main(){
	LL int index;
	double array[5005];
	LL int init_value = 3452;
	LL int bound = 5000;
	double lambda = 0.2;

	function(init_value, 4696, 0, 5003, bound, array);
	exponential(lambda, array, bound);
	printf("\n");

	return 0;
}
