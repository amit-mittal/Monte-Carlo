#include <iostream>
#include <cstdio>

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

void function(LL int initial, LL int a, LL int b, LL int m, int end){
	LL int val, count = 0, array[20] = {0};
	int index, quot;
	double random;

	val = initial;
	random = func_rand(val, m);

	while(count<end){
		val = func_val(val, a, b, m);
		random = func_rand(val, m);

		quot = random/0.05;
		array[quot]++;
		++count;
	}

	for(index = 0 ; index<20 ; ++index){
		printf("%lf - %lf\t%lld\n", 0.05*index, 0.05*(index+1), array[index]);
	}
}

int main(){
	LL int index, a[5] = {100, 1000, 2000, 4000, 5000};
	LL int init_values[5] = {5, 73, 128, 23451, 2341};
	LL int bound = 1000;

	for(index = 0; index<5; ++index){
		printf("x0=%lld\n", init_values[index]);
		function(init_values[index], 1597, 0, 244944, bound);
		cout<<endl;
	}

	cout<<"x0 = "<<init_values[4]<<endl;
	for(index = 0; index < 5; ++index){
		printf("Bound = %lld\n", a[index]);
		function(init_values[4], 1597, 0, 244944, a[index]);
		printf("\n");
	}
	
	printf("\n\n\n");
	
	for(index = 0; index<5; ++index){
		printf("x0=%lld\n", init_values[index]);
		function(init_values[index], 51749, 0, 244944, bound);
		cout<<endl;
	}
	
	cout<<"x0 = "<<init_values[4]<<endl;
	for(index = 0; index < 5; ++index){
		printf("Bound = %lld\n", a[index]);
		function(init_values[4], 51749, 0, 244944, a[index]);
		printf("\n");
	}

	return 0;
}
