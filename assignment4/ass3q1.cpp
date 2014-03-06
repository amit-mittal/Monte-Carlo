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

void function(LL int initial, LL int a, LL int b, LL int m, int end){
	LL int val, count = 0, array[20] = {0};
	LL int index, quot;
	double random, prev;

	val = initial;
	random = func_rand(val, m);

	while(count<end){
		prev = random;
		val = func_val(val, a, b, m);
		random = func_rand(val, m);
		
		printf("%lf , %lf\n", prev, random);

		quot = random/0.05;
		array[quot]++;
		++count;
	}
	
	for(index = 0 ; index<20 ; ++index){
		printf("\"%.2lf\", %lld\n", 0.05*(index+1), array[index]);
	}
}

int main(){
	LL int index, a[3] = {1000, 10000, 100000};
	LL int init_value = 3452;

	for(index = 0; index < 3; ++index){
		function(init_value, 16807, 0, pow(2, 31) - 1, a[index]);
		printf("\n");
	}
	
	printf("\n\n\n");
	
	for(index = 0; index<3; ++index){
		function(init_value, 40692, 0, 2147483399, a[index]);
		cout<<endl;
	}
	
	printf("\n\n\n");
	
	for(index = 0; index<3; ++index){
		function(init_value, 40014, 0, 2147483563, a[index]);
		cout<<endl;
	}
	
	printf("\n\n\n");

	return 0;
}
