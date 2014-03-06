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
		printf("%lf,", random);
//		printf("%lld\t%lld\t%lf\n", count, val, random);
		val = func_val(val, a, b, m);
		random = func_rand(val, m);

		quot = random/0.05;
		array[quot]++;
		++count;
	}
}

int main(){
	LL int initial = 172;

	function(initial, 1229, 1, 2048, 150);

	return 0;
}