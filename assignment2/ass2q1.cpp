#include <iostream>
#include <cstdio>

using namespace std;

double func_rand(int x, int M){
	return (double)x/M;
}

int func_val(int prev, int A, int B, int M){
	int next;
	next = ((A*prev) + B)%M;

	return next;
}

int main(){
	int init, val;
	
	for(init = 0 ; init < 11 ; ++init){
		val=init;
		
		printf("x%d=%d\t",init, init);
		do{
			printf("%d\t", val);
			val = func_val(val, 6, 0, 11);
		}while(val!=init);
		
		printf("%d\n", init);
	}
	
	printf("\n");
	
	for(init = 0 ; init < 11 ; ++init){
		val=init;
		
		printf("x%d=%d\t",init, init);
		do{
			printf("%d\t", val);
			val = func_val(val, 3, 0, 11);
		}while(val!=init);
		
		printf("%d\n", init);
	}
	
	return 0;
}