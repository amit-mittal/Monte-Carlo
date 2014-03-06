#include <iostream>
#include <cmath>
#include <cstdio>

#define E 2.71828

using namespace std;

long double function_value(long double x){//this gives the value of the function at x
	long double y;
	y = (3*x*x) - pow(E, x);
	return y;
}

long double derivative_value(long double x){//this gives the value of the derivative of function at x
	long double y;
	y = (6*x) - pow(E, x);
	return y;
}

void func(long double x){//this implements the Newton-Raphson Method
	long double val_func, val_der, next_val;
	long double prec = 0.00001;
	
	cout.precision(15);//setting precision of output
	
	val_func = function_value(x);
	val_der = derivative_value(x);
	
	next_val = x - (val_func/val_der);
	
	if(abs(x-next_val)<prec){//this function checks if value is in required precision
		cout<<x<<endl;
	}
	else{
		func(next_val);
	}
}

int main(){	
	long double x;
	//calling the function to get the 3 roots
	cout<<"The 3 roots of given function are:\n";
	func(0.0);
	func(2.0);
	func(4.0);
	
	return 0;
}
