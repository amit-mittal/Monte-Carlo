func<-function(x){
	prec<-0.00001;
	func_val<-((3*x*x) - (2.71828^x));
	deri_val<-((6*x) - (2.71828^x));
	
	next_val<-(x-(func_val/deri_val));
	
	if(abs(x-next_val)<prec){
		return(x);
	}
	else{
		func(next_val);
	}
}

main<-function(){
	val<-c(func(0),func(2),func(4));
	return(val);
}
