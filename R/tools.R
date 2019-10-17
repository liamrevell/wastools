sign_test<-function(x,h0=0,conf.level=0.95){
	## exclude differences exactly equal to h0
	x<-x[!(x==h0)]
	## count all differences >h0 (e.g., positive)
	S<-sum(x>h0)
	## run a binomial test on the count
	obj<-binom.test(S,length(x),p=0.5)
	c.levs<-1-2*pbinom(1:length(x),length(x),0.5)
	c.levs<-c.levs[c.levs>=conf.level]
	ii<-which((c.levs-conf.level)^2==min((c.levs-conf.level)^2))+1
	ci<-sort(x)[c(ii,length(x)-ii+1)]
	result<-list(S=S,n=length(x),p.value=obj$p.value,h0=h0,
		conf.int=ci,conf.level=c.levs[length(c.levs)])
	class(result)<-"sign.test"
	result
}

print.sign.test<-function(x,...){
	if(hasArg(signif)) signif<-list(...)$signif
	else signif<-5
	cat("\n   One-sample sign test\n")
	cat(paste("   S = ",x$S,", number of differences = ",
		x$n,", p-value = ",round(x$p.value,signif),
		"\n",sep=""))
	cat("   Null hypothesis, true median equal to: ",x$h0,"\n")
	cat("  ",paste(round(x$conf.level*100,1),
		"% confidence interval: [",
		round(x$conf.int[1],signif),", ",
		round(x$conf.int[2],signif),"]\n\n",
		sep=""))
}

permute_test<-function(x1,x2,nsim=1000){
	n1<-length(x1)
	n2<-length(x2)
	d<-vector()
	for(i in 1:nsim){
		d[i]<-mean(x1)-mean(x2)
		x<-sample(c(x1,x2))
		x1<-x[1:n1]
		x2<-x[1:n2+n1]
	}
	P<-2*mean(d>abs(d[1]))
	object<-list(difference=d[1],p.value=P,
		diffs=d)
	class(object)<-"permute.test"
	object
}

print.permute.test<-function(x,...){
	if(hasArg(signif)) signif<-list(...)$signif
	else signif<-5
	cat("\n   Two-sample permutation test for a difference in mean.\n")
	cat(paste("   Difference in means: ",
		round(x$difference,signif),"\n",
		sep=""))
	cat(paste("   P-value from ",length(x$diffs),
		" random permutation: ",
		round(x$p.value,signif),"\n\n",sep=""))
}

plot.permute.test<-function(x,...){
	bs<-abs(diff(range(x$diffs))/40)
	b1<-seq(x$difference,min(x$diffs),
		by=if(x$difference>min(x$diffs)) -bs else bs)
	b2<-seq(x$difference+bs,max(x$diffs),
		by=if((x$difference+bs)<max(x$diffs)) bs else -bs)
	breaks<-c(min(b1)-bs,b1[length(b1):1],
		b2,max(b2)+bs)
	h<-hist(x$diffs,breaks=breaks,plot=FALSE)
	if(x$difference>0) cols<-c("white",
		"red")[(h$mids>=x$difference)+1]
	else cols<-c("white","red")[(h$mids<=x$difference)+1]
	plot(h,col=cols,
		main="Null distribution for difference in mean",
		font.main=3,
		xlab="Difference in mean (based on permutation)")
	lines(rep(x$difference,2),c(0,max(h$counts)))
	text(x$difference,0.95*max(h$counts),
		"observed\ndifference",cex=0.7,pos=2)
}