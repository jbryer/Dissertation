cbal.stes <- function(X,tr.cr,ps,breaks=NULL,pstrata=NULL,nstrata=NULL,minsize=2,trM=0, trV=.2,abses=FALSE, fcol=0, VERBOSE=FALSE, axisx=NULL , ...) {
{
	if (sum(fcol)!=0) {
		col2=fcol
	}
	if (sum(fcol)==0) {
		cl=dim(X)[2]
		xclass=matrix(sapply(X,is.numeric))
		for (z in 1:cl) {
			if(!xclass[z]) {
				fcol[z]=z
			} else {
				fcol[z]=0
			}
		}
		if (sum(fcol)==0) {
			col2=fcol
			X3=X
			print("The original covariate matrix is numeric so it needed no transformation")
		} else if (sum(fcol)!=0) {
			col2=fcol[fcol[]!=0]
		}
	}
	#the above codes will identify all non-numeric columns in the original covariate matrix.
	if (sum(col2)!=0) {
		colnum=length(col2)
		if (colnum==1) XX=matrix(X[,col2])
		if (colnum!=1) XX=X[,col2]
		d=matrix(0,nrow=dim(XX)[1],ncol=colnum)
		cc=matrix(0,nrow=1,ncol=colnum)
		for(cn in 1:colnum) {
			b=matrix(XX[,cn])
			c=matrix( unique(b)[order(unique(b))] )
			len1=length(c)
			tc=t(c)
			rownames(tc)=dimnames(X)[[2]][col2[cn]]
			if (len1==2) {
				colnames(tc)= c("0", "1")
			}
			if (len1>2) {
				colnames(tc)=paste(dimnames(X)[[2]][col2[cn]],1 :len1)
			}
			print(tc)
			if(len1==2) {
				cc[1,cn]=1
			} else if (len1>2) {
				cc[1,cn]=len1
			}
			len2=length(b)
			#len2 and len1 contain lengths of all observations as well as unique values in each column(covariate) of X .
			for (i in 1:len2) {
				for (k in 1:len1) {
					if (c[k]==b[i]) {
						d[i,cn]=k-1
					}
				}
			}
		}
		dd=matrix(0, nrow=dim(XX)[1],ncol=sum(cc))
		dlabel=matrix(0, nrow=1, ncol=sum(cc))
		ko=0
		for (p in 1:colnum) {
			h1=ko+1
			ko=ko+cc[1,p]
			h2=ko
			if (cc[1,p]>2) {
				mmdp <- matrix(0, length(d[,p]), length(unique(d[,p])))
				dd[,h1:h2] <- ifelse(d[,p] == (col(mmdp)-1), 1, 0)
				dlabel[,h1:h2]= paste(dimnames(X)[[2]][col2[p]],1:cc[1,p])
			}
			if (cc[1,p]==1) {
				dd[,h2]= d[,p]
				dlabel[,h2] = dimnames(X)[[2]][col2[p]]
			}
			X[,col2[p]]=d[,p]
		}
		colnames(dd)=dlabel[1,]
		X2=cbind(X,dd)
		X3=X2[,-col2]
	}
}
X=X3
#the above codes transform all non-numeric columns into numeric (covariates) in original covariate matrix, as necessary.
if(is.null(pstrata)) {
	if( !is.null(nstrata) & !is.null(breaks) ) {
		sho=table(tr.cr,cut2(ps,breaks))
		psct=unclass(cut2(ps,breaks))
		print("ATTENTION: If nstrata is given together with the breaks vector, only the breaks vector is used")
		print("The 2 X nstrata counts matrix")
		print(sho)
	}
	#If pstrata is not given, the function will look between nstrata/breaks
	#If nstrata is given together with the breaks vector, only the breaks vector is used
	if ( is.null(nstrata) & !is.null(breaks) ) {
		sho=table(tr.cr,cut2(ps,breaks))
		print("The 2 X nstrata counts matrix")
		print(sho)
		psct=unclass(cut2(ps,breaks))
	}
	#If the break vector is given without nstrata, the breaks vector is used.
	if ( is.null(nstrata) & is.null(breaks) ) {
		unibre=sort(unique(ps))
		brkrpt=c(unibre,1.1)
		if (length(brkrpt)<min(26, nrow(X)/5)) {
			breaks=brkrpt
			sho=table(tr.cr,cut2(ps,breaks))
			sho2=matrix(sho,nrow=2)
			colnames(sho2)=round(unibre,3)
			print("The 2 X nstrata counts matrix")
			print(sho2)
			psct=unclass(cut2(ps,breaks))
		}
		#If nstrata and breaks are all not given out, unique ps values are used.
		#If the number of unique ps values is smaller than min(26, nrow(X)/5)), the unique ps values will be used as breaks.
		if (length(brkrpt)>min(25, nrow(X)/5)) {
			print("ATTENTION:nstrata is defaulted at 5 here as it is not defined")
			sho=table(tr.cr,cut2(ps,g=5))
			print("The 2 X nstrata counts matrix")
			print(sho)
			psct=unclass(cut2(ps,g=5))}
		}
		#If the number of unique ps values is bigger than min(25, nrow(X)/5)), the function will use default of nstrata=5 to form strata.
		if ( !is.null(nstrata) & is.null(breaks) ) {
			sho=table(tr.cr,cut2(ps,g=nstrata))
			print("The 2 X nstrata counts matrix")
			print(sho)
			psct=unclass(cut2(ps,g=nstrata))
		}
	}
	#If only nstrata is given out, nstrata will be used to form strata.
	if(!is.null(pstrata)) {
		psct=pstrata
		sho=table(tr.cr,psct)
	}
	#If pstrata is given out, it will be used to form strata.
	shom=matrix(sho,nrow=2)
	dimnames(shom)=list( c("tc1","tc2"), c( 1:(length(shom)/2)) )
	cutstr=0
	for (i in 1:(length(shom)/2)) {
		if (min(shom[,i])<minsize) cutstr[i]=i
		else cutstr[i]=0
	}
	cutstr=cutstr[cutstr[]!=0]
	#All those strata with the minsize of (treatment and control) smaller than 'minsize' definition will not be taken into consideration.
	if (sum(cutstr)!=0) {
		shomn=shom[,-cutstr]
	} else {
		shomn=shom
	}
	som=rbind(shomn,colSums(shomn))
	rownames(som)=c("tc1","tc2","csum")
	wcsum=sum(som[3,])
	#som is the matrix with counts in each strata on treatment/control as well as their sums after minsize restriction is enforced.
	psct2=psct
	if (sum(cutstr)!=0) {
		for (i in 1:length(psct)) {
			for (j in 1:length(cutstr))
				if (cutstr[j]== psct[i]) psct2[i]="na"
		}
	}
	XX=cbind(X,tr.cr,psct2)[cbind(X,tr.cr,psct2)[,ncol(X)+2]!="na", ]
	utc=unique(tr.cr)
	namesvs=colnames(X)
	nv=length(namesvs)
	pstr=sort(as.numeric(unique(psct2[psct2[]!="na"])))
	#The psct with strata information for each observation is modified after some of observations in unqualifed strata(minsize restriction) are deleted.
	colnames(som)=pstr
	kk=length(pstr)
	if(kk>min(26,nrow(X)/5)) {
		stop("The value of nstrata (after minsize adjustment) cannot exceed 26 or N/5")
	}
	uess=matrix(0,nrow=nv,ncol=2)
	{
		{
			esji=matrix(0,nrow=nv,ncol=kk)
			esjim=matrix(0,nrow=nv,ncol=kk)
			tabvar=matrix(0,nrow=nv, ncol=2*kk)
			dm=matrix(0,nrow=nv,ncol=kk)
			dmadj=matrix(0,nrow=nv,ncol=kk)
			#dm is the mean difference between tr/cr for each covariate across strata
			psd=matrix(0,nrow=nv,ncol=1)
			psd2= matrix(0,nrow=nv,ncol=1)
			dmi2=matrix(0,nrow=nv,ncol=1)
			#psd is the PS-adjusted pooled variance for each covariate across strata
			for (j in 1:nv) {
				for (i in 1:kk) {
					ha=XX[XX[nv+2]==pstr[i],c(j,nv+1)]
					vari=matrix(tapply(ha[,1],ha[,2],var))
					dm[j,i]= (mean(ha[ha[,2]==utc[1] ,1],trim=trM) - mean(ha[ha[,2]==utc[2] ,1],trim=trM))
					dmadj[j,i]=dm[j,i]*som[3,i]/wcsum
					tabvar[j,i]=vari[1]
					tabvar[j,i+kk]=vari[2]
				}
				#dmadj is the mean difference adjusted by strata size
				#tabvar is the variance for each cells in strata*tr/ct table
				psd[j,1]= sqrt(mean(tabvar[j,], trim=trV))
				esji[j,]=if(psd[j,1]>0) dm[j,]/psd[j,1] else 0
				esjim[j,]=if(psd[j,1]>0) dmadj[j,]/psd[j,1] else 0
				uess[j,2]=sum(esjim[j,])
				#esji provides the effect size dm/psd for each stratum for each covariate; these are shown as letters final plot
				#esjim is the middle step to calculate uess[j,2] which is the sum of dm/psd in all strata weighted by strata sizes.
				#uess[j,2] is going to be shown as weighted-avg of above dots from esji in the final plot.
				dmi2[j,1]=(mean(XX[XX[,nv+1]==utc[1],j],trim=trM) - mean(XX[XX[,nv+1]==utc[2],j],trim=trM))
				psd2[j,1]=sqrt((sd(XX[XX[,nv+1]==utc[1],j])^2+ sd(XX[XX[,nv+1]==utc[2],j])^2)/2)
				uess[j,1]=if(psd2[j,1]>0) dmi2[j,1]/psd2[j,1] else 0
				# uess[j,1] contains unadjusted ES for each covariate by direct calculation
				#dmi2 and psd2 are dm and psd for each covariate without propensity score adjustment.
			}
			psd3=cbind(psd2,psd)
			kk2=kk*2
			dimnames(psd3)=list(namesvs,c("PSD.una","PSD.adj"))
			dimnames(uess)=list(namesvs,c("stES-una","stES-adj"))
			dimnames(dm)=list(namesvs,pstr)
			dimnames(dmadj)=list(namesvs,pstr)
			dimnames(esji)=list(namesvs,pstr)
			dimnames(dmi2)=list(namesvs,"dm-una")
			dimnames(tabvar)=list(namesvs,paste("cellvar",1:kk2))
			#The above dimname function are used to give those major outputs
			uess
		}
		sq=letters
		if (abses==TRUE) {
			esji=abs(esji)
			dm=abs(dm)
			dmadj=abs(dmadj)
			dmi2=abs(dmi2)
			uess[,2]=rowSums(esjim)
			ka=abs(uess)
		}
		#when abses is set as true, all the above vector elements will be set to their absolute values.
		if (abses==FALSE) {
			ka=uess
		}
		sk=dim(ka)[1]
		ss=matrix(rep(1:sk,(2+kk)),sk,(2+kk))
		se=order(ka[,1])
		se2=order(ka[,1],decreasing = T)
		ordka=ka[se,]
		ordka2=ka[se2,]
		psd.adj=psd[se2,]
		psd.una=psd2[se2,]
		#uess matrix is ordered according to unadjusted ES values
		esji1=esji[se,]
		esji2=esji[se2,]
		dmadj=dmadj[se2,]
		dm.una=dmi2[se2,]
		dm=dm[se2,]
		dm.adj=rowSums(dmadj)
		colnames(esji2)= sq[1:kk]
		final=cbind(ordka,esji1)
		# final matrix contains all the final ES values as well as stratum-specific values for plotting.
		rr=range(final)
		xvl=max(abs(floor(rr[1]*2)),abs(ceiling(rr[2]*2)) )
		xleft=(-1/2)*xvl
		xright=(1/2)*xvl
		# xleft and xright are xlim values that make plot symmetric around x=0; the maximum (absolute) x value rounds to nearest .5
		if (!is.null(axisx)) {
			xleft=axisx[1]
			xright=axisx[2]
		}
		# if xleft and xright are given, they will be used instead
		if (abses==TRUE) {
			xleft=0
			plot(final,ss, xlim=c(xleft,xright), ylim=NULL, axes=FALSE ,
				xlab='stES-una (open circle) & stES-adj (solid circle)\nlower case letters depict individual strata', ylab=' ',
				main='Absolute Standardized Effect Sizes w/ & w/o\nPS adjustment across covariates',font=2, cex=0 , ...)
		}
		# If abses is set as true, xleft will become 0.
		if (abses==FALSE) {
			plot(final,ss, xlim=c(xleft,xright), ylim=NULL, axes=FALSE ,
				xlab='stES-una (open circle) & stES-adj (solid circle)\nlower case letters depict individual strata', ylab=' ',
				main='Standardized Effect Sizes\nw/ & w/o PS adjustment across covariates',font=2, cex=0 , ...)
		}
		axis(1, font=2, las=1)
		axis(2, 1:sk, dimnames(ordka)[[1]],font=2,las=1,tick=F,cex.axis=.79)
		points(final[,1],ss[,1],col = "red",pch=21)
		lines(final[,1],ss[,1],col="red")
		points(final[,2],ss[,2],col = "blue",pch=19)
		lines(final[,2],ss[,2],col="blue")
		#points and lines are used to map the values in final matrix onto the plot
		for (m in 3:(2+kk))
			points(final[,m],ss[,m],col="blue",pch=94+m,cex=.66)
		abline(v=0,lty=2,lwd=1.2)
		abline(v=c(seq(xleft,xright, .25 )),lty=3,lwd=.7)
		box()
		sd.ESs= apply(esji2,1,sd)
		final2=round(cbind(ordka2,esji2, sd.ESs),2)
		print("First six lines of final matrix of covariate values are")
		print(head(XX))
		print("The new counts matrix, tc1-tc2 by strata, after minsize adjustment is")
		print(som)
		print("Standardized Effect Sizes for all covariates (see documentation)")
		print(final2)
		if (VERBOSE==TRUE)
			list(originalSTRATA=shom,
				strataADJbyMINSIZE=som,
				treatmentNcontrol= utc,
				finalCOVARIATEmatrix=XX,
				strataMeanDiff.unwtd=round(dm,2),
				strataMeanDiff.wtd=round(dmadj,2),
				unadjMeanDiff=round(dmi2,2),
				ESbystrataGRAPH=round(esji2,2),
				ordEScovGRAPH=final2,
				Allvar=round(cbind(psd3,tabvar),2)
			# when verbose is set as TRUE, all the above results will be shown in the final output
		)
	}
}