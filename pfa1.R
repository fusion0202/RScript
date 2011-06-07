pfa1<-function(r){
   n<-ncol(r)
   r[row(r)==col(r)]<-0
   commu1<-apply(abs(r),1,max)
   r[row(r)==col(r)]<-commu1
   r0<-r
   dif<-1000
   for ( i  in 1:100){
     r[row(r)==col(r)]<-commu1
     evalue<-eigen(r)$values
     evector<-eigen(r)$vectors
     for ( j in 1:ncol(evector)) if(evector[1,j]<=0) evector[,j]<--evector[,j]
     evalue1<-evalue[round(evalue,6)>0]
     nc<-length(evalue1)
     cont.evalue1<-evalue1/n
     d1<-matrix(0,ncol=nc,nrow=nc)
     d1[col(d1)>=row(d1)]<-1
     cum.evalue<-cont.evalue1%*%d1
     lamda<-diag(evalue1,nc)
     evector<-evector[,1:nc]
     b<-apply(evector^2,2,sum)
     for ( j in 1:nc) evector[,j]<-evector[,j]/sqrt(b[j])
     a<-evector%*%sqrt(lamda)
     commu2<-apply(a^2,1,sum)
     dif<-sqrt(mean((commu1-commu2)^2))
     commu1<-commu2
     if(dif < 0.01) {iter<-i;break}
   }
   par(mfrow=c(2,2),pty="s",las=1,lwd=2,tck=0.02)
   number.axis<-c(1:nc)
   plot(number.axis,evalue1,type="b")
   plot(number.axis,cum.evalue,type="b")
   axis1<-c(-1,1)
   axis2<-c(-1,1)
   plot(axis1,axis2,type="n")
   text(a[,1],a[,2])
   write.table(c("loading"),file="pfa-table.xls",
      row.name=F,col.names=F,append=T,quote=F,sep="\t")
   write.table(round(a,3),file="pfa-table.xls",
      col.names=F,append=T,quote=F,sep="\t")
   list(r.org=round(r0,3),r.final=round(r,3),iter=iter,
        evalue=round(evalue1,3),cont=round(cont.evalue1,3),
        cum=round(cum.evalue,3),loading=round(a,3),
        communarity=round(commu2,3))
}
