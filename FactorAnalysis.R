getEV <- function(data.file){
#  data.mat <- matrix(data, ncol=12, byrow=T)
#  data.matrix <- as.matrix(read.table(data.file))
  data.matrix <- as.matrix(read.table(data.file))
  no.of.sub <- length(data.matrix[,1])/20
  data.matrix2 <- data.matrix[1:20,]
  if(no.of.sub > 1){
    for(i in 2:no.of.sub)
    {
      pointer <- (i-1)*20
      data.matrix2 <- data.matrix2 + data.matrix[(pointer+1):(pointer+20),]
    }
  }
  data.matrix2 <- data.matrix2 / no.of.sub
  r <- cor(data.matrix2)
  result <- pfa(r)
  list(DataMatrix=round(data.matrix2, digits=2),
       CorrelationMatrix=round(r, digits=3),
       EigenValues=result$evalue)
}

FactorAnalysis <- function(data.file, nev){
#  data.mat <- matrix(data, ncol=12, byrow=T)
#  data.matrix <- as.matrix(read.table(data.file))
  data.matrix <- as.matrix(read.table(data.file))
  no.of.sub <- length(data.matrix[,1])/20
  data.matrix2 <- data.matrix[1:20,]
  if(no.of.sub > 1){
    for(i in 2:no.of.sub)
    {
      pointer <- (i-1)*20
      data.matrix2 <- data.matrix2 + data.matrix[(pointer+1):(pointer+20),]
    }
  }
  data.matrix2 <- data.matrix2 / no.of.sub

  r <- cor(data.matrix2)
  result <- pfa(r)
  result2 <- varimax2(result$loading[,1:nev])
  result3 <- fscore(data.matrix2, result2$loading)
  print(result3)
}


pfa<-function(r){
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
#   par(mfrow=c(2,2),pty="s",las=1,lwd=2,tck=0.02)
   number.axis<-c(1:nc)
   plot(number.axis,evalue1, xlab="Factor", ylab="Eigen Value", type="b")
#   write.table(c("loading"),file="pfa-table.csv",
#      row.name=F,col.names=F,append=T,quote=F,sep="\t")
#   write.table(round(a,3),file="pfa-table.csv",
#      col.names=F,append=T,quote=F,sep="\t")
   list(r.org=round(r0,3),r.final=round(r,3),iter=iter,
        evalue=round(evalue1,3),cont=round(cont.evalue1,3),
        cum=round(cum.evalue,3),loading=round(a,3),
        communality=round(commu2,3))
}

varimax2<-function(am){
   m <- ncol(am)
   n <- nrow(am)
   commu <- apply(am^2,1,sum)
   icomm <- 1/sqrt(commu)
   h <- diag(icomm,nrow=length(commu))
   aini <- h%*%am
   b <- aini
   m1 <- m-1
   kk <- 1000
   for ( k in 1:kk){b2 <- b;
      for ( p in 1:m1) {p1<- p+1;
         for ( q in p1:m){ 
            bb <- b^2; bpq1 <-  bb[,p] - bb[,q];
            bpq2 <- b[,p]*b[,q];
            a1 <- sum(bpq1); b1 <- 2*sum(bpq2);
            c1 <- sum(bpq1^2)-4*sum(bpq2^2);
            d1 <- 4*sum(bpq1*bpq2);
            v1 <- d1 - 2*a1*b1/n;
            v2 <- c1 - (a1^2-b1^2)/n;
            if ( v1 > 0 && v2 > 0) theta4 <- atan(v1/v2);
            if ( v1 > 0 && v2 < 0) theta4 <- atan(v1/v2) + 3.14;
            if ( v1 < 0 && v2 > 0) theta4 <- atan(v1/v2);
            if ( v1 < 0 && v2 < 0) theta4 <- atan(v1/v2) - 3.14;
            theta <- theta4/4;
            costhe <- cos(theta);
            sinthe <- sin(theta);
            t1 <- diag(m);
            t1[p,p] <- costhe;
            t1[p,q] <- -sinthe;
            t1[q,q] <- costhe;
            t1[q,p] <- sinthe;
            b <- b%*%t1;
         }
      }
         bidf<- (b2 - b)^2;
         dif <- sum(bidf)/(m*n);
         if(dif < 0.001) {
            kk<- k;
            break
         }
   }
   b<-solve(h)%*%b ;
   eigen1<- apply(b^2,2,sum);
#   write.table(c("varimax rotaion"),file="varimax2-table.csv",
#      row.name=F,col.names=F,append=T,quote=F,sep="\t")
#   write.table(round(b,3),file="varimax2-table.csv",
#      col.names=F,append=T,quote=F,sep="\t")
   list(data=round(am,3),commu=round(commu,3),
        loading=round(b,3),evalue=round(eigen1,2),iter=kk)
}

fscore<-function(x,a)
{
   n<-ncol(x) 
   m<-ncol(a)
   n1<-nrow(x)
   n2<-ncol(x)
   sd<-sqrt(apply(x,2,var)*(n1-1)/n1)
   z<-scale(x,T,F)/matrix(rep(sd,n1),ncol=n2,byrow=T)
   a2<-t(a)%*%a
   f<-z%*%a%*%solve(a2)
#   par(mfrow=c(2,2),pty="s",las=1,lwd=2,tck=0.02)
#   factor1<-range(f[,1])
#   factor2<-range(f[,2])
#   plot(factor1,factor2,type="n")
#   text(f[,1],f[,2],font=2)
#   write.table(c("fscore"),file="fscore-table.csv",
#      row.name=F,col.names=F,append=T,quote=F,sep="\t")
   write.table(round(f,3),file="fscore-table.dat",
      col.names=F, row.names=F, sep="\t")
  list(loading=round(a,3),fscore=round(f,3))
}
