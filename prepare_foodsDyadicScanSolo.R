###############
######prepare dyadic.all, scan.all and solo.all for prepare_behaviorsJson.R::prepareBehaviorsJson()
###############

insert.foods <- function(dat, foods)
{
### divide data into those with context FEE and those of other contexts
if(is.null(dat$X_FOOD)) return(dat)
if(ncol(dat)==1) return(foods)
dyadic_fee <- dat[dat$X_FOOD=="_FOOD" & !is.na(dat$X_FOOD),] # 
dyadic_nonfee <- dat[!(dat$X_FOOD=="_FOOD" & !is.na(dat$X_FOOD)),]

### duplicate food_ rows with all possible foods rows (all plant species and items)
dyadic_fee2 <- dyadic_fee[rep(seq(1,nrow(dyadic_fee)), each=nrow(foods)),]
dim(dyadic_fee2)
indexx <- which(names(dyadic_fee2)=="X_FOOD")#returns # of column which contains FOOD 
if(ncol(dyadic_fee2)==indexx){
  dyadic_fee3 <- data.frame(dyadic_fee2[,1:(indexx-1)], foods[rep(1:nrow(foods), nrow(dyadic_fee)),])
  names(dyadic_fee3)[1:(indexx-1)]<-names(dyadic_fee2)[1:(indexx-1)]
} else if (indexx==1) {
	dyadic_fee3 <- data.frame(foods[rep(1:nrow(foods), nrow(dyadic_fee)),],dyadic_fee2[,(indexx+1):ncol(dyadic_fee2)])
	names(dyadic_fee3)[(indexx+ncol(foods)):ncol(dyadic_fee3)] <- names(dyadic_fee2)[(indexx+1):ncol(dyadic_fee2)]
} else {
dyadic_fee3 <- data.frame(dyadic_fee2[,1:(indexx-1)], foods[rep(1:nrow(foods), nrow(dyadic_fee)),],dyadic_fee2[,(indexx+1):ncol(dyadic_fee2)])
names(dyadic_fee3)[1:(indexx-1)]<-names(dyadic_fee2)[1:(indexx-1)]
names(dyadic_fee3)[(indexx+ncol(foods)):ncol(dyadic_fee3)]<-names(dyadic_fee2)[(indexx+1):ncol(dyadic_fee2)]
}
#head(dyadic_fee3, 250)
empty <- foods[1,]; empty[1,] <- NA
if(ncol(dyadic_nonfee)==indexx){
  dyadic_nonfee3 <- data.frame(dyadic_nonfee[,1:(indexx-1)], empty[rep(1, nrow(dyadic_nonfee)),])
} else if (indexx==1) {
	dyadic_nonfee3 <- data.frame(empty[rep(1, nrow(dyadic_nonfee)),],dyadic_nonfee[,(indexx+1):ncol(dyadic_nonfee)])
} else {
dyadic_nonfee3 <- data.frame(dyadic_nonfee[,1:(indexx-1)], empty[rep(1, nrow(dyadic_nonfee)),], dyadic_nonfee[,(indexx+1):ncol(dyadic_nonfee)])
}
names(dyadic_nonfee3)<-names(dyadic_fee3)

dyadic2 <- rbind(dyadic_nonfee3, dyadic_fee3)
return(dyadic2)
}

dyadicScanSolo <- function(dyadic, scan, solo, foods){
dyadic2 <- insert.foods(dat=dyadic, foods=foods)
scans2 <- insert.foods(dat=scan, foods=foods)
self2 <- insert.foods(dat=solo, foods=foods)
return(list(dyadic.all=dyadic2, scan.all=scans2, solo.all=self2))
}
