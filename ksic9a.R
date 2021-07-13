install.packages("readxl")
library(readxl)
ksic9<-read_excel("KSIC2007.xls",col_names=c("code","name_ko","name_en"))
ksic91<-ksic9[which(!is.na(ksic9$code)),]
ksic91$code_a<-substr(ksic91$code,1,1)
for(i in 1:nrow(ksic91)){
  if(grepl("[A-Z]",ksic91[i,]$code_a)) temp<-ksic91[i,]$code_a
  else ksic91[i,]$code_a<-temp
}
ksic92<-data.frame(ksic91[grep("^[A-Z]",ksic91$code),1]) # 대분류 (알파벳 대문자로 시작하는...)
ksic93<-data.frame(ksic91[grep("^[^A-Z]",ksic91$code),]) # 대분류를 제외한 나머지 (중분류, 소분류 기다려라)

# ksic9a : 대분류
ksic921<-strsplit(ksic92[,1],"\n")
ksic922<-unlist(ksic921)
ksic923<-t(matrix(ksic922,2,21))
ksic924<-data.frame(ksic923)
code_a<-substr(ksic924$X1,1,1)
name_a_ko<-substr(ksic924$X1,3,100)
name_a_en<-ksic924$X2
ksic9a<-data.frame(code_a,name_a_ko,name_a_en)
write.csv(ksic9a,"ksic9a.csv")

ksic93$code_b<-substr(ksic93$code,1,2)
ksic93$code_c<-substr(ksic93$code,1,3)
ksic93$code_d<-substr(ksic93$code,1,4)
ksic93$code_e<-substr(ksic93$code,1,5)
ksic93<-ksic93[,c("code_a","code_b","code_c","code_d","code_e","code","name_ko","name_en")]

# ksic9b : 중,소,세,쎄분류
ksic9b<-ksic93[which(nchar(ksic93$code)==2),c("code_a","code_b","name_ko","name_en")]
names(ksic9b)<-c("code_a","code_b","name_b_ko","name_b_en")
ksic9c<-ksic93[which(nchar(ksic93$code)==3),c("code_a","code_b","code_c","name_ko","name_en")]
names(ksic9c)<-c("code_a","code_b","code_c","name_c_ko","name_c_en")
ksic9d<-subset(ksic93[which(nchar(ksic93$code)==4),],select=-c(code_e,code))
names(ksic9d)<-c("code_a","code_b","code_c","code_d","name_d_ko","name_d_en")
ksic9e<-subset(ksic93[which(nchar(ksic93$code)==5),],select=-code)
names(ksic9e)<-c("code_a","code_b","code_c","code_d","code_e","name_e_ko","name_e_en")

ksic9ba<-merge(x=ksic9a,y=ksic9b,key=code_a,all.y=T)[,c("code_a","name_a_ko","code_b","name_b_ko")]
ksic9ca<-merge(x=ksic9ba,y=ksic9c,key=code_b,all.y=T)[,c("code_a","name_a_ko","code_b","name_b_ko","code_c","name_c_ko")]
ksic9da<-merge(x=ksic9ca,y=ksic9d,key=code_b,all.y=T)[,c("code_a","name_a_ko","code_b","name_b_ko","code_c","name_c_ko","code_d","name_d_ko")]
ksic9ea<-merge(x=ksic9da,y=ksic9e,key=code_b,all.y=T)[,c("code_a","name_a_ko","code_b","name_b_ko","code_c","name_c_ko","code_d","name_d_ko","code_e","name_e_ko")]
write.csv(ksic9ba,"ksic9ba.csv")
write.csv(ksic9ca,"ksic9ca.csv")
write.csv(ksic9da,"ksic9da.csv")
write.csv(ksic9ea,"ksic9ea.csv")