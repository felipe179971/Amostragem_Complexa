#Total e Média
stratified_random_sample_mean_total<-function(N,N_h,n_h,average_h,s2_h,alpha){
  #Mean
  mean=1/N*sum( N_h*average_h )
  var_mean=sum( (N_h/N)^2 * (N_h-n_h)/N_h * s2_h/n_h )
  
  #Total
  total=sum( N_h*average_h )
  var_total=sum( N_h * (N_h-n_h) * s2_h/n_h )
  
  #IC
  a_h= N_h*(N_h-n_h)/n_h 
  d= ( sum(a_h*s2_h) )^2 / ( sum( (a_h*s2_h)^2/(n_h-1) ) )
  t=qt(1-alpha/2,round(d))
  
  erro_mean=t*sqrt(var_mean)
  erro_total=t*sqrt(var_total)
  IC_mean=c(mean-erro_mean,mean+erro_mean )
  IC_total=c(total-erro_total,total+erro_total )
  
  #Resultado
 print(data.frame(
    `.`=c("Average","Total"),
    Pontual=c(round(mean,4),round(total,4)),
    `Variância`=c(round(var_mean,4),round(var_total,4)),
    `erro`=c(round(erro_mean,4),round(erro_total,4)),
    IC=c(paste0("(",round(IC_mean[1],4),";",round(IC_mean[2],4),")"),
         paste0("(",round(IC_total[1],4),";",round(IC_total[2],4),")")
         ))
 )
 return(list(mean=mean,var_mean=var_mean,total=total,var_total=var_total,a_h=a_h,d=d,t=t,erro_mean=erro_mean,erro_total=erro_total,IC_mean=IC_mean,IC_total=IC_total))
}
#Tamanho da amostra
stratified_random_sample_allocation<-function(N,N_h,n_h,average_h,s2_h,alpha){
  n_h=1/N * n *N_h
  print("Proportional Allocation:")
  print(n_h)
  print("This does not take into consideration the variability within each stratum and is not the optimal choice.")
  if(all(!is.na(s2_h))){
    print("##########################################")
    n_h = ( n*N_h*sqrt(s2_h) ) / ( sum( N_h*sqrt(s2_h) ) )
    print("Optimal allocation where the cost of sampling from each stratum is the same:")
    print(round(n_h))
  }
  print("##########################################")
  print("-Note for Optimal allocation:")
  print("1. allocate a larger sample size to the larger and more variable stratum.")
  print("2. allocates smaller sample sizes to the more expensive stratum.")
}
#Pos-stratification
Pos_stratification_mean_total<-function(N,n,n_h,average_h,s2_h,real_prop,alpha,fti){
  if(all(is.na(N)) | fti==FALSE){
    print("Ignoring the finite correction factor")
    #Mean
    mean=sum( real_prop*average_h )
    if(all(is.na(s2_h)) ){
      var_mean="I need s2_h"
    }else{
      var_mean= 1/n * sum( real_prop*s2_h ) + 1/n^2 * sum( (1- real_prop) * s2_h )
    }
    print(paste('Average: ',mean,"| Var(Average): ",var_mean))
    
  }else{
    print("I need N")
  }
  

}

################################################################################
#[Sampling (Steven K. Thompson)]
#Example 1: The results of a stratified random sample are summarized in Table 11.1.
N_h=c(20,9,12)
n_h=c(5,3,4)
average_h=c(1.6,2.8,0.6)
s2_h=c(3.3,4,2.2)
alpha=0.05
N=sum(N_h)
stratified_random_sample_mean_total(N,N_h,n_h,average_h,s2_h)
################################################################################
#https://online.stat.psu.edu/stat506/book/export/html/655
#Example 6-1: Average Hours Watching TV Per Week
TVhour<-data.frame(
  Hour=c(35,43,36,39,28,28,29,25,38,27,26,32,29,40,35,41,37,31,45,34,27,15,4,41,49,25,10,30,8,14,12,15,30,32,21,20,34,7,11,24)
  ,Area=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3)
)
#Foi dado que
stratum=TVhour$Area
y=TVhour$Hour
N_h=c(155,62,93)
#Obtendo os outros dados
n_h=tapply(y,stratum,length)
average_h=tapply(y,stratum,mean)
s2_h=tapply(y,stratum,var)
alpha=0.05
N=sum(N_h)
#Resultado
stratified_random_sample_mean_total(N,N_h,n_h,average_h,s2_h)
#Tentando fazer usando weighted.mean o resultado não bate. PQ???
TVhour$peso=NA
TVhour$peso[which(TVhour$Area==1)]<-N_h[1]/N
TVhour$peso[which(TVhour$Area==2)]<-N_h[2]/N
TVhour$peso[which(TVhour$Area==3)]<-N_h[3]/N
weighted.mean(x=TVhour$Hour,w=TVhour$peso)
#se coloco qualque letra no lugar de w, funciona (PQ?)
weighted.mean(x=TVhour$Hour,a=TVhour$peso)
weighted.mean(x=TVhour$Hour,z=TVhour$peso)

#Usando a fórmula do peso, bate
sum(tapply(TVhour$Hour, TVhour$Area, mean)*c(N_h[1]/N,N_h[2]/N,N_h[3]/N))
################################################################################
#Example 4: Allocation. A population consists of three strata of sizes
#Tamanho da amostra
N_h=c(150,90,120)
n=12
s2_h=c(100^2,200^2,300^2)
s2_h=NA
N=sum(N_h)
stratified_random_sample_allocation(N,N_h,n_h,average_h,s2_h)
#
N_h=c(155,62,93)
n=40
s2_h=c(5^2,15^2,10^2)
N=sum(N_h)
stratified_random_sample_allocation(N,N_h,n_h,average_h,s2_h)
################################################################################
#6.3 - Poststratification and further topics on stratification
#https://online.stat.psu.edu/stat506/lesson/6/6.3
n_h=c(20,80)
average_h=c(180,120)
s2_h=NA
alpha=0.05
N=NA
real_prop=c(.5,.5)
n=100
fti=F #ignore the finite correction factor = FALSE
Pos_stratification_mean_total(N,n,n_h,average_h,s2_h,real_prop,alpha,fti)


#Example 6-2: Account Receivable
#https://online.stat.psu.edu/stat506/lesson/6/6.3
n_h=c(70,30)
average_h=c(520,280)
s2_h=c(210^2,90^2)
alpha=0.05
N=NA
real_prop=c(.4,.6)
n=100
fti=F #ignore the finite correction factor = FALSE
Pos_stratification_mean_total(N,n,n_h,average_h,s2_h,real_prop,alpha,fti)

