"Az elsõdleges célom az volt hogy ne kelljen digigyakok közben
egérrel rajzolgatni mindenféle eloszlásokat.
Ehhez született meg két sufnituning függvény:

-draw_Ttest_1sample()
-draw_Ttest_2sample()

Ezek egy vizuálisan is interpretálható kiegészítéssel tolják meg
a sima t.test() függvényeket. Használatuk teljesen megegyezik a szokásos t.test()-tel,
a motorháztetõ alatt valójában ezek is futnak.
Ugynakkor az egy-, és kétoldali próbákat külön függvényekként definiáltam,
mert annyira nem értek az R-hez.
Fõbb funkciók:
-Megfelelõ t-eloszlások sûrûségfüggvényeinek kirajzolása
-Adott alfához tartózó elvetési tartományok megjelenítése egy/kétoldali próba alapján
-Teszthez tartozó t-statisztika kirajzolása
-Hipotézisek megjelenítése, leírás
-Kétmintás próba esetén boxplotok megjelenítése"

#draw_Ttest_1sample######
draw_Ttest_1sample = function(x, alternative=c("two.sided", "less", "greater"),
                              mu, conf.level=0.95) {
  
  #par(mfrow = c(2,1))
  #hist(x, main="Our sample")
  #abline(v=mu,col="blue",lwd=4,lty=3)
  N=length(x)
  DF=N-1
  TT=t.test(x=x, alternative=alternative, mu=mu, conf.level = conf.level)
  axlimit=5
  if (abs(TT$statistic)>axlimit){
    axlimit=abs(TT$statistic)+1
  }
  range=seq(-axlimit,axlimit,0.001)
  y=dt(range,df=DF)
  
  if (alternative=="two.sided"){
    H0="\nH0: mean of sample is equal to"
    HA="\nHA: mean of sample is not equal to"}
  if (alternative=="less"){
    H0="\nH0: mean of sample is greater than"
    HA="\nHA: mean of sample is less than"}
  if (alternative=="greater"){
    H0="\nH0: mean of sample is less than"
    HA="\nHA: mean of sample is greater than"}
  
  #plotbase paste("One sample t-test","\nt-distribution; df =", DF)
  meanx=mean(x)
  MU=mu
  konf=conf.level
  alpha=1-konf
  par(mfrow = c(1,1))
  par(mar = c(4, 4, 6, 2))
  plot(range,y,main=paste("One sample t-test ( alpha =",alpha,
                          ")",H0,mu,HA,mu,
                          "\nt-distribution; df =", DF),
       type="l", xlab="t-value",ylab="density",
       xlim=c(-axlimit,axlimit))
  
  
  
  if (alternative=="two.sided"){
    #TWO.SIDED
    tkrit1=qt(alpha/2,lower.tail = T,df=DF)
    tkrit2=qt(alpha/2,lower.tail = F,df=DF)
    #lower.tail
    cord.a=c(min(range),seq(min(range),tkrit1,0.001),tkrit1)
    cord.b=c(0,dt(seq(min(range),tkrit1,0.001),df=DF),0)
    polygon(cord.a,cord.b, col="red")
    #upper.tail
    cord.c=c(tkrit2,seq(tkrit2,max(range),0.001),max(range))
    cord.d=c(0,dt(seq(tkrit2,max(range),0.001),df=DF),0)
    polygon(cord.c,cord.d, col="red")
  }
  
  if (alternative=="less"){
    #LESS
    tkrit1=qt(alpha,lower.tail = T,df=DF)
    #lower.tail
    cord.a=c(min(range),seq(min(range),tkrit1,0.001),tkrit1)
    cord.b=c(0,dt(seq(min(range),tkrit1,0.001),df=DF),0)
    polygon(cord.a,cord.b, col="red")
  }
  
  if (alternative=="greater"){
    #GREATER
    tkrit2=qt(alpha,lower.tail = F,df=DF)
    #upper.tail
    cord.c=c(tkrit2,seq(tkrit2,max(range),0.001),max(range))
    cord.d=c(0,dt(seq(tkrit2,max(range),0.001),df=DF),0)
    polygon(cord.c,cord.d, col="red")
  }
  
  #drawing t-value, printing t.test results
  pcolor=ifelse(TT$p.value<alpha, 'red','chartreuse4')
  abline(v=TT$statistic,col=pcolor,lwd=4,lty=3)
  print(TT)
}

#TEST
myvector1=rnorm(n = 20, mean = 24.8, sd = 3)
draw_Ttest_1sample(x=myvector1,alternative="less",mu=25, conf.level=0.99)


#draw_Ttest_2sample#######
draw_Ttest_2sample = function(x, alternative=c("two.sided", "less", "greater"),
                              y, conf.level=0.95, paired = F, var.equal = T) {
  par(mfrow = c(2,1))
  df=data.frame(
    value=c(x,y),
    trait= c(rep("X",length(x)), rep("Y", length(y)))
  )
  boxplot(df$value~df$trait, boxfill=c("deepskyblue","yellow"),
          main="Comparison of samples X and Y",
          horizontal=F,
          ylab="Values")
  N=length(x)+length(y)
  DF=N-2
  TT=t.test(x=x, alternative=alternative, y=y, conf.level = conf.level,
            paired = paired, var.equal = var.equal)
  axlimit=5
  if (abs(TT$statistic)>axlimit){
    axlimit=abs(TT$statistic)+1
  }
  range=seq(-axlimit,axlimit,0.001)
  y=dt(range,df=DF)
  
  if (alternative=="two.sided"){
    H0="\nH0: mean of X is equal to mean of Y"
    HA="\nHA: mean of X is not equal to mean of Y"}
  if (alternative=="less"){
    H0="\nH0: mean of X is greater than mean of Y"
    HA="\nHA: mean of X is less than mean of Y"}
  if (alternative=="greater"){
    H0="\nH0: mean of X is less than mean of Y"
    HA="\nHA: mean of X is greater than mean of Y"}
  
  #plotbase paste("One sample t-test","\nt-distribution; df =", DF)
  konf=conf.level
  alpha=1-konf
  par(mar = c(4, 4, 6, 2))
  plot(range,y,main=paste("Two sample t-test ( alpha =",alpha,
                          ")",H0,HA,
                          "\nt-distribution; df =", DF),
       type="l", xlab="t-value",ylab="density",
       xlim=c(-axlimit,axlimit))
  
  
  
  if (alternative=="two.sided"){
    #TWO.SIDED
    tkrit1=qt(alpha/2,lower.tail = T,df=DF)
    tkrit2=qt(alpha/2,lower.tail = F,df=DF)
    #lower.tail
    cord.a=c(min(range),seq(min(range),tkrit1,0.001),tkrit1)
    cord.b=c(0,dt(seq(min(range),tkrit1,0.001),df=DF),0)
    polygon(cord.a,cord.b, col="red")
    #upper.tail
    cord.c=c(tkrit2,seq(tkrit2,max(range),0.001),max(range))
    cord.d=c(0,dt(seq(tkrit2,max(range),0.001),df=DF),0)
    polygon(cord.c,cord.d, col="red")
  }
  
  if (alternative=="less"){
    #LESS
    tkrit1=qt(alpha,lower.tail = T,df=DF)
    #lower.tail
    cord.a=c(min(range),seq(min(range),tkrit1,0.001),tkrit1)
    cord.b=c(0,dt(seq(min(range),tkrit1,0.001),df=DF),0)
    polygon(cord.a,cord.b, col="red")
  }
  
  if (alternative=="greater"){
    #GREATER
    tkrit2=qt(alpha,lower.tail = F,df=DF)
    #upper.tail
    cord.c=c(tkrit2,seq(tkrit2,max(range),0.001),max(range))
    cord.d=c(0,dt(seq(tkrit2,max(range),0.001),df=DF),0)
    polygon(cord.c,cord.d, col="red")
  }
  
  #drawing t-value, printing t.test results
  pcolor=ifelse(TT$p.value<alpha, 'red','chartreuse4')
  abline(v=TT$statistic,col=pcolor,lwd=4,lty=3)
  print(TT)
  par(mfrow = c(1,1))
}


#TEST1
myvector1=rnorm(20, mean = 26, sd = 3)
myvector2=rnorm(20, mean = 25, sd = 3)

draw_Ttest_2sample(x=myvector1,alternative="two.sided",y=myvector2, conf.level=0.9,
                   paired = F, var.equal = T)


#TEST2
myvector1=rnorm(20, mean = 26, sd = 3)
myvector2=rnorm(20, mean = 25, sd = 3)

draw_Ttest_2sample(x=myvector1,alternative="greater",y=myvector2, conf.level=0.9,
                   paired = F, var.equal = T)



"A t-eloszlások demonstrációja 'kísérletesen'"
#Demonstrating "the distribution of t-differences"
par(mfrow = c(1,1))
set.seed(1156421456) #véletlenszám generátor beállítása
POP_SIZE = 1000000 #populációméretek

#normáleloszlás paraméterei
MU = 25
SIGMA = 3

population = rnorm(mean = MU, sd = SIGMA, n = POP_SIZE)
sample_size=100
DF=sample_size-1


#t-value=(mean(x)-MU)/(SIGMA/(sqrt(n)))
T_sample=replicate(10000,(t.test(x=(sample(population, sample_size)),mu=MU))$statistic)

par(mfrow = c(2,1))

hist(T_sample, xlab="t-távolság",
     main=paste("'t-távolságok' eloszlása (hisztogram); mintaelemszám:", sample_size))

T_density <- density(T_sample) # sûrûségfüggvény becslése
plot(T_density, col="black", lwd=8,
     main=paste("'t-távolságok' eloszlása; mintaelemszám:", sample_size,
                "\nt-eloszlás df =", DF),
     xlab="t-távolság")

range=seq(-5,5,0.001)
y=dt(range,df=DF)
lines(range,y, col="red",lwd=4, lty=2)

par(mfrow = c(1,1))