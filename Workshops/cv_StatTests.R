################################################
### Testing a classifier -- a simulation
################################################
#
# Run this code to get 
#   D        ... population of correctly and incorretly classified test examples
#   acc.avg  ... population of test accuracy averages
#   T.values ... population of t-values
#

D = c(rep(T, 830000), rep(F, 170000))

set.seed(123); acc = numeric(10); acc.avg = numeric(10000)
for(j in 1:10000){
  for(i in 1:10){ acc[i] = sum(sample(D, 100, rep=T)) }
  acc.avg[j] = mean(acc) 
} 


### Computing t-statistics

set.seed(123); T.values = numeric(10000)
for(j in 1:10000){
  for(i in 1:10){ acc[i] = sum(sample(D, 100, rep=T)) }
  T.values[j] = (mean(acc) - 83)/sd(acc) * sqrt(10)
}

qt(0.975, df=9)


A = c(171.2, 163.5, 172.9, 208.4, 178.4, 164.3, 180.5, 173.6, 185.7, 183.4, 177.8, 158.1, 182.0, 181.1, 182.8)
B = c(180.6, 189.0, 166.9, 174.1, 167.2, 170.3, 181.8)
mean(A)

mean(A) + sd(A)/sqrt(length(A))*qt(0.025, df=length(A)-1)
mean(A) - sd(A)/sqrt(length(A))*qt(0.025, df=length(A)-1)

mean(B) + sd(B)/sqrt(length(B))*qt(0.025, df=length(B)-1)
mean(B) - sd(B)/sqrt(length(B))*qt(0.025, df=length(B)-1)


# Predpokladame, ze rozdeleni studentu je 60% F a 40% M 
# V nahodnem vzorku 100 studentu bylo 53 F a 47 M

# Bud pouzijeme binomicke rozdeleni na F
# nebo pouzijeme chi squared rozdeleni

# H_0 : p1  = 60%
# H_A : p1 != 60%
# Y_1 = #zen ve vzorku, E(Y_1) = n*p1 = 60, var(Y1) = n*p1*p2 = 24
# Y_2 = #muzu ve vzorku, 

# Definujme Z = (Y1 - n*p1) / sqrt(n*p1*p2)

