
setwd('e:/School/MFF/MachineLearning/Intro')

# zobrazte prubeh funkce 
x = seq(0, 1, 0.001)
y = -x * log2(x)
plot(x, y, type='l')

# zobrazte prubeh funkce H(p, 1-p) pro 0 <= p <= 1
p = (0:1000)/1000
plot(p, -p * log2(p) - (1-p) * log2(1-p), type='l')

observations = read.table('xy.100.csv', header=T)

table(observations$x)
plot(observations$x)

table(observations$y)
plot(observations$y)

table(observations)
plot(observations)

# definice nezavislosti
# for all x, y: p(x)*p(y) = p(x, y)

# marginalni rozdeleni
p.x = table(observations$x)/100
p.y = table(observations$y)/100

# nyni staci porovnat dve nasledujici tabulky
(p.x %*% t(p.y)) * 100 
table(observations)

# H(X) - entropie pro x
-sum(p.x * log2(p.x))

# H(Y) - entropie pro y
-sum(p.y * log2(p.y))

# Spocitejte podminene entropie
# HW H(X|Y), H(Y|X), I(X;Y)
# Jelikoz jsou promenne generovany nahodne, tak by se H(X|Y) mela blizit H(X), obdodne pro Y.
