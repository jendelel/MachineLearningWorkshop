# hodime 100x kostkou - kolik padne sestek?
# jake je rozdeleni poctu sestek pri 100 hodech


# vyber z hodnot [1,6], 100x s opakovanim
sample(1:6, 100, replace=T)

#kolik nam padlo sestek?
sum(sample(1:6, 100, replace=T) == 6)

# totez pomoci funce rbinom
rbinom(10, 100, 1/6)

# hustota
dbinom(10:25, 100, 1/6)
plot(dbinom(10:25, 100, 1/6))

# hodim 100x kostkou a padne 10 sestek -> je ta kostka prava?
dbinom(10, 100, 1/6) # s 2% pravdepodobnosti se to muze stat, nemuzeme vyloucit, ze neni prava

# jaka je pravdepodobnost, ze sestek padne 10 nebo mene
sum(dbinom(0:10, 100, 1/6))
# pomoci funkce pbinom
pbinom(10, 100, 1/6)

#HW zopakovat co to je kvantilova fce