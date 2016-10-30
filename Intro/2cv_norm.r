vyska muzu ~ N(180.3, 100)
vyska zen  ~ N(167.2, 50 )

#hustota pro muze
plot(dnorm(seq(160, 200, 0.1), 180.3, sqrt(100)))

#hustota pro zeny
plot(dnorm(seq(140, 190, 0.1), 167.2, sqrt(50)))

# pravdepodobnost ze vyska zeny je mezi 166 a 168
pnorm(168, 167.2, sqrt(50)) - pnorm(166, 167.2, sqrt(50))

# pravdepodobnost, ze muz je vyssi nez 200 nebo mensi nez 165
1 - pnorm(200, 180.3, sqrt(100)) + pnorm(165, 180.3, sqrt(100))

# HW nakreslit obrazek s vybarvenymi pravdepodobnostmi


# who is more typical? woman 169 or man 178
(pnorm(169, 167.2, sqrt(50)) - 0.5) * 2
# result 0.2009358
(pnorm(178, 180.3, 10) - 0.5) * 2
# result -0.1819082
# therefore man 178 is more typical
