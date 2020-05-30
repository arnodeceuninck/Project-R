# Gegevens van heart.csv bestand
data <- read.table(file="heart.csv",header=TRUE)

# Verwijder de nodige rijen op basis van studienummer s0181217
i <- 2
j <- 1
k <- 7
data <- data[-c(k+1, j+1, i+1, j*k+1, i*j+1, i*k+1, i*j*k+1, i+j+k+1), ]


# 1. Bestudeer en bespreek de verdeling van de variabele los.
# Bespreek hiertoe gepaste grafische voorstellingen
# Ga ook op een formele manier na of de gegevens normaal verdeeld zijn.
# Indien dit niet het geval is, in welke zin wijken de gegevens af van normaal verdeelde gegevens.
# Kan je de gegevens transformeren naar normaal verdeelde gegevens? Bespreek.


# 2. Hangt de duur van het ziekenhuisverblijf af van het levend of gestorven zijn van de
# patiënten? M.a.w. is de duur van het ziekenhuisverblijf identiek verdeeld bij de popu-
# latie levende en gestorven patiënten? Voer een gepaste test uit.


# 3. Ga na of er een verband is tussen het type hartinfarct en de ontslagstatus uit het ziekenhuis
# na opname. Voer opnieuw een gepaste test uit.


# 4. Kan je uit de leeftijd van de patiënt het BMI voorspellen? Beschrijf uitvoerig.