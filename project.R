# Gegevens van heart.csv bestand
data <- read.table(file="heart.csv",header=TRUE,sep=";",dec=",")

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
los <- data$los
print(paste0("IQR: ", IQR(los)))
print(paste0("standaardeviatie: ", sd(los)))
min_max <- range(data$los)
min <- min_max[1]
max <- min_max[2]
print(paste0("Min: ", min))
print(paste0("Max: ", max))
print(paste0("Range: ", max-min))
boxplot(los, ylab="Boxplot duur ziekenhuisbezoek (in dagen)") # Boxplot assymetrisch
hist(los, ylab="Histogram duur ziekenhuisbezoek (in dagen)") # Assymetrisch # https://www.datamentor.io/r-programming/histogram/
# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/qqnorm
# https://stackoverflow.com/questions/15589601/print-string-and-variable-contents-on-the-same-line-in-r
# https://stackoverflow.com/questions/6123378/how-to-read-in-numbers-with-a-comma-as-decimal-separator
# Strategie Computerzitting R1 2 p.4
qqnorm(los, main="QQ-plot duur ziekenhuisbezoek (in dagen)") # Qqnorm om te bepalen of normaal verdeeld, indien normaal zou dit lineair moeten zijn
# Bovenstaand plot vertoont een exponentieel verband, onderzoeken of we een lineair verband krijgen indien we de log nemen
log_los <- log(los)
log_los <- log_los[is.finite(log_los)] # Verwijder de oneindige waarden (met oneindig wil R niet plotten bij mij) # src: https://stackoverflow.com/questions/36590230/how-to-remove-rows-with-inf-from-a-dataframe-in-r
qqnorm(log_los, main="QQ-plot log van de duur ziekenhuisbezoek (in dagen)")
boxplot(log_los, main="Boxplot log van de duur ziekenhuisbezoek (in dagen)")
hist(log_los, main="Histogram log van de duur ziekenhuisbezoek (in dagen)")
print(paste0("Approx: X~N(", mean(log_los), ", ", sd(log_los), ")"))

# 2. Hangt de duur van het ziekenhuisverblijf af van het levend of gestorven zijn van de
# patiënten? M.a.w. is de duur van het ziekenhuisverblijf identiek verdeeld bij de popu-
# latie levende en gestorven patiënten? Voer een gepaste test uit.
# H_0: ze zijn afhankelijk van elkaar
# H_1: Niet afhankelijk van elkaar
# Schatter voor gemiddelde en sd ziekenhuisverblijf en zien of beide groepen binnen het aanvaardingsgebied vallen


# 3. Ga na of er een verband is tussen het type hartinfarct en de ontslagstatus uit het ziekenhuis
# na opname. Voer opnieuw een gepaste test uit.
# H_0: ze zijn afhankelijk van elkaar
# H_1: Niet afhankelijk van elkaar

# 4. Kan je uit de leeftijd van de patiënt het BMI voorspellen? Beschrijf uitvoerig.
# Verband tussen twee continue variabelen
plot(data$age, data$bmi, type="p", xlab="Leeftijd", ylab="BMI", main="Verband leeftijd en BMI")