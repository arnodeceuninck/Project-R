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
print("1. Variabele los")
print("-------------------------------------------")

los <- data$los
print(paste0("IQR: ", IQR(los)))
print(paste0("standaardeviatie: ", sd(los)))
min_max <- range(data$los)
min <- min_max[1]
max <- min_max[2]
print(paste0("Min: ", min))
print(paste0("Max: ", max))
print(paste0("Range: ", max-min))
shapiro.test(los)
print(paste0("Shapiro: ", "W = 0.76851, p-value < 2.2e-16")) # Shapiro test om aan te tonen dat het niet normala verdeeld is, resultaat shapiro.test(los) er zelf ingeplakt, aangezien die functie de data returnt
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
avg <- mean(log_los)
sdev <- sd(log_los)
print(paste0("Approx: X~N(", avg, ", ", sdev, ")"))
shapiro.test(log_los)
print(paste0("Shapiro: ", "W = 0.97409, p-value = 1.197e-07")) # De log data is wel normaal verdeeld, # W staat voor r_Q # p-waarde zo laag waarschijnlijk door discrete dagen


# 2. Hangt de duur van het ziekenhuisverblijf af van het levend of gestorven zijn van de
# patiënten? M.a.w. is de duur van het ziekenhuisverblijf identiek verdeeld bij de popu-
# latie levende en gestorven patiënten? Voer een gepaste test uit.
print("2. Verband duur, dead/alive")
print("-------------------------------------------")

# H_0: m = m
# H_1: m != m
# Schatter voor gemiddelde en sd ziekenhuisverblijf en zien of beide groepen binnen het aanvaardingsgebied vallen
# Onduidelijk levend/gestorven na laatste opvolging of in het ziekenhuis, ik ga ervan uit dat het bij de laatste opvolging is
alive <- data$los[data$fstat=="0"] # Patienten levend bij laatste opvolging
dead <- data$los[data$fstat=="1"] # Patienten dood bij laatste opvolging
var.test(alive, dead) # 0.6906 > 0.05 -> Var dus hetzelfde
t.test(dead, alive, var.equal=TRUE) # 0.08266 > 0.05 -> H0 aanvaarden (zijn dus hetzelfde) -> Niet afhankelijk

# p-value 0.08266 > 0.05 -> Reject (? H0 aanvaarden), er is dus wel een verband tussen de twee

# 3. Ga na of er een verband is tussen het type hartinfarct en de ontslagstatus uit het ziekenhuis
# na opname. Voer opnieuw een gepaste test uit.
# H_0: ze zijn afhankelijk van elkaar
# H_1: Niet afhankelijk van elkaar
print("3. Verband type hartinfarct/ontslagstatus")
print("-------------------------------------------")
# Discrete variabelen: X^2 test
# Leuk tabelletje opstellen
# alive ( Q golf, geen Q golf) # linksboven 0,0
# dead ( Q golf, geen Q golf) # rechtsonder 1,1
dstat <- data$dstat
mitype <- data$mitype

alive <- c(nrow(data[dstat=="0" & mitype=="0",]), nrow(data[dstat=="0" & mitype=="1",]))
dead <- c(nrow(data[dstat=="1" & mitype=="0",]), nrow(data[dstat=="1" & mitype=="1",]))
ctable <- data.frame(alive, dead)

chiSq <- chisq.test(ctable)
chiSq
chiSq$observed
chiSq$expected

noqray <- c(nrow(data[dstat=="0" & mitype=="0",]), nrow(data[dstat=="1" & mitype=="0",]))
qray <- c(nrow(data[dstat=="0" & mitype=="1",]), nrow(data[dstat=="1" & mitype=="1",]))
ctable <- data.frame(noqray, qray)

chiSq <- chisq.test(ctable)
chiSq
chiSq$observed
chiSq$expected

# p-value 0.9306 > 0.05, H0 wordt dus aanvaard (Ze zijn dus onafhankelijk van elkaar)



# 4. Kan je uit de leeftijd van de patiënt het BMI voorspellen? Beschrijf uitvoerig.
# Verband tussen twee continue variabelen
print("4. Leeftijd voorspellen uit BMI")
print("-------------------------------------------")
qqnorm(data$age, main = "Normaalverdeling leeftijd")
shapiro.test(data$age)
qqnorm(data$bmi, main = "Normaalverdeling BMI")
shapiro.test(data$bmi)
# Allebei niet normaal verdeeld volgens shapiro test (ondanks lineair op eerste zicht), we kunnen Pearsons correlatiecoefficient dus niet gebruiken
plot(data$age, data$bmi, type="p", xlab="Leeftijd", ylab="BMI", main="Verband leeftijd en BMI") # Op het eerste zicht geen lineair verband

# We zouden het dus niet op de manier hieronder mogen testen (alsnog gedaan als oefening)
cor.test(data$age, data$bmi, method = "pearson") # -> Duidelijk geen correlatie
# Niet bivariaat verdeeld, dus gebruik maken van Spearmann
# H 0 : er is geen monotoon verband tussen X en Y
#H 1 : er is een mate van monotoon verband tussen X en Y
cor.test(data$age, data$bmi, method = "spearman") # Ook hierbij is onze p-waarde veel te klein, geen correlatie, dus nope, da gaat ni
