# Datensatz Inhaltsanalyse: Produktplatzierungen in Musikvideos

set.seed(123)
musikvideos <- data.frame(
  ID = 1:300,
  Genre = sample(c("Hip-Hop", "Rock", "Pop", "Electro"), 300, replace = TRUE, prob = c(0.35, 0.25, 0.25, 0.15))
)

# Genre-spezifische Wahrscheinlichkeit für Platzierungen
musikvideos$Platzierung <- mapply(function(genre) {
  prob_ja <- switch(genre,
                    "Hip-Hop" = 0.6,
                    "Pop" = 0.4,
                    "Rock" = 0.2,
                    "Electro" = 0.15)
  sample(c("ja", "nein"), 1, prob = c(prob_ja, 1 - prob_ja))
}, musikvideos$Genre)

# Wenn Platzierung ja, dann Anzahl zwischen 1 und 5 bei Hip-Hop, sonst 0–3
musikvideos$Anzahl_PP <- ifelse(
  musikvideos$Platzierung == "ja",
  ifelse(
    musikvideos$Genre == "Hip-Hop",
    sample(1:5, sum(musikvideos$Platzierung == "ja" & musikvideos$Genre == "Hip-Hop"), replace = TRUE),
    sample(1:3, sum(musikvideos$Platzierung == "ja" & musikvideos$Genre != "Hip-Hop"), replace = TRUE)
  ),
  0
)


write.csv(musikvideos, "daten/musikvideos.csv", row.names = FALSE)

# Datensatz Beobachtung: Menschen in der Silent Disco

set.seed(456)
silent_disco <- data.frame(
  Beobachtung = 1:300,
  tanzen = rbinom(300, size = 1, prob = 0.9),  # 90% tanzen mindestens einmal
  Dauer_Kopfhoerer_Min = round(rnorm(300, mean = 35, sd = 10)),  # Min. 0, Max. ca. 60
  Grund_Abnehmen = sample(c("Getränke bestellen", "Gespräch führen", "WC-Besuch"), 300, replace = TRUE, prob = c(0.4, 0.4, 0.2))
)

write.csv(silent_disco, "daten/silent_disco.csv", row.names = FALSE)

# Datensatz Befragung: Musiker*innen über Lampenfieber

set.seed(789)

# Extraversion: kontinuierlich verteilt (1–5)
Extraversion <- round(runif(100, 1, 5), 2)

# Jahre_Erfahrung: ebenfalls kontinuierlich, unabhängig von Extraversion
Jahre_Erfahrung <- round(runif(100, 0, 25), 1)

# Lampenfieber: negativ abhängig von Extraversion und Erfahrung + zufälliger Fehler
Lampenfieber <- 6 - 0.6 * Extraversion - 0.05 * Jahre_Erfahrung + rnorm(100, mean = 0, sd = 0.3)

# Werte auf 1–5 begrenzen
Lampenfieber <- round(pmax(pmin(Lampenfieber, 5), 1), 2)

musikerbefragung <- data.frame(
  ID = 1:100,
  Lampenfieber = Lampenfieber,
  Extraversion = Extraversion,
  Jahre_Erfahrung = Jahre_Erfahrung
)

write.csv(musikerbefragung, "daten/musikerbefragung.csv", row.names = FALSE)

# Experiment: Einfluss von Eintrittsformaten bei Festivalwerbung

set.seed(101)
experiment <- data.frame(
  VP_ID = 1:120,
  Bedingung = rep(c("Freier Eintritt + Spende", "10 Euro Eintritt, wird gespendet"), each = 60),
  Spendenbereitschaft = c(
    sample(5:7, 60, replace = TRUE, prob = c(0.3, 0.4, 0.3)),
    sample(2:5, 60, replace = TRUE, prob = c(0.2, 0.4, 0.3, 0.1))
  ),
  Festivalinteresse = c(
    sample(3:6, 60, replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2)),
    sample(3:7, 60, replace = TRUE, prob = c(0.1, 0.2, 0.2, 0.4, 0.1))
  )
)

write.csv(experiment, "daten/experiment.csv", row.names = FALSE)
