# Lade benötigte Pakete
library(tidyverse)
library(ggthemes)
library(ggpubr)
library(RColorBrewer)

# Lege Ordner für Grafiken an
if (!dir.exists("grafiken")) dir.create("grafiken")

# Setze transparentes ggplot-Theme
theme_transparent <- theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill = "transparent")
  )

# Farbschemata
palette1 <- brewer.pal(4, "Set2")
palette2 <- brewer.pal(3, "Dark2")
palette3 <- brewer.pal(5, "Pastel1")
palette4 <- brewer.pal(7, "Set1")

### --------------------------------
### 1. Inhaltsanalyse Musikvideos
### --------------------------------

musik <- read.csv("daten/musikvideos.csv")

# Deskriptive Statistik
table(musik$Genre)
table(musik$Platzierung)
prop.table(table(musik$Platzierung))

# Visualisierung: Platzierung pro Genre
ggplot(musik, aes(x = Genre, fill = Platzierung)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = palette1) +
  labs(title = "Produktplatzierungen in Musikvideos nach Genre", y = "Anteil", x = "Genre") +
  theme_transparent +
  ggsave("grafiken/musikvideos_genre_platzierung.png", bg = "transparent", width = 8, height = 5)

# Kreuztabelle + Chi2-Test
table_genre_platz <- table(musik$Genre, musik$Platzierung)
chisq.test(table_genre_platz)

# Durchschnittliche Anzahl PP bei Platzierung ja
musik %>%
  filter(Platzierung == "ja") %>%
  group_by(Genre) %>%
  summarise(Mittelwert_PP = mean(Anzahl_PP), SD_PP = sd(Anzahl_PP))

# Mittelwert + SD pro Genre (nur wenn Platzierung vorhanden ist)
platz_summary <- musik %>%
  filter(Platzierung == "ja") %>%
  group_by(Genre) %>%
  summarise(
    Mittelwert_PP = mean(Anzahl_PP),
    SD_PP = sd(Anzahl_PP),
    N = n(),
    SE = SD_PP / sqrt(N)
  )

# Visualisierung mit Fehlerbalken (SE)
ggplot(platz_summary, aes(x = Genre, y = Mittelwert_PP, fill = Genre)) +
  geom_col(show.legend = FALSE, color = "black") +
  geom_errorbar(aes(ymin = Mittelwert_PP - SE, ymax = Mittelwert_PP + SE), width = 0.2) +
  scale_fill_manual(values = palette1) +
  labs(title = "Durchschnittliche Anzahl an Platzierungen (bei vorhandener Platzierung)",
       y = "Ø Anzahl Produktplatzierungen", x = "Genre") +
  theme_transparent +
  ggsave("grafiken/musikvideos_anzahl_pp_pro_genre.png", bg = "transparent", width = 8, height = 5)


### --------------------------------
### 2. Beobachtung Silent Disco
### --------------------------------

disco <- read.csv("daten/silent_disco.csv")

# Deskriptiv
summary(disco$tanzen)
summary(disco$Dauer_Kopfhoerer_Min)
table(disco$Grund_Abnehmen)

# Visualisierung: Verteilung der Gründe
ggplot(disco, aes(x = Grund_Abnehmen, fill = Grund_Abnehmen)) +
  geom_bar() +
  scale_fill_manual(values = palette2) +
  labs(title = "Gründe für das Abnehmen der Kopfhörer", x = NULL, y = "Anzahl") +
  theme_transparent +
  theme(legend.position = "none") +
  ggsave("grafiken/disco_gruende.png", bg = "transparent", width = 7, height = 5)

# Chi2-Test: Erwartete Gleichverteilung der Gründe?
chisq.test(table(disco$Grund_Abnehmen))

### --------------------------------
### 3. Befragung Musiker*innen
### --------------------------------

musikbefragung <- read.csv("daten/musikerbefragung.csv")

# Deskriptiv
summary(musikbefragung)

# Korrelationen
cor_matrix <- musikbefragung %>%
  select(Lampenfieber, Extraversion, Jahre_Erfahrung) %>%
  cor()

print(cor_matrix)

# Funktion zur Berechnung von Korrelationen mit p-Werten
cor_with_p <- function(df) {
  vars <- colnames(df)
  result <- matrix(NA, nrow = length(vars), ncol = length(vars),
                   dimnames = list(vars, vars))
  p_mat <- matrix(NA, nrow = length(vars), ncol = length(vars),
                  dimnames = list(vars, vars))

  for (i in 1:length(vars)) {
    for (j in 1:length(vars)) {
      test <- cor.test(df[[i]], df[[j]])
      result[i, j] <- round(test$estimate, 2)
      p_mat[i, j] <- test$p.value
    }
  }

  return(list(cor = result, p = p_mat))
}

# Auswahl der Variablen und Berechnung
korrelationsergebnisse <- cor_with_p(musikbefragung %>%
                                       select(Lampenfieber, Extraversion, Jahre_Erfahrung))

# Korrelationen anzeigen
print("Korrelationsmatrix:")
print(korrelationsergebnisse$cor)

# P-Werte anzeigen
print("P-Werte (Signifikanzniveaus):")
print(korrelationsergebnisse$p)


# Visualisierung: Extraversion vs. Lampenfieber
ggplot(musikbefragung, aes(x = Extraversion, y = Lampenfieber)) +
  geom_jitter(width = 0.2, height = 0.2, color = palette3[2]) +
  geom_smooth(method = "lm", se = FALSE, color = palette3[5]) +
  labs(title = "Zusammenhang zwischen Extraversion und Lampenfieber", x = "Extraversion", y = "Lampenfieber") +
  theme_transparent +
  ggsave("grafiken/lampenfieber_extraversion.png", bg = "transparent", width = 7, height = 5)

# Regression: Lampenfieber ~ Extraversion + Jahre_Erfahrung
modell <- lm(Lampenfieber ~ Extraversion + Jahre_Erfahrung, data = musikbefragung)
summary(modell)

### --------------------------------
### 4. Experiment Festivalwerbung
### --------------------------------

exp <- read.csv("daten/experiment.csv")

# Deskriptiv
exp %>%
  group_by(Bedingung) %>%
  summarise(M = mean(Spendenbereitschaft), SD = sd(Spendenbereitschaft))

# Visualisierung: Spendenbereitschaft
ggplot(exp, aes(x = Bedingung, y = Spendenbereitschaft, fill = Bedingung)) +
  geom_boxplot() +
  scale_fill_manual(values = palette4[c(2,4)]) +
  labs(title = "Spendenbereitschaft nach Bedingung", y = "Spendenbereitschaft (1–7)", x = NULL) +
  theme_transparent +
  theme(legend.position = "none") +
  ggsave("grafiken/experiment_spendenbereitschaft.png", bg = "transparent", width = 7, height = 5)

# t-Test: Spendenbereitschaft
t.test(Spendenbereitschaft ~ Bedingung, data = exp)

# t-Test: Festivalinteresse
t.test(Festivalinteresse ~ Bedingung, data = exp)

# Visualisierung: Festivalinteresse
ggplot(exp, aes(x = Bedingung, y = Festivalinteresse, fill = Bedingung)) +
  geom_boxplot() +
  scale_fill_manual(values = palette4[c(1,5)]) +
  labs(title = "Festivalinteresse nach Bedingung", y = "Interesse (1–7)", x = NULL) +
  theme_transparent +
  theme(legend.position = "none") +
  ggsave("grafiken/experiment_interesse.png", bg = "transparent", width = 7, height = 5)
