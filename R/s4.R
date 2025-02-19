### Importiere die Excel-Datei: 
### 1) Gehe zum Menü "File" -> "Import dataset" -> "From Excel..." (klicke auf OK oder Ja, um die erforderlichen Pakete installieren, wenn nötig)
### 2) Wähle die Excel-Datei aus
### 3) Ändere den Namen der Variable der Tabelle bei Bedarf (im Fenster "Import Options" unten links), hier z.B. als "df"
### 4) Klicke auf "Import"

### BITTE DEN NAMEN DES DATAFRAMES (df) UND DIE NAMEN DER SPALTEN (z.B. `Age group`, `Hospitalization length (days)`,...)
### nach eurer Tabelle anpassen!!!
### Um den Namen der Spalte richtig einzugeben, verwende das "$"-Zeichen nach dem Dataframe-Name (z.B. df$) und dann
### die entsprechende Spalte auswählen!!!

### Checke den Dataframe. HINWEIS: wenn ihr den Namen der Tabelle nicht geändert habt, müsst ihr in allen Befehlen "df" durch
### den Namen eurer Tabelle ersetzen. Falls ihr die Datei data.xlsx direkt verwendet, heißt sie wahrscheinlich "data".
df <- data
df
str(df)

# Die letzten 2 leeren Zeilen löschen
df <- df[!is.na(df$patient),]
# Eine Kopie erstellen
df_bkp <- df # warum ist es wichtig?

# Installiere das dplyr Paket, wenn es noch nicht existiert.
install.packages("dplyr")
# und lade das Paket
library(dplyr)

################# AUFGABE A4.1 ################# 

### Berechnen den Mittelwert des vorgergesagten 25(OH)D-Spiegels über die Montate
### Benötige Spalte: month_exam und predicted_ohd_measure_date

df %>% group_by(month_exam) %>% summarize(avg_ohd = mean(predicted_ohd_measure_date))

################# AUFGABE A4.1 ################# 

### Berechnen den Min-/Max-Wert und den Median des 25(OH)D-Spiegels vor der 
### Infektion innerhalb jeder Schweregradgruppe
### Benötige Spalte: pre_ohd und max_severity_hospitalization

df %>% group_by(max_severity_hospitalization) %>% 
    summarize(
        min_ohd = min(pre_ohd), max_ohd = max(pre_ohd), 
        median_ohd = median(pre_ohd), mean_ohd = mean(pre_ohd), sd_ohd = sd(pre_ohd)
    )

################# AUFGABE A4.3 #################

### Reproduzieren Sie die Ergebnisse der Tabelle 3 aus dem Paper: 
### 25(OH)D-Spiegel stratifiziert nach der Schweregradkategorie der 
### COVID-19-Erkrankung vor und nach der Cosinor-Korrektur für den jährlichen 
### Mittelwert sowie das Hospitalisierungsdatum jedes Patienten.
### (Hier nur das erste Teil: Pre-infection 20(OH)D level evaluated 14-730 days 
### before positive COVID-19 test)

### Benötige Spalte: max_severity_hospitalization und pre_ohd_cat

# Wandel die Krankheitsschweregradkategorien von 0,1,2,3 in Mild, Moderate, Severe and Critical
# Zuerst müssen wir unsere Tabelle in einem Dataframe umformatieren
df <- data.frame(df)
# dann die Krankheitsschweregradkategorien umbenennen
df[df$max_severity_hospitalization == 0,]$max_severity_hospitalization <- "Mild"
df[df$max_severity_hospitalization == 1,]$max_severity_hospitalization <- "Moderate"
df[df$max_severity_hospitalization == 2,]$max_severity_hospitalization <- "Severe"
df[df$max_severity_hospitalization == 3,]$max_severity_hospitalization <- "Critical"
# checken, ob es geklappt hat
levels(as.factor(df$max_severity_hospitalization))

# Wandel die 25(OH)D-Spiegel-Kategorien vor der Infektion von 1,2,3,4 in <20, 20-29.9, 30-39.9 und >=40
df[df$pre_ohd_cat == 1,]$pre_ohd_cat <- "<20"
df[df$pre_ohd_cat == 2,]$pre_ohd_cat <- "20-29.9"
df[df$pre_ohd_cat == 3,]$pre_ohd_cat <- "30-39.9"
df[df$pre_ohd_cat == 4,]$pre_ohd_cat <- ">=40"
levels(as.factor(df$pre_ohd_cat))

# Zähle Patienten für jede 25(OH)D-Spiegel-Kategorie, gruppiert nach der Krankheitsschweregradkategorien
df %>% group_by(pre_ohd_cat) %>% count(max_severity_hospitalization)
# Wir können das Ergebnis in einer Tabelle speichern
ohd_count_df <- df %>% group_by(pre_ohd_cat) %>% count(max_severity_hospitalization)
# Um den Prozentsatz zu berechnen, brauchen wir noch die Total Anzahl der Patienten jeder Krankheitsschweregradkategorie
total_severity_count <- df %>% count(max_severity_hospitalization)
# Kombiniere total_severity_count und ohd_count_df
ohd_count_df_combined <- merge(ohd_count_df, total_severity_count, by = "max_severity_hospitalization")
# berechne den Prozensatz
ohd_count_df_combined$percentage <- (ohd_count_df_combined$n.x / ohd_count_df_combined$n.y) * 100
# runde den Prozensatz auf eine Dezimalstelle
ohd_count_df_combined$percentage <- round(ohd_count_df_combined$percentage, 1)

# Es fehlt nur noch die Medianwerten der 25(OH)D-Spiegel vor der Infektion für jede Krankheitsschweregradkategorie
# Die Werte werden auf eine Dezimalstelle gerundet mit der round() Funktion
median_pre_ohd_df <- df %>% group_by(max_severity_hospitalization) %>% summarise("value" = round(median(pre_ohd),1))

### (OPTIONAL) Wir können jetzt die Tabelle ohd_count_df_combined so formatieren, 
### dass sie genauso wie Tabelle 3 aussieht.

# 1a. Kombinere die absolute Anzahl und Prozensatz der Patienten in der Tabelle "ohd_count_df_combined" zusammen
ohd_count_df_combined$value <- paste0(ohd_count_df_combined$n.x, "(", ohd_count_df_combined$percentage, ")")
# 1b. Die unnötige Spalte weg machen
ohd_count_df_combined <- ohd_count_df_combined %>% select(-c(n.x, n.y, percentage))

# 2. Kombiniere die beiden Tabelle "ohd_count_df_combined" und "median_pre_ohd_df" 
# 2a. Dafür müssen wir zuerst eine zusätzliche Spalte pre_ohd_cat für die Tabelle 
# "median_pre_ohd_df" erstellen
median_pre_ohd_df$pre_ohd_cat <- "median" # hier "median" ist wie eine der pre-ohd Kategorien
# 2b. dann kombiere den beiden Tabellen
pre_ohd_severity_df <- rbind(ohd_count_df_combined, median_pre_ohd_df)

# 3. Konvertiere ein schmales (long) Dataframe in ein breitest (wide) Dataframe
library(data.table) # wenn das Paket noch nicht existiert, müsst ihr es erst installieren
final_table <- dcast(setDT(pre_ohd_severity_df), pre_ohd_cat ~ max_severity_hospitalization, value.var = "value")

# 4. Sortiere die Spalte und Reihe
# 4a. wandel final_table in einem Dataframe um
final_table <- data.frame(final_table)
# erstelle die Reihefolge für die 25(OH)D-Spiegel-Kategorie
final_table$pre_ohd_cat <- factor(final_table$pre_ohd_cat, levels = c("median", "<20", "20-29.9", "30-39.9", ">=40"))
# erstell die Reihefolge für die Spalte
col_order <- c("pre_ohd_cat", "Mild", "Moderate", "Severe", "Critical")
# jetzt sortiere die final_table
final_table[, col_order] %>% arrange(pre_ohd_cat)

######## Der Hypothesistest

### H0: Es gibt kein signifikanten Unterschied in den Vitamin-D-Spiegeln innerhalb der Schweregradkategorien
### Wenn der Test signifikant ist (p-Wert < 0,05), wird die Null-Hypothese
### abgelent.Das bedeutet, dass die Variablen zwischen den verglichenen Gruppen
### signifikant unterschiedlich sind.
###
### 25(OH)D-Spiegel und Schweregradkategorien sind unabhängig Variablen => verwenden
### wir enweder den unabhängigen t-Test (wenn die Variablen normalverteilt sind),
### oder Mann-Whitney-U-Test (wenn die Variablen nicht normalverteilt sind).
###
### Hier müssen wir Mann-Whitney-U-Test (oder den Wilcoxon-Rangsummentest) benutzen.
###
### Wenn wir für mehr als 2 Gruppen testen (z.B. alle 4 Schweregragkategorien),
### müssen wir den Krusal-Wallis-Test, die erweiterte Version des 
### Wilcoxon-Rangsummentests, verwenden.
levels(as.factor(df$max_severity_hospitalization))
levels(as.factor(df$pre_ohd_cat))

### 01. Vergleiche die Vitamin-D-Spiegel vor der Infektion innerhalb aller Schweregradkategorien

### Zuerst überprüfen wir, ob die 25(OH)D-Spiegel in jeder Schweregradkategorien normalverteilt sind
# Unsere H0: die Variable sind normalverteilt
# Das heißt: wenn der Test signifikant ist (p-Wert < 0.05), sind die Variable nicht normalverteilt
# Dafür verwenden wir den Shapiro Test

# Bitte beachten, dass die Daten für 25(OH)D-Spiegel >= 40 nicht verwendet wurden
filtered_df <- df %>% filter(pre_ohd_cat != ">=40")

# Für die "Mild" Gruppe
test_df <- filtered_df %>% filter(max_severity_hospitalization == "Mild")
shapiro.test(test_df$pre_ohd)
# Der Test ergibt: W = 0.82805, p-value = 0.000002893
# Da p-Wert < 0.05 ist, sind die Variable nicht normalverteilt
# d.h., wir müssen einen nichtparametrischen Test verwenden.
# Mache dasselbe für die anderen Gruppen
test_df <- filtered_df %>% filter(max_severity_hospitalization == "Moderate")
shapiro.test(test_df$pre_ohd)
test_df <- filtered_df %>% filter(max_severity_hospitalization == "Severe")
shapiro.test(test_df$pre_ohd)
test_df <- filtered_df %>% filter(max_severity_hospitalization == "Critical")
shapiro.test(test_df$pre_ohd)

### Verwenden wir jetzt den Kruskal-Wallis-Test (weil wir mehr als 2 Gruppen haben)
# H0: Die 25(OH)D-Spiegel unerscheiden sich nicht signifikant zwichen allen 4 Schweregradkategorien
kruskal.test(filtered_df$pre_ohd, filtered_df$max_severity_hospitalization)
# Der test ergibt: Kruskal-Wallis chi-squared = 116.32, df = 3, p-value < 0.00000000000000022
# => d.h. Die 25(OH)D-Spiegel vor der Infektion unerscheiden sich signifikant
# zwichen allen 4 Schweregradkategorien

### 02. Vergleiche die Vitamin-D-Spiegel vor der Infektion in 2 Schweregradkategorien-Gruppen (serve/critical vs mild/moderate)

### Normalverteilung prüfen
test_df <- filtered_df %>% filter(max_severity_cat == 0)
print(shapiro.test(test_df$pre_ohd))
test_df <- filtered_df %>% filter(max_severity_cat == 1)
print(shapiro.test(test_df$pre_ohd))
# p-Wert < 0.05 => Verwende den nichtparametrischen Wilcoxon-Rangsummen-Test (auch bekannt als Mann-Whitney-U-Test)
wilcox.test(filtered_df$pre_ohd, filtered_df$max_severity_cat, alternative = "two.sided")
# Der test ergibt: W = 45285, p-value < 2.2e-16
# => d.h. Die 25(OH)D-Spiegel vor der Infektion unerscheiden sich signifikant
# zwichen 2 Schweregradkategorien-Gruppen (serve/critical vs mild/moderate)

