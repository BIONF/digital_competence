### Importiere die Excel-Datei: 
### 1) Gehe zum Menü "File" -> "Import dataset" -> "From Excel..." (klicke auf OK oder Ja, um die erforderlichen Pakete installieren, wenn nötig)
### 2) Wähle die Excel-Datei aus
### 3) Ändere den Namen der Variable der Tabelle bei Bedarf (im Fenster "Import Options" unten links), hier z.B. als "df"
### 4) Klicke auf "Import"

### BITTE DEN NAMEN DES DATAFRAMES (df) UND DIE NAMEN DER SPALTEN (z.B. `Age group`, `Hospitalization length (days)`,...)
### nach eurer Tabelle anpassen!!!
### Um den Namen der Spalte richtig einzugeben, verwende das "$"-Zeichen nach dem Dataframe-Name (z.B. df$) und dann
### die entsprechende Spalte auswählen!!!

### Checke die Tabelle / den Dataframe
collected_data
str(collected_data)

### Installiere das Paket dplyr wenn nötig (muss nur 1 Mal gemacht werden)
### mit dem Command: install.packages("dplyr")
### Lade das Paket dplyr ein
library(dplyr)

###################################
### F1: Wie viele männliche Patienten gibt es?
## Lösung 1:
# Zähle die weibl. und männl. Patienten und speichere in einem Dataframe
count_patiens <- collected_data %>% count(Gender)
# Nimmt die Anzahl der männl. Patienten raus
count_patiens %>% filter(Gender == "Male")
## Lösung 2: kombiniere die beiden oberen Befehl zusammen
collected_data %>% count(Gender) %>% filter(Gender == "Male")

###################################
### F2: Prozentsatz der Patienten, die älter als 64 Jahre sind
## Lösung 1: filtere nur die Zeile von der Daten raus, wo es ">64" bei der Spalte `Age group` gibt. Dann berechne den Prozentsatz
# Filtere den Dataframe, um nur die Zeile mit `Age group` gleich ">64" ist 
# und speichere das Ergebnis in der Variable `old_patients_df`
old_patients_df <- collected_data %>% filter(`Age group` == ">64")
# Zähle die Patienten im Dataframe old_patients_df und speichere das Ergebnis in der Variable `number_old_patients`
number_old_patients <- nrow(old_patients_df)
# Berechne die gesamte Anzahl der Patienten und speichere das Ergebnis in der Variable `number_all_patients`
number_all_patients <- nrow(collected_data)
# Berechne den Prozensatz der Patienten, die älter als 64 Jahre sind
(number_old_patients / number_all_patients) * 100
# Oder drucke den ganzen Satz:
paste("Percentage of patients older than 64y: ", (number_old_patients / number_all_patients) * 100, "%")

## Lösung 2: berechne zuerst den Prozentsatz für jede Altergruppe, dann nehme den Wert für die Gruppe ">64"
# Zähle die Patienten für jede Altergruppe und speichere das Ergebnis in der Variable `count_age_group`
count_age_group <- collected_data %>% count(`Age group`)
# Berechne die gesamte Anzahl der Patienten und speichere das Ergebnis in der Variable `number_of_patiens`
number_of_patients <- nrow(collected_data)
# Berechne den Prozentsatz der Patienten in jeder Altergruppe
count_age_group$percentage <- (count_age_group$n / number_of_patients)*100
count_age_group
# Drucke nur den Prozentsatz der Patienten, die älter als 64 Jahre sind
count_age_group[count_age_group$`Age group` == ">64",]$percentage
# Oder drucke den ganzen Satz:
paste("Percentage of patients older than 64y: ", count_age_group[count_age_group$`Age group` == ">64",]$percentage, "%")

###################################
### F3: durchschnittliche Krankenhausaufenthaltsdauer
mean(collected_data$`Hospitalization length (days)`)

###################################
### F4: Welche Gruppe von Patienten weist den schwereren (severe) Schweregrad auf?
collected_data %>% filter(`Maximal degree of severity during hospitalization` == "Severe")
# oder
collected_data[collected_data$`Maximal degree of severity during hospitalization` == "Severe",]
