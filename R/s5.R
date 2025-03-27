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

df
str(df)

# Die letzten 2 leeren Zeilen löschen (wenn nötig)
df <- df[!is.na(df$patient),]
# Eine Kopie erstellen
df_bkp <- df # warum ist es wichtig?

# Installiere das dplyr Paket, wenn es noch nicht existiert.
install.packages("dplyr")
# und lade das Paket
library(dplyr)

# Ähnlich für das ggplot2 Paket
install.packages("ggplot2")
library(ggplot2)

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

# Sortiere die Krankheitsschweregrad neu
df$max_severity_hospitalization <- factor(
    df$max_severity_hospitalization, levels = c("Mild", "Moderate", "Severe", "Critical")
)
levels(df$max_severity_hospitalization)

################# AUFGABE A5.1 #################
### Plotten die Mittelwerte des vorhergesagten 25(OH)D-Spiegels, korrigiert 
### auf das Hospitalisierungsdatum, über die Monate hinweg

# Berechne den durchschnittlichen vorhergesagten Vitamin-D Spiegels für jeden Monat
plot_df <- df %>% group_by(month_exam) %>% summarise(mean_ohd = mean(predicted_ohd_measure_date))
plot_df

# Wandel die Monate von numerisch zu Worten um, und sortiere die Monate in richtiger Reihenfolge
plot_df$month_exam <- factor(
    c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"), 
    levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
)
plot_df

### OPTION 1: Barplot / Säulendiagramm
plot1 <- ggplot(plot_df, aes(x = month_exam, y = mean_ohd, fill = month_exam)) + # Erzeuge ein ggplot-Objekt
    geom_col() # Wähle den Diagrammtyp aus. Rufe ?geom_col auf, um über die Parameter zu lernen
plot1

# Formatiere das Diagramm
plot1 <- plot1 +
    labs(x = "", y = "Mean predicted 25(OH)D levels (ng/mL)") + # Achsentitel ändern
    theme_classic() +
    theme(
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.position = "None" # optional bei Verwendung von Füllfarben
    ) +
    scale_fill_brewer(palette = "Paired") # optional: Farben ändern
plot1


################# AUFGABE A5.2 #################
### Boxplot der präinfektion 25(OH)D-Spiegel vor der Hospitalisierung (Abbildung 2)

# Boxplot generieren
plot <- ggplot(df, aes(x = max_severity_hospitalization, y = pre_ohd)) + 
    geom_boxplot(outlier.shape = 1)
plot

# Füge die Fehlerbalken hinzu
plot <- plot +
    stat_boxplot(geom = "errorbar", width = 0.2)
plot

# Ergänze die Anzahl der Variablen auf den x-Achsenbeschriftungen 
count_mild <- nrow(df %>% filter(max_severity_hospitalization == "Mild"))
count_moderate <- nrow(df %>% filter(max_severity_hospitalization == "Moderate"))
count_severe <- nrow(df %>% filter(max_severity_hospitalization == "Severe"))
count_critical <- nrow(df %>% filter(max_severity_hospitalization == "Critical"))
plot <- plot +
    scale_x_discrete(
        labels = c(
            "Mild" = paste("Mild", paste("N =", count_mild), sep = "\n"), 
            "Moderate" = paste("Moderate", paste("N =", count_moderate), sep = "\n"), 
            "Severe" = paste("Severe", paste("N =", count_severe), sep = "\n"), 
            "Critical" = paste("Critical", paste("N =", count_critical), sep = "\n")
        )
    )
plot

# Füge weitere Zahlen zur y-Achse hinzu
plot <- plot +
    scale_y_continuous (breaks = seq(0, max(df$pre_ohd) + 20, by = 20))
plot

# Formatiere das Diagramm
plot <- plot +
    theme_classic() +
    labs(y = "Vitamin D (ng/mL)") +
    theme(
        axis.title.x = element_blank(),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18)
    )
plot
