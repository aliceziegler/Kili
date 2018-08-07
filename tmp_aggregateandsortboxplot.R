library(ggplot2)

#Ursprungsdatensatz
df <- data.frame(name = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n"), alter = c(5,1,5,9,8,7,4,6,7,4,5,8,9,6), klasse = c("A", "A", "B", "B", "B", "B", "C", "C", "C", "D", "D", "D", "D", "D"))
df$klasse <- factor(df$klasse, levels = c("D", "C", "B", "A"))

#plotten
ggplot(aes_string(x = "klasse", y = "alter"), data = df) +
  geom_boxplot(aes(fill = klasse), lwd = 0.3) 


#nach absteigendem Median sortiert plotten
df_clean <- df[,c(2,3)]
df_agg <- aggregate(df_clean, by = list(df_clean$klasse), FUN = mean) ###funktioniert hier nicht mit median
df_srt <- df_agg[with(df_agg, order(df_agg$alter, decreasing = T)),]

#sortierung auf levels übertragen
df_new <- df
df_new$klasse <- factor(df_new$klasse, levels = c(as.character(df_srt$Group.1)))


#plotten
ggplot(aes_string(x = "klasse", y = "alter"), data = df_new) +
  geom_boxplot(aes(fill = klasse), lwd = 0.3) 

bymedian <- with(df, reorder(klasse, -alter, median))
boxplot(alter ~ bymedian, data = df)

#fac <- with(df, reorder(klasse, -alter, median, order = TRUE))

df$klasse <- factor(df$klasse, levels = levels(bymedian))
ggplot(df, aes(klasse,alter))+
  geom_boxplot(aes(fill = klasse), lwd = 0.3) 
