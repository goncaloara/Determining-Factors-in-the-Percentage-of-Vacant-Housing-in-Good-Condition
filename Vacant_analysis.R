# Packages necessários

library(readr)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(forcats)
library(dplyr)
library(grid)
library(tidyr)
library(kableExtra)
library(car)

# Importar o dataset

datase <- read_csv(
  "C:/Users/gonca/Downloads/241213VAGOS2.csv", 
  
  col_types = cols(
    PERCENTAGEM_VAGOS = col_number(), 
    PERCENTAGEM_VAGOS2 = col_number(), 
    `Valor venda` = col_number(), 
    `Variação poulacional` = col_number(),
    
    `tipo de territorio` = col_factor(levels = c("ALTA DENSIDADE", 
                                                 "BAIXA DENSIDADE", 
                                                 "RAA", 
                                                 "RAM")), 
    
    `PRESSÃO URBANISTICA` = col_character(), 
    
    Riqueza = col_skip(), 
    `Vagos.Reparação Ligeiras` = col_number(),
    `Vagos.Reparação médias` = col_skip(), 
    `Vagos.Reparação profundas` = col_skip(), 
    `Total alojamentos` = col_number(),
    `Total Vagos 2011_PER` = col_number(),
    `Total Vagos 2021_PER` = col_number()
  )
)

#Overview dos dados

datase <- as.data.frame(datase)
View(datase)
print(head(datase))
print(str(datase))
print(dim(datase))

#Mudar o nome das variáveis

colnames(datase)[colnames(datase) == "PRESSÃO URBANISTICA"] <- "pressao_urbanistica"
colnames(datase)[colnames(datase) == "tipo de territorio"] <- "tipo_territorio"
colnames(datase)[colnames(datase) == "Variação poulacional"] <- "variacao_populacional"
colnames(datase)[colnames(datase) == "Valor venda"] <- "valor_venda"
colnames(datase)[colnames(datase) == "Total alojamentos"]<-"total_alojamentos"
colnames(datase)[colnames(datase) == "Total Vagos 2011_PER"]<-"vagos_2011_per"
colnames(datase)[colnames(datase) == "Total Vagos 2021_PER"]<-"vagos_2021_per"
colnames(datase)[colnames(datase) == "Vagos.Reparação não"]<-"vagos_ok"
colnames(datase)[colnames(datase) == "Vagos.Reparação Ligeiras"]<-"vagos_ligeiras"
colnames(datase)[colnames(datase) == "Vagos.Reparação médias"]<-"vagos_medias"
colnames(datase)[colnames(datase) == "Vagos.Reparação profundas"]<-"vagos_profundas"
colnames(datase)[colnames(datase) == "NUTSIII"]<-"subregiao"
colnames(datase)[colnames(datase) == "PERCENTAGEM_VAGOS"]<-"vagosbom_totalalojamentos"
colnames(datase)[colnames(datase) == "PERCENTAGEM_VAGOS2"]<-"vagosbom_totalvagos"


# Criar 2 novas variaveis: total de alojamentos vagos bom estado e variação de alojamento vagos, anos 2021-2011

datase$total_vagosbom <- datase$vagos_ok+datase$vagos_ligeiras
datase$variaçao_vagos <- round(datase$vagos_2021_per-datase$vagos_2011_per,3)

# Alterar ordem do dataframe para facilitar interpretação 

change_order <- c( "lugar", "codigo", "subregiao", "tipo_territorio", "valor_venda", 
                   "variacao_populacional", "pressao_urbanistica", "vagos_ok", 
                   "vagos_ligeiras", "total_vagosbom", "total_alojamentos", "vagosbom_totalvagos", 
                   "vagosbom_totalalojamentos", "vagos_2011_per", "vagos_2021_per")


datase <- datase[, change_order]

# Arrendondar valores para 3 casas decimais

datase$vagosbom_totalvagos <- round(datase$vagosbom_totalvagos,3)
datase$vagosbom_totalalojamentos <- round(datase$vagosbom_totalalojamentos,3)

# Verificar a existencia de valores nulos e remove-los

total_na_count <- sum(is.na(datase))
print(total_na_count)


datase <- datase[!is.na(datase$`valor_venda`), ]
print(sum(is.na(datase$`valor_venda`)))

# Criar um dataframe com as variaveis de interesse e valores unicos respetivos

datatype <- sapply(datase, class)
datatype_count <- table(datatype)
print(datatype_count)


col_of_interest <- c("vagosbom_totalalojamentos","vagosbom_totalvagos","valor_venda", 
                     "variacao_populacional","tipo_territorio","pressao_urbanistica",
                     "vagos_2011_per","vagos_2021_per")

missing_cols <- setdiff(col_of_interest, colnames(datase))
if (length(missing_cols) > 0) stop("Columns not found: ", paste(missing_cols, collapse = ", "))


column_info <- lapply(col_of_interest, function(col) {
  unique_count <- length(unique(datase[[col]]))
  data.frame(Column_Name = col, Data_Type = class(datase[[col]])[1], Unique_Count = unique_count)
})
column_info_df <- do.call(rbind, column_info)
print(column_info_df)



# Análise univariadas: variaveis continuas

## Percentagem de alojamentos vagos em bom estado

histogram <- ggplot(datase, aes(x = vagosbom_totalalojamentos)) + 
  geom_histogram(binwidth = 4, fill = "steelblue3", color = "black") +
  labs(title = NULL,
       x = "% de alojamentos", y = "Frequência") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

boxplot_plot <- ggplot(datase, aes(x = NULL, y = vagosbom_totalalojamentos)) +
  geom_boxplot(fill = "steelblue3") +
  labs(title = NULL, x = "", 
       y = "% de alojamentos" ) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(
  histogram, boxplot_plot,
  ncol = 2,
  top = textGrob(
    "Alojamentos vagos: proporção em bom estado",
    gp = gpar(fontsize = 14, fontface = "bold")
  )
)

mean_alojamentos <- mean(datase$vagosbom_totalalojamentos, na.rm = T)
sd_alojamentos <- sd(datase$vagosbom_totalalojamentos, na.rm = T)

vagos_stats <- data.frame(Statistic = c("Mean","Standard deviation"),
                          Value = round(c(mean_alojamentos,sd_alojamentos),3))

print(vagos_stats)


## Valor_venda

histogram2 <- ggplot(datase, aes(x = valor_venda)) +
  geom_histogram(binwidth = 180, fill = "lightgreen", color = "black") +
  labs(title = NULL, x = "€/m2", y = "Frequencia") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

boxplot_plot2 <- ggplot(datase, aes(x="", y = valor_venda)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = NULL, x = "" , y = "€/m2") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(
  histogram2, boxplot_plot2, 
  ncol = 2,
  top = textGrob("Valor de venda",
                 gp = gpar(fontsize = 14, fontface = "bold"))
)


median_venda <- median(datase$valor_venda, na.rm = T)
range_venda <- range(datase$valor_venda, na.rm = T)

venda_stats <- data.frame(Statistic = c("Median","Min","Max"),
                          Value = round(c(median_venda,range_venda[1],
                                          range_venda[2]),4))
print(venda_stats)

## taxa_variação populacional

histogram3 <- ggplot(datase, aes(x = variacao_populacional)) +
  geom_histogram(binwidth = 3, fill = "pink3", color = "black") +
  labs(title = NULL, x = "Taxa de variação", y = "Frequencia") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

boxplot_plot3 <- ggplot(datase, aes(x="", y = variacao_populacional)) +
  geom_boxplot(fill = "pink3") +
  labs(title = NULL, x = "" , y = "Taxa de variação") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(
  histogram3, boxplot_plot3, 
  ncol = 2,
  top = textGrob("Variação populacional 2011-2021",
                 gp = gpar(fontsize = 14, fontface = "bold"))
)


mean_variacaopop <- mean(datase$variacao_populacional)
sd_variacaopop <- sd(datase$variacao_populacional)

variaçaopop_stats <- data.frame(Statistic = c("Mean","Standard deviation"),
                             Value = round(c(mean_variacaopop,sd_variacaopop),4))
print(variaçaopop_stats)


## Percentagem de alojamentos vagos 2011 e 2021

datase <- datase %>%
  mutate(index = row_number())

datase_long <- datase %>%
  pivot_longer(
    cols = c(vagos_2011_per, vagos_2021_per),
    names_to = "year",
    values_to = "value"
  )

ggplot(datase_long, aes(x = index, y = value, color = year)) +
  geom_point(size = 2, alpha = 0.4) +  
  geom_smooth(se = FALSE, linewidth = 1.2) +  
  labs(
    title = "Evolution of Vagos (2011 vs 2021)",
    x = "Index",
    y = "Percentage",
    color = "Year"
  ) +
  theme_minimal() +
  scale_color_manual(
    values = c("vagos_2011_per" = "blue", "vagos_2021_per" = "red"),
    labels = c("2011", "2021")
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

## variação_vagos

histogram4 <- ggplot(datase, aes(x = variaçao_vagos)) +
  geom_histogram(binwidth = 3, fill = "khaki3", color = "black") +
  labs(title = NULL, x = "Percentagem de alojamentos vagos", y = "Frequencia") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

boxplot_plot4 <- ggplot(datase, aes(x="", y = variaçao_vagos)) +
  geom_boxplot(fill = "khaki3") +
  labs(title = NULL, x = "" , y = "Percentagem de alojamentos") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(
  histogram4, boxplot_plot4, 
  ncol = 2,
  top = textGrob("Alojamentos vagos em bom estado: variação",
                 gp = gpar(fontsize = 14, fontface = "bold"))
)


mean_variaçao2 <- mean(datase$variaçao_vagos, na.rm = T)
sd_variaçao2 <- sd(datase$variaçao_vagos)

variaçao_stats2 <- data.frame(Statistic = c("Mean","Standard deviation"),
                             Value = round(c(mean_variaçao2,sd_variaçao2),4))
print(variaçao_stats2)



# Análises Univariadas: variáveis categóricas

## tipo_territorio

freq_dist <- table(datase$tipo_territorio)
relative_freq <- prop.table(freq_dist)

freq_table <- data.frame(
  tipo_territorio = names(freq_dist),
  Frequency = as.integer(freq_dist),
  Relative_Frequency = round(as.numeric(relative_freq), 4))

print(freq_table)


ggplot(freq_table, aes(x = fct_reorder(tipo_territorio, Relative_Frequency, .desc = TRUE), 
                       y = Relative_Frequency)) +
  geom_bar(stat = "identity", fill = "coral2") +
  labs(title = "Distribuição do tipo de territorio",x = "Tipo de territorio",
    y = "Frequência Relativa") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(size = 10, hjust = 0.5))


ggplot(freq_table, aes(x = "", y = Relative_Frequency, fill = tipo_territorio)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribuição do tipo de território") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_text(aes(label = paste0(round(Relative_Frequency * 100, 1), "%")), 
            position = position_stack(vjust = 0.50),size=3)



## pressao_urbanistica

freq_dist2 <- table(datase$pressao_urbanistica)
relative_freq2 <- prop.table(freq_dist2)

freq_table2 <- data.frame(
  Pressao_urbanistica = names(freq_dist2), 
  Frequency = as.integer(freq_dist2),  
  Relative_Frequency = round(as.numeric(relative_freq2), 4)
)

print(freq_table2)

ggplot(freq_table2, aes(x = factor(Pressao_urbanistica), y = Relative_Frequency)) +
  geom_bar(stat = "identity", fill = "springgreen3") +
  labs(
    title = "Distribuição da Pressão urbanistica",
    x = "Verifica-se Pressão urbanistica?",
    y = "Relative Frequency"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(freq_table2, aes(x = "", y = Relative_Frequency, fill = Pressao_urbanistica)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Verifica-se Pressão urbanistica?") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_text(aes(label = paste0(round(Relative_Frequency * 100, 1), "%")), 
            position = position_stack(vjust = 0.50),size=3)



#Analises Bivariadas

#Variaveis continuas

#Scatterplot de percentagem de alojamentos vagos em bom estado vs variaçao_populacional

biv_plot1 <- ggplot(datase, aes(x = variacao_populacional, y = vagosbom_totalalojamentos)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "% Vagos bom estado vs Taxa de variação populacional",
       x = "Taxa de Variação Populacional",
       y = "% Alojamentos Vagos bom estado") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

# Scatterplot de percentagem de alojamentos vagos em bom estado vs valor_venda

biv_plot2 <- ggplot(datase, aes(x = valor_venda, y = vagosbom_totalalojamentos)) +
  geom_point(color = "purple", alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "% Vagos bom estado vs Valor de venda",
       x = "Valor de Venda",
       y = "% Alojamentos Vagos bom estado") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

# Scatterplot de percentagem de alojamentos vagos em bom estado vs variaçao_vagos

biv_plot3 <- ggplot(datase, aes(x = variaçao_vagos, y = vagosbom_totalalojamentos)) +
  geom_point(color = "darkgreen", alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "% Vagos bom estado vs Variação de alojamentos vagos",
       x = "Variação de alojamentos vagos",
       y = "% Alojamentos Vagos bom estado") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(
  biv_plot1, 
  grid::textGrob(" ", gp = grid::gpar(fontsize = 12)),
  biv_plot2,
  grid::textGrob(" ", gp = grid::gpar(fontsize = 12)),
  biv_plot3,
  ncol = 1,
  heights = c(10, 1, 10,1,10))

# Matriz de correlação

correlation_data <- datase[, c("vagosbom_totalalojamentos","vagosbom_totalvagos", 
                               "variacao_populacional", "valor_venda","variaçao_vagos")]


correlation_matrix <- round(cor(correlation_data, use = "complete.obs"), 2)


correlation_matrix_df <- as.data.frame(as.table(correlation_matrix))


correlation_matrix_df$fill_value <- correlation_matrix_df$Freq
diag_indices <- which(correlation_matrix_df$Var1 == correlation_matrix_df$Var2)
correlation_matrix_df$fill_value[diag_indices] <- NA
correlation_matrix_df$label <- as.character(correlation_matrix_df$Freq)
correlation_matrix_df$label[diag_indices] <- "--"

print(correlation_matrix)

# Matriz de correlação com cores (heatmap)

heatmap_plot <- ggplot(correlation_matrix_df, aes(x = Var1, y = Var2, fill = fill_value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = label), color = "black", size = 4) +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white", midpoint = 0, 
    limit = c(-1, 1), space = "Lab", name = "Pearson",
    na.value = "grey90"  
  ) +
  labs(title = "Correlation Matrix Heatmap", x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(hjust = 0.5)
  )

print(heatmap_plot)


#Variaveis Categoricas

# Percentagem alojamentos vagos em bom estado x Tipo de territorio

boxplot_territory <- ggplot(datase, aes(x = tipo_territorio, y = vagosbom_totalalojamentos
                                        , fill = tipo_territorio)) +
  geom_boxplot() +
  labs(
    title = "Distribuição de Alojamentos Vagos por tipo de território",
    x = "Tipo de Território",
    y = "Total Alojamentos Vagos",
    fill = "Tipo de Território"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )


territory_types <- unique(datase$tipo_territorio)
mean_alojamentos <- numeric(length(territory_types))
sd_alojamentos <- numeric(length(territory_types))
counts <- numeric(length(territory_types))

for(i in seq_along(territory_types)) {
  subset_data <- datase$total_vagosbom[datase$tipo_territorio == territory_types[i]]
  mean_alojamentos[i] <- mean(subset_data, na.rm = TRUE)
  sd_alojamentos[i] <- sd(subset_data, na.rm = TRUE)
  counts[i] <- length(subset_data)
}


mean_by_territory <- data.frame(
  tipo_territorio = territory_types,
  mean_alojamentos = mean_alojamentos,
  sd_alojamentos = sd_alojamentos,
  count = counts
)


barplot_territory <- ggplot(mean_by_territory, 
                            aes(x = tipo_territorio, y = mean_alojamentos, fill = tipo_territorio)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_alojamentos - sd_alojamentos,
                    ymax = mean_alojamentos + sd_alojamentos),
                width = 0.2) +
  labs(
    title = "Média de Alojamentos Vagos por tipo de território",
    x = "Tipo de Território",
    y = "Média Total Alojamentos Vagos",
    fill = "Tipo de Território"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

statistics_territory <- data.frame(
  tipo_territorio = territory_types,
  Mean = round(sapply(territory_types, function(x) 
    mean(datase$total_vagosbom[datase$tipo_territorio == x], na.rm = TRUE)), 2),
  Median = round(sapply(territory_types, function(x) 
    median(datase$total_vagosbom[datase$tipo_territorio == x], na.rm = TRUE)), 2),
  SD = round(sapply(territory_types, function(x) 
    sd(datase$total_vagosbom[datase$tipo_territorio == x], na.rm = TRUE)), 2),
  Min = round(sapply(territory_types, function(x) 
    min(datase$total_vagosbom[datase$tipo_territorio == x], na.rm = TRUE)), 2),
  Max = round(sapply(territory_types, function(x) 
    max(datase$total_vagosbom[datase$tipo_territorio == x], na.rm = TRUE)), 2),
  Count = sapply(territory_types, function(x) 
    sum(datase$tipo_territorio == x))
)


grid.arrange(boxplot_territory, barplot_territory, ncol = 2)
print(statistics_territory)



# Percentagem de alojamentos vagos em bom estado x Pressão urbanistica

boxplot_pressure <- ggplot(datase, aes(x = pressao_urbanistica, y = vagosbom_totalalojamentos, 
                                       fill = pressao_urbanistica)) +
  geom_boxplot() +
  labs(
    title = "Impacto da pressao urbanistica nos alojamentos vagos",
    x = "Verifica-se Pressão urbanistica?",
    y = "Total Alojamentos Vagos",
    fill = "Pressao urbanistica"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
    legend.position = "none"
  )


pressure_types <- unique(datase$pressao_urbanistica)
mean_alojamentos2 <- numeric(length(pressure_types))
sd_alojamentos2 <- numeric(length(pressure_types))
counts2 <- numeric(length(pressure_types))

for (i in seq_along(pressure_types)) {
  subset_data2 <- datase$total_vagosbom[datase$pressao_urbanistica == pressure_types[i]]
  mean_alojamentos2[i] <- mean(subset_data2, na.rm = TRUE)
  sd_alojamentos2[i] <- sd(subset_data2, na.rm = TRUE)
  counts2[i] <- length(subset_data2)
}


mean_by_pressure <- data.frame(
  pressure_types = pressure_types,
  mean_alojamentos2 = mean_alojamentos2,
  sd_alojamentos2 = sd_alojamentos2,
  count2 = counts2
)

barplot_pressure <- ggplot(mean_by_pressure, 
                           aes(x = pressure_types, y = mean_alojamentos2, fill = pressure_types)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_alojamentos2 - sd_alojamentos2,
                    ymax = mean_alojamentos2 + sd_alojamentos2),
                width = 0.2) +
  labs(
    title = "Impacto da pressao urbanistica na media de alojamentos",
    x = "Pressao urbanistica?",
    y = "Média Total Alojamentos Vagos",
    fill = "Pressao urbanistica"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "none"
  )

territory_types <- unique(datase$tipo_territorio)

statistics_territory <- data.frame(
  tipo_territorio = territory_types,
  Mean = round(sapply(territory_types, function(x) 
    mean(datase$total_vagosbom[datase$tipo_territorio == x], na.rm = TRUE)), 2),
  Median = round(sapply(territory_types, function(x) 
    median(datase$total_vagosbom[datase$tipo_territorio == x], na.rm = TRUE)), 2),
  SD = round(sapply(territory_types, function(x) 
    sd(datase$total_vagosbom[datase$tipo_territorio == x], na.rm = TRUE)), 2),
  Min = round(sapply(territory_types, function(x) 
    min(datase$total_vagosbom[datase$tipo_territorio == x], na.rm = TRUE)), 2),
  Max = round(sapply(territory_types, function(x) 
    max(datase$total_vagosbom[datase$tipo_territorio == x], na.rm = TRUE)), 2),
  Count = sapply(territory_types, function(x) 
    sum(datase$tipo_territorio == x))
)


grid.arrange(boxplot_pressure, barplot_pressure, ncol = 2)
print(statistics_territory)

#Correlação variaveis categoricas x alojamento

categorical_vars <- datase[, c("tipo_territorio", "pressao_urbanistica")]
continuous_var <- datase$vagosbom_totalalojamentos


categorical_encoded <- categorical_vars %>% mutate_all(as.factor) %>% mutate_all(as.numeric)
correlations <- sapply(categorical_encoded, function(cat_var) {
  cor(cat_var, continuous_var, method = "spearman")
})


cor_data <- data.frame(Variable = names(correlations), Correlation = correlations)
cor_data_melted <- melt(cor_data)


ggplot(cor_data_melted, aes(x = Variable, y = "% Alojamentos vagos em bom estado", fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limits = c(-1, 1)) +
  labs(
    title = "Spearman Correlation Heatmap",
    x = "Categorical Variables",
    fill = "Spearman"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  coord_fixed(ratio = 0.2) 


##Analises Multivariadas

plot_theme <- theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    plot.margin = margin(10, 10, 10, 10)
  )

breaks_venda <- c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500)
labels_venda <- c("0-500", "501-1000", "1001-1500", "1501-2000", "2001-2500", 
                  "2501-3000", "3001-3500", "3501-4000","4001-4500")

breaks_variation <- c(-20, -15, -9, -3, 3, 9, 15, 20, 25)
labels_variation <- c("-20 to -15", "-14 to -9", "-8 to -3", "-2 to 3", 
                      "4 to 9", "10 to 15", "16 to 20", "21 to 25")

breaks_vagos <- c(-10, -5, 0, 5, 10,15)
labels_vagos <- c("-10 to -5", "-5 to 0", "0 to 5", "5 to 10", "11 to 15")


datase <- datase %>%
  mutate(
    valor_venda_category = cut(valor_venda, 
                               breaks = breaks_venda, 
                               labels = labels_venda, 
                               right = TRUE, 
                               include.lowest = TRUE),
    
    variacao_category = cut(variacao_populacional, 
                            breaks = breaks_variation, 
                            labels = labels_variation, 
                            right = TRUE, 
                            include.lowest = TRUE),
    
    variaçao_vagos = as.numeric(as.character(variaçao_vagos)),
    
    variacaovagos_category = cut(variaçao_vagos, 
                                 breaks = breaks_vagos, 
                                 labels = labels_vagos, 
                                 right = TRUE, 
                                 include.lowest = TRUE),
    
    vagosbom_totalalojamentos_rescaled = vagosbom_totalalojamentos / 10
  )


create_mean_summary <- function(data, group_vars) {
  data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(mean_vagosbom = mean(vagosbom_totalalojamentos_rescaled, na.rm = TRUE))
}


create_plot <- function(data, x_var, fill_var, title) {
  ggplot(data, aes(x = .data[[x_var]], y = mean_vagosbom, fill = .data[[fill_var]])) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      title = title,
      x = paste(tools::toTitleCase(gsub("_", " ", x_var))),
      y = "Mean Rescaled Percentage (0-10)",
      fill = tools::toTitleCase(gsub("_", " ", fill_var))
    ) +
    plot_theme
}


# 1. Variação populacional por tipo de territorio

mean_data_1 <- create_mean_summary(datase, c("variacao_category", "tipo_territorio"))
mult1 <- create_plot(mean_data_1, "variacao_category", "tipo_territorio",
                     "Mean Percentage of Vacant Properties by Territory Type and Population Variation")
print(mult1)

# 2. Variação populacional por pressao urbanistica

mean_data_2 <- create_mean_summary(datase, c("variacao_category", "pressao_urbanistica"))
mult2 <- create_plot(mean_data_2, "variacao_category", "pressao_urbanistica",
                     "Mean Percentage of Vacant Properties by Urban Pressure and Population Variation")
print(mult2)

# 3. Valor de venda por pressao urbanistica

mean_data_3 <- create_mean_summary(datase, c("valor_venda_category", "pressao_urbanistica"))
mult3 <- create_plot(mean_data_3, "valor_venda_category", "pressao_urbanistica",
                     "Mean Percentage of Vacant Properties by Urban Pressure and Sales Value")
print(mult3)

# 4. Valor de venda por tipo de territorio

mean_data_4 <- create_mean_summary(datase, c("valor_venda_category", "tipo_territorio"))
mult4 <- create_plot(mean_data_4, "valor_venda_category", "tipo_territorio",
                     "Mean Percentage of Vacant Properties by Territory Type and Sales Value")
print(mult4)

# 5. Variação de alojamentos vagos por tipo de territorio

mean_data_5 <- create_mean_summary(datase, c("variacaovagos_category", "tipo_territorio"))
mult5 <- create_plot(mean_data_5, "variacaovagos_category", "tipo_territorio",
                     "Mean Percentage of Vacant Properties by Territory Type and Vacancy Variation")
print(mult5)

# 6. Variação de alojamentos vagos por pressao urbanistica

mean_data_6 <- create_mean_summary(datase, c("variacaovagos_category", "pressao_urbanistica"))
mult6 <- create_plot(mean_data_6, "variacaovagos_category", "pressao_urbanistica",
                     "Mean Percentage of Vacant Properties by Urban Pressure and Vacancy Variation")
print(mult6)                                             


## Modelos de regressão linear


# categorizar variáveis contínuas

datase$vagos_2021_cat<-as.factor(ifelse(datase$vagos_2021_per<10,"0",ifelse(
  datase$vagos_2021_per<16,"1","2"
)))

datase$vagos_2011_cat<-as.factor(ifelse(datase$vagos_2011_per<10,"0",ifelse(
  datase$vagos_2011_per<16,"1","2"
)))

datase$vagosbom_totalvagos_cat<-ifelse(datase$vagosbom_totalvagos<56.8,"0",ifelse(
  datase$vagosbom_totalvagos<65.73,"1","2"
))


# criar modelos

MOD1<-lm(vagosbom_totalalojamentos~vagos_2021_per+tipo_territorio+pressao_urbanistica+
           valor_venda+variacao_populacional+vagos_2011_per+variaçao_vagos,data=datase);summary(MOD1) 

#modelo com todos os dados relevantes -à exceção do vagosbom_totalvagos; sem interações
#nem tem significância estatística + suspeitas autocorrelaçao. Não é particularmente bom;

MOD2<-update(MOD1,.~.-vagos_2021_per-vagos_2011_per-valor_venda-variaçao_vagos+
               vagos_2021_cat*variaçao_vagos+vagos_2011_cat+log2(valor_venda));summary(MOD2)

#todos os dados anteriores, dois deles categorizados, um em escala logaritmica, uma interação
#pior significância estatística + autocorrelação confirmada. É bom do ponto de vista do domínio


MOD3<-update(MOD2,.~.-log2(valor_venda)-vagos_2021_cat*variaçao_vagos+
               vagos_2021_cat+vagosbom_totalvagos_cat*log2(valor_venda));summary(MOD3)

#mantem-se todas às variáveis, à exceção da variável variação vagos, muda a interação
#melhor significância de todos, mas tem autocorrelação e multicolinearidade. É o melhor até á data

MOD4<-update(MOD2,.~.-variacao_populacional-vagos_2011_cat+
               vagos_2011_cat*variacao_populacional+vagosbom_totalvagos);summary(MOD4)

#regressa ao modelo 2 para recuperar 
#--------------o logaritmo do valor de venda
#--------------a interação variação_vagos*vagos_2021_cat
#--------------a variação populacional, que passa a interagir com vagos_2011_cat
#mantem-se o vagosbom_totalvagos pela sua importância, mas passa para contínua
#significância muito boa, não tem autocorrelação nem multicolinearidade. É bom do ponto de vista do domínio

MOD5<-update(MOD4,.~.-tipo_territorio-log2(valor_venda)+
               tipo_territorio*valor_venda);summary(MOD5)

#constroi sobre o anterior
#põe a interagir duas variáveis já existentes:tipo de território e valor de venda
#a significância melhora sem criar problemas. Continua a ser bom do ponto de vista do domínio

MOD6<-update(MOD5,.~.-tipo_territorio*valor_venda-vagos_2021_cat*variaçao_vagos+
               valor_venda+variaçao_vagos+vagos_2021_cat+tipo_territorio);summary(MOD6)

#constroi sobre o anterior
#retira duas interações mas mantem todas essas variáveis
#retira o logaritmo do valor de venda
#continua a ser significante e sem problemas. Continua a ser bom do ponto de vista do dominio


# Avaliar cada modelo separadamente

RESULTADOS <- function(MODELO) {
  print(summary(MODELO))
  
  # BIC e AIC
  cat("\nBIC:", BIC(MODELO), "\n")
  cat("AIC:", AIC(MODELO), "\n")
  
  # Multicolinearidade
  library(car)
  print(vif(MODELO))
  
  # Teste de Durbin-Watson (autocorrelação)
  print(dwtest(MODELO))
  
  # Gráfico de resíduos (fitted)
  plot(MODELO$residuals,MODELO$fitted.values, main = "Resíduos do Modelo")
  
  # Gráfico de resíduos 2
  plot(MODELO$residuals, main= "Resíduos do Modelo2")
  
  # QQ-Plot
  qqnorm(MODELO$residuals)
  qqline(MODELO$residuals, col = "red")
}

RESULTADOS(MOD6)


# Compilar os valores de cada um dos modelos, para selecionar o melhor

modelos <- list(MOD1, MOD2, MOD3, MOD4, MOD5, MOD6)

R2_Ajustado <- numeric(length(modelos))
BIC_Valores <- numeric(length(modelos))
AIC_Valores <- numeric(length(modelos))
Nomes_Modelos <- character(length(modelos))  

for (i in seq_along(modelos)) {
  modelo <- modelos[[i]]
  
  R2_Ajustado[i] <- summary(modelo)$adj.r.squared
  BIC_Valores[i] <- BIC(modelo)
  AIC_Valores[i] <- AIC(modelo)
  Nomes_Modelos[i] <- paste("Modelo", i)
}

resultados_df <- data.frame(
  Modelo = Nomes_Modelos,
  R2_Ajustado = R2_Ajustado,
  BIC = BIC_Valores,
  AIC = AIC_Valores
)

print(resultados_df)

# Tendo em conta os critérios de seleção, o modelo escolhido foi o modelo 6


# alínea (c.i) for a continuous variable X1 and a categorical variable with more than
#two categories X2 included in the final model (or the initial model in case
#the final model does not have this type of variable):
#------i. interpret the corresponding raw and adjusted effects;

#X1 <- datase$variacao_populacional
#X2 <- datase$vagos_2011_cat

#X1 - RAW EFFECT
X1<-lm(vagosbom_totalalojamentos~variacao_populacional, data=datase); summary(X1)
#quando todas as variáveis são 0, cada aumento na unidade da variável é um decréscimo de -0.13980 nas unidades da variável resposta
#e um p-value de 3.3e-09

#X1 - EFEITO AJUSTADO
#com o efeito ajustado, o impacto na variável resposta é 0.0362195, e o p-value 0.05362, mantendo todas as outras fixas


#X2 -RAW EFFECT
X2<-lm(vagosbom_totalalojamentos~vagos_2011_cat, data=datase); summary(X2)
#quando todas as variáveis são 0, 
#-----------para a categoria de referência o a variação na resopsta é 6.2387, e o p-value < 2e-16
#-----------para primeira categoria a variação na resposta é 2.1772 mais do que na categoria de referência, e o p-value 7.23e-13
#-----------para a segunda categoria a variação na resposta é 4.9030 mais do que na categoria de referência, e o p-value 2e-16

#X2 - EFEITO AJUSTADO
#quando todas as variáveis são 0, e as variáveis categóricas estão na categoria de referência:
#-----------para a categoria de referência o a variação na resposta é -3.6483670, e o p-value < 2e-16
#-----------para primeira categoria a variação na resposta é 1.4823079   mais do que na categoria de referência, e o p-value 7.23e-13
#-----------para segunda categoria a variação na resposta é 3.1813719 mais do que na categoria de referência, e o p-value 7.23e-13



# alinea (c.ii). interpret the effect caused on the response by a change from the third
#category of X2 to the second, as well as 95% and 90% confidence intervals for this effect;

coefs <- coef(MOD6)
vcov_matrix <- vcov(MOD6)

estimate_diff <- coefs["vagos_2011_cat1"] - coefs["vagos_2011_cat2"]

se_diff <- sqrt(vcov_matrix["vagos_2011_cat1", "vagos_2011_cat1"] +
                  vcov_matrix["vagos_2011_cat2", "vagos_2011_cat2"] -
                  2 * vcov_matrix["vagos_2011_cat1", "vagos_2011_cat2"])

df <- nrow(datase) - length(coefs)

t_95 <- qt(0.975, df)
t_90 <- qt(0.95, df)

ci_95 <- c(estimate_diff - t_95 * se_diff, estimate_diff + t_95 * se_diff)
ci_90 <- c(estimate_diff - t_90 * se_diff, estimate_diff + t_90 * se_diff)

list(
  Estimate = estimate_diff,
  SE = se_diff,
  CI_95 = ci_95,# (-2.097208,-1.300901) 
  CI_90 = ci_90 # (-2.032862,-1.365246)
)


# alinea (c.iii). investigate the existence of a significant interaction between X1 and X2.

# a variável contínua, para a categoria de referência, tem um efeito quase significativo na variável resposta
####p-value: 0.05362
####impacto: 0.0362195

# mas este impacto é diferente em função da categoria:
# ----na categoria 1, temos um impacto de 0.0362195-0.0402316 na variável resposta, mantendo todas as outras variáveis fixas
# ---na categoria 2, temos um impacto de 0.0362195-0.0971813 na variável resposta, mantendo todas as outras variáveis fixas

# 2.Consider a transformation of the previously used response variable, separating
#values greater than or equal to its median from the lower values. Repeat the
#previous exercise assuming a logistic regression model with the transformed
#response variable.

median(datase$vagosbom_totalalojamentos)#8.019
datase$variavel_logistica<-as.numeric(ifelse(datase$vagosbom_totalalojamentos>=8.019,"1","0"))

M1<-glm(variavel_logistica ~ pressao_urbanistica + vagos_2011_cat + 
          variacao_populacional + vagosbom_totalvagos + valor_venda + 
          variaçao_vagos + vagos_2021_cat + tipo_territorio + 
          vagos_2011_cat*variacao_populacional, family="binomial",data = datase);summary(M1)

exp(1.194e-01)
#para o segundo exercício, mantemos o modelo anteriormente escolhido


#X1 - RAW EFFECT
X1l<-lm(variavel_logistica~variacao_populacional, data=datase); summary(X1l)
#quando todas as variáveis preditoras são 0, cada aumento de 1 unidade na variável 
# explicativa está associado a uma diminuição nos odds do evento de exp(-0.022930),com p-value = 4.23e-07.

#X1 - EFEITO AJUSTADO
## Com o efeito ajustado, cada aumento de 1 unidade na variável explicativa está associado 
# a uma diminuição nos odds do evento de exp(-0.022930), mantendo todas as outras variáveis constantes. O p-value é 4.23e-07.


#X2 -RAW EFFECT
X2l<-glm(variavel_logistica~vagos_2011_cat, family="binomial", data=datase); summary(X2l)
#quando todas as variáveis são 0, 
#-----------para a categoria de referência, os odds do evento ocorrer são exp(-1.7610), com significância estatística (p-value = 6.84e-08).
#-----------para a primeira categoria, os odds do evento são exp(1.9498) vezes maiores que na categoria de referência, com p-value = 6.59e-08.
#-----------para a segunda categoria, os odds do evento são exp(3.4139) vezes maiores que na categoria de referência, com p-value = 2.86e-12.
#X2 -EFEITO AJUSTADO
#quando todas as variáveis são 0, e as variáveis categóricas estão na categoria de referência:
#----------- para a categoria de referência, os odds do evento ocorrer são exp(-4.063e+01),com p-value = 8.51e-10.
#----------- para a primeira categoria, os odds do evento são exp(5.893e+00) vezes maiores do que na categoria de referência, com p-value = 1.45e-05.
#----------- para a segunda categoria, os odds do evento são exp(1.053e+01) vezes maiores do que na categoria de referência, com p-value = 4.16e-06.

#ii. interpret the effect caused on the response by a change from the third
#category of X2 to the second, as well as 95% and 90% confidence intervals for this effect;

coefs <- coef(M1)
vcov_matrix <- vcov(M1)

estimate_diff <- coefs["vagos_2011_cat1"] - coefs["vagos_2011_cat2"]

se_diff <- sqrt(vcov_matrix["vagos_2011_cat1", "vagos_2011_cat1"] +
                  vcov_matrix["vagos_2011_cat2", "vagos_2011_cat2"] -
                  2 * vcov_matrix["vagos_2011_cat1", "vagos_2011_cat2"])

df <- nrow(datase) - length(coefs)

t_95 <- qt(0.975, df)
t_90 <- qt(0.95, df)

ci_95 <- c(estimate_diff - t_95 * se_diff, estimate_diff + t_95 * se_diff)
ci_90 <- c(estimate_diff - t_90 * se_diff, estimate_diff + t_90 * se_diff)

list(
  Estimate = estimate_diff,
  SE = se_diff,
  CI_95 = ci_95,# (-7.164433,-2.107713) 
  CI_90 = ci_90 # (-6.755826,-2.516320)
)

#iii. investigate the existence of a significant interaction between X1 and X2.

# a variável contínua, para a categoria de referência, tem um efeito quase significativo na variável resposta
####p-value: 0.3173
####impacto: exp(-4.063e+01)

#mas este impacto é diferente em função da categoria:
#----na categoria 1, temos um impacto de exp(-4.063e+01-1.573e-01) na variável resposta, mantendo todas as outras variáveis fixas
#---na categoria 2, temos um impacto de exp(-4.063e+01-2.503e-01) na variável resposta, mantendo todas as outras variáveis fixas

