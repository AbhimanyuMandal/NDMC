library(ggplot2)
#install.packages("corrplot")
library(corrplot)
library(readxl)
library(reshape2)
#install.packages("pheatmap")
library(pheatmap)



data <- read_excel("All_counts_clus.xlsx")
data <- data.frame(data,row.names = 1)
data1 <- t(data)

cor_matrix <- cor(data1,method = "pearson")

#corrplot(cor_matrix, method = "color", tl.col = "black", tl.srt = 45)


pheatmap(cor_matrix, clustering_distance_rows = "euclidean", clustering_distance_cols = "euclidean")



melted_cor <- melt(cor_matrix)

ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "yellow", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab") +
  theme_minimal() +
  theme(axis.title.x = element_text(angle = 45, hjust = 1)) +
  labs(fill = "Correlation")



install.packages("pathview")
BiocManager::install("KEGGREST")

library(pathviewr)

data$avg_expression <- rowMeans(data[, 1:20]) # Adjust column range to your dataset


# Example: Mapping to a KEGG pathway
pathview(
  gene.data = data$avg_expression,
  gene.idtype = "entrez",        # Adjust if you use other types, e.g., "symbol"
  pathway.id = "hsa04110",       # Replace with your KEGG pathway ID
  species = "hsa",               # 'hsa' for human
  out.suffix = "pathview_demo"   # Output file name suffix
)





