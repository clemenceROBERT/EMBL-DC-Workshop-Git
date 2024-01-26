library(tidyverse)

test_results <- read_csv("data/test_result.csv")
test_results

# gene column -> gene name
# baseMean column -> mnormalised expression level of a gene
# log2FoldChange column -> amount of change between 2 conditions
# lfcSE column -> standard error associated to log2FoldChange value
# stat column -> statistics value computed as log2FoldChange / lfcSE
# pvalue -> p-value associated with the change
# padj -> p-value corrected for multiple hypothesis testing (= adjusted p-value)
# comparison -> comparison group 

# MA plot
# CHALLENGE: generate an MA plot (baseMean vs log2FoldChange), 
# organise panels by comparison (time point). Hint: consider
# Hint: consider log-transform base mean
test_results %>%
  ggplot(aes(x = log10(baseMean), y = log2FoldChange)) +
  geom_point(alpha = 0.1) +
  facet_wrap(facets = vars(comparison))

ma_plot <- test_results %>%
  mutate(sig = ifelse(padj < 0.01, log2FoldChange, NA)) %>%
  ggplot(aes(x = log10(baseMean), y = log2FoldChange)) +
  geom_point(alpha = 0.1) +
  geom_point(aes(y = sig), color = "tomato", size = 1) +
  geom_hline(yintercept = 0, color = "dodgerblue") +
  facet_wrap(facets = vars(comparison))

(ma_plot | pca_plot)

# visualising of expression trends
# 1. Get candidate gene (aka padj < 0.01)
candidate_gene <- test_results %>%
  filter(padj < 0.01) %>%
  pull(gene) %>% # test_results[,"gene"] akatest_results$gene
  unique()

# 1a. Get the trans_cts table in long format
trans_cts_long <- trans_cts %>%
  pivot_longer(cols = wt_0_r1:mut_180_r3, names_to = "sample", 
               values_to = "cts") %>%
  full_join(sample_info, by = "sample")

# 2. filter trans_cts_long for candidate gene
# and compute mean expression value for each gene type and each genotype
trans_cts_mean <- trans_cts_long %>%
  filter(gene %in% candidate_gene) %>%
  group_by(gene, strain, minute) %>%
  summarize(mean_cts = mean(cts), nrep = n()) %>%
  ungroup()

# 3. Plot trends
trans_cts_mean %>%
  ggplot(aes(x = minute, y = mean_cts)) +
  geom_line(aes(group = gene), alpha = 0.3) +
  facet_grid(rows = vars(strain))

# Scaling data to improve visualisation
trans_cts_mean <- trans_cts_long %>%
  filter(gene %in% candidate_gene) %>%
  group_by(gene) %>%
  mutate(cts_scaled = (cts - mean(cts)) / sd(cts)) %>%
  group_by(gene, strain, minute) %>%
  summarise(mean_cts_scaled = mean(cts_scaled),
            nrep = n()) %>%
  ungroup()

trans_cts_mean %>%
  ggplot(aes(x = minute, y = mean_cts_scaled)) +
  geom_line(aes(group = gene), alpha = 0.3) +
  geom_hline(yintercept = 0, color = "brown",
             linetype = "dashed") +
  facet_grid(rows = vars(strain)) +
  scale_x_continuous(breaks = unique(trans_cts_mean$minute))

# Clustering
# 1. Create a matrix of counts
hclust_matrix <- trans_cts %>%
  select(-gene) %>%
  as.matrix()
rownames(hclust_matrix) <- trans_cts$gene
hclust_matrix <- hclust_matrix[candidate_gene,]

hclust_matrix <- hclust_matrix %>%
  t() %>%
  scale() %>%
  t()

# hierarchical clustering
gene_dist <- dist(hclust_matrix)
gene_hclust <- hclust(gene_dist, method = "complete")
plot(gene_hclust, labels = F)
abline(h = 10, col = "brown", lwd = 2)

# make clusters based on the number that I want
cutree(gene_hclust, k = 5)

gene_cluster <- cutree(gene_hclust, k = 5)%>%
  enframe() %>%
  rename(gene = name, cluster = value)

trans_cts_cluster <- trans_cts_mean %>%
  inner_join(gene_cluster, by = "gene")

trans_cts_cluster %>%
  ggplot(aes(x = minute, y = mean_cts_scaled)) +
  geom_line(aes(group = gene)) +
  facet_grid(cols = vars(cluster), rows = vars(strain))

# create heatmaps
library(ComplexHeatmap)

Heatmap(hclust_matrix, show_row_names = F)
