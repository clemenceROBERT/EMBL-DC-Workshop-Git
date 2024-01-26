library(tidyverse)

trans_cts <- read_csv("data_rnaseq/counts_transformed.csv")
sample_info <- read_csv("data_rnaseq/sample_info.csv")

pca_matrix <- trans_cts %>%
  column_to_rownames("gene") %>%
  # converts our dataframe to a matrix
  as.matrix() %>%
  # t = to transpose operation
  t()

sample_pca <- prcomp(pca_matrix)
class(sample_pca)
str(sample_pca)
summary(sample_pca)

pca_matrix[1:10, 1:5]

# transform matrix in table --> tibble = easiest way to feed data into ggplot
as_tibble(pca_matrix)
as_tibble(pca_matrix, rownames = "sample")

pc_eigenvalues <- sample_pca$sdev^2

pc_eigenvalues <- tibble(PC = factor(1:length(pc_eigenvalues)),
                         variance = pc_eigenvalues) %>%
  mutate(pct = variance/sum(variance)*100) %>%
  mutate(pct_cum = cumsum(pct))

# pareto plot / chart (pareto = cumulative bar + line)
pc_eigenvalues %>%
  ggplot(aes(x = PC))+
  geom_col(aes(y = pct)) +
  geom_line(aes(y = pct_cum, group = 1)) +
  geom_point(aes(y = pct_cum)) +
  labs(x = "Principal component", y = "Fraction variance explained")

pc_scores <- sample_pca$x %>%
  as_tibble(rownames = "sample")

pc_scores %>%
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point()

pca_plot <- pc_scores %>%
  full_join(sample_info, by = "sample") %>%
  ggplot(aes(x = PC1, y = PC2, 
             color = factor(minute),
             shape = strain)) +
  geom_point()

pc_loadings <- sample_pca$rotation %>%
  as_tibble(rownames = "gene")

top_genes <- pc_loadings %>%
  select(gene, PC1, PC2) %>%
  pivot_longer(matches("PC"), names_to = "PC", values_to = "loading") %>%
  group_by(PC) %>%
  arrange(desc(abs(loading))) %>%
  slice(1:10) %>%
  pull(gene) %>%
  unique()

top_loadings <- pc_loadings %>%
  filter(gene %in% top_genes)

loadings_plot <-  ggplot(data = top_loadings) +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2),
                arrow = arrow(length = unit(0.1, "in")),
                color = "brown") +
  geom_text(aes(x = PC1, y = PC2, label = gene),
            nudge_y = 0.005, size = 3) +
  scale_x_continuous(expand = c(0.02, 0.02))
# the longer the segment, the more this gene has an influence on PC1 and PC2

library(patchwork)

(pca_plot | loadings_plot) +
  plot_annotation(tag_levels = "A")

library(ggfortify)
autoplot(sample_pca)
autoplot(sample_pca, data = sample_info %>% mutate(minute = as.factor(minute)),
         colour = "minute", shape = "strain")
(pca_plot | pca_plot | pca_plot) / loadings_plot

library(broom)

tidy(sample_pca, matrix = "eigenvalues")
tidy(sample_pca, matrix = "loadings")


