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

test_results %>%
  mutate(sig = ifelse(padj < 0.01, log2FoldChange, NA)) %>%
  ggplot(aes(x = log10(baseMean), y = log2FoldChange)) +
  geom_point(alpha = 0.1) +
  geom_point(aes(y = sig), color = "tomato", size = 1) +
  geom_hline(yintercept = 0, color = "dodgerblue") +
  facet_wrap(facets = vars(comparison))
