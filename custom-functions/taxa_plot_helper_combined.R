# TAXA PLOT HELPER FUNCTION
# Combining total abundances with relative abundances

taxaplot_helper_combined <- function(psobject, taxalevel = "Phylum") {
## Total abundances plot
taxa_data <-  microbiome::aggregate_taxa(psobject, paste(taxalevel))
#  taxa_data <- microbiome::transform(taxa_data, "compositional")
taxa_data = prune_taxa(taxa_sums(taxa_data) > 0, taxa_data)
taxa_data <-  phyloseq::psmelt(taxa_data)

abun <- taxa_data %>%
  dplyr::group_by(PMA_treat, Sample) %>%
  dplyr::summarise(Total = sum(Abundance))

sp1 <- abun %>%
  ggbarplot( x= "PMA_treat", y = "Total",
             fill = "Grey", color = "Black",
             position = position_stack(),
             legend = "right",
             ylab = "Total Sequences",
             add = "mean_se",
             error.plot = "errorbar",
             label = FALSE,
             lab.nb.digits = 1,
             ylim = c(0, 79000)) +
  theme_bw() +
  #   facet_grid(cols = vars(PMA_treat),
  #             scales = "free_x",
  #             space = "free_x") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend( byrow = FALSE))   +
  # rotate_x_text(angle = 45) +
  ggtitle(paste(unique(sample_data(psobject)$Digester)))

## Relative abundances plot
  taxa_data <-  microbiome::aggregate_taxa(psobject, paste(taxalevel))
  taxa_data <- microbiome::transform(taxa_data, "compositional")
  taxa_data = prune_taxa(taxa_sums(taxa_data) > 0, taxa_data)
  taxa_data <-  phyloseq::psmelt(taxa_data)

  abun <- taxa_data %>%
    dplyr::group_by(PMA_treat, Phylum) %>%
    dplyr::summarise(Meanabu = mean(Abundance)) %>%
    dplyr::arrange(desc(Meanabu))
  order <- abun$Phylum %>% unique
  abun$Phylum <- factor(abun$Phylum, levels = order)

  sp2 <- abun %>%
    ggbarplot( x= "PMA_treat", y = "Meanabu",
               fill = paste(taxalevel),
               position = position_stack(),
               legend = "right",
               ylab = "Relative Abundances") +
    theme_bw() +
    # facet_grid(cols = vars(PMA_treat),
    #           scales = "free_x",
    #          space = "free_x") +
    theme(axis.title.x=element_blank(),
          #    axis.text.x=element_blank(),
          #     axis.ticks.x=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_fill_manual(values = cols, limits = order[1:22]) +
    guides(fill = guide_legend(byrow = FALSE,
                               ncol = 1))   +
    rotate_x_text(angle = 45)
  #  ggtitle(paste(unique(sample_data(psobject)$Digester)))

  g1 <- ggarrange(sp1, sp2,
            common.legend = TRUE,
            nrow = 2, legend = "right",
            heights = c(0.5,1),
            align = "v")
  g1
}
