#PCA FIGURE FUNCTION to reduce code load in script
# Requires a 'cols' and shapes vector in correct number

pca_helper <- function(psobject,color = NULL, shape = NULL, title = NULL) {
  #ordination
  ordination <- phyloseq::ordinate(psobject , "RDA") # using the centred-log transformed abundances

  #plotting ordination
  PCAplot <- phyloseq::plot_ordination(psobject,
                                        ordination ,
                                        color = {{ color }},
                                        shape = {{ shape }},
                                        # option to add shapes to data symbols
                                        title = {{ title }} ) +
    #geom_point(aes(size = Shannon), alpha = 0.3) +    # shape size reflective of Pielou
    annotate(geom = 'text', label = paste("PCA, Aitchison distances"),
             x = 0, y = -Inf, hjust = -0.05, vjust = -1, size = 3)  +
       guides(color = guide_legend(override.aes = list(size = 3)),
           shape = guide_legend(override.aes = list(size = 3), order = 1))
  PCAplot
}
