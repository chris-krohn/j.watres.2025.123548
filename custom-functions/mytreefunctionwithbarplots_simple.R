# create a tree from a phylofactor object and a phyloseq object
# point is to highlight the phylofactors, show a colored tree and abundances for each tip
# need 'treecols' vector.

function(ps.phylo,
         layout = "circular",
         branch.length = "none",
         offset.text = 5,
         pwidth = 0.05) {


  melt_simple <- psmelt(ps.phylo) %>%
    #filter(Abundance  120) %>%
    dplyr::select(OTU, val=Abundance) %>% unique() #unquote metadatacolumn

  # Plot the tree
  ## First Add groups to ASVs into the tree that help associate ASVs with their taxon
  ## Create a list to split otus by phyla, this will be used for colour-grouping the tree
  ## Extract a taxadataframe
  taxa.df.phylum <- phyloseq::tax_table(ps.phylo)@.Data %>%
    as.data.frame %>%
    rownames_to_column("OTUID") %>%
    dplyr::select(OTUID, Phylum) %>%
    column_to_rownames("OTUID")
  phyla.list <- split(rownames(taxa.df.phylum),
                      taxa.df.phylum$Phylum,
                      drop = TRUE)
  ### add list grouping to tree and call it Phylum
  ggtree_gps <- tidytree::groupOTU(phyloseq::phy_tree(ps.phylo),
                                 phyla.list, "Phylum")

  p <-  ggtree::ggtree(ggtree_gps,
                       aes(color=Phylum),
                       layout = layout,
                       branch.length = "none",
                       key_glyph = "rect",
                       open.angle = 15) +
    #xlim(-2.5, NA) +   # to shorten the tree tips
    theme(legend.position="right")

  # Add barplots of abundances at the end
  #p <- rotate_tree(p, -90)
  p <- p + ggnewscale::new_scale_fill() +
    geom_fruit(
      data=melt_simple,
      geom=geom_bar,
      mapping = aes(
        y=OTU,
        x=val,
        group=label,
        fill=Phylum),
      pwidth=0.38,
      orientation="y",
      stat="identity",
      show.legend = FALSE)

  return(p)
}

