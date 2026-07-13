#########################################################################################
# Plot serious search clustering graphics on a sample
# 
# 
#
# source("src/plot/sample_search_cluster_plot.R")
#########################################################################################



if(exists("res.HCPC_sample"))  res.HCPC_sample$desc.axes

#=====================================
#
# Clustering
#
#=====================================
#library(ggdendro)

#ggtree <- ggdendrogram(res.HCPC_sample$call$t$tree, rotate = FALSE, size = 0.5)

#cluster_color <- c("red", "green", "blue")

cluster_color <- c("black", "red", "green")


if(exists("res.HCPC_sample")) {
  
  pdf("out/pdf/serious_search/acp_clustering/hierarchica_tree.pdf")
  plot.HCPC(res.HCPC_sample,choice='tree',
            title='Hierarchical tree')
  dev.off()
  
  pdf("out/pdf/serious_search/acp_clustering/hierarchica_tree_pca.pdf")
  plot.HCPC(res.HCPC_sample,
            choice='3D.map',
            ind.names=FALSE,centers.plot=FALSE,angle=60,
            title='Hierarchical tree on the factorial plane 12')
  dev.off()
  
  
  #==========================
  # Plan 12
  #==========================
  
  pdf("out/pdf/serious_search/acp_clustering/cluster_plot_all.pdf")  
  cluster_plot_all <- fviz_pca_ind(
    res.PCA_sample,
    axes = c(1, 2),
    geom.ind = "point",
    habillage = analysis_df_transformed_sample$research_category,
    palette = cluster_color,
    #    addEllipses = TRUE,
    alpha.ind = 0.2,             
    pointshape = 1            
  )
  
  
  print(cluster_plot_all)
  dev.off()  
  
  pdf("out/pdf/serious_search/acp_clustering/cluster_plot_filt.pdf")    
  cluster_plot_filt <- fviz_pca_ind(
    res.PCA_sample,
    axes = c(1, 2),
    select.ind = list(cos2 = 0.5),
    geom.ind = "point",
    habillage = analysis_df_transformed_sample$research_category,
    palette = cluster_color,
    #    addEllipses = TRUE,
    alpha.ind = 0.2,             
    pointshape = 1            
  )
  
  print(cluster_plot_filt)
  dev.off()  
  
  
  
  cluster_plot_all_noleg_12 <- fviz_pca_ind(
    res.PCA_sample,
    axes = c(1, 2),
    geom.ind = "point",
    habillage = analysis_df_transformed_sample$research_category,
    palette = cluster_color,
    #  addEllipses = TRUE,
    alpha.ind = 0.2,
    pointshape = 1,
    title = "all observations"
  ) +
    theme(legend.position = "none")
  
  cluster_plot_filt_leg_cust_12 <- fviz_pca_ind(
    res.PCA_sample,
    axes = c(1, 2),
    select.ind = list(cos2 = 0.5),
    geom.ind = "point",
    habillage = analysis_df_transformed_sample$research_category,
    palette = cluster_color,
    #  addEllipses = TRUE,
    alpha.ind = 0.2,
    pointshape = 1,
    title = "well represented observations"
  ) +
    labs(
      color = "Research category",
      fill  = "Research category"   
    ) +
    theme(
      legend.text  = element_text(size = 11),
      legend.title = element_text(size = 12)
    )
  
  
  
  
  pdf("out/pdf/serious_search/acp_clustering/clust_both.pdf", width = 15)
  print(
    (cluster_plot_all_noleg_12 + cluster_plot_filt_leg_cust_12) +
      plot_layout(widths  = 15, heights = 0.5)
  )
  dev.off()
  
  
  
  
  
  #==========================
  # Plan 13
  #==========================
  
  
  
  pdf("out/pdf/serious_search/acp_clustering/hierarchica_tree_pca_13.pdf")
  plot.HCPC(res.HCPC_sample,
            choice='3D.map',
            ind.names=FALSE,centers.plot=FALSE,angle=60,
            axes=c(1,3),
            title='Hierarchical tree on the factorial plane 13')
  dev.off()
  
  
  
  
  
  
  
  pdf("out/pdf/serious_search/acp_clustering/cluster_plot_all_13.pdf")  
  cluster_plot_all_13 <- fviz_pca_ind(
    res.PCA_sample,
    axes = c(1, 3),
    geom.ind = "point",
    habillage = analysis_df_transformed_sample$research_category,
    palette = cluster_color,
    #    addEllipses = TRUE,
    alpha.ind = 0.2,             
    pointshape = 1            
  )
  
  
  print(cluster_plot_all_13)
  dev.off()  
  
  pdf("out/pdf/serious_search/acp_clustering/cluster_plot_filt_13.pdf")    
  cluster_plot_filt_13 <- fviz_pca_ind(
    res.PCA_sample,
    axes = c(1, 3),
    select.ind = list(cos2 = 0.5),
    geom.ind = "point",
    habillage = analysis_df_transformed_sample$research_category,
    palette = cluster_color,
    #    addEllipses = TRUE,
    alpha.ind = 0.2,             
    pointshape = 1            
  )
  
  print(cluster_plot_filt_13)
  dev.off()  
  
  
  cluster_plot_all_noleg_13 <- fviz_pca_ind(
    res.PCA_sample,
    axes = c(1, 3),
    geom.ind = "point",
    habillage = analysis_df_transformed_sample$research_category,
    palette = cluster_color,
    #  addEllipses = TRUE,
    alpha.ind = 0.2,
    pointshape = 1,
    title = "all observations"
  ) +
    theme(legend.position = "none")
  
  cluster_plot_filt_leg_cust_13 <- fviz_pca_ind(
    res.PCA_sample,
    axes = c(1, 3),
    select.ind = list(cos2 = 0.5),
    geom.ind = "point",
    habillage = analysis_df_transformed_sample$research_category,
    palette = cluster_color,
    #  addEllipses = TRUE,
    alpha.ind = 0.2,
    pointshape = 1,
    title = "well represented observations"
  ) +
    labs(
      color = "Research category",
      fill  = "Research category"   
    ) +
    theme(
      legend.text  = element_text(size = 11),
      legend.title = element_text(size = 12)
    )
  
  
  
  
  
  pdf("out/pdf/serious_search/acp_clustering/clust_both_13.pdf", width = 15)
  print(
    (cluster_plot_all_noleg_13 + cluster_plot_filt_leg_cust_13) +
      plot_layout(widths  = 15, heights = 0.5)
  )
  dev.off()
  
  
  
  
  
  
  
  
  #======================================
  # Clsutering quality
  #======================================
  
  
  sil <- cluster::silhouette(as.numeric(res.HCPC_sample$data.clust$clust), dist(res.PCA_sample$ind$coord))
  if (interactive()) plot(sil,
                          main = "Silhouette Widths by Cluster",
                          col = cluster_color)
  
  
  pdf("out/pdf/serious_search/acp_clustering/silhouette.pdf", width = 13)
  plot(sil,
       main = "Silhouette widths by cluster",
       col = cluster_color
  )
  dev.off()
  
  aggregate(sil[, 3], list(cluster = as.numeric(res.HCPC_sample$data.clust$clust)), mean)
  
}
