
colorFromTopLvl <- function(network,
                            toplvl,
                            colortoplvl,
                            lvl,
                            clustertype,
                            colorlvl,
                            shuffle = FALSE)
{
  require(colorspace)
  # lvl <- "lvl2"
  # toplvl <- "lvl1"
  # clustertype <- "cluster.type.lvl2"
  # colortoplvl <- "col.all.lvl1"
  # colorlvl <- "col.all.lvl2"
  # 
  network[, colorlvl] <- grey(.43)
  
  # To make the loop only on top levels for which lvl has major clusters
  network.majorclusters <- network[which(network[, clustertype] == "major"), ]
  
  col.tab <- NULL
  for(cur.lvl in unique(network.majorclusters[, toplvl]))
  {
    # Subset of table with current top level only
    tmpnet <- network[which(network[, toplvl] == cur.lvl), ]
    
    # Color of top level
    coltoplvl <- unique(tmpnet[, colortoplvl])
    
    # Major clusters for current level
    major.cl <- as.character(unique(tmpnet[which(tmpnet[, clustertype] == "major"), lvl]))
    
    # Number of colors necessary for current level
    nb.cols <- length(major.cl)
    
    # Creation of color palette
    col_df <- data.frame(cluster = major.cl,
                         color = lighten(coltoplvl, 
                                         amount = seq(-.5, .5, length = nb.cols)))
    if(shuffle)
    {
      col_df$color <- sample(col_df$color)
    }
    
    col.tab <- rbind(col.tab,
                     col_df)
    
    
  }
  
  network[which(network[, clustertype] == "major"), colorlvl] <- 
    col.tab$color[match(
      network[which(network[, clustertype] == "major"), lvl],
      col.tab$cluster
    )]
  
  return(network)
}
