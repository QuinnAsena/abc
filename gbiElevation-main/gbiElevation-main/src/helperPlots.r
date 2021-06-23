mtx.descr <- function(X)
{
  X.pa <- vegan::decostand(X, method = 'pa')
  sing <- sum(colSums(X.pa) == 1) 
  pr.zeros <- 1 - (sum(X > 0) / prod(dim(X)))
  dbi <- mean(scale(X, center = FALSE, scale =  apply(X, 2, max)))
  
  list(n.elem = prod(dim(X)),
       n = dim(X)[1],
       p = dim(X)[2],
       singletons = round(sing, 3), 
       prop.zero = round(pr.zeros, 3), 
       dbi = round(dbi, 3))
}

plot.mds.gg <- function(mds, txt.col = 'blue', txt.x = 0, txt.y = 0, clusters = NULL, labels = FALSE)
{
  require(tidyverse)
  
  xy <- data.frame(scores(mds)) %>%
    rownames_to_column(var = 'site')
  n <- dim(scores(mds))[1]
  
  stress.lbl <- paste('Stress = ', round(mds$stress,3))
  
  if(is.null(clusters))
  {  
    plot.obj <- ggplot(data = xy) + 
      geom_point(aes(x = NMDS1, y = NMDS2), alpha  = 1, size = 1) + 
      annotate(geom = 'text', x = txt.x, y = txt.y, label = stress.lbl, col = txt.col) +
      labs(x = 'NDMS 1', y = 'NMDS 2') +
      theme_minimal() +
      coord_equal() + 
      theme(legend.position = "none")
  } else 
  {    
    xy$cl <- as.factor(clusters)
    
    plot.obj <- ggplot(data = xy) + 
      geom_point(aes(x = NMDS1, y = NMDS2, col = cl, shape = cl), size = 3, alpha  = 1) + 
      annotate(geom = 'text', x = txt.x, y = txt.y, label = stress.lbl, col = txt.col) +
      labs(x = 'NDMS 1', y = 'NMDS 2') +
      theme_minimal() +
      coord_equal() 
  }
  
  if (labels == TRUE)
  {
    plot.obj <- plot.obj + geom_text(aes(x = NMDS1, y = NMDS2, label = site))
  }

  plot.obj
}

# Function to extract the elevation range that each spp occurs over
extract.limits <- function(sppsite, spp.list, elev)
{
  e.min <- function(x) { min(elev[which (x != 0)]) }  # assume pres-absence
  e.max <- function(x) { max(elev[which (x != 0)]) }  
  
  sppsite <- sppsite[, spp.list]
  sppsite <- sppsite[rowSums(sppsite) > 0, ]
  
  elev.rg <- cbind(apply(sppsite, 2, e.min), apply(sppsite, 2, e.max))
  elev.rg <- as.data.frame(elev.rg) %>%
    rownames_to_column(var = "spp") %>%
    rename("min" = V1, "max" = V2) %>%
    mutate(spp = str_to_title(spp))

  elev.rg$single <- ifelse(elev.rg$max == elev.rg$min, 1, 0)
    
  elev.rg.s <- dplyr::filter(elev.rg, single != 0)
  elev.rg$spp <- fct_reorder(elev.rg$spp, elev.rg$min, min)
  elev.rg.s$spp <- fct_reorder(elev.rg.s$spp, elev.rg.s$min, min)

  list(elev.lims = elev.rg, elev.lims.sgl = elev.rg.s)
}

# get.elevation.limits <- function(sppsite, spp.list, elev)
# {
#   sppsite <- sppsite[, spp.list]
#   sppsite <- sppsite[rowSums(sppsite) > 0, ]
#   
#   sppsite <- as.data.frame(sppsite) %>%
#     rownames_to_column(var = "site.elev") %>%
#     separate(col = "site.elev", into = c("site", "elev")) %>%
#     mutate(elev = as.numeric(elev)) %>%
#     arrange(elev)
# 
#   e.range <- extract.limits(sppsite, elev)
#   e.range
# }  