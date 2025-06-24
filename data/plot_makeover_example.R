library(tidyverse)
library(patchwork)
library(ggrepel)

umap_df <- read.csv('https://raw.githubusercontent.com/Sarah145/CRT-data-viz/refs/heads/main/data/RCC_umap_df.csv')

# Before...
p1 <- ggplot(umap_df, aes(x = umap_1, y = umap_2, colour = sampletype)) +
  geom_point() +
  labs(title = 'Tissue')

p2 <- ggplot(umap_df, aes(x = umap_1, y = umap_2, colour = cluster)) +
  geom_point() +
  labs(title = 'Cluster')

p3 <- ggplot(umap_df, aes(x = cluster, fill = sampletype)) +
  geom_bar(position = 'fill') +
  labs(title = 'Proportion') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

(p1 | p2 | p3) + plot_layout(widths = c(0.4,0.4,0.2))


#------------

# After...

# set up colour palettes
tissue_cols <- c(RCC = '#7D1E5A', adjNorm = '#FD2F21')
cluster_cols <- c("#5DA5DAFF", "#FAA43AFF", "#60BD68FF", "#F15854FF", "#B276B2FF", "#8D4B08FF", "#DECF3FFF", "#F17CB0FF", "#66E3D9FF", "#00FF7FFF")
names(cluster_cols) <- sort(unique(umap_df$cluster))

# set up umap arrows
umap_arrows <- ggplot(data.frame(x = 100, y = 100), aes(x=x,y=y)) +
  geom_point()+
  xlim(c(0,10)) + ylim(c(0,10)) +
  theme_classic() +
  ylab('UMAP2') + xlab('UMAP1') +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_line(arrow = arrow(angle = 15, length = unit(.5, 'cm'), type = 'closed'))
  )

# tissue umap
tissue_plot <- ggplot(umap_df, aes(x = umap_1, y = umap_2, col = sampletype)) +
  geom_point(size = 0.5, show.legend = T) +
  scale_colour_manual(values = tissue_cols, name = NULL) +
  labs(title = 'Tissue') +
  guides(colour = guide_legend(override.aes = list(size = 3))) +
  theme_void() +
  theme(legend.position = 'inside',
        legend.position.inside = c(0.14, 1),
        legend.direction = 'horizontal')

layout <- c(
  area(t=1, l=2, b = 11, r=11),
  area(t=10, l=1, b=12, r=2)
)

tissue_umap <- tissue_plot + umap_arrows + plot_layout(design = layout)

# cluster umap
cluster_umap <- ggplot(umap_df, aes(x = umap_1, y = umap_2, col = cluster)) +
  geom_point(size = 0.5, show.legend = F) +
  geom_label_repel(data = umap_df %>% group_by(cluster) %>% summarise(x = median(umap_1), y = median(umap_2)), 
                   aes(x=x, y=y, label = cluster), 
                   nudge_y = -1, show.legend = F, fill = alpha('#ffffff', 0.8), fontface = 'bold') +
  scale_colour_manual(values = cluster_cols) +
  labs(title = 'Cluster') +
  theme_void()

# cluster proportions
prop_plot <- ggplot(umap_df, aes(y = cluster, fill = sampletype)) +
  geom_bar(position = 'fill') +
  scale_fill_manual(values = tissue_cols) +
  scale_y_discrete(limits = rev(names(cluster_cols))) +
  scale_x_continuous(expand = c(0,0), position = 'top') +
  labs(title = 'Proportion', x = NULL, y = 'Cluster', fill = 'Tissue') +
  theme_classic() +
  theme(axis.text = element_text(colour = 'black'),
        legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5))


(tissue_umap | cluster_umap | prop_plot) + plot_layout(widths = c(0.4,0.4,0.2))
