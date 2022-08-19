# clear environment
rm(list=ls())

setwd('C:/grant/paranoia_ref_per/paranoiaRefPer')

#source("C:/Pandemic_2020/revisions/code/theme_publication.R")
source("C:/grant/paranoia_ref_per/paranoiaRefPer/archive/socialnetworkPRL/code/theme_publication.R")

# read in network data with conspiracy self-reports
network_dat <- read.csv("C:/grant/paranoia_ref_per/paranoiaRefPer/network_data/network_data.csv")

# recode conspiracy columns
network_dat[, grepl("consp", names(network_dat))] <- ifelse(network_dat[, grepl("consp", names(network_dat))] == "Strongly disagree",1,
                                                            ifelse(network_dat[, grepl("consp", names(network_dat))] == "Disagree",2,
                                                                   ifelse(network_dat[, grepl("consp", names(network_dat))] == "Neither agree nor disagree",3,
                                                                          ifelse(network_dat[, grepl("consp", names(network_dat))] == "Agree",4,
                                                                                 ifelse(network_dat[, grepl("consp", names(network_dat))] == "Strongly agree",5,"")))))


network_dat$conspiracy_score <- rowMeans(as.data.frame(sapply(network_dat[, grepl("ego_consp", names(network_dat))], as.numeric)), na.rm = TRUE)


# calculate assumed belief with continuous alter attributes
network_homophily_dat <-  network_dat[,c(1,35:49,231:310)] # extract ties, ego conspiracy responses, alter conspiracy responses

# recode ego-alter ties
network_homophily_dat[, grepl("ties", names(network_homophily_dat))] <- ifelse(network_homophily_dat[, grepl("ties", names(network_homophily_dat))] == "especially close",1,
                                                                               ifelse(network_homophily_dat[, grepl("ties", names(network_homophily_dat))] == "not especially close",0,""))


# calculate mean conspiracy score for ego
network_homophily_dat$ego_consp_score <- rowMeans(as.data.frame(sapply(network_dat[, grepl("ego_consp", names(network_dat))], as.numeric)), na.rm = TRUE)


# calculate mean conspiracy score for alters
network_homophily_dat$alter1_consp_score <- rowMeans(as.data.frame(sapply(network_homophily_dat[, grepl("_alter1", names(network_homophily_dat))][,c(1,8,15,22,29)], as.numeric)), na.rm = TRUE)
network_homophily_dat$alter2_consp_score <- rowMeans(as.data.frame(sapply(network_homophily_dat[, grepl("_alter2", names(network_homophily_dat))], as.numeric)), na.rm = TRUE)
network_homophily_dat$alter3_consp_score <- rowMeans(as.data.frame(sapply(network_homophily_dat[, grepl("_alter3", names(network_homophily_dat))], as.numeric)), na.rm = TRUE)
network_homophily_dat$alter4_consp_score <- rowMeans(as.data.frame(sapply(network_homophily_dat[, grepl("_alter4", names(network_homophily_dat))], as.numeric)), na.rm = TRUE)
network_homophily_dat$alter5_consp_score <- rowMeans(as.data.frame(sapply(network_homophily_dat[, grepl("_alter5", names(network_homophily_dat))], as.numeric)), na.rm = TRUE) 
network_homophily_dat$alter6_consp_score <- rowMeans(as.data.frame(sapply(network_homophily_dat[, grepl("_alter6", names(network_homophily_dat))], as.numeric)), na.rm = TRUE)
network_homophily_dat$alter7_consp_score <- rowMeans(as.data.frame(sapply(network_homophily_dat[, grepl("_alter7", names(network_homophily_dat))], as.numeric)), na.rm = TRUE)
network_homophily_dat$alter8_consp_score <- rowMeans(as.data.frame(sapply(network_homophily_dat[, grepl("_alter8", names(network_homophily_dat))], as.numeric)), na.rm = TRUE)
network_homophily_dat$alter9_consp_score <- rowMeans(as.data.frame(sapply(network_homophily_dat[, grepl("_alter9", names(network_homophily_dat))], as.numeric)), na.rm = TRUE)
network_homophily_dat$alter10_consp_score <- rowMeans(as.data.frame(sapply(network_homophily_dat[, grepl("_alter10", names(network_homophily_dat))], as.numeric)), na.rm = TRUE)
network_homophily_dat$alter11_consp_score <- rowMeans(as.data.frame(sapply(network_homophily_dat[, grepl("_alter11", names(network_homophily_dat))], as.numeric)), na.rm = TRUE)
network_homophily_dat$alter12_consp_score <- rowMeans(as.data.frame(sapply(network_homophily_dat[, grepl("_alter12", names(network_homophily_dat))], as.numeric)), na.rm = TRUE)
network_homophily_dat$alter13_consp_score <- rowMeans(as.data.frame(sapply(network_homophily_dat[, grepl("_alter13", names(network_homophily_dat))], as.numeric)), na.rm = TRUE)
network_homophily_dat$alter14_consp_score <- rowMeans(as.data.frame(sapply(network_homophily_dat[, grepl("_alter14", names(network_homophily_dat))], as.numeric)), na.rm = TRUE)
network_homophily_dat$alter15_consp_score <- rowMeans(as.data.frame(sapply(network_homophily_dat[, grepl("_alter15", names(network_homophily_dat))], as.numeric)), na.rm = TRUE)

# clean data (remove individuals who forgot to answer for their alters)
network_homophily_dat$net_size <- rowSums(!is.na(as.data.frame(sapply(network_homophily_dat[, grepl("ties", names(network_homophily_dat))], as.numeric))))
network_homophily_dat$alter_complete_responses <- rowSums(!is.na(as.data.frame(sapply(network_homophily_dat[, grepl("consp_score", names(network_homophily_dat))][,2:16], as.numeric))))

network_homophily_dat <- network_homophily_dat[which(network_homophily_dat$net_size == network_homophily_dat$alter_complete_responses),]


## assumed belief with continuous alter attributes
ego_alter_ties <- list()
for (i in 1:nrow(network_homophily_dat)){
  tmp <- as.numeric(network_homophily_dat[, grepl("ties", names(network_homophily_dat))][i,])
  ego_alter_ties[[i]] <- tmp[!is.na(tmp)]
  
}


ego_responses <- list()
alter_responses <- list()
for (i in 1:nrow(network_homophily_dat)){
  ego_responses[[i]] <- rep(network_homophily_dat$ego_consp_score[i],length(ego_alter_ties[[i]]))
  
  tmp1 <- as.numeric(network_homophily_dat[, grepl("consp_score", names(network_homophily_dat))][,2:16][i,])
  alter_responses[[i]] <- tmp1[!is.na(tmp1)]
  
}

# calculate homophily
similarity_score <- list()
mean_similarity_score <- list()
weighted_sum_similarity <- list()

for (i in 1:nrow(network_homophily_dat)){
  similarity_score[[i]] <- alter_responses[[i]]/ego_responses[[i]]
  mean_similarity_score[[i]] <- mean(similarity_score[[i]])
  weighted_sum_similarity[[i]] <- sum(similarity_score[[i]]*ego_alter_ties[[i]])
}

network_homophily_dat$similarity_mean <- unlist(mean_similarity_score)
network_homophily_dat$similarity_weightedSum <- unlist(weighted_sum_similarity)

network_homophily_scores_dat <- network_homophily_dat[,c("id","similarity_mean","similarity_weightedSum")]

network_homophily_cleaned_dat <- merge(network_dat,network_homophily_scores_dat, by=c("id"))


master_pre <- read.csv('data//master_pre.csv')

network_clustering_dat <- master_pre[,c(1,306:314,316)]

network_clustering_consp <- merge(network_clustering_dat, network_homophily_cleaned_dat, by=c("id"))


# dataset 4 - social network data
dat4 <- read.csv("data/social_network_dat_merged.csv")
dat4 <- dat4[which(dat4$id %in% network_clustering_consp$id),]
dat4$dataset <- "social_network"

network_cluster_dat <- merge(network_clustering_consp,dat4, by=c("id"))



clustering_scaled <- scale(network_cluster_dat[,c(2:11)]) #-c(1:2,4:5,7:8) c*(2,5:11)


# Compute k-means with k=4
library(factoextra) #install.packages("factoextra")


## elbow method to find optimal number of clusters
fviz_nbclust(clustering_scaled, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

# set.seed(123)
# km.res <- kmeans(clustering_scaled,3,nstart = 25)
# fviz_cluster(km.res, geom = "point", data = network_cluster_dat[,c(2:11)], ellipse.type = "norm", shape = 16) +
#   geom_point(aes(color = network_cluster_dat$paranoia_group), alpha = 0.5)+
#   scale_fill_manual(values=c("#800000FF", "#5B8FA8FF", "#725663FF")) + 
#   scale_color_manual(values=c("#800000FF", "#5B8FA8FF", "#725663FF"))
  

  network_data <- network_cluster_dat[,c(1:11,323,324,325)]
  colnames(network_data) <- c("id","net_size","tot_edges","tot_weak_ties","tot_strong_ties",
                              "max_deg","mean_deg","density","centrality","constraint",
                              "kinship","assumed_belief","paranoia_score","paranoia_group")
  
  library(FactoMineR)
  library(tidyverse)
  pca    <- prcomp(network_data[,c(2,5,8:12)], retx = TRUE, scale. = TRUE) # scaled pca [exclude species col]
  pca_iris <- PCA(network_data[,c(2,5,8:12)], graph = FALSE)
  var_iris <- pca_iris$var$coord %>%
    as.data.frame() %>%
    rownames_to_column(var = "var") #%>%
    #separate(var, into = c("flower", "measure"), sep = "\\.") %>%
    #as_tibble()
  scores <- pca$x[, 1:3]                        # scores for first three PC's
  
  # k-means clustering [assume 3 clusters]
  km1     <- kmeans(scores, centers = 3, nstart = 5)
  ggdata <- data.frame(scores, Cluster = km1$cluster, Group = network_data$paranoia_group, id = network_data$id)
  # get some custom colors
  my_col_var <-   c("#4974a5","#36454F",
                    "#3e4e0c","black","#008000",
                    "#a2c9aa","#A054AD")#ggsci::pal_npg("nrc")(7)
  my_col_ell <- ggsci::pal_uchicago("dark")(3)
  
  var_iris$var <- factor(var_iris$var, levels = c("net_size","tot_strong_ties",
                                                  "density","centrality","constraint",
                                                  "kinship","assumed_belief"))
  
  var_iris$var <- c("network size","strong ties","density","centrality","constraint","kinship","assumed belief")
  # c("#000080","ff6c3a",
  #   "#232c3b","#6a7483","green",
  #   "#d18fae","#301934")
  
  library(ggnewscale) #install.packages("ggnewscale")
  library(ggrepel) #install.packages("ggrepel")
  
  ggdata_high <- ggdata[which(ggdata$Group == "high"),]
  
  set.seed(528)
  fig2a <- ggplot(ggdata, aes(x = PC1, y = PC2)) +
    geom_point(aes(x = PC1, y = PC2, shape = factor(Cluster), color = Group), size = 2) + #geom_label_repel(data = subset(ggdata, id=="A123Z5E5ULFEX7"),
                                                                                          #          aes(x=PC1,y=PC2,label=id), nudge_x = .5, nudge_y = 1) +
    scale_color_manual(values = c("#800000","#5B8FAF")) +
    stat_ellipse(aes(x = PC1, y = PC2, fill = factor(Cluster)),
                 geom = "polygon", level = 0.95, alpha = 0.4) +
    new_scale_color() +
    geom_segment(data = var_iris, aes(x = 0, xend = Dim.1 * 2, colour = var,
                                       y = 0, yend = Dim.2 * 2), size = 1.2, arrow = arrow(length = unit(0.03, "npc"))) +
    # geom_text(data = var_iris, aes(x = Dim.1 * 2, colour = var, label = var,
    #                                y = Dim.2 * 2), nudge_x = 0.2, nudge_y = 0.3, show.legend = FALSE) +
    scale_fill_manual(values = c("#4f3645","#36394f","#364f4d")) + #"#FFB547FF","#8A8B79FF","#725663FF"
    scale_color_manual(values = c(my_col_var)) + #c("#800000","#5B8FAF")
    #scale_color_manual(values = c("#800000","#5B8FAF")) +
    labs(fill = "Cluster",
         shape = "Cluster",
         colour = "Loadings") +
    theme_Publication() + 
    theme(#legend.position = "top",
          panel.spacing.x = unit(2, "lines"),
          panel.spacing.y = unit(3, "lines"),
          strip.text.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          #axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank()
          #       axis.text = element_text(size=20),
          #       axis.title.y = element_text(size=30),
          #       axis.title.x = element_text(size=20)
    ) 
  fig2a

  
  network_clustering_data <- cbind(network_data, cluster=km1$cluster)
  # plot distribution of features per cluster
  library(rstatix)
  network_data_long_df <- network_clustering_data %>%
    gather(key = "net_features", value = "score", net_size, tot_strong_ties,density,
           centrality,constraint,kinship,assumed_belief,paranoia_score) %>%
    convert_as_factor(id,cluster)
  
  
  library(plotrix)
  library(dplyr)
  
  network_data_stats <- network_data_long_df %>%
    dplyr::group_by(net_features, cluster) %>%
    dplyr::summarise(param_mean = mean(score, na.rm = TRUE),
                     param_se = std.error(score, na.rm = TRUE))
  
  
  
  
  
  # mean task behavior and belief-updating across groups
  library(ggplot2)
  p3 <- ggplot(network_data_stats, aes(x = net_features, y = param_mean, fill = cluster)) +
    geom_bar(stat = "identity", position = position_dodge(), width = .5, alpha=0.7) + #position = position_dodge()
    #geom_errorbar(aes(x=group, ymin=wsr_mean-wsr_se, ymax=wsr_mean+wsr_se),
    #              width=0.2, colour="black", alpha=0.9, size=1) + #position_dodge(0.5)
    
    geom_errorbar(aes(x=net_features, ymin=param_mean-param_se, ymax=param_mean+param_se),
                  width=0.2, colour="black", alpha=0.9, size=1, position = position_dodge(0.5)) +
    #geom_point(data=capr_pandemic, aes(x=phenotype,y=paranoia_score), alpha=0.3,position = position_jitterdodge()) +
    facet_wrap(~net_features,scales = "free", labeller = label_parsed, nrow = 2, ncol = 4) +
    labs(x = "features", y = "mean") +
    # scale_x_discrete(labels=c("kappa" = expression(kappa),
    #                           "mu[2]^0" = expression(mu[2]^0),
    #                           "mu[3]^0" = expression(mu[3]^0),
    #                           "omega[2]" = expression(omega[2]),
    #                           "omega[3]" = expression(omega[3]),
    #                           "WSR-switch" = "WSR",
    #                           "paranoia" = "paranoia")) +
    # scale_x_discrete(labels=c("mu03_mean" = expression(mu[3]^0),
    #                           "omega2_mean" = expression(omega[2]))) +
    #theme_bw()
    #scale_color_manual(values = c("#f3c483", "#5c1a33","blue"))+
    scale_fill_manual(#name = "ref-per group",
      #labels = c("group 1" = "low ref-low per (n=694)",
      #           "group 2" = "low ref-high per (n=89)",
      #           "group 3" = "high ref-low per (n=13)",
      #           "group 4" = "high ref-high per (n=142)"),
      values = c("#4f3645","#36394f","#364f4d")) + #"blue","#69be28","#ff8849","#4d2f92","#1b047c"
    #scale_colour_Publication()+
    theme_Publication() + 
    theme(legend.position = "none",
          panel.spacing.x = unit(2, "lines"),
          panel.spacing.y = unit(3, "lines"),
          strip.text.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          #axis.text.y = element_blank(),
          axis.title.y = element_blank()
          #       axis.text = element_text(size=20),
          #       axis.title.y = element_text(size=30),
          #       axis.title.x = element_text(size=20)
    ) 

  p3 

  
  
  
  
  
  
  
  
  
  
  
















