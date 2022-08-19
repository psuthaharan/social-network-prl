# extract id and paranoia scores from all social network data
dat1 <- read.csv('C:\\grant\\paranoia_ref_per\\paranoiaRefPer\\network_data\\cleaning\\data1_new.csv')
dat2 <- read.csv('C:\\grant\\paranoia_ref_per\\paranoiaRefPer\\network_data\\cleaning\\data2_new.csv')
dat3 <- read.csv('C:\\grant\\paranoia_ref_per\\paranoiaRefPer\\network_data\\cleaning\\data3_new.csv')
dat4 <- read.csv('C:\\grant\\paranoia_ref_per\\paranoiaRefPer\\network_data\\cleaning\\data4_new.csv')

dat1 <- dat1[-c(1:2),]
dat1_paranoia <- dat1[,c(1311,959:966,975:984)]
colnames(dat1_paranoia) <- c("id",
                             "rgpts_ref1","rgpts_ref2","rgpts_ref3","rgpts_ref4","rgpts_ref5","rgpts_ref6","rgpts_ref7","rgpts_ref8",
                             "rgpts_per1","rgpts_per2","rgpts_per3","rgpts_per4","rgpts_per5","rgpts_per6","rgpts_per7","rgpts_per8","rgpts_per9","rgpts_per10")

dat2_paranoia <- as.data.frame(sapply(dat2[, grepl("gpts_", names(dat2))], as.numeric))
dat2_paranoia$workerId <- dat2$workerId
dat2_paranoia <- dat2_paranoia[,c(19,1:18)]
colnames(dat2_paranoia) <- c("id",
                             "rgpts_ref1","rgpts_ref2","rgpts_ref3","rgpts_ref4","rgpts_ref5","rgpts_ref6","rgpts_ref7","rgpts_ref8",
                             "rgpts_per1","rgpts_per2","rgpts_per3","rgpts_per4","rgpts_per5","rgpts_per6","rgpts_per7","rgpts_per8","rgpts_per9","rgpts_per10")

dat3_paranoia <- as.data.frame(sapply(dat3[, grepl("gpts_", names(dat3))], as.numeric))
dat3_paranoia$workerId <- dat3$workerId
dat3_paranoia <- dat3_paranoia[,c(19,1:18)]
colnames(dat3_paranoia) <- c("id",
                             "rgpts_ref1","rgpts_ref2","rgpts_ref3","rgpts_ref4","rgpts_ref5","rgpts_ref6","rgpts_ref7","rgpts_ref8",
                             "rgpts_per1","rgpts_per2","rgpts_per3","rgpts_per4","rgpts_per5","rgpts_per6","rgpts_per7","rgpts_per8","rgpts_per9","rgpts_per10")


dat4 <- dat4[-c(1:2),]
dat4_paranoia <- as.data.frame(sapply(dat4[, grepl("gpts_", names(dat4))], as.numeric))
dat4_paranoia$workerId <- dat4$workerId
dat4_paranoia <- dat4_paranoia[,c(19,1:18)]
colnames(dat4_paranoia) <- c("id",
                             "rgpts_ref1","rgpts_ref2","rgpts_ref3","rgpts_ref4","rgpts_ref5","rgpts_ref6","rgpts_ref7","rgpts_ref8",
                             "rgpts_per1","rgpts_per2","rgpts_per3","rgpts_per4","rgpts_per5","rgpts_per6","rgpts_per7","rgpts_per8","rgpts_per9","rgpts_per10")

# rbind data
dat_paranoia <- rbind(dat1_paranoia,dat2_paranoia,dat3_paranoia,dat4_paranoia)


# calculate scores
dat_paranoia$paranoia_tot_score <- rowMeans(as.data.frame(sapply(dat_paranoia[, grepl("rgpts", names(dat_paranoia))], as.numeric)), na.rm = TRUE)
dat_paranoia$paranoia_ref_score <- rowMeans(as.data.frame(sapply(dat_paranoia[, grepl("rgpts_ref", names(dat_paranoia))], as.numeric)), na.rm = TRUE)
dat_paranoia$paranoia_per_score <- rowMeans(as.data.frame(sapply(dat_paranoia[, grepl("rgpts_per", names(dat_paranoia))], as.numeric)), na.rm = TRUE)
dat_paranoia$paranoia_group <- ifelse(rowSums(as.data.frame(sapply(dat_paranoia[, grepl("rgpts_per", names(dat_paranoia))], as.numeric)), na.rm = TRUE) > 10, "high","low")
dat_paranoia$paranoia_ref_group <- ifelse(rowSums(as.data.frame(sapply(dat_paranoia[, grepl("rgpts_ref", names(dat_paranoia))], as.numeric)), na.rm = TRUE) > 15, "high","low")


dat_paranoia_subset <- dat_paranoia[,c("id","paranoia_tot_score","paranoia_ref_score","paranoia_per_score","paranoia_group")]


# merge with social network list
# dataset 4 - social network data
sna_dat <- read.csv("data/social_network_dat_merged.csv")
sna_dat <- sna_dat[,-c(3)]
sna_dat <- sna_dat[which(sna_dat$id %in% network_clustering_consp$id),]

dat_merged <- merge(sna_dat,dat_paranoia_subset,by="id")

dat_sna_merged <- merge(dat_merged,network_clustering_consp,by="id")

dat_sna_merged$net_size_group <- ifelse(dat_sna_merged$network_size <= mean(dat_sna_merged$network_size), "low","high")
dat_sna_merged$assumed_belief_group <- ifelse(dat_sna_merged$similarity_weightedSum > mean(dat_sna_merged$similarity_weightedSum),1,0)
dat_sna_merged$strong_ties_prop <- ifelse(dat_sna_merged$total.strongTies <= mean(dat_sna_merged$total.strongTies), "low","high")

dat_sna_merged$netsize_assumedbelief_group <- ifelse(dat_sna_merged$net_size_group == "low" & dat_sna_merged$assumed_belief_group == 1 & dat_sna_merged$net_size_group == "low",1,
                                                     ifelse(dat_sna_merged$net_size_group == "low" & dat_sna_merged$assumed_belief_group == 0 & dat_sna_merged$net_size_group == "low",2,
                                                            ifelse(dat_sna_merged$net_size_group == "high" & dat_sna_merged$assumed_belief_group == 1 & dat_sna_merged$net_size_group == "high",3,
                                                                   ifelse(dat_sna_merged$net_size_group == "high" & dat_sna_merged$assumed_belief_group == 0 & dat_sna_merged$net_size_group == "high",4,""))))

dat_sna_merged$self_other_prop <- (dat_sna_merged$paranoia_ref_score + 0.0001)/(dat_sna_merged$paranoia_per_score + 0.0001)


dat_sna_merged_plot <- dat_sna_merged[,c("id","paranoia_ref_score","paranoia_per_score","self_other_prop","netsize_assumedbelief_group")]
#dat_sna_merged_plot$new_group <- ifelse(dat_sna_merged_plot$netsize_assumedbelief_group != 2,1,2)

#dat_sna_merged_plot <- merged_dat1[,c("paranoia_ref_score","self_other_prop","netsize_assumedbelief_group")]

dat_sna_merged_subset <- dat_sna_merged_plot[which(dat_sna_merged_plot$netsize_assumedbelief_group == 2 | dat_sna_merged_plot$netsize_assumedbelief_group == 3 ),]


t.test(dat_sna_merged_subset$paranoia_ref_score ~ dat_sna_merged_subset$netsize_assumedbelief_group,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)


library(plotrix)
library(dplyr)

library(rstatix)
dat_sna_merged_plot_long_df <- dat_sna_merged_subset %>%
  gather(key = "subscale", value = "score", self_other_prop) %>%
  convert_as_factor(id, netsize_assumedbelief_group)

dat_sna_merged_plot_stats <- dat_sna_merged_subset  %>%
  dplyr::group_by(netsize_assumedbelief_group) %>%
  dplyr::summarise(score_mean = mean(paranoia_ref_score, na.rm = TRUE),
                   score_se = std.error(paranoia_ref_score, na.rm = TRUE))



p1 <- ggplot(dat_sna_merged_plot_stats, aes(x = netsize_assumedbelief_group, y = score_mean, fill = netsize_assumedbelief_group)) +
  geom_bar(stat = "identity", position = position_dodge(), width = .5, alpha=0.7) + #position = position_dodge()
  #geom_errorbar(aes(x=group, ymin=wsr_mean-wsr_se, ymax=wsr_mean+wsr_se),
  #              width=0.2, colour="black", alpha=0.9, size=1) + #position_dodge(0.5)
  
  geom_errorbar(aes(x=netsize_assumedbelief_group, ymin=score_mean-score_se, ymax=score_mean+score_se),
                width=0.2, colour="black", alpha=0.9, size=1, position = position_dodge(0.5)) +
  #geom_point(data=capr_pandemic, aes(x=phenotype,y=paranoia_score), alpha=0.3,position = position_jitterdodge()) +
  #facet_wrap(~cluster,scales = "free", labeller = label_parsed) +
  labs(x = "group", y = "mean referential paranoia") +
  scale_fill_manual(#name = "ref-per group",
    #labels = c("group 1" = "low ref-low per (n=694)",
    #           "group 2" = "low ref-high per (n=89)",
    #           "group 3" = "high ref-low per (n=13)",
    #           "group 4" = "high ref-high per (n=142)"),
    values = c("#6eb095", "#318f68")) +
  theme_Publication() + 
  theme(legend.position = "none",
        #panel.spacing.y = unit(2, "lines"),
        strip.text.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank()
        #       axis.text = element_text(size=20),
        #       axis.title.y = element_text(size=30),
        #       axis.title.x = element_text(size=20)
  )
p1


dat_sna_merged_plot_stats1 <- dat_sna_merged_subset  %>%
  dplyr::group_by(netsize_assumedbelief_group) %>%
  dplyr::summarise(score_mean = mean(paranoia_per_score, na.rm = TRUE),
                   score_se = std.error(paranoia_per_score, na.rm = TRUE))



p2 <- ggplot(dat_sna_merged_plot_stats1, aes(x = netsize_assumedbelief_group, y = score_mean, fill = netsize_assumedbelief_group)) +
  geom_bar(stat = "identity", position = position_dodge(), width = .5, alpha=0.7) + #position = position_dodge()
  #geom_errorbar(aes(x=group, ymin=wsr_mean-wsr_se, ymax=wsr_mean+wsr_se),
  #              width=0.2, colour="black", alpha=0.9, size=1) + #position_dodge(0.5)
  
  geom_errorbar(aes(x=netsize_assumedbelief_group, ymin=score_mean-score_se, ymax=score_mean+score_se),
                width=0.2, colour="black", alpha=0.9, size=1, position = position_dodge(0.5)) +
  #geom_point(data=capr_pandemic, aes(x=phenotype,y=paranoia_score), alpha=0.3,position = position_jitterdodge()) +
  #facet_wrap(~cluster,scales = "free", labeller = label_parsed) +
  labs(x = "group", y = "mean persecutory paranoia") +
  scale_fill_manual(#name = "ref-per group",
    #labels = c("group 1" = "low ref-low per (n=694)",
    #           "group 2" = "low ref-high per (n=89)",
    #           "group 3" = "high ref-low per (n=13)",
    #           "group 4" = "high ref-high per (n=142)"),
    values = c("#b06e8a", "#8f3158")) +
  theme_Publication() + 
  theme(legend.position = "none",
        #panel.spacing.y = unit(2, "lines"),
        strip.text.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank()
        #       axis.text = element_text(size=20),
        #       axis.title.y = element_text(size=30),
        #       axis.title.x = element_text(size=20)
  )
p2

library(Bolstad)
bayes.t.test(paranoia_ref_score ~ netsize_assumedbelief_group, data = dat_sna_merged_subset) # t[75] = 0.47, p = 0.643


tmp_dat <- merge(network_cluster_dat, assumedbelief_df, by=("id"))

library(BayesFactor) # install.packages("BayesFactor")

# compute Bayes Factor: mu03 difference between assumed belief group
ttestBF(dat_sna_merged_subset[which(dat_sna_merged_subset$netsize_assumedbelief_group == 2),]$paranoia_ref_score,
        dat_sna_merged_subset[which(dat_sna_merged_subset$netsize_assumedbelief_group == 3),]$paranoia_ref_score)

ttestBF(dat_sna_merged_subset[which(dat_sna_merged_subset$netsize_assumedbelief_group == 2),]$paranoia_per_score,
        dat_sna_merged_subset[which(dat_sna_merged_subset$netsize_assumedbelief_group == 3),]$paranoia_per_score)







