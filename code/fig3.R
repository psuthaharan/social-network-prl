# clear environment
rm(list=ls())

setwd('C:/grant/paranoia_ref_per/paranoiaRefPer')

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

# # compile sabotage scores
# net_dat1 <- read.csv("C:\\grant\\paranoia_ref_per\\paranoiaRefPer\\network_data\\cleaning\\data1_new.csv")
# net_dat1 <- net_dat1[-c(1:2),]
# net_dat1_sabotage <- net_dat1[,c("Q2.1","Q12.5_1")]
# colnames(net_dat1_sabotage) <- c("mturk_id","avatar_sabotage")
# 
# net_dat2 <- read.csv("C:\\grant\\paranoia_ref_per\\paranoiaRefPer\\network_data\\cleaning\\data2_new.csv")
# net_dat2_sabotage <- net_dat2[,c("mturk_id","avatar_sabotage")]
# 
# 
# net_dat3 <- read.csv("C:\\grant\\paranoia_ref_per\\paranoiaRefPer\\network_data\\cleaning\\data3_new.csv")
# net_dat3_sabotage <- net_dat3[,c("mturk_id","avatar_sabotage")]
# 
# 
# net_dat4 <- read.csv("C:\\grant\\paranoia_ref_per\\paranoiaRefPer\\network_data\\cleaning\\data4_new.csv")
# net_dat4 <- net_dat4[-c(1:2),]
# net_dat4_sabotage <- net_dat4[,c("mturk_id","avatar_sabotage")]
# 
# net_dat_sabotage <- rbind(net_dat1_sabotage,net_dat2_sabotage,net_dat3_sabotage,net_dat4_sabotage)
# colnames(net_dat_sabotage) <- c("id","sabotage_belief")

cluster_network_df <- network_clustering_consp[,c(1:11,321:323)]
names(cluster_network_df) <- c("id","net_size","tot_edges","tot_weak_ties","tot_strong_ties","max_deg","mean_deg","density_val",
                               "deg_centrality_score","constraint","kinship","conspiracy_score","similarity_mean","similarity_weightedSum")

cluster_network_paranoia_df <- merge(cluster_network_df, dat4, by=c("id"))


anxiety_depression_dat <- read.csv('C:\\grant\\paranoia_ref_per\\paranoiaRefPer\\data\\anxiety_depression.csv')

# recoding anxiety
anxiety_depression_dat[,2:22] <- ifelse(anxiety_depression_dat[,2:22] == "Not at all" , 0,
                                        ifelse(anxiety_depression_dat[,2:22] == "Mildly, but it didn't bother me much", 1,
                                               ifelse(anxiety_depression_dat[,2:22] == "Moderately, it wasn't pleasant at times", 2,
                                                      ifelse(anxiety_depression_dat[,2:22] == "Severely, it bothered me a lot", 3,''))))
# recoding depression
anxiety_depression_dat[,23] <- ifelse(anxiety_depression_dat[,23] == "0 - I do not feel sad" , 0,
                                      ifelse(anxiety_depression_dat[,23] == "1 - I feel sad much of the time.", 1,
                                             ifelse(anxiety_depression_dat[,23] == "2 - I am sad all the time.", 2,
                                                    ifelse(anxiety_depression_dat[,23] == "3 - I am so sad or unhappy that I can't stand it.", 3,''))))

anxiety_depression_dat[,24] <- ifelse(anxiety_depression_dat[,24] == "0 - I am not discouraged about my future." , 0,
                                      ifelse(anxiety_depression_dat[,24] == "1 - I feel more discouraged about my future than I used to be.", 1,
                                             ifelse(anxiety_depression_dat[,24] == "2 - I do not expect things to work out for me.", 2,
                                                    ifelse(anxiety_depression_dat[,24] == "3 - I feel my future is hopeless and will only get worse.", 3,''))))

anxiety_depression_dat[,25] <- ifelse(anxiety_depression_dat[,25] == "0 - I do not feel like a failure." , 0,
                                      ifelse(anxiety_depression_dat[,25] == "1 - I have failed more than I should have.", 1,
                                             ifelse(anxiety_depression_dat[,25] == "2 - As I look back, I see a lot of failures.", 2,
                                                    ifelse(anxiety_depression_dat[,25] == "3 - I feel I am a total failure as a person.", 3,''))))

anxiety_depression_dat[,26] <- ifelse(anxiety_depression_dat[,26] == "0 - I get as much pleasure as I ever did from the things I enjoy." , 0,
                                      ifelse(anxiety_depression_dat[,26] == "1 - I don't enjoy things as much as I used to.", 1,
                                             ifelse(anxiety_depression_dat[,26] == "2 - I get very little pleasure from the things I used to enjoy.", 2,
                                                    ifelse(anxiety_depression_dat[,26] == "3 - I can't get any pleasure from the things I used to enjoy.", 3,''))))

anxiety_depression_dat[,27] <- ifelse(anxiety_depression_dat[,27] == "0 - I don't feel particularly guilty." , 0,
                                      ifelse(anxiety_depression_dat[,27] == "1 - I feel guilty over many things I have done or should have done.", 1,
                                             ifelse(anxiety_depression_dat[,27] == "2 - I feel quite guilty most of the time.", 2,
                                                    ifelse(anxiety_depression_dat[,27] == "3 - I feel guilty all of the time.", 3,''))))

anxiety_depression_dat[,28]<- ifelse(anxiety_depression_dat[,28] == "0 - I don't feel I am being punished." , 0,
                                     ifelse(anxiety_depression_dat[,28] == "1 - I feel I may be punished", 1,
                                            ifelse(anxiety_depression_dat[,28] == "2 - I expect to be punished.", 2,
                                                   ifelse(anxiety_depression_dat[,28] == "3 - I feel I am being punished.", 3,''))))

anxiety_depression_dat[,29] <- ifelse(anxiety_depression_dat[,29] == "0 - I feel the same about myself as ever." , 0,
                                      ifelse(anxiety_depression_dat[,29] == "1 - I have lost confidence in myself.", 1,
                                             ifelse(anxiety_depression_dat[,29] == "2 - I am disappointed in myself.", 2,
                                                    ifelse(anxiety_depression_dat[,29] == "3 - I dislike myself.", 3,''))))

anxiety_depression_dat[,30] <- ifelse(anxiety_depression_dat[,30] == "0 - I don't criticize or blame myself more than usual." , 0,
                                      ifelse(anxiety_depression_dat[,30] == "1 - I am more critical of myself than I used to be.", 1,
                                             ifelse(anxiety_depression_dat[,30] == "2 - I criticize myself for all of my faults.", 2,
                                                    ifelse(anxiety_depression_dat[,30] == "3 - I blame myself for everything bad that happens.", 3,''))))

anxiety_depression_dat[,32] <- ifelse(anxiety_depression_dat[,32] == "0 - I don't cry more than I used to." , 0,
                                      ifelse(anxiety_depression_dat[,32] == "1 - I cry more than I used to.", 1,
                                             ifelse(anxiety_depression_dat[,32] == "2 - I cry over every little thing.", 2,
                                                    ifelse(anxiety_depression_dat[,32] == "3 - I feel like crying, but I can't.", 3,''))))

anxiety_depression_dat[,33] <- ifelse(anxiety_depression_dat[,33] == "0 - I feel no more restless or wound up than usual." , 0,
                                      ifelse(anxiety_depression_dat[,33] == "1 - I feel more restless or wound up than usual", 1,
                                             ifelse(anxiety_depression_dat[,33] == "2 - I am so restless or agitated that it's hard to stay still.", 2,
                                                    ifelse(anxiety_depression_dat[,33] == "3 - I am so restless or agitated that I have to keep moving or doing something.", 3,''))))

anxiety_depression_dat[,34] <- ifelse(anxiety_depression_dat[,34] == "0 - I have not lost interest in other people or activities." , 0,
                                      ifelse(anxiety_depression_dat[,34] == "1 - I am less interested in other people or things than before.", 1,
                                             ifelse(anxiety_depression_dat[,34] == "2 - I have lost most of my interest in other people or things.", 2,
                                                    ifelse(anxiety_depression_dat[,34] == "3 - It is hard to get interested in anything.", 3,''))))

anxiety_depression_dat[,35] <- ifelse(anxiety_depression_dat[,35] == "0 - I make decisions about as well as ever" , 0,
                                      ifelse(anxiety_depression_dat[,35] == "1 - I find it more difficult to make decisions than usual.", 1,
                                             ifelse(anxiety_depression_dat[,35] == "2 - I have much greater difficulty in making decisions than I used to.", 2,
                                                    ifelse(anxiety_depression_dat[,35] == "3 - I have trouble making any decisions.", 3,''))))

anxiety_depression_dat[,36] <- ifelse(anxiety_depression_dat[,36] == "0 - I do not feel I am worthless." , 0,
                                      ifelse(anxiety_depression_dat[,36] == "1 - I don't consider myself as worthwhile and useful as I used to.", 1,
                                             ifelse(anxiety_depression_dat[,36] == "2 - I feel more worthless as compared to other people.", 2,
                                                    ifelse(anxiety_depression_dat[,36] == "3 - I feel utterly worthless.", 3,''))))

anxiety_depression_dat[,37] <- ifelse(anxiety_depression_dat[,37] == "0 - I have as much energy as ever." , 0,
                                      ifelse(anxiety_depression_dat[,37] == "1 - I have less energy than I used to have.", 1,
                                             ifelse(anxiety_depression_dat[,37] == "2 - I don't have enough energy to do very much.", 2,
                                                    ifelse(anxiety_depression_dat[,37] == "3 - I don't have enough energy to do anything.", 3,''))))

anxiety_depression_dat[,38] <- ifelse(anxiety_depression_dat[,38] == "0 - I have not experienced any change change in my sleeping pattern." , 0,
                                      ifelse(anxiety_depression_dat[,38] == "1a - I sleep somewhat more than usual" , 1,
                                             ifelse(anxiety_depression_dat[,38] == "1b - I sleep somewhat less than usual", 1,
                                                    ifelse(anxiety_depression_dat[,38] == "2a - I sleep a lot more than usual" , 2,
                                                           ifelse(anxiety_depression_dat[,38] == "2b - I sleep a lot less than usual", 2,
                                                                  ifelse(anxiety_depression_dat[,38] == "3a - I sleep most of the day" , 3,
                                                                         ifelse(anxiety_depression_dat[,38] == "3b - I wake up 1-2 hours early and can't get back to sleep.", 3,'')))))))

anxiety_depression_dat[,39] <- ifelse(anxiety_depression_dat[,39] == "0 - I am no more irritable than usual" , 0,
                                      ifelse(anxiety_depression_dat[,39] == "1 - I am more irritable than usual", 1,
                                             ifelse(anxiety_depression_dat[,39] == "2 - I am much more irritable than usual", 2,
                                                    ifelse(anxiety_depression_dat[,39] == "3 - I am irritable all the time", 3,''))))

anxiety_depression_dat[,40] <- ifelse(anxiety_depression_dat[,40] == "0 - I have not experienced any change in appetite" , 0,
                                      ifelse(anxiety_depression_dat[,40] == "1a - My appetite is somewhat less than usual.", 1,
                                             ifelse(anxiety_depression_dat[,40] == "1b - My appetite is somewhat greater than usual" , 1,
                                                    ifelse(anxiety_depression_dat[,40] == "2a - My appetite is much less than before." , 2,
                                                           ifelse(anxiety_depression_dat[,40] == "2b - My appetite is much greater than usual.", 2,
                                                                  ifelse(anxiety_depression_dat[,40] == "3a - I have no appetite at all." , 3,
                                                                         ifelse(anxiety_depression_dat[,40] == "3b - I crave food all the time.", 3,'')))))))

anxiety_depression_dat[,41] <- ifelse(anxiety_depression_dat[,41] == "0 - I can concentrate as well as ever." , 0,
                                      ifelse(anxiety_depression_dat[,41] == "1 - I can't concentrate as well as usual.", 1,
                                             ifelse(anxiety_depression_dat[,41] == "2 - It's hard to keep my mind on anything for very long.", 2,
                                                    ifelse(anxiety_depression_dat[,41] == "3 - I find I can't concentrate on anything.", 3,''))))

anxiety_depression_dat[,42] <- ifelse(anxiety_depression_dat[,42] == "0 - I am no more tired or fatigued than usual." , 0,
                                      ifelse(anxiety_depression_dat[,42] == "1 - I get more tired or fatigued more easily than usual.", 1,
                                             ifelse(anxiety_depression_dat[,42] == "2 - I am too tired or fatigued to do a lot of the things I used to do.", 2,
                                                    ifelse(anxiety_depression_dat[,42]== "3 - I am too tired or fatigued to do most of the things I used to do.", 3,''))))

anxiety_depression_dat[,43] <- ifelse(anxiety_depression_dat[,43] == "0 - I have not noticed any recent change in my interest in sex." , 0,
                                      ifelse(anxiety_depression_dat[,43] == "1 - I am less interested in sex than I used to be.", 1,
                                             ifelse(anxiety_depression_dat[,43] == "2 - I am much less interested in sex now.", 2,
                                                    ifelse(anxiety_depression_dat[,43] == "3 - I have lost interest in sex completely.", 3,''))))


# calculate mean anxiety score per individual
anxiety_depression_dat$anxiety_score <- rowMeans(as.data.frame(sapply(anxiety_depression_dat[, grepl("bai_", names(anxiety_depression_dat))], as.numeric)), na.rm = TRUE)
# cutoff for clinically significant anxiety on the BAI is 16
anxiety_depression_dat$anxiety_group <- ifelse(rowSums(as.data.frame(sapply(anxiety_depression_dat[, grepl("bai_", names(anxiety_depression_dat))], as.numeric)), na.rm = TRUE) > 15, "high","low")

# calculate mean depression score per individual
anxiety_depression_dat$depression_score <- rowMeans(as.data.frame(sapply(anxiety_depression_dat[, grepl("bdi_", names(anxiety_depression_dat))], as.numeric)), na.rm = TRUE)
# cutoff for clinically significant anxiety on the BDI* is 19
# *excluding one question on the BDI as it refers to suicide
anxiety_depression_dat$depression_group <- ifelse(rowSums(as.data.frame(sapply(anxiety_depression_dat[, grepl("bdi_", names(anxiety_depression_dat))], as.numeric)), na.rm = TRUE) > 18, "high","low")

anxiety_depression_dat$anomie_score <- rowMeans(as.data.frame(sapply(anxiety_depression_dat[, c("bai_5",
                                                                                                "bai_9",
                                                                                                "bai_14",
                                                                                                "bai_17",
                                                                                                "bdi_1",
                                                                                                "bdi_2",
                                                                                                "bdi_11",
                                                                                                "bdi_17")], as.numeric)), na.rm = TRUE)


anxiety_depression_scores_dat <- anxiety_depression_dat[,c(1,44:48)]

merged_dat1 <- merge(cluster_network_paranoia_df, anxiety_depression_scores_dat, by=c("id"))


library(BayesFactor) # install.packages("BayesFactor")

# compute Bayes Factor: mu03 difference between assumed belief group
bayes.t.test(mu03 ~ assumed_belief_group, data = merged_dat1) 
ttestBF(merged_dat1[which(merged_dat1$assumed_belief_group == 1),]$mu03,
        merged_dat1[which(merged_dat1$assumed_belief_group == 0),]$mu03) 

# compute Bayes Factor: paranoia difference between assumed belief group
bayes.t.test(gpts_bai_bdi_score ~ assumed_belief_group, data = merged_dat1) 
ttestBF(merged_dat1[which(merged_dat1$assumed_belief_group == 1),]$gpts_bai_bdi_score,
        merged_dat1[which(merged_dat1$assumed_belief_group == 0),]$gpts_bai_bdi_score)






# tmp$sabotage_belief <- as.numeric(tmp$sabotage_belief)
# 
# tmp$net_size_group <- ifelse(tmp$net_size > mean(tmp$net_size),1,0)
# tmp$tot_strong_ties_group <- ifelse(tmp$tot_strong_ties > mean(tmp$tot_strong_ties),1,0)
# tmp$assumedbelief_group <- ifelse(tmp$similarity_weightedSum > mean(tmp$similarity_weightedSum),1,0)
# tmp$net_ties_assumedbelief_group <- rowSums(as.data.frame(sapply(tmp[,c("net_size_group","tot_strong_ties_group","assumedbelief_group")], as.numeric)))
# 
# tmp_group0_group3 <- tmp[which(tmp$net_ties_assumedbelief_group == 0 | tmp$net_ties_assumedbelief_group == 3),]
# 
# t.test(tmp_group0_group3$conspiracy_score ~ tmp_group0_group3$net_ties_assumedbelief_group,
#        mu=0,
#        alt="two.sided",
#        conf=0.95,
#        var.eq=F,
#        paired=F)
# 
# 
# tmp1 <- merge(merged_dat1,net_dat_sabotage,by="id")
# 
# tmp1$sabotage_belief <- as.numeric(tmp1$sabotage_belief)
# 
# tmp1$net_size_group <- ifelse(tmp1$net_size > mean(tmp1$net_size),1,0)
# tmp1$tot_strong_ties_group <- ifelse(tmp1$tot_strong_ties > mean(tmp1$tot_strong_ties),1,0)
# tmp1$assumedbelief_group <- ifelse(tmp1$similarity_weightedSum > mean(tmp1$similarity_weightedSum),1,0)
# tmp1$net_ties_assumedbelief_group <- rowSums(as.data.frame(sapply(tmp1[,c("net_size_group","tot_strong_ties_group","assumedbelief_group")], as.numeric)))
# 
# tmp1_group0_group3 <- tmp1[which(tmp1$net_ties_assumedbelief_group == 0 | tmp1$net_ties_assumedbelief_group == 3),]
# 
# t.test(tmp1_group0_group3$conspiracy_score ~ tmp1_group0_group3$net_ties_assumedbelief_group,
#        mu=0,
#        alt="two.sided",
#        conf=0.95,
#        var.eq=F,
#        paired=F)
# 
# 
# library("PerformanceAnalytics")
# my_data <- tmp1[, c(2:12,14:15,30)]
# chart.Correlation(my_data, histogram=TRUE, pch=19)
# 
# 
# library("PerformanceAnalytics")
# my_data1 <- tmp[, c(2:12,14:15,23)]
# chart.Correlation(my_data1, histogram=TRUE, pch=19)




# creating assumed belief group from mean split
merged_dat1$assumed_belief_group <- ifelse(merged_dat1$similarity_weightedSum > mean(merged_dat1$similarity_weightedSum),1,0)
merged_dat1$gpts_bai_bdi_score <- rowSums(merged_dat1[,c("anxiety_score","depression_score","paranoia_score"),], na.rm = TRUE)

# predicting psychopathology based on network features including assumed belief
lm_full <- lm(gpts_bai_bdi_score ~ net_size*tot_strong_ties*kinship*assumed_belief_group, data = merged_dat1)
summary(lm_full)
backward_full = step(lm(lm_full, data = merged_dat1), method = "backward")
summary(backward_full)

# predicting mu03 based on network features including assumed belief
lm_full <- lm(mu03 ~ paranoia_score*net_size*tot_strong_ties*kinship*assumed_belief_group, data = merged_dat1)
summary(lm_full)
backward_full = step(lm(lm_full, data = merged_dat1), method = "backward")
summary(backward_full)

# theme_publication (for plot aesthetics) code can be found in Suthaharan et al 2021, Nature
source("C:/grant/paranoia_ref_per/paranoiaRefPer/archive/socialnetworkPRL/code/theme_publication.R")

# fit model with interaction
library(sjPlot)
fit1 <- lm(gpts_bai_bdi_score ~ net_size*tot_strong_ties*assumed_belief_group, data = merged_dat1)
fig4a <- plot_model(fit1, type = "pred", terms = c("net_size", "assumed_belief_group"),
                 colors = c("#a054ad","#301934"))
fig4a + theme_Publication() + 
  theme(legend.position = "none",
        #panel.spacing.x = unit(3, "lines"),
        strip.text.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_blank()
        #       axis.text = element_text(size=20),
        #       axis.title.y = element_text(size=30),
        #       axis.title.x = element_text(size=20)
  )



# fit model with interaction
fit2 <- lm(mu03 ~ paranoia_score*net_size*tot_strong_ties*assumed_belief_group*kinship, data = merged_dat1)
fig4b <- plot_model(fit2, type = "pred", terms = c("paranoia_score", "tot_strong_ties [0,6,53]", "assumed_belief_group"),
                 colors = c("#9494FF","#0B0BFF","#000080"))
fig4b$facet$params$nrow=2
fig4b + theme_Publication() + 
  theme(legend.position = "none",
        #panel.spacing.y = unit(3, "lines"),
        strip.text.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_blank()
        #       axis.text = element_text(size=20),
        #       axis.title.y = element_text(size=30),
        #       axis.title.x = element_text(size=20)
  )
  
  
  




