## read in raw data
paranoia_conspiracy_dat <- read.csv('C:\\grant\\paranoia_ref_per\\paranoiaRefPer\\data\\raw\\paranoia_conspiracy_dat.csv')
#paranoia_conspiracy_complete_dat <- paranoia_conspiracy_dat[which(complete.cases(paranoia_conspiracy_dat)),]

# clean data of duplicates
paranoia_conspiracy_dedup_dat <- paranoia_conspiracy_dat[which(!duplicated(paranoia_conspiracy_dat$id)),]

## calculate scores

# paranoia score
paranoia_conspiracy_dedup_dat$paranoia_score <- rowMeans(as.data.frame(sapply(paranoia_conspiracy_dedup_dat[, grepl("rgpts", names(paranoia_conspiracy_dedup_dat))], as.numeric)), na.rm = TRUE)
#sa_raw_paranoia_dat$paranoia_ref_score <- rowMeans(as.data.frame(sapply(sa_raw_paranoia_dat[, grepl("rgpts_ref", names(sa_raw_paranoia_dat))], as.numeric)), na.rm = TRUE)
#sa_raw_paranoia_dat$paranoia_per_score <- rowMeans(as.data.frame(sapply(sa_raw_paranoia_dat[, grepl("rgpts_per", names(sa_raw_paranoia_dat))], as.numeric)), na.rm = TRUE)

# paranoia group
paranoia_conspiracy_dedup_dat$paranoia_group <- ifelse(rowSums(as.data.frame(sapply(paranoia_conspiracy_dedup_dat[, grepl("rgpts_per", names(paranoia_conspiracy_dedup_dat))], as.numeric)), na.rm = TRUE) > 10, "high","low")

# conspiracy score - general
paranoia_conspiracy_dedup_dat[, grepl("gcbs_", names(paranoia_conspiracy_dedup_dat))] <- ifelse(paranoia_conspiracy_dedup_dat[, grepl("gcbs_", names(paranoia_conspiracy_dedup_dat))] == "Disagree",1,
                                                                                                    ifelse(paranoia_conspiracy_dedup_dat[, grepl("gcbs_", names(paranoia_conspiracy_dedup_dat))] == "2",2,
                                                                                                           ifelse(paranoia_conspiracy_dedup_dat[, grepl("gcbs_", names(paranoia_conspiracy_dedup_dat))] == "Neutral",3,
                                                                                                                  ifelse(paranoia_conspiracy_dedup_dat[, grepl("gcbs_", names(paranoia_conspiracy_dedup_dat))] == "4",4,
                                                                                                                         ifelse(paranoia_conspiracy_dedup_dat[, grepl("gcbs_", names(paranoia_conspiracy_dedup_dat))] == "Agree",5,"")))))

paranoia_conspiracy_dedup_dat$gcbs_score <- rowMeans(as.data.frame(sapply(paranoia_conspiracy_dedup_dat[, grepl("gcbs_", names(paranoia_conspiracy_dedup_dat))], as.numeric)), na.rm = TRUE)

paranoia_conspiracy_dedup_dat[, grepl("gen_consp", names(paranoia_conspiracy_dedup_dat))] <- ifelse(paranoia_conspiracy_dedup_dat[, grepl("gen_consp", names(paranoia_conspiracy_dedup_dat))] == "Strongly disagree",1,
                                                            ifelse(paranoia_conspiracy_dedup_dat[, grepl("gen_consp", names(paranoia_conspiracy_dedup_dat))] == "Disagree",2,
                                                                   ifelse(paranoia_conspiracy_dedup_dat[, grepl("gen_consp", names(paranoia_conspiracy_dedup_dat))] == "Neither agree nor disagree",3,
                                                                          ifelse(paranoia_conspiracy_dedup_dat[, grepl("gen_consp", names(paranoia_conspiracy_dedup_dat))] == "Agree",4,
                                                                                 ifelse(paranoia_conspiracy_dedup_dat[, grepl("gen_consp", names(paranoia_conspiracy_dedup_dat))] == "Strongly agree",5,"")))))

paranoia_conspiracy_dedup_dat$gen_consp_score <- rowMeans(as.data.frame(sapply(paranoia_conspiracy_dedup_dat[, grepl("gen_consp", names(paranoia_conspiracy_dedup_dat))], as.numeric)), na.rm = TRUE)


# conspiracy score - QAnon

# conspiracy score - covid
paranoia_conspiracy_dedup_dat[, grepl("covid_conspiracy_vaccine_", names(paranoia_conspiracy_dedup_dat))] <- ifelse(paranoia_conspiracy_dedup_dat[, grepl("covid_conspiracy_vaccine_", names(paranoia_conspiracy_dedup_dat))] == "Strongly disagree",1,
                                                                                                    ifelse(paranoia_conspiracy_dedup_dat[, grepl("covid_conspiracy_vaccine_", names(paranoia_conspiracy_dedup_dat))] == "Disagree",2,
                                                                                                           ifelse(paranoia_conspiracy_dedup_dat[, grepl("covid_conspiracy_vaccine_", names(paranoia_conspiracy_dedup_dat))] == "Somewhat disagree",3,
                                                                                                                  ifelse(paranoia_conspiracy_dedup_dat[, grepl("covid_conspiracy_vaccine_", names(paranoia_conspiracy_dedup_dat))] == "Neutral",4,
                                                                                                                         ifelse(paranoia_conspiracy_dedup_dat[, grepl("covid_conspiracy_vaccine_", names(paranoia_conspiracy_dedup_dat))] == "Somewhat agree",5,
                                                                                                                                ifelse(paranoia_conspiracy_dedup_dat[, grepl("covid_conspiracy_vaccine_", names(paranoia_conspiracy_dedup_dat))] == "Agree",6,
                                                                                                                                       ifelse(paranoia_conspiracy_dedup_dat[, grepl("covid_conspiracy_vaccine_", names(paranoia_conspiracy_dedup_dat))] == "Strongly agree",7,"")))))))



paranoia_conspiracy_dedup_dat$covid_consp_score <- rowMeans(as.data.frame(sapply(paranoia_conspiracy_dedup_dat[, grepl("covid_conspiracy_vaccine_", names(paranoia_conspiracy_dedup_dat))], as.numeric)), na.rm = TRUE)
                                                                                                           
                                                                                                           
                                                                                                           
                                                                                                           
# match ids with model parameters estimates data
setwd('C:/grant/paranoia_ref_per/paranoiaRefPer')

# read in data
pandemic_dat <- read.csv('data/pandemicPRL.csv')

# extract relevant subset
pandemic_subset_dat <- pandemic_dat[-which(pandemic_dat$dataset == "elife2020"),]
pandemic_subset_dat <- pandemic_subset_dat[,c(2,24:65,66:83,150:151,154:163)]


# calculate referential and paranoia scores/groups
pandemic_subset_dat$paranoia_rgpts_score <- rowMeans(as.data.frame(sapply(pandemic_subset_dat[, grepl("rgpts", names(pandemic_subset_dat))], as.numeric)), na.rm = TRUE)
pandemic_subset_dat$paranoia_ref_score <- rowMeans(as.data.frame(sapply(pandemic_subset_dat[, grepl("rgpts_ref", names(pandemic_subset_dat))], as.numeric)), na.rm = TRUE)
pandemic_subset_dat$paranoia_per_score <- rowMeans(as.data.frame(sapply(pandemic_subset_dat[, grepl("rgpts_per", names(pandemic_subset_dat))], as.numeric)), na.rm = TRUE)

pandemic_subset_dat$paranoia_ref_group <- ifelse(rowSums(as.data.frame(sapply(pandemic_subset_dat[, grepl("rgpts_ref", names(pandemic_subset_dat))], as.numeric)), na.rm = TRUE) > 15, "high","low")
pandemic_subset_dat$paranoia_per_group <- ifelse(rowSums(as.data.frame(sapply(pandemic_subset_dat[, grepl("rgpts_per", names(pandemic_subset_dat))], as.numeric)), na.rm = TRUE) > 10, "high","low")

pandemic_subset_dat$anxiety_bai_score <- rowMeans(as.data.frame(sapply(pandemic_subset_dat[, grepl("bai_", names(pandemic_subset_dat))], as.numeric)), na.rm = TRUE)
pandemic_subset_dat$anxiety_group <- ifelse(rowSums(as.data.frame(sapply(pandemic_subset_dat[, grepl("bai_", names(pandemic_subset_dat))], as.numeric)), na.rm = TRUE) > 15, "high","low")

pandemic_subset_dat$depression_bdi_score <- rowMeans(as.data.frame(sapply(pandemic_subset_dat[, grepl("bdi_", names(pandemic_subset_dat))], as.numeric)), na.rm = TRUE)
pandemic_subset_dat$depression_group <- ifelse(rowSums(as.data.frame(sapply(pandemic_subset_dat[, grepl("bdi_", names(pandemic_subset_dat))], as.numeric)), na.rm = TRUE) > 18, "high","low")




pandemic_subset_dat$wsr <- rowMeans(cbind(pandemic_subset_dat$wsr_block1,pandemic_subset_dat$wsr_block2))
pandemic_subset_dat$kappa <- rowMeans(cbind(pandemic_subset_dat$kappa2_1,pandemic_subset_dat$kappa2_2))
pandemic_subset_dat$mu02 <- rowMeans(cbind(pandemic_subset_dat$mu02_1,pandemic_subset_dat$mu02_2))
pandemic_subset_dat$mu03 <- rowMeans(cbind(pandemic_subset_dat$mu03_1,pandemic_subset_dat$mu03_2))
pandemic_subset_dat$omega2 <- rowMeans(cbind(pandemic_subset_dat$omega2_1,pandemic_subset_dat$omega2_2))
pandemic_subset_dat$omega3 <- rowMeans(cbind(pandemic_subset_dat$omega3_1,pandemic_subset_dat$omega3_2))

# dataset 1 - pandemic (n=938)
dat1 <- pandemic_subset_dat[,c(1,74,84:88)]#pandemic_df[,c(1:2,5:9)]
colnames(dat1) <- c("id","paranoia_score","kappa","mu02","mu03","omega2","omega3")
dat1$dataset1 <- "pandemic"

# dataset 2 - election data
dat2 <- read.csv("data/election_dat.csv")
dat2 <- dat2[,-c(3)]
dat2$dataset1 <- "election"

# dataset 3 - vaccine messaging data
dat3 <- read.csv("data/vaccine_messaging_dat.csv")
dat3 <- dat3[,-c(3)]
dat3$dataset1 <- "vaccine_messaging"

# dataset 4 - social network data
dat4 <- read.csv("data/social_network_dat_merged.csv")
dat4 <- dat4[,-c(3)]
dat4 <- dat4[which(dat4$id %in% network_clustering_consp$id),]
dat4$dataset1 <- "social_network"

# dataset 5 - substance abuse data
dat5 <- read.csv("data/substance_abuse_dat.csv")
dat5 <- dat5[,-c(3)]
dat5$dataset1 <- "substance_abuse"

# dataset 5 - longitudinal data
dat6 <- read.csv("data/longitudinal_dat.csv")
dat6 <- dat6[,-c(3:4)]
dat6$dataset1 <- "longitudinal"

# combine datasets
combined_dat <- rbind(dat4,dat1,dat2,dat3,dat5,dat6)

# use non-duplicated values
combined_cleaned_dat <- combined_dat[which(!duplicated(combined_dat$id)),]


## create plots
paranoia_conspiracy_subset <- paranoia_conspiracy_dedup_dat[,c("id","task_type","poli_party","qanon_rating","paranoia_score","paranoia_group","gcbs_score","gen_consp_score","covid_consp_score")]
combined_cleaned_subset <- combined_cleaned_dat[,c("id","kappa","mu02","mu03","omega2","omega3")]
merged_dat <- merge(paranoia_conspiracy_subset , combined_cleaned_subset, by=c("id"))
merged_dat$mu03_norm <- (merged_dat$mu03-min(merged_dat$mu03, na.rm = TRUE))/(max(merged_dat$mu03, na.rm = TRUE)-min(merged_dat$mu03, na.rm = TRUE))
merged_dat$paranoia_norm <- (merged_dat$paranoia_score-min(merged_dat$paranoia_score, na.rm = TRUE))/(max(merged_dat$paranoia_score, na.rm = TRUE)-min(merged_dat$paranoia_score, na.rm = TRUE))



# plot wsr and mu03 in high and low paranoia
wsr_df <- read.csv("C:\\grant\\paranoia_ref_per\\paranoiaRefPer\\data\\archive\\wsr_dat.csv")

tmp_df <- merge(merged_dat,wsr_df,by=c("id"))

wsr_mu03_df <- data.frame(id = tmp_df$id,
                          wsr = tmp_df$wsr,
                          mu03 = tmp_df$mu03,
                          paranoia_group = tmp_df$paranoia_group)

library(rstatix)
wsr_mu03_long_df<- wsr_mu03_df %>%
  gather(key = "param", value = "score", wsr,mu03) %>%
  convert_as_factor(id, paranoia_group)


library(plotrix)
library(dplyr)

wsr_mu03_stats <- wsr_mu03_long_df %>%
  dplyr::group_by(param,paranoia_group) %>%
  dplyr::summarise(param_mean = mean(score, na.rm = TRUE),
                   param_se = std.error(score, na.rm = TRUE))

# plot distribution of assumed belief
p1 <- ggplot(wsr_mu03_stats, aes(x = paranoia_group, y = param_mean, fill = paranoia_group)) +
  geom_bar(stat = "identity", position = position_dodge(), width = .5, alpha=0.7) + #position = position_dodge()
  #geom_errorbar(aes(x=group, ymin=wsr_mean-wsr_se, ymax=wsr_mean+wsr_se),
  #              width=0.2, colour="black", alpha=0.9, size=1) + #position_dodge(0.5)
  
  geom_errorbar(aes(x=paranoia_group, ymin=param_mean-param_se, ymax=param_mean+param_se),
                width=0.2, colour="black", alpha=0.9, size=1, position = position_dodge(0.5)) +
  geom_point(data=wsr_mu03_long_df, aes(x=paranoia_group,y=score), alpha=0.1,position = position_jitterdodge()) +
  facet_wrap(~param,scales = "free", labeller = label_parsed, ncol=1) +
  labs(x = "paranoia group", y = "mean") +
  scale_fill_manual(values = c("#800000","#5B8FAF"))+
  theme_Publication() + 
  theme(legend.position = "none",
        #panel.spacing.x = unit(3, "lines"),
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


# Figure 1C
source("C:/grant/paranoia_ref_per/paranoiaRefPer/archive/socialnetworkPRL/code/theme_publication.R")

library(ggplot2)
# Figure 1C-1: paranoia vs mu03
plot1 <- ggplot(merged_dat, aes(x=mu03, y=paranoia_score)) + 
  geom_smooth(method="lm" , fill = "#1b0000", color = "black") + 
  geom_point(aes(color = paranoia_group),shape=16,size=2,alpha=0.4) + #ggpubr::stat_cor(label.x=-4.5,label.y=3.2, size=7)+
  labs(title = "", x=expression(mu[3]^0), y="Paranoia") +
  scale_color_manual(values = c("#800000","#5B8FAF")) +
theme_Publication() + 
  theme(legend.position = "none"
        #axis.text.x = element_blank(),
        #axis.title.x = element_blank(),
        #axis.text.y = element_blank(),
        #axis.title.y = element_blank()
  #       axis.text = element_text(size=20),
  #       axis.title.y = element_text(size=30),
  #       axis.title.x = element_text(size=20)
  )

# Figure 1C-2: paranoia vs c19 vaccine conspiracy
plot2 <- ggplot(merged_dat, aes(x=covid_consp_score, y=paranoia_score)) + 
  geom_smooth(method="lm" , fill = "#1b0000", color = "black") + 
  geom_point(aes(color = paranoia_group),shape=16,size=2,alpha=0.4) + #ggpubr::stat_cor(label.x=-4.5,label.y=3.2, size=7)+
  labs(title = "", x="C19 vaccine conspiracy belief", y="Paranoia") +
  scale_color_manual(values = c("#800000","#5B8FAF")) +
  theme_Publication() + 
  theme(legend.position = "none"#,
        #axis.text.x = element_blank(),
        #axis.title.x = element_blank(),
        #axis.text.y = element_blank(),
        #axis.title.y = element_blank()
        #       axis.text = element_text(size=20),
        #       axis.title.y = element_text(size=30),
        #       axis.title.x = element_text(size=20)
  )

# Figure 1C-3: paranoia vs QAnon conspiracy
plot3 <- ggplot(merged_dat, aes(x=qanon_rating, y=paranoia_score)) + 
  geom_smooth(method="lm" , fill = "#1b0000", color = "black") + 
  geom_point(aes(color = paranoia_group),shape=16,size=2,alpha=0.4) + #ggpubr::stat_cor(label.x=-4.5,label.y=3.2, size=7)+
  labs(title = "", x="QAnon conspiracy belief", y="Paranoia") +
  scale_color_manual(values = c("#800000","#5B8FAF")) +
  theme_Publication() + 
  theme(legend.position = "none"#,
        #axis.text.x = element_blank(),
        #axis.title.x = element_blank(),
        #axis.text.y = element_blank(),
        #axis.title.y = element_blank()
        #       axis.text = element_text(size=20),
        #       axis.title.y = element_text(size=30),
        #       axis.title.x = element_text(size=20)
  )

# Figure 1C-4: paranoia vs general conspiracy
plot4 <- ggplot(merged_dat, aes(x=gen_consp_score, y=paranoia_score)) + 
  geom_smooth(method="lm" , fill = "#1b0000", color = "black") + 
  geom_point(aes(color = paranoia_group),shape=16,size=2,alpha=0.4) + #ggpubr::stat_cor(label.x=-4.5,label.y=3.2, size=7)+
  labs(title = "", x="General conspiracy belief", y="Paranoia") +
  scale_color_manual(values = c("#800000","#5B8FAF")) +
  theme_Publication() + 
  theme(legend.position = "none"#,
        #axis.text.x = element_blank(),
        #axis.title.x = element_blank(),
        #axis.text.y = element_blank(),
        #axis.title.y = element_blank()
        #       axis.text = element_text(size=20),
        #       axis.title.y = element_text(size=30),
        #       axis.title.x = element_text(size=20)
  )

# Figure 1C-5: mu03 vs c19 vaccine conspiracy
plot5 <- ggplot(merged_dat, aes(x=covid_consp_score, y=mu03)) + 
  geom_smooth(method="lm" , fill = "#1b0000", color = "black") + 
  geom_point(aes(color = paranoia_group),shape=16,size=2,alpha=0.4) + #ggpubr::stat_cor(label.x=-4.5,label.y=3.2, size=7)+
  labs(title = "", x="C19 vaccine conspiracy belief", y=expression(mu[3]^0)) +
  scale_color_manual(values = c("#800000","#5B8FAF")) +
  theme_Publication() + 
  theme(legend.position = "none"#,
        #axis.text.x = element_blank(),
        #axis.title.x = element_blank(),
        #axis.text.y = element_blank(),
        #axis.title.y = element_blank()
        #       axis.text = element_text(size=20),
        #       axis.title.y = element_text(size=30),
        #       axis.title.x = element_text(size=20)
  )

# Figure 1C-6: mu03 vs QAnon conspiracy
plot6 <- ggplot(merged_dat, aes(x=qanon_rating, y=mu03)) + 
  geom_smooth(method="lm" , fill = "#1b0000", color = "black") + 
  geom_point(aes(color = paranoia_group),shape=16,size=2,alpha=0.4) + #ggpubr::stat_cor(label.x=-4.5,label.y=3.2, size=7)+
  labs(title = "", x="QAnon conspiracy belief", y=expression(mu[3]^0)) +
  scale_color_manual(values = c("#800000","#5B8FAF")) +
  theme_Publication() + 
  theme(legend.position = "none"#,
        #axis.text.x = element_blank(),
        #axis.title.x = element_blank(),
        #axis.text.y = element_blank(),
        #axis.title.y = element_blank()
        #       axis.text = element_text(size=20),
        #       axis.title.y = element_text(size=30),
        #       axis.title.x = element_text(size=20)
  )

# Figure 1C-7: mu03 vs general conspiracy
plot7 <- ggplot(merged_dat, aes(x=gen_consp_score, y=mu03)) + 
  geom_smooth(method="lm" , fill = "#1b0000", color = "black") + 
  geom_point(aes(color = paranoia_group),shape=16,size=2,alpha=0.4) + ggpubr::stat_cor(label.x=-4.5,label.y=3.2, size=7)+
  labs(title = "", x="General conspiracy belief", y=expression(mu[3]^0)) +
  scale_color_manual(values = c("#800000","#5B8FAF")) +
  theme_Publication() + 
  theme(legend.position = "none"#,
        #axis.text.x = element_blank(),
        #axis.title.x = element_blank(),
        #axis.text.y = element_blank(),
        #axis.title.y = element_blank()
        #       axis.text = element_text(size=20),
        #       axis.title.y = element_text(size=30),
        #       axis.title.x = element_text(size=20)
  )


# Figure 1D-1: mu02 differences between paranoia groups in both versions of PRL 
fig1d1_points <- ggplot(merged_dat, aes(x = paranoia_group,
                                       y= mu02,
                                       fill = as.factor(task_type))) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(), show.legend = FALSE) + 
  geom_boxplot(alpha = 0.7, width=0.5, lwd=1.2) +
  scale_fill_manual(name = "", 
                    values = c("nonsocial" = "#282828", "social" = "#3a2023"))


fig1d1_points + theme_Publication() + theme(axis.title.y = element_blank(),
                                           axis.title.x = element_blank(),
                                           axis.text = element_blank(), 
                                           axis.line = element_line(colour="black", size = 1.5),
                                           axis.ticks = element_line(colour="black", size = 1.5),
                                           legend.text = element_blank(),
                                           legend.position = "none")


# Figure 1D-2: omega2 differences between paranoia groups in both versions of PRL 
fig1d2_points <- ggplot(merged_dat, aes(x = paranoia_group,
                                       y= omega2,
                                       fill = as.factor(task_type))) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(), show.legend = FALSE) + 
  geom_boxplot(alpha = 0.7, width=0.5, lwd=1.2) +
  scale_fill_manual(name = "", 
                    values = c("nonsocial" = "#282828", "social" = "#3a2023"))


fig1d2_points + theme_Publication() + theme(axis.title.y = element_blank(),
                                           axis.title.x = element_blank(),
                                           axis.text = element_blank(), 
                                           axis.line = element_line(colour="black", size = 1.5),
                                           axis.ticks = element_line(colour="black", size = 1.5),
                                           legend.text = element_blank(),
                                           legend.position = "none")


# Figure 1D-3: kappa differences between paranoia groups in both versions of PRL 
fig1d3_points <- ggplot(merged_dat, aes(x = paranoia_group,
                                       y= kappa,
                                       fill = as.factor(task_type))) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(), show.legend = FALSE) + 
  geom_boxplot(alpha = 0.7, width=0.5, lwd=1.2) +
  scale_fill_manual(name = "", 
                    values = c("nonsocial" = "#282828", "social" = "#3a2023"))


fig1d3_points + theme_Publication() + theme(axis.title.y = element_blank(),
                                           axis.title.x = element_blank(),
                                           axis.text = element_blank(), 
                                           axis.line = element_line(colour="black", size = 1.5),
                                           axis.ticks = element_line(colour="black", size = 1.5),
                                           legend.text = element_blank(),
                                           legend.position = "none")


# Figure 1D-4: mu03 differences between paranoia groups in both versions of PRL 
fig1d4_points <- ggplot(merged_dat, aes(x = paranoia_group,
                                       y= mu03,
                                       fill = as.factor(task_type))) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(), show.legend = FALSE) + 
  geom_boxplot(alpha = 0.7, width=0.5, lwd=1.2) +
  scale_fill_manual(name = "", 
                    values = c("nonsocial" = "#282828", "social" = "#3a2023"))


fig1d4_points + theme_Publication() + theme(axis.title.y = element_blank(),
                                           axis.title.x = element_blank(),
                                           axis.text = element_blank(), 
                                           axis.line = element_line(colour="black", size = 1.5),
                                           axis.ticks = element_line(colour="black", size = 1.5),
                                           legend.text = element_blank(),
                                           legend.position = "none")


# Figure 1D-5: omega3 differences between paranoia groups in both versions of PRL 
fig1d5_points <- ggplot(merged_dat, aes(x = paranoia_group,
                                       y= omega3,
                                       fill = as.factor(task_type))) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(), show.legend = FALSE) + 
  geom_boxplot(alpha = 0.7, width=0.5, lwd=1.2) +
  scale_fill_manual(name = "", 
                    values = c("nonsocial" = "#282828", "social" = "#3a2023"))


fig1d5_points + theme_Publication() + theme(axis.title.y = element_blank(),
                                           axis.title.x = element_blank(),
                                           axis.text = element_blank(), 
                                           axis.line = element_line(colour="black", size = 1.5),
                                           axis.ticks = element_line(colour="black", size = 1.5),
                                           legend.text = element_blank(),
                                           legend.position = "none")


# two-way ANOVA: points earned
aov2_mu03 <- aov(kappa ~ paranoia_group * task_type, data = merged_dat)
summary(aov2_mu03)
Anova(aov2_mu03, type="III")

