poliparty_tmp <- read.csv('C:\\grant\\paranoia_ref_per\\paranoiaRefPer\\data\\politicalparty_dat.csv')

# read in poliparty data
poliparty_dat <- read.csv('C:\\grant\\paranoia_ref_per\\paranoiaRefPer\\data\\politicalparty_cleaned_rep_dem_dat.csv')

q_weighted_similarity_dat <- read.csv("C:\\grant\\paranoia_ref_per\\paranoiaRefPer\\data\\assumed_weighted_similarity_q_cleaned.csv")


# recode values
poliparty_dat$poliparty_ego <- ifelse(poliparty_dat$poliparty_ego == "1 - Republican Party",1,
                                      ifelse(poliparty_dat$poliparty_ego == "2 - Democratic Party",2,""))

poliparty_dat[,3:17] <- ifelse(poliparty_dat[,3:17] == "Yes",-1,
                               ifelse(poliparty_dat[,3:17] == "No",1,""))


poliparty_merge <- merge(poliparty_dat,q_weighted_similarity_dat,by=c("id")) # replaced network_homophily_scores_dat with q_weighted_similarity_dat

poliparty_merge_cleaned <- poliparty_merge[complete.cases(poliparty_merge$similarity_weightedSum),]
poliparty_merge_cleaned <- poliparty_merge_cleaned[which(poliparty_merge_cleaned$similarity_weightedSum != "Inf"),]

poliparty_paranoia_merge_cleaned <- merge(poliparty_merge_cleaned,network_clustering_consp,by=c("id")) # replaced merge_dat with network_clustering_consp

t.test(poliparty_merge_cleaned$similarity_weightedSum ~ poliparty_merge_cleaned$poliparty_ego,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)

poliparty_cleaned_dat <- poliparty_merge_cleaned[,c("similarity_weightedSum","poliparty_ego")]#poliparty_merge_cleaned[,c("similarity_weightedSum","poliparty_ego")]

library(dplyr)
library(plotrix)
poliparty_cleaned_stats <- poliparty_cleaned_dat %>%
  dplyr::group_by(poliparty_ego) %>%
  dplyr::summarise(assumedQAnon_mean = mean(similarity_weightedSum, na.rm = TRUE),
                   assumedQAnon_se = std.error(similarity_weightedSum, na.rm = TRUE))


library(ggplot2)
p1 <- ggplot(poliparty_cleaned_stats, aes(x = poliparty_ego, y = assumedQAnon_mean, fill = poliparty_ego)) +
  geom_bar(stat = "identity", position = position_dodge(), width = .5, alpha=0.7) + #position = position_dodge()
  #geom_errorbar(aes(x=group, ymin=wsr_mean-wsr_se, ymax=wsr_mean+wsr_se),
  #              width=0.2, colour="black", alpha=0.9, size=1) + #position_dodge(0.5)
  
  geom_errorbar(aes(x=poliparty_ego, ymin=assumedQAnon_mean-assumedQAnon_se, ymax=assumedQAnon_mean+assumedQAnon_se),
                width=0.2, colour="black", alpha=0.9, size=1, position = position_dodge(0.5)) +
  geom_point(data=poliparty_cleaned_dat, aes(x=poliparty_ego,y=similarity_weightedSum), alpha=0.3,position = position_jitterdodge()) +
  #facet_wrap(~param,scales = "free", labeller = label_parsed) +
  labs(x = "Political Party", y = "Assumed QAnon Belief") +
  # scale_x_discrete(labels=c("kappa" = expression(kappa),
  #                           "mu[2]^0" = expression(mu[2]^0),
  #                           "mu[3]^0" = expression(mu[3]^0),
  #                           "omega[2]" = expression(omega[2]),
  #                           "omega[3]" = expression(omega[3]),
  #                           "paranoia" = "paranoia")) +
  scale_x_discrete(labels=c("1" = "Republican",
                            "2" = "Democrat")) +
  #theme_bw()
  #scale_color_manual(values = c("#f3c483", "#5c1a33","blue"))+
  scale_fill_manual(#name = "ref-per group",
    #labels = c("group 1" = "low ref-low per (n=694)",
    #           "group 2" = "low ref-high per (n=89)",
    #           "group 3" = "high ref-low per (n=13)",
    #           "group 4" = "high ref-high per (n=142)"),
    values = c("red", "blue")) + #"blue","#69be28","#ff8849","#4d2f92","#1b047c"
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


