rm(list = objects())
# plot ROC curves

library(plotROC)
library(ggplot2)
library("gridExtra")
library(rlist)
library("grid")

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#

# ---- load models ----
setwd("L:/Uni Skripte/PhD Project/Paper/Feature_Selection_In_EcoMetabolomics/originalData/")

# load MetaData
load (file = "MetaData.rda")
# load Models
load (file = "ANOVA.rda")
load (file = "PLS.rda")
load (file = "SVM.rda")
load (file = "RF.rda")

# ---- plot ROC PLS-SVm-RF MTBLS679 -----
# set selected Indices for chosen model
selectedIndices_pls <- model_pls679$pred$ncomp == 9
selectedIndices_svm <- model_svm679$pred$cost == 0.25
selectedIndices_rf <-  model_rf679$pred$mtry == 13

# order samples
order_pls <- order(model_pls679$pred$obs[selectedIndices_pls])
order_rf <-order(model_rf679$pred$obs[selectedIndices_rf])
order_svm <- order(model_svm679$pred$obs[selectedIndices_svm])

# store plots in list to save as PDF
plotlist<-vector(mode="list",length = length(levels(feat_class679)))
names(plotlist)<-levels(feat_class679)
for(species_code in levels(feat_class679)) {
#species_code<-"POAPRA"

if(all.equal(model_rf679$pred$obs[selectedIndices_rf][order_rf], model_pls679$pred$obs[selectedIndices_pls][order_pls]) == all.equal(model_pls679$pred$obs[selectedIndices_pls][order_pls], model_svm679$pred$obs[selectedIndices_svm][order_svm])){
  print("Model equality check passed.")
} else {
  stop("Model equality check not passed.")  
}


roc_data<-data.frame(D=as.numeric(model_rf679$pred$obs[selectedIndices_rf][order_rf] == species_code),
  Mrf=model_rf679$pred[[species_code]][selectedIndices_rf][order_rf],
  Mpls=model_pls679$pred[[species_code]][selectedIndices_pls][order_pls], 
  Msvm=model_svm679$pred[[species_code]][selectedIndices_svm][order_svm]
  )


plot<-ggplot(melt_roc(roc_data,"D", c("Mrf", "Mpls", "Msvm")), 
    aes(d = D, m = M, colour=name)) + 
    geom_roc(n.cuts=5, labelsize = 2) + style_roc() + theme(legend.position = c(0.9, 0.2), legend.background = element_blank(),legend.title = element_blank(), legend.key = element_rect(fill="gray97", color = "black"), legend.text = element_text(face="bold")) +
    scale_color_manual(values = c("deeppink", "springgreen4", "purple"), labels=c("PLS", "RF", "SVM")) + #lable colours are ins right order (alphabetical)! is double checked
    labs(title=paste("Feature Selection Models",species_code)) 
   
plot<- plot + annotate("text", x = .75, y = .23, color="deeppink",
           label = paste("AUC =", round(calc_auc(plot)$AUC, 2)[1])) +
  annotate("text", x = .75, y = .15, color="springgreen4",
             label = paste("AUC =", round(calc_auc(plot)$AUC, 2)[2])) +
  annotate("text", x = .75, y = .07, color="purple",
             label = paste("AUC =", round(calc_auc(plot)$AUC, 2)[3]))

plotlist[[species_code]]<-plot

}
plot

pdflayout<-marrangeGrob(plotlist, ncol=2, nrow=2, left=2, vp=viewport(width=0.93, height=0.95))
ggsave("./VascularPlants_MacBeSSt/Results_Manuscript/ggplotROC679.pdf", plot=pdflayout, device = "pdf",  height = 210, width = 297, units = "mm", dpi=300)



# ---- plot ROC PLS-SVm-RF MTBLS520 -----
# set selected Indices for chosen model
selectedIndices_pls <- model_pls520$pred$ncomp == 10
selectedIndices_svm <- model_svm520$pred$cost == 0.25
selectedIndices_rf <-  model_rf520$pred$mtry == 5
# order samples
order_pls <- order(model_pls520$pred$obs[selectedIndices_pls])
order_rf <-order(model_rf520$pred$obs[selectedIndices_rf])
order_svm <- order(model_svm520$pred$obs[selectedIndices_svm])

# store plots in list to save as PDF
plotlist<-vector(mode="list",length = length(levels(feat_class520)))
names(plotlist)<-levels(feat_class520)
for(species_code in levels(feat_class520)) {
  #species_code<-"POAPRA"
  
  if(all.equal(model_rf520$pred$obs[selectedIndices_rf][order_rf], model_pls520$pred$obs[selectedIndices_pls][order_pls]) == all.equal(model_pls520$pred$obs[selectedIndices_pls][order_pls], model_svm520$pred$obs[selectedIndices_svm][order_svm])){
    print("Model equality check passed.")
  } else {
    stop("Model equality check not passed.")  
  }
  
  
  roc_data<-data.frame(D=as.numeric(
    model_rf520$pred$obs[selectedIndices_rf][order_rf] == species_code),
    Mrf=model_rf520$pred[[species_code]][selectedIndices_rf][order_rf],
    Mpls=model_pls520$pred[[species_code]][selectedIndices_pls][order_pls], 
    Msvm=model_svm520$pred[[species_code]][selectedIndices_svm][order_svm]
  )
  
  
  plot<-ggplot(melt_roc(roc_data,"D", c("Mrf", "Mpls", "Msvm")), 
               aes(d = D, m = M, colour=name)) + 
    geom_roc(n.cuts=5, labelsize = 2) + style_roc() + theme(legend.position = c(0.9, 0.2), legend.background = element_blank(),legend.title = element_blank(), legend.key = element_rect(fill="gray97", color = "black"), legend.text = element_text(face="bold")) +
    scale_color_manual(values = c("deeppink", "springgreen4", "purple"), labels=c("PLS", "RF", "SVM")) + #lable colours are ins right order (alphabetical)! is double checked
    labs(title=paste("Feature Selection Models",species_code)) 
  
  plot<- plot + annotate("text", x = .75, y = .23, color="deeppink",
                         label = paste("AUC =", round(calc_auc(plot)$AUC, 2)[1])) +
    annotate("text", x = .75, y = .15, color="springgreen4",
             label = paste("AUC =", round(calc_auc(plot)$AUC, 2)[2])) +
    annotate("text", x = .75, y = .07, color="purple",
             label = paste("AUC =", round(calc_auc(plot)$AUC, 2)[3]))
  
  plotlist[[species_code]]<-plot
  
}

pdflayout<-marrangeGrob(plotlist, ncol=2, nrow=2, left=2, vp=viewport(width=0.93, height=0.95))
ggsave("./Bryophytes_KPeters/Results_Manuscript/ggplotROC520.pdf", plot=pdflayout, device = "pdf",  height = 210, width = 297, units = "mm", dpi=300)





# ---- single plot ROC paper ----

species_code<-"POAPRA"

if(all.equal(model_rf679$pred$obs[selectedIndices_rf][order_rf], model_pls679$pred$obs[selectedIndices_pls][order_pls]) == all.equal(model_pls679$pred$obs[selectedIndices_pls][order_pls], model_svm679$pred$obs[selectedIndices_svm][order_svm])){
  print("Model equality check passed.")
} else {
  stop("Model equality check not passed.")  
}


roc_data<-data.frame(D=as.numeric(model_rf679$pred$obs[selectedIndices_rf][order_rf] == species_code),
                     Mrf=model_rf679$pred[[species_code]][selectedIndices_rf][order_rf],
                     Mpls=model_pls679$pred[[species_code]][selectedIndices_pls][order_pls], 
                     Msvm=model_svm679$pred[[species_code]][selectedIndices_svm][order_svm]
)


ROC679<-ggplot(melt_roc(roc_data,"D", c("Mrf", "Mpls", "Msvm")), 
             aes(d = D, m = M, colour=name)) +  
  geom_roc(n.cuts=5, labelsize = 2) + 
  style_roc() + 
  theme(legend.position = c(0.9, 0.2), 
        legend.background = element_blank(),
        legend.title = element_blank(), 
        legend.key = element_rect(fill="gray97", color = "black"), 
        legend.text = element_text(face="bold")) +
  scale_color_manual(values = c("deeppink", "springgreen4", "purple"), 
                     labels=c("PLS", "RF", "SVM")) + #lable colours are ins right order (alphabetical)! is double checked
  labs(title=paste("Feature Selection Models",species_code)) 

ROC679<- ROC679 + annotate("text", x = .75, y = .23, color="deeppink",
                       label = paste("AUC =", round(calc_auc(plot)$AUC, 2)[1])) +
  annotate("text", x = .75, y = .15, color="springgreen4",
           label = paste("AUC =", round(calc_auc(plot)$AUC, 2)[2])) +
  annotate("text", x = .75, y = .07, color="purple",
           label = paste("AUC =", round(calc_auc(plot)$AUC, 2)[3]))


plotlist[[species_code]]<-plot
plot


