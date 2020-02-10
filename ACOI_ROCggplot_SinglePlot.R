rm(list = objects())
# plot ROC curves

library(plotROC)
library(ggplot2)
library("gridExtra")
library("grid")
# library(rlist)


# ---- load models ----
setwd("L:/Uni Skripte/PhD Project/Paper/Feature_Selection_In_EcoMetabolomics/originalData/")

# load MetaData
load (file = "MetaData.rda")
# load Models
# load (file = "ANOVA.rda")
load (file = "PLS.rda")
load (file = "SVM.rda")
load (file = "RF.rda")

# ---- plot ROC PLS-SVM-RF MTBLS679 -----
# set selected Indices for chosen model
selectedIndices_pls679 <- model_pls679$pred$ncomp == 9
selectedIndices_svm679 <- model_svm679$pred$cost == 0.25
selectedIndices_rf679 <-  model_rf679$pred$mtry == 13

# order samples
order_pls679 <- order(model_pls679$pred$obs[selectedIndices_pls679])
order_rf679 <-order(model_rf679$pred$obs[selectedIndices_rf679])
order_svm679 <- order(model_svm679$pred$obs[selectedIndices_svm679])


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
# plotlist<-vector(mode="list",length = length(levels(feat_class520)))
# names(plotlist)<-levels(feat_class520)









species_code679 <-"POAPRA"

if(all.equal(model_rf679$pred$obs[selectedIndices_rf679][order_rf679], 
             model_pls679$pred$obs[selectedIndices_pls679][order_pls679]) == all.equal(model_pls679$pred$obs[selectedIndices_pls679][order_pls679], model_svm679$pred$obs[selectedIndices_svm679][order_svm679])){
  print("Model equality check passed.")
} else {
  stop("Model equality check not passed.")  
}


roc_data<-data.frame(D=as.numeric(model_rf679$pred$obs[selectedIndices_rf679][order_rf679] == species_code679),
                     Mrf=model_rf679$pred[[species_code679]][selectedIndices_rf679][order_rf679],
                     Mpls=model_pls679$pred[[species_code679]][selectedIndices_pls679][order_pls679], 
                     Msvm=model_svm679$pred[[species_code679]][selectedIndices_svm679][order_svm679]
)


ROC679 <- ggplot(melt_roc(roc_data,"D", c("Mrf", "Mpls", "Msvm")), aes(d = D, m = M, colour=name)) + 
  geom_roc(n.cuts=5, labelsize = 2) + 
  style_roc() + theme(legend.position = c(0.9, 0.2), 
                      legend.background = element_blank(),
                      legend.title = element_blank(), 
                      legend.key = element_rect(fill="gray97", color = "black"), 
                      legend.text = element_text(face="bold", size = 14),
                      axis.title=element_text(size=14,face="bold")) +    
  scale_color_manual(values = c("deeppink", "springgreen4", "purple"), labels=c("PLS-DA", "RF", "SVM")) #+ #lable colours are ins right order (alphabetical)! is double checked
# labs(title=paste("Feature Selection Models",species_code679)) 

ROC679 <- ROC679 + 
  annotate("text", x = .75, y = .20, color="deeppink", size = 6,
           label = paste("AUC =", round(calc_auc(ROC679)$AUC, 2)[1])) +
  annotate("text", x = .75, y = .17, color="springgreen4", size = 6,
           label = paste("AUC =", round(calc_auc(ROC679)$AUC, 2)[2])) +
  annotate("text", x = .75, y = .14, color="purple", size = 6,
           label = paste("AUC =", round(calc_auc(ROC679)$AUC, 2)[3]))

ROC679

# pdflayout<-marrangeGrob(plotlist, ncol=2, nrow=2, left=2, vp=viewport(width=0.93, height=0.95))
# ggsave("./VascularPlants_MacBeSSt/Results_Manuscript/ggplotROC679.pdf", plot=pdflayout, device = "pdf",  height = 210, width = 297, units = "mm", dpi=300)



species_code<-"Brarut"

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
  geom_roc(n.cuts=5, labelsize = 2) + style_roc() + theme(legend.position = c(0.9, 0.2), 
                                                          legend.background = element_blank(),
                                                          legend.title = element_blank(), 
                                                          legend.key = element_rect(fill="gray97", color = "black"), 
                                                          legend.text = element_text(face="bold", size = 14),
                                                          axis.title=element_text(size=14,face="bold")) +
  scale_color_manual(values = c("deeppink", "springgreen4", "purple"), labels=c("PLS-DA", "RF", "SVM")) # + #lable colours are ins right order (alphabetical)! is double checked
#labs(title=paste("Feature Selection Models",species_code)) 

plot<- plot + annotate("text", x = .75, y = .20, color="deeppink", size = 6,
                       label = paste("AUC =", round(calc_auc(plot)$AUC, 2)[1])) +
  annotate("text", x = .75, y = .17, color="springgreen4", size = 6,
           label = paste("AUC =", round(calc_auc(plot)$AUC, 2)[2])) +
  annotate("text", x = .75, y = .14, color="purple", size = 6,
           label = paste("AUC =", round(calc_auc(plot)$AUC, 2)[3]))

# store plots in list to save as PDF
plotlist<-vector(mode="list",length = 2)
names(plotlist)<- c("POAPRA", "Brarut")

plotlist[["POAPRA"]]<-ROC679  
plotlist[["Brarut"]]<-plot


# pdflayout<-marrangeGrob(plotlist, ncol=2, nrow=1, left=2, vp=viewport(width=0.93, height=0.95))
# marrangeGrob(plotlist, ncol=2, nrow=1, left=2, vp=viewport(width=0.93, height=0.95), top = "")
# ggsave("ROC520.pdf", plot=pdflayout, device = "pdf",  height = 210, width = 297, units = "mm", dpi=300)

grid.arrange(ROC679, plot, ncol = 2, nrow = 1, bottom = c("a", "b"), padding = unit(0.5, "line"))


