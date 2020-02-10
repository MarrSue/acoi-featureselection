rm(list = objects())
# plot ROC curves

library(gplots)
# library(ggplot2)
# library(gridExtra)
# library(rlist)
# library(VennDiagram)

# ---- load models ----
setwd("L:/Uni Skripte/PhD Project/Paper/Feature_Selection_In_EcoMetabolomics/originalData/")

# load MetaData
load (file = "MetaData.rda")
# load Models
# load (file = "ANOVA.rda")
load (file = "PLS.rda")
load (file = "SVM.rda")
load (file = "RF.rda")

# direct heatmap without function inclusion
# pdf("HeatmapRFVascularPlants.pdf", paper = "a4r", width=11, height=8)
# sel_list <-  as.data.frame(feat_list679[is.element(feat_class679, c("ANTODO","POAPRA", "GERPRA", "CENJAC")),c(sort(as.character(unique(unlist(sel_rf679))))), drop=FALSE])


# ---- function heatmap
# ---------- Draw heatmap of selected features ----------
f.heatmap.selected_features <- function(feat_list, sel_feat, filename, main, metadata) {
  # Data frame with only selected features
  sel_list <-  as.data.frame(feat_list[,c(sort(as.character(unique(unlist(sel_feat))))), drop=FALSE])  
  # Draw heatmap
  # if (! is.null(filename)) pdf(file=as.character(filename), encoding="ISOLatin1", pointsize=10, width=10, height=10, family="Helvetica")
  heatmap.2(x=as.matrix(sel_list), scale="col", cexRow=0.06, cexCol=0.1, main=main, dendrogram = "col",
            # Rowv=as.dendrogram(hclust(dist(scale(sel_list),method="maximum"),method="ward.D"), center=T), offsetRow=0, #<< samples need to be sorted by SpecCode (not clustered)
            # Rowv=as.dendrogram(hclust(dist(as.numeric(as.factor(mymetadata$SpecCode))),method="ward.D"), center=T), offsetRow=0, #<< samples need to be sorted by SpecCode (not clustered)
            Rowv=as.dendrogram(hclust(dist(as.numeric(as.factor(metadata))),method="ward.D"), center=T), offsetRow=0, #<< samples need to be sorted by SpecCode (not clustered)
            Colv=as.dendrogram(hclust(dist(t(scale(sel_list)),method="maximum"),method="ward.D"), center=T), offsetCol=0,
            col=colorRampPalette(c('darkblue','white','darkred'), alpha=0.1, bias=1)(256),
            trace="none", margins=c(4,8.5),
            key=TRUE, key.title="Color key")
  #if (! is.null(filename))
  # dev.off()
}


# # # #

jpeg(filename = "HeatMapRFVascularPlants.jpg", width = 2000, height = 2000, units = "px", pointsize = 20,
     quality = 100);


sel_list679 <-  as.data.frame(feat_list679[,c(sort(as.character(unique(unlist(sel_rf679))))), drop=FALSE])
hm679 <- heatmap.2(x=as.matrix(sel_list679), 
                   dendrogram = "col",
                   scale="col",
                   cexCol=0.5,
                   main= NA,# "Random Forest - MTBLS679",
                   Rowv=as.dendrogram(hclust(dist(as.numeric(as.factor(feat_class679))),method="ward.D"), center=T), #<< samples need to be sorted by SpecCode (not clustered)
                   offsetRow=0, 
                   Colv=as.dendrogram(hclust(dist(t(scale(sel_list679)),method="maximum"),method="ward.D"), center=T), 
                   offsetCol=0,
                   col=colorRampPalette(c('darkblue','white','darkred'), alpha=0.1, bias=1)(300),
                   trace="none", 
                   margins=c(5,2), 
                   key=TRUE, 
                   key.title="Color key",
                   key.xlab = ,
                   key.ylab = ,
                   xlab = "RF selected features",
                   ylab = "samples",
                   labRow = "",  #levels(feat_class679), #c("A","B","C","D","E","F", "M", "J", "S")
                   cexRow = 1,#3,
                   srtRow = 90,
                   adjRow=c(0, 1))
dev.off()

# 
# sel_list520 <-  as.data.frame(feat_list520[,c(sort(as.character(unique(unlist(sel_rf520))))), drop=FALSE])
# hm520 <- heatmap.2(x=as.matrix(sel_list520), 
#                    dendrogram = "col",
#                    scale="col", 
#                    cexCol=0.1,
#                    main="Random Forest - MTBLS520",
#                    Rowv=as.dendrogram(hclust(dist(as.numeric(as.factor(feat_class520))),method="ward.D"), center=T), #<< samples need to be sorted by SpecCode (not clustered)
#                    offsetRow=0, 
#                    Colv=as.dendrogram(hclust(dist(t(scale(sel_list520)),method="maximum"),method="ward.D"), center=T), 
#                    offsetCol=0,col=colorRampPalette(c('darkblue','white','darkred'), alpha=0.1, bias=1)(300),
#                    trace="none", 
#                    margins=c(4,5.5), 
#                    key=TRUE, 
#                    key.title="Color key",
#                    cexRow=0.06,)
# 




# dev.off()

# # store plots in list to save as PDF
# plotlistHM<-vector(mode="list",length = 2)
# names(plotlistHM)<- c("HM679", "HM520")
# library("ggplotify")
# plotlistHM[["HM679"]]<-as.grob(hm679)  
# HM <-as.grob(hm679)  
# plotlistHM[["HM520"]]<-hm520
# 
# 
# # pdflayout<-marrangeGrob(plotlist, ncol=2, nrow=1, left=2, vp=viewport(width=0.93, height=0.95))
# marrangeGrob(plotlistHM, ncol=2, nrow=1, left=2, vp=viewport(width=0.93, height=0.95))
# marrangeGrob(hm679, ncol=2, nrow=1, left=2, vp=viewport(width=0.93, height=0.95))
# 


# ---- save all heatmaps in one PDF ----

pdf("Heatmaps.pdf", paper = "a4r", width=11, height=8)
#print(paste("Number of selected features:", f.count.selected_features(sel_feat=sel_pls679)))
f.heatmap.selected_features(feat_list=feat_list679, sel_feat=sel_pls679, 
                            main="PLS-DA Vascular Plants", metadata = feat_class679)
f.heatmap.selected_features(feat_list=feat_list679, sel_feat=sel_svm679, 
                            main="SVM Vascular Plants", metadata = feat_class679)
f.heatmap.selected_features(feat_list=feat_list679, sel_feat=sel_rf679, 
                            main="RF Vascular Plants", metadata = feat_class679)

f.heatmap.selected_features(feat_list=feat_list520, sel_feat=sel_pls520,
                            main="PLS-DA Bryophytes", metadata = feat_class520)
f.heatmap.selected_features(feat_list=feat_list520, sel_feat=sel_svm520,
                            main="SVM Bryophytes", metadata = feat_class520)
f.heatmap.selected_features(feat_list=feat_list520, sel_feat=sel_rf520,
                            main="RF Bryophytes", metadata = feat_class520)


dev.off()



















































