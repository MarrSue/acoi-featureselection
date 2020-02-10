rm(list = objects())
# plot ROC curves

library(plotROC)
library(ggplot2)
library("gridExtra")
library(rlist)
library("grid")

# ---- load models ----
setwd("L:/Uni Skripte/PhD Project/Paper/Feature_Selection_In_EcoMetabolomics/originalData/")

# load MetaData
load (file = "MetaData.rda")
# load Models
# load (file = "ANOVA.rda")
load (file = "PLS.rda")
load (file = "SVM.rda")
load (file = "RF.rda")


# ---------- Comparison charts ----------
# Draw Venn diagram
library(VennDiagram)

# load(file = "Venn.rda")
# model list MTBLS679
model_list679 <- list()
# model_list[["ANOVA"]] <- as.character(unique(unlist(sel_aov679)))
model_list679[["PLS-DA"]] <- as.character(unlist(sel_pls679)) #as.character(unique(unlist(sel_pls679)))
model_list679[["RF"]] <- as.character(unlist(sel_rf679)) #as.character(unique(unlist(sel_rf679)))
model_list679[["SVM"]] <- as.character(unlist(sel_svm679)) #as.character(unique(unlist(sel_svm679)))

#venn(data=model_list, intersections=TRUE, show.plot=TRUE)
pdf("./VascularPlants_MacBeSSt/Results_Manuscript/VennComparison679.pdf", paper = "a4r", width=11, height=8)
# model_venn679 <- venn.diagram(x=model_list, filename=NULL, col="transparent", fill=rainbow(n=4),
#                               main="Feature Selection Models Vascular Plants",
#                               alpha=0.5, cex=1.0, cat.cex=1.0, cat.pos=0.1, cat.dist=c(0.1,0.1,0.04,0.03),#cat.dist=c(0.1,0.1,0.04,0.03,0.1),
#                               cat.fontface="bold", rotation.degree=0, margin=c(0,0,0,0),
#                               cat.fontfamily="Helvetica", fontfamily="Helvetica")

model_venn679 <- venn.diagram(x=model_list679, filename=NULL, col="transparent",  fill= c("deeppink", "springgreen4", "purple"), #rainbow(n=4),
                              main= "a", main.pos = c(1,0.1), main.cex = 2, #"Feature Selection Models Vascular Plants",
                              alpha=0.6, cex=2.0, cat.cex=2.0, cat.pos=c(- 25, 25, 0.1), cat.dist=c(-0.04,-0.04,-0.36),#cat.dist=c(0.1,0.1,0.04,0.03,0.1),
                              cat.fontface="bold", rotation.degree=0, margin=c(0,0,0,0),
                              cat.fontfamily="Helvetica", fontfamily="Helvetica", force.unique = FALSE)


grid.newpage()
grid.draw(model_venn679)
# mtext(text="Comparison of Total Feature Selection Models", adj=0.5, line=2, font=3, cex=1.2)
dev.off()



# model list MTBLS520
model_list520 <- list()
# model_list[["ANOVA"]] <- as.character(unique(unlist(sel_aov520)))
model_list520[["PLS-DA"]] <- as.character(unique(unlist(sel_pls520)))
model_list520[["RF"]] <- as.character(unique(unlist(sel_rf520)))
model_list520[["SVM"]] <- as.character(unique(unlist(sel_svm520)))

#venn(data=model_list, intersections=TRUE, show.plot=TRUE)
pdf("./Bryophytes_KPeters/Results_Manuscript/VennComparison520.pdf", paper = "a4r", width=11, height=8)
# model_venn520 <- venn.diagram(x=model_list, filename=NULL, col="transparent", fill=rainbow(n=4),
#                               main="Feature Selection Models Bryophytes",
#                               alpha=0.5, cex=1.0, cat.cex=1.0, cat.pos=0.1, cat.dist=c(0.1,0.1,0.04,0.03),#cat.dist=c(0.1,0.1,0.04,0.03,0.1),
#                               cat.fontface="bold", rotation.degree=0, margin=c(0,0,0,0),
#                               cat.fontfamily="Helvetica", fontfamily="Helvetica")

model_venn520 <- venn.diagram(x=model_list520, filename=NULL, col="transparent",  fill= c("deeppink", "springgreen4", "purple"), #rainbow(n=4),
                              main= "b", main.pos = c(1,0), main.cex = 2,# Feature Selection Models Bryophytes",
                              alpha=0.6, cex=2.0, cat.cex=2.0, cat.pos=c(20, -20, 0), cat.dist=c(-0.05, -0.05,-0.49),#cat.dist=c(0.1,0.1,0.04,0.03,0.1),
                              cat.fontface="bold", rotation.degree=180, margin=c(0,0,0,0),
                              cat.fontfamily="Helvetica", fontfamily="Helvetica", force.unique = FALSE)

grid.newpage()
grid.draw(model_venn520)
# mtext(text="Comparison of total feature selection models", adj=0.5, line=2, font=3, cex=1.2)
dev.off()


save(model_venn679, model_venn520, file = "Venn.rda")


# ---- plot Venn Digramms in one panel


library(gridExtra)
grid.arrange(gTree(children=model_venn679), gTree(children=model_venn520), ncol=2)



# ---- plot one species wise ----

# create lists
mListANTODO <- list()
mListAVEPUB <- list()
mListCENJAC <- list()
mListDACGLO <- list()
mListFESRUB <- list()
mListGERPRA <- list()
mListHOLLAN <- list()
mListKNAARV <- list()
mListLEUVUL <- list()
mListPHLPRA <- list()
mListPLALAN <- list()
mListPOAPRA <- list()
mListRANACR <- list()

mListBrarut <- list()
mListCalcus <- list()
mListFistax <- list()
mListGripul <- list()
mListHypcup <- list()
mListMarpol <- list()
mListPlaund <- list()
mListPolstr <- list()
mListRhysqu <- list()

# ---- subset models in each list per species
mListANTODO[["PLS"]] <- as.character(unlist(sel_pls679$ANTODO)) #as.character(unique(unlist(sel_pls679)))
mListANTODO[["RF"]]  <- as.character(unlist(sel_rf679$ANTODO)) #as.character(unique(unlist(sel_rf679)))
mListANTODO[["SVM"]] <- as.character(unlist(sel_svm679$ANTODO)) #as.character(unique(unlist(sel_svm679)))

mListAVEPUB[["PLS"]] <- as.character(unlist(sel_pls679$AVEPUB)) #as.character(unique(unlist(sel_pls679)))
mListAVEPUB[["RF"]]  <- as.character(unlist(sel_rf679$AVEPUB)) #as.character(unique(unlist(sel_rf679)))
mListAVEPUB[["SVM"]] <- as.character(unlist(sel_svm679$AVEPUB)) #as.character(unique(unlist(sel_svm679)))

mListCENJAC[["PLS"]] <- as.character(unlist(sel_pls679$CENJAC)) #as.character(unique(unlist(sel_pls679)))
mListCENJAC[["RF"]]  <- as.character(unlist(sel_rf679$CENJAC)) #as.character(unique(unlist(sel_rf679)))
mListCENJAC[["SVM"]] <- as.character(unlist(sel_svm679$CENJAC)) #as.character(unique(unlist(sel_svm679)))

mListDACGLO[["PLS"]] <- as.character(unlist(sel_pls679$DACGLO)) #as.character(unique(unlist(sel_pls679)))
mListDACGLO[["RF"]]  <- as.character(unlist(sel_rf679$DACGLO)) #as.character(unique(unlist(sel_rf679)))
mListDACGLO[["SVM"]] <- as.character(unlist(sel_svm679$DACGLO)) #as.character(unique(unlist(sel_svm679)))

mListFESRUB[["PLS"]] <- as.character(unlist(sel_pls679$FESRUB)) #as.character(unique(unlist(sel_pls679)))
mListFESRUB[["RF"]]  <- as.character(unlist(sel_rf679$FESRUB)) #as.character(unique(unlist(sel_rf679)))
mListFESRUB[["SVM"]] <- as.character(unlist(sel_svm679$FESRUB)) #as.character(unique(unlist(sel_svm679)))

mListGERPRA[["PLS"]] <- as.character(unlist(sel_pls679$GERPRA)) #as.character(unique(unlist(sel_pls679)))
mListGERPRA[["RF"]]  <- as.character(unlist(sel_rf679$GERPRA)) #as.character(unique(unlist(sel_rf679)))
mListGERPRA[["SVM"]] <- as.character(unlist(sel_svm679$GERPRA)) #as.character(unique(unlist(sel_svm679)))

mListHOLLAN[["PLS"]] <- as.character(unlist(sel_pls679$HOLLAN)) #as.character(unique(unlist(sel_pls679)))
mListHOLLAN[["RF"]]  <- as.character(unlist(sel_rf679$HOLLAN)) #as.character(unique(unlist(sel_rf679)))
mListHOLLAN[["SVM"]] <- as.character(unlist(sel_svm679$HOLLAN)) #as.character(unique(unlist(sel_svm679)))

mListKNAARV[["PLS"]] <- as.character(unlist(sel_pls679$KNAARV)) #as.character(unique(unlist(sel_pls679)))
mListKNAARV[["RF"]]  <- as.character(unlist(sel_rf679$KNAARV)) #as.character(unique(unlist(sel_rf679)))
mListKNAARV[["SVM"]] <- as.character(unlist(sel_svm679$KNAARV)) #as.character(unique(unlist(sel_svm679)))

mListLEUVUL[["PLS"]] <- as.character(unlist(sel_pls679$LEUVUL)) #as.character(unique(unlist(sel_pls679)))
mListLEUVUL[["RF"]]  <- as.character(unlist(sel_rf679$LEUVUL)) #as.character(unique(unlist(sel_rf679)))
mListLEUVUL[["SVM"]] <- as.character(unlist(sel_svm679$LEUVUL)) #as.character(unique(unlist(sel_svm679)))

mListPHLPRA[["PLS"]] <- as.character(unlist(sel_pls679$PHLPRA)) #as.character(unique(unlist(sel_pls679)))
mListPHLPRA[["RF"]]  <- as.character(unlist(sel_rf679$PHLPRA)) #as.character(unique(unlist(sel_rf679)))
mListPHLPRA[["SVM"]] <- as.character(unlist(sel_svm679$PHLPRA)) #as.character(unique(unlist(sel_svm679)))

mListPLALAN[["PLS"]] <- as.character(unlist(sel_pls679$PLALAN)) #as.character(unique(unlist(sel_pls679)))
mListPLALAN[["RF"]]  <- as.character(unlist(sel_rf679$PLALAN)) #as.character(unique(unlist(sel_rf679)))
mListPLALAN[["SVM"]] <- as.character(unlist(sel_svm679$PLALAN)) #as.character(unique(unlist(sel_svm679)))

mListPOAPRA[["PLS"]] <- as.character(unlist(sel_pls679$POAPRA)) #as.character(unique(unlist(sel_pls679)))
mListPOAPRA[["RF"]]  <- as.character(unlist(sel_rf679$POAPRA)) #as.character(unique(unlist(sel_rf679)))
mListPOAPRA[["SVM"]] <- as.character(unlist(sel_svm679$POAPRA)) #as.character(unique(unlist(sel_svm679)))

mListRANACR[["PLS"]] <- as.character(unlist(sel_pls679$RANACR)) #as.character(unique(unlist(sel_pls679)))
mListRANACR[["RF"]]  <- as.character(unlist(sel_rf679$RANACR)) #as.character(unique(unlist(sel_rf679)))
mListRANACR[["SVM"]] <- as.character(unlist(sel_svm679$RANACR)) #as.character(unique(unlist(sel_svm679)))


mListBrarut[["PLS"]] <- as.character(unlist(sel_pls520$Brarut)) #as.character(unique(unlist(sel_pls679)))
mListBrarut[["RF"]]  <- as.character(unlist(sel_rf520$Brarut)) #as.character(unique(unlist(sel_rf679)))
mListBrarut[["SVM"]] <- as.character(unlist(sel_svm520$Brarut)) #as.character(unique(unlist(sel_svm679)))

mListCalcus[["PLS"]] <- as.character(unlist(sel_pls520$Calcus)) #as.character(unique(unlist(sel_pls679)))
mListCalcus[["RF"]]  <- as.character(unlist(sel_rf520$Calcus)) #as.character(unique(unlist(sel_rf679)))
mListCalcus[["SVM"]] <- as.character(unlist(sel_svm520$Calcus)) #as.character(unique(unlist(sel_svm679)))

mListFistax[["PLS"]] <- as.character(unlist(sel_pls520$Fistax)) #as.character(unique(unlist(sel_pls679)))
mListFistax[["RF"]]  <- as.character(unlist(sel_rf520$Fistax)) #as.character(unique(unlist(sel_rf679)))
mListFistax[["SVM"]] <- as.character(unlist(sel_svm520$Fistax)) #as.character(unique(unlist(sel_svm679)))

mListGripul[["PLS"]] <- as.character(unlist(sel_pls520$Gripul)) #as.character(unique(unlist(sel_pls679)))
mListGripul[["RF"]]  <- as.character(unlist(sel_rf520$Gripul)) #as.character(unique(unlist(sel_rf679)))
mListGripul[["SVM"]] <- as.character(unlist(sel_svm520$Gripul)) #as.character(unique(unlist(sel_svm679)))

mListHypcup[["PLS"]] <- as.character(unlist(sel_pls520$Hypcup)) #as.character(unique(unlist(sel_pls679)))
mListHypcup[["RF"]]  <- as.character(unlist(sel_rf520$Hypcup)) #as.character(unique(unlist(sel_rf679)))
mListHypcup[["SVM"]] <- as.character(unlist(sel_svm520$Hypcup)) #as.character(unique(unlist(sel_svm679)))

mListMarpol[["PLS"]] <- as.character(unlist(sel_pls520$Marpol)) #as.character(unique(unlist(sel_pls679)))
mListMarpol[["RF"]]  <- as.character(unlist(sel_rf520$Marpol)) #as.character(unique(unlist(sel_rf679)))
mListMarpol[["SVM"]] <- as.character(unlist(sel_svm520$Marpol)) #as.character(unique(unlist(sel_svm679)))

mListPlaund[["PLS"]] <- as.character(unlist(sel_pls520$Plaund)) #as.character(unique(unlist(sel_pls679)))
mListPlaund[["RF"]]  <- as.character(unlist(sel_rf520$Plaund)) #as.character(unique(unlist(sel_rf679)))
mListPlaund[["SVM"]] <- as.character(unlist(sel_svm520$Plaund)) #as.character(unique(unlist(sel_svm679)))

mListPolstr[["PLS"]] <- as.character(unlist(sel_pls520$Polstr)) #as.character(unique(unlist(sel_pls679)))
mListPolstr[["RF"]]  <- as.character(unlist(sel_rf520$Polstr)) #as.character(unique(unlist(sel_rf679)))
mListPolstr[["SVM"]] <- as.character(unlist(sel_svm520$Polstr)) #as.character(unique(unlist(sel_svm679)))

mListRhysqu[["PLS"]] <- as.character(unlist(sel_pls520$Rhysqu)) #as.character(unique(unlist(sel_pls679)))
mListRhysqu[["RF"]]  <- as.character(unlist(sel_rf520$Rhysqu)) #as.character(unique(unlist(sel_rf679)))
mListRhysqu[["SVM"]] <- as.character(unlist(sel_svm520$Rhysqu)) #as.character(unique(unlist(sel_svm679)))




# ---- create species specific Venn MTBLS679 ----
VennANTODO <- venn.diagram(x=mListANTODO, filename=NULL, col="transparent",  fill= c("deeppink", "springgreen4", "purple"), #rainbow(n=4),
                              main="ANTODO", height = 2, width = 2, imagetype = "pdf", units = "cm", borders = TRUE,
                              alpha=0.6, cex=1.0, cat.cex=1.0, cat.pos=c(-35, 35, 0), cat.dist=c(0.13,0.13,-0.6),#cat.dist=c(0.1,0.1,0.04,0.03,0.1),
                              cat.fontface="bold", rotation.degree=0, margin=c(0.2,0.2,0.2,0.2),
                              cat.fontfamily="Helvetica", fontfamily="Helvetica", force.unique = FALSE)

VennAVEPUB <- venn.diagram(x=mListAVEPUB, filename=NULL, col="transparent",  fill= c("deeppink", "springgreen4", "purple"), #rainbow(n=4),
                               main="AVEPUB", height = 2, width = 2, imagetype = "pdf", units = "cm",
                               alpha=0.6, cex=1.0, cat.cex=1.0, cat.pos=c(- 25, 25, 0.1), cat.dist=c(0.13,0.13,-0.4),#cat.dist=c(0.1,0.1,0.04,0.03,0.1),
                               cat.fontface="bold", rotation.degree=0, margin=c(0.2,0.2,0.2,0.2),
                               cat.fontfamily="Helvetica", fontfamily="Helvetica", force.unique = FALSE)

VennCENJAC <- venn.diagram(x=mListCENJAC, filename=NULL, col="transparent",  fill= c("deeppink", "springgreen4", "purple"), #rainbow(n=4),
                         main="CENJAC", height = 2, width = 2,
                         alpha=0.6, cex=1.0, cat.cex=1.0, cat.pos=c(-60, 25, 0.1), cat.dist=c(0.13,0.1,-0.1),#cat.dist=c(0.1,0.1,0.04,0.03,0.1),
                         cat.fontface="bold", rotation.degree=0, margin=c(0.2,0.2,0.2,0.2),
                         cat.fontfamily="Helvetica", fontfamily="Helvetica", force.unique = FALSE)

VennDACGLO <- venn.diagram(x=mListDACGLO, filename=NULL, col="transparent",  fill= c("deeppink", "springgreen4", "purple"), #rainbow(n=4),
                         main="DACGLO", height = 2, width = 2,
                         alpha=0.6, cex=1.0, cat.cex=1.0, cat.pos=c(-40, 25, 0.1), cat.dist=c(0.1,0.13,-.1),#cat.dist=c(0.1,0.1,0.04,0.03,0.1),
                         cat.fontface="bold", rotation.degree=0, margin=c(0.2,0.2,0.2,0.2),
                         cat.fontfamily="Helvetica", fontfamily="Helvetica", force.unique = FALSE)

VennFESRUB <- venn.diagram(x=mListFESRUB, filename=NULL, col="transparent",  fill= c("deeppink", "springgreen4", "purple"), #rainbow(n=4),
                         main="FESRUB", height = 2, width = 2,
                         alpha=0.6, cex=1.0, cat.cex=1.0, cat.pos=c(-30, 30, 0.1), cat.dist=c(0.1,0.1,-0.1),#cat.dist=c(0.1,0.1,0.04,0.03,0.1),
                         cat.fontface="bold", rotation.degree=0, margin=c(0.2,0.2,0.2,0.2),
                         cat.fontfamily="Helvetica", fontfamily="Helvetica", force.unique = FALSE)

VennGERPRA <- venn.diagram(x=mListGERPRA, filename=NULL, col="transparent",  fill= c("deeppink", "springgreen4", "purple"), #rainbow(n=4),
                         main="GERPRA", height = 2, width = 2,
                         alpha=0.6, cex=1.0, cat.cex=1.0, cat.pos=c(-45, 25, 0.1), cat.dist=c(0.12,0.13,-0.1),#cat.dist=c(0.1,0.1,0.04,0.03,0.1),
                         cat.fontface="bold", rotation.degree=0, margin=c(0.2,0.2,0.2,0.2),
                         cat.fontfamily="Helvetica", fontfamily="Helvetica", force.unique = FALSE)

VennHOLLAN <- venn.diagram(x=mListHOLLAN, filename=NULL, col="transparent",  fill= c("deeppink", "springgreen4", "purple"), #rainbow(n=4),
                         main="HOLLAN", height = 2, width = 2,
                         alpha=0.6, cex=1.0, cat.cex=1.0, cat.pos=c(0, 35, 0), cat.dist=c(0.13,0.21,-0.15),#cat.dist=c(0.1,0.1,0.04,0.03,0.1),
                         cat.fontface="bold", rotation.degree=0, margin=c(0.2,0.2,0.2,0.2),
                         cat.fontfamily="Helvetica", fontfamily="Helvetica", force.unique = FALSE)

VennKNAARV <- venn.diagram(x=mListKNAARV, filename=NULL, col="transparent",  fill= c("deeppink", "springgreen4", "purple"), #rainbow(n=4),
                         main="KNAARV", height = 2, width = 2,
                         alpha=0.6, cex=1.0, cat.cex=1.0, cat.pos=c(0.2, 35, 0), cat.dist=c(0.13,0.23,-0.1),#cat.dist=c(0.1,0.1,0.04,0.03,0.1),
                         cat.fontface="bold", rotation.degree=0, margin=c(0.2,0.2,0.2,0.2),
                         cat.fontfamily="Helvetica", fontfamily="Helvetica", force.unique = FALSE)

VennLEUVUL <- venn.diagram(x=mListLEUVUL, filename=NULL, col="transparent",  fill= c("deeppink", "springgreen4", "purple"), #rainbow(n=4),
                         main="LEUVUL", height = 2, width = 2,
                         alpha=0.6, cex=1.0, cat.cex=1.0, cat.pos=c(-0.2, 35, 0), cat.dist=c(0.13,0.19,-0.1),#cat.dist=c(0.1,0.1,0.04,0.03,0.1),
                         cat.fontface="bold", rotation.degree=0, margin=c(0.2,0.2,0.2,0.2),
                         cat.fontfamily="Helvetica", fontfamily="Helvetica", force.unique = FALSE)

VennPHLPRA <- venn.diagram(x=mListPHLPRA, filename=NULL, col="transparent",  fill= c("deeppink", "springgreen4", "purple"), #rainbow(n=4),
                         main="PHLPRA", height = 2, width = 2,
                         alpha=0.6, cex=1.0, cat.cex=1.0, cat.pos=c(0, -0.1, 0), cat.dist=c(0.1, 0.05, -0.1),#cat.dist=c(0.1,0.1,0.04,0.03,0.1),
                         cat.fontface="bold", rotation.degree=0, margin=c(0.2,0.2,0.2,0.2),
                         cat.fontfamily="Helvetica", fontfamily="Helvetica", force.unique = FALSE)

VennPLALAN <- venn.diagram(x=mListPLALAN, filename=NULL, col="transparent",  fill= c("deeppink", "springgreen4", "purple"), #rainbow(n=4),
                         main="PLALAN", height = 2, width = 2,
                         alpha=0.6, cex=1.0, cat.cex=1.0, cat.pos=c(-20, 0, 20), cat.dist=c(-0.05,-0.05,-0.1),#cat.dist=c(0.1,0.1,0.04,0.03,0.1),
                         cat.fontface="bold", rotation.degree=0, margin=c(0.2,0.2,0.2,0.2),
                         cat.fontfamily="Helvetica", fontfamily="Helvetica", force.unique = FALSE)

VennPOAPRA <- venn.diagram(x=mListPOAPRA, filename=NULL, col="transparent",  fill= c("deeppink", "springgreen4", "purple"), #rainbow(n=4),
                         main="POAPRA", height = 2, width = 2,
                         alpha=0.6, cex=1.0, cat.cex=1.0, cat.pos=c(110, -100, 0), cat.dist=c(-0.16,-0.1, -0.1),#cat.dist=c(0.1,0.1,0.04,0.03,0.1),
                         cat.fontface="bold", rotation.degree=0, margin=c(0.2,0.2,0.2,0.2),
                         cat.fontfamily="Helvetica", fontfamily="Helvetica", force.unique = FALSE)

VennRANACR <- venn.diagram(x=mListRANACR, filename=NULL, col="transparent",  fill= c("deeppink", "springgreen4", "purple"), #rainbow(n=4),
                         main="RANACR", height = 2, width = 2,
                         alpha=0.6, cex=1.0, cat.cex=1.0, cat.pos=c(-20, 0, 20), cat.dist=c(-0.05,-0.05,-0.1),#cat.dist=c(0.1,0.1,0.04,0.03,0.1),
                         cat.fontface="bold", rotation.degree=0, margin=c(0.2,0.2,0.2,0.2),
                         cat.fontfamily="Helvetica", fontfamily="Helvetica", force.unique = FALSE)



# grid.newpage()
# grid.draw(VennPLALAN)
library(gridExtra)

#jpeg(filename = "VennVascularPlants.jpg", width = 2000, height = 2000, units = "px", pointsize = 12,
#     quality = 100);
# ---- plot as PDF
pdf(file = "VennVascularPlants.pdf", paper = "a4", width = 8, height = 10)
grid.arrange(gTree(children=VennANTODO), 
             gTree(children=VennAVEPUB), 
             gTree(children=VennCENJAC), 
             gTree(children=VennDACGLO), 
             gTree(children=VennFESRUB), 
             gTree(children=VennGERPRA), 
             gTree(children=VennHOLLAN), 
             gTree(children=VennKNAARV), 
             gTree(children=VennLEUVUL), 
             gTree(children=VennPHLPRA), 
             gTree(children=VennPLALAN), 
             gTree(children=VennPOAPRA), 
             gTree(children=VennRANACR), 
             ncol = 3, nrow = 5, top = "Venn Diagramms - Vascular Plants")
dev.off()


# ---- species specific Venn MTBLS520 ----
VennBrarut <- venn.diagram(x=mListBrarut, filename=NULL, col="transparent",  fill= c("deeppink", "springgreen4", "purple"), #rainbow(n=4),
                         main="Brarut", height = 2, width = 2,
                         alpha=0.6, cex=0.5, cat.cex=1.0, cat.pos=c(-30, 30, 0), cat.dist=c(0.1,0.1,-0.15),#cat.dist=c(0.1,0.1,0.04,0.03,0.1),
                         cat.fontface="bold", rotation.degree=0, margin=c(0.2,0.2,0.2,0.2),
                         cat.fontfamily="Helvetica", fontfamily="Helvetica", force.unique = FALSE)

VennCalcus <- venn.diagram(x=mListCalcus, filename=NULL, col="transparent",  fill= c("deeppink", "springgreen4", "purple"), #rainbow(n=4),
                         main="Calcus", height = 2, width = 2,
                         alpha=0.6, cex=1.0, cat.cex=1.0, cat.pos=c(0, 0, 0), cat.dist=c(0.1,0.1,-0.1),#cat.dist=c(0.1,0.1,0.04,0.03,0.1),
                         cat.fontface="bold", rotation.degree=0, margin=c(0.2,0.2,0.2,0.2),
                         cat.fontfamily="Helvetica", fontfamily="Helvetica", force.unique = FALSE)

VennFistax <- venn.diagram(x=mListFistax, filename=NULL, col="transparent",  fill= c("deeppink", "springgreen4", "purple"), #rainbow(n=4),
                         main="Fistax", height = 2, width = 2,
                         alpha=0.6, cex=1.0, cat.cex=1.0, cat.pos=c(0, 0, 0), cat.dist=c(-0.1,0.1,-0.1),#cat.dist=c(0.1,0.1,0.04,0.03,0.1),
                         cat.fontface="bold", rotation.degree=0, margin=c(0.2,0.2,0.2,0.2),
                         cat.fontfamily="Helvetica", fontfamily="Helvetica", force.unique = FALSE)

VennGripul <- venn.diagram(x=mListGripul, filename=NULL, col="transparent",  fill= c("deeppink", "springgreen4", "purple"), #rainbow(n=4),
                         main="Gripul", height = 2, width = 2,
                         alpha=0.6, cex=1.0, cat.cex=1.0, cat.pos=c(-30, 30, 0), cat.dist=c(0.1,0.09,-0.15),#cat.dist=c(0.1,0.1,0.04,0.03,0.1),
                         cat.fontface="bold", rotation.degree=0, margin=c(0.2,0.2,0.2,0.2),
                         cat.fontfamily="Helvetica", fontfamily="Helvetica", force.unique = FALSE)

VennHypcup <- venn.diagram(x=mListHypcup, filename=NULL, col="transparent",  fill= c("deeppink", "springgreen4", "purple"), #rainbow(n=4),
                         main="Hypcup", height = 2, width = 2,
                         alpha=0.6, cex=1.0, cat.cex=1.0, cat.pos=c(0, 0, 0), cat.dist=c(0.1,0.1,-0.1),#cat.dist=c(0.1,0.1,0.04,0.03,0.1),
                         cat.fontface="bold", rotation.degree=0, margin=c(0.2,0.2,0.2,0.2),
                         cat.fontfamily="Helvetica", fontfamily="Helvetica", force.unique = FALSE)

VennMarpol <- venn.diagram(x=mListMarpol, filename=NULL, col="transparent",  fill= c("deeppink", "springgreen4", "purple"), #rainbow(n=4),
                         main="Marpol", height = 2, width = 2,
                         alpha=0.6, cex=1.0, cat.cex=1.0, cat.pos=c(0, 0, 80), cat.dist=c(0.13,0.13,-0.3),#cat.dist=c(0.1,0.1,0.04,0.03,0.1),
                         cat.fontface="bold", rotation.degree=0, margin=c(0.2,0.2,0.2,0.2),
                         cat.fontfamily="Helvetica", fontfamily="Helvetica", force.unique = FALSE)

VennPlaund <- venn.diagram(x=mListPlaund, filename=NULL, col="transparent",  fill= c("deeppink", "springgreen4", "purple"), #rainbow(n=4),
                         main="Plaund", height = 2, width = 2,
                         alpha=0.6, cex=1.0, cat.cex=1.0, cat.pos=c(0,-10, 0), cat.dist=c(0.05,-0.02,-0.1),#cat.dist=c(0.1,0.1,0.04,0.03,0.1),
                         cat.fontface="bold", rotation.degree=0, margin=c(0.2,0.2,0.2,0.2),
                         cat.fontfamily="Helvetica", fontfamily="Helvetica", force.unique = FALSE)

VennPolstr <- venn.diagram(x=mListPolstr, filename=NULL, col="transparent",  fill= c("deeppink", "springgreen4", "purple"), #rainbow(n=4),
                         main="Polstr", height = 2, width = 2,
                         alpha=0.6, cex=0.6, cat.cex=1.0, cat.pos=c(-25, 0, -80), cat.dist=c(0.07,0.1,-0.13),#cat.dist=c(0.1,0.1,0.04,0.03,0.1),
                         cat.fontface="bold", rotation.degree=0, margin=c(0.2,0.2,0.2,0.2),
                         cat.fontfamily="Helvetica", fontfamily="Helvetica", force.unique = FALSE)

VennRhysqu <- venn.diagram(x=mListRhysqu, filename=NULL, col="transparent",  fill= c("deeppink", "springgreen4", "purple"), #rainbow(n=4),
                         main="Rhysqu", height = 2, width = 2, ext.text = TRUE, ext.dist = -0.15, ext.length = 0.9,
                         alpha=0.6, cex= 0.4, cat.cex=1.0, cat.pos=c(- 25, 30, 0.1), cat.dist=c(0.1,0.1,-0.1),#cat.dist=c(0.1,0.1,0.04,0.03,0.1),
                         cat.fontface="bold", rotation.degree=0, margin=c(0.2,0.2,0.2,0.2),
                         cat.fontfamily="Helvetica", fontfamily="Helvetica", force.unique = FALSE)

# ---- plot Venn Digramms in one panel and save as PDF
pdf("VennBryophytes.pdf", paper = "a4", width=8, height=8)
grid.arrange(gTree(children=VennBrarut), 
             gTree(children=VennCalcus), 
             gTree(children=VennFistax), 
             gTree(children=VennGripul), 
             gTree(children=VennHypcup), 
             gTree(children=VennMarpol), 
             gTree(children=VennPlaund), 
             gTree(children=VennPolstr), 
             gTree(children=VennRhysqu), 
             ncol = 3, nrow = 3, top = "Venn Diagramms - Bryophytes")

dev.off()
# ---- stop Cluster & beep when done ----
stopCluster(cl)
beep(2)