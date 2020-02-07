# ---- functions ----
# ---------- Select features from model ----------
f.select_features_from_model <- function(feat_list, model_varimp, feat_class) {
  # Create list
  sel_list <- list()
  
  # Limit selected features between keepx_max and keepx_min
  for (i in unique(feat_class)) {
    elements <- colnames(feat_list)[which(model_varimp[,i] >= 0.95 * max(model_varimp[,i]))]
    
    if (length(elements) > keepx_max) {
      sel_list[[i]] <- colnames(feat_list)[base::order(model_varimp[, i], decreasing=TRUE)[1:keepx_max]]
    } else if (length(elements) < keepx_min) {
      sel_list[[i]] <- colnames(feat_list)[base::order(model_varimp[, i], decreasing=TRUE)[1:keepx_min]]
    } else {
      sel_list[[i]] <- elements
    }
    
    sel_list[[i]] <- sort(sel_list[[i]])
  }
  
  # Return selected features
  return(sel_list)
}



# ---------- number of selected features ----------
f.count.selected_features <- function(sel_feat) {
  # Return number of selected features
  return(length(unlist(sel_feat))) # return(length(unique(unlist(sel_feat))))
}

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


