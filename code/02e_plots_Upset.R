source('./source/libs.R')
source('./source/libraries.R')
source('./source/functions.R')
library(splitstackshape)

studies <- readRDS('./data/studies.Rds')


# data preaparation for plot ----------------------------------------------

vol_indices01 <- studies[, .(id, timeseries_eval)]
cat_indices01 <- studies[, .(id, categ_eval)]


vol_indices02 <- cSplit(vol_indices01, "timeseries_eval", ",") #split into multiple columns
cat_indices02 <- cSplit(cat_indices01, "categ_eval", ",")

indices_03 <- vol_indices02[cat_indices02, on = 'id', allow.cartesian=TRUE]

indicescomb04 <- unite(indices_03, newcol, c(timeseries_eval_1, timeseries_eval_2,
                                             timeseries_eval_3, timeseries_eval_4,
                                             timeseries_eval_5, timeseries_eval_6, 
                                             timeseries_eval_7, categ_eval_1, categ_eval_2, 
                                             categ_eval_3, categ_eval_4, categ_eval_5, 
                                             categ_eval_6), remove=TRUE, sep=",")

indices_split05 <- split_tidy2(indicescomb04)

indices_split05$newcol <- factor(indices_split05$newcol, 
                                 levels = c("COR", "RMSE", "RBias", "SD", "POD", "FAR", "CSI", "HSS"))

indices_split06 <- subset(indices_split05, !is.na(newcol))

indices_split06$newcol <- recode(indices_split06$newcol, 
                                 # CSI = "Others",
                                 # RBias = "Others", 
                                 SD = "Others", 
                                 HSS = "Others")

indices_split06$newcol <- factor(indices_split06$newcol, 
                                 levels = c("COR", "RMSE", "RBias", "POD", "FAR", "CSI"))

indices_07 <- indices_split06[!duplicated(indices_split06[ , c("id","newcol")]),]

indices_binary <- with(indices_07, table(id,newcol))
indices_binary <- with(indices_split06, table(id,newcol))


# Plot --------------------------------------------------------------------


m = make_comb_mat(indices_binary)
UpSet(m)

#upset(m, order.by = "freq")
#UpSet(m, comb_order = order(comb_size(m)))

m <- m[, 8:1]
ss = set_size(m)
cs = comb_size(m)
ht = UpSet(m, pt_size = unit(5, "mm"), #dot size
           set_order = order(-ss),
           comb_order = order(-comb_size(m), -(cs)),
           top_annotation = HeatmapAnnotation(
             "Indices Intersections" = anno_barplot(cs, 
                                                  ylim = c(0, max(cs)*1.1),
                                                  border = FALSE, 
                                                  gp = gpar(fill = "black"), 
                                                  height = unit(4, "cm")
             ), 
             annotation_name_side = "left", 
             annotation_name_rot = 90),
           left_annotation = rowAnnotation(
             "Studies Per Indices" = anno_barplot(-ss, 
                                               baseline = 0,
                                               axis_param = list(
                                                 at = c(0, -20, -40, -60, -80),
                                                 labels = c(0, 20, 40, 60, 80),
                                                 labels_rot = 0),
                                               border = FALSE, 
                                               gp = gpar(fill = "black"), 
                                               width = unit(4, "cm")
             ),
             set_name = anno_text(set_name(m), 
                                  location = 0.5, 
                                  just = "center",
                                  width = max_text_width(set_name(m)) + unit(4, "mm"))
           ), 
           right_annotation = NULL,
           show_row_names = FALSE)
ht = draw(ht)
od = column_order(ht)
decorate_annotation("Indices Intersections", {
  grid.text(cs[od], x = seq_along(cs), y = unit(cs[od], "native") + unit(2, "pt"), 
            default.units = "native", just = c("left", "bottom"), 
            gp = gpar(fontsize = 8, col = "#000000"), rot = 45)
})

