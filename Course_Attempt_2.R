library(readxl)
vehicles <- read_excel("/Users/fareedkhan/Documents/vehicles.xltx")
head(vehicles)
summary(vehicles)
vehicles_updated = subset(vehicles, select = -c(Samples,Class))

head(vehicles_updated)
summary(vehicles_updated)
View(vehicles_updated)

#No outliers
boxplot(vehicles_updated$Comp)

boxplot.stats(vehicles_updated$Comp)$out



#No outliers
boxplot(vehicles_updated$Circ)

boxplot.stats(vehicles_updated$Circ)$out


#No outliers
boxplot(vehicles_updated$D.Circ)

boxplot.stats(vehicles_updated$D.Circ)$out


#Has outliers
boxplot(vehicles_updated$Rad.Ra)

boxplot.stats(vehicles_updated$Rad.Ra)$out

out <- boxplot.stats(vehicles_updated$Rad.Ra)$out
out_ind <- which(vehicles_updated$Rad.Ra %in% c(out))
out_ind



vehicles_updated[out_ind, ]

boxplot(vehicles_updated$Rad.Ra,
        ylab = "Rad.Ra")
mtext(paste("Outliers: ", paste(out, collapse = ", ")))



boxplot(vehicles_updated$Pr.Axis.Ra)

boxplot.stats(vehicles_updated$Pr.Axis.Ra)$out

out <- boxplot.stats(vehicles_updated$Pr.Axis.Ra)$out
out_ind <- which(vehicles_updated$Pr.Axis.Ra %in% c(out))
out_ind

vehicles_updated[out_ind, ]

boxplot(vehicles_updated$Pr.Axis.Ra,
        ylab = "Pr.Axis.Ra")
mtext(paste("Outliers: ", paste(out, collapse = ", ")))

#Has outliers
boxplot(vehicles_updated$Max.L.Ra)

boxplot.stats(vehicles_updated$Max.L.Ra)$out

out <- boxplot.stats(vehicles_updated$Max.L.Ra)$out
out_ind <- which(vehicles_updated$Max.L.Ra %in% c(out))
out_ind

vehicles_updated[out_ind, ]

boxplot(vehicles_updated$Max.L.Ra,
        ylab = "Max.L.Ra")
mtext(paste("Outliers: ", paste(out, collapse = ", ")))

#No outliers
boxplot(vehicles_updated$Scat.Ra)

boxplot.stats(vehicles_updated$Scat.Ra)$out

#No outliers
boxplot(vehicles_updated$Elong)

boxplot.stats(vehicles_updated$Elong)$out

#No outliers
boxplot(vehicles_updated$Pr.Axis.Rect)

boxplot.stats(vehicles_updated$Pr.Axis.Rect)$out

#No outliers
boxplot(vehicles_updated$Max.L.Rect)

boxplot.stats(vehicles_updated$Max.L.Rect)$out

out <- boxplot.stats(vehicles_updated$Max.L.Rect)$out
out_ind <- which(vehicles_updated$Max.L.Rect %in% c(out))
out_ind

vehicles_updated[out_ind, ]

boxplot(vehicles_updated$Max.L.Rect,
        ylab = "Max.L.Rect")
mtext(paste("Outliers: ", paste(out, collapse = ", ")))

#24 outliers

boxplot(vehicles_updated$Sc.Var.Maxis)

boxplot.stats(vehicles_updated$Sc.Var.Maxis)$out

out <- boxplot.stats(vehicles_updated$Sc.Var.Maxis)$out
out_ind <- which(vehicles_updated$Sc.Var.Maxis %in% c(out))
out_ind

vehicles_updated[out_ind, ]

boxplot(vehicles_updated$Sc.Var.Maxis,
        ylab = "Sc.Var.Maxis")
mtext(paste("Outliers: ", paste(out, collapse = ", ")))

boxplot(vehicles_updated$Sc.Var.maxis)

boxplot.stats(vehicles_updated$Sc.Var.maxis)$out

out <- boxplot.stats(vehicles_updated$Sc.Var.maxis)$out
out_ind <- which(vehicles_updated$Sc.Var.maxis %in% c(out))
out_ind

vehicles_updated[out_ind, ]

boxplot(vehicles_updated$Sc.Var.maxis,
        ylab = "Sc.Var.maxis")
mtext(paste("Outliers: ", paste(out, collapse = ", ")))

boxplot(vehicles_updated$Skew.Maxis)

boxplot.stats(vehicles_updated$Skew.Maxis)$out

out <- boxplot.stats(vehicles_updated$Skew.Maxis)$out
out_ind <- which(vehicles_updated$Skew.Maxis %in% c(out))
out_ind

vehicles_updated[out_ind, ]

boxplot(vehicles_updated$Skew.Maxis,
        ylab = "Skew.Maxis")
mtext(paste("Outliers: ", paste(out, collapse = ", ")))

boxplot(vehicles_updated$Skew.maxis)

boxplot.stats(vehicles_updated$Skew.maxis)$out

out <- boxplot.stats(vehicles_updated$Skew.maxis)$out
out_ind <- which(vehicles_updated$Skew.maxis %in% c(out))
out_ind

vehicles_updated[out_ind, ]

boxplot(vehicles_updated$Skew.maxis,
        ylab = "Skew.maxis")
mtext(paste("Outliers: ", paste(out, collapse = ", ")))

boxplot(vehicles_updated$Kurt.maxis)

boxplot.stats(vehicles_updated$Kurt.maxis)$out

out <- boxplot.stats(vehicles_updated$Kurt.maxis)$out
out_ind <- which(vehicles_updated$Kurt.maxis %in% c(out))
out_ind

vehicles_updated[out_ind, ]

boxplot(vehicles_updated$Kurt.maxis,
        ylab = "Kurt.maxis")
mtext(paste("Outliers: ", paste(out, collapse = ", ")))


New <- boxplot(vehicles_updated)$out
New
length(New)


