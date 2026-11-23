library(maps)
eu <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czech Republic","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden")
groups <- list(
  binding   = c(eu, "South Korea", "Vietnam"),
  targeted  = c("China", "India", "USA"),
  guidance  = c("Japan", "Taiwan", "Australia"),
  pending   = c("Brazil"),
  none      = c("Canada", "UK")
)
cols <- c(binding = "#A51C30", targeted = "#44546A", guidance = "#8996A0", pending = "#E87D1E", none = "#BAC5C6")
png("world_ai_regs.png", width = 2000, height = 1250, res = 220)
par(mar = c(5.5, 0, 0, 0))
map("world", fill = TRUE, col = "#EDEDED", border = "white", lwd = 0.3, ylim = c(-58, 85), mar = c(5.5, 0, 0, 0))
for (g in names(groups)) {
  map("world", regions = groups[[g]], fill = TRUE, col = cols[[g]], border = "white", lwd = 0.3, add = TRUE)
}
par(xpd = TRUE)
legend("bottom", inset = c(0, -0.30), ncol = 3, bty = "n", cex = 1.35, x.intersp = 0.6, text.width = NULL,
       fill = cols, border = NA,
       legend = c("Binding AI law", "Targeted rules", "Guidance only", "Pending bill", "No AI law"))
dev.off()
missing <- setdiff(unlist(groups), unique(sub(":.*$", "", map("world", plot = FALSE)$names)))
cat("unmatched:", missing, "\n")
