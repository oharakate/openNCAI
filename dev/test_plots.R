# How are the values of final index, wellbeing index and flow index related?

# 1. Make the plot
years <- as.numeric(rownames(ncai_objects$overall_index))
ncai_raw <- ncai_objects$overall_index$raw_index
wellbeing_raw <- ncai_objects$wellbeing_index$raw_index
flow_raw <- ncai_objects$flow_index$raw_index

plot(years, ncai_raw,
     type = "b", col = "blue", pch = 16, lwd = 2,
     ylim = range(c(ncai_raw, wellbeing_raw, flow_raw), na.rm = TRUE),
     xlab = "Year", ylab = "Index (Base Year = 100)",
     main = "NCAI Components & Correlation")

lines(years, wellbeing_raw, type = "b", col = "darkgreen", pch = 17, lwd = 2)
lines(years, flow_raw, type = "b", col = "firebrick", pch = 18, lwd = 2)
abline(h = 100, lty = 2, col = "gray")

# 2. Calculate Correlations
cor_matrix <- cor(data.frame(Overall = ncai_raw, Wellbeing = wellbeing_raw, Flow = flow_raw))

# 3. Format the table text for the legend
# We'll show the correlation of Overall against the other two
cor_text <- c(
  paste0("Cor(Overall, Wellbeing): ", round(cor_matrix["Overall", "Wellbeing"], 3)),
  paste0("Cor(Overall, Flow): ", round(cor_matrix["Overall", "Flow"], 3))
)

# 4. Add the legend and the correlation "table"
legend("bottomleft",
       legend = c("Overall NCAI", "Potential Wellbeing", "Flow (Condition)"),
       col = c("blue", "darkgreen", "firebrick"),
       pch = c(16, 17, 18), lwd = 2, bty = "n", cex = 0.8)

legend("bottomright",
       legend = cor_text,
       title = "Correlations",
       cex = 0.7, bty = "o", bg = "white", box.col = "gray")

# Looks like the index has been driven more strong by condition changes than by
# habitat changes on an overall level.
