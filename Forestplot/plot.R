library(readxl)
library(dplyr)
library(forestploter)
library(grid)


df <- readxl::read_excel("path_to_file")
colnames(df) <- c("Variables"," ","OR","L-OR","H-OR","OR (95% CI)","P-value","Case","Control")

df$Variables <- ifelse(is.na(df$OR), 
                       df$Variables,
                       paste0("   ", df$Variables))
df$`  ` <- paste(rep(" ", 20), collapse = " ") # adjust the size of plot zone

df$`P-value` <- ifelse(
  is.na(df$`P-value`) | df$`P-value` == "-",
  df$`P-value`,
  ifelse(
    is.na(as.numeric(df$`P-value`)),
    df$`P-value`,
    as.numeric(format(signif(as.numeric(df$`P-value`), digits = 3), scientific = TRUE))
  )
)# numerization of p


format_scientific <- function(x, digits = 2) {
  s <- formatC(x, format = "e", digits = digits)
  parts <- strsplit(s, "e")[[1]]
  base <- parts[1]
  exp_num <- as.integer(parts[2])
  exp_sign <- ifelse(exp_num < 0, "⁻", "")
  exp_abs <- abs(exp_num)
  superscript_digits <- c("⁰", "¹", "²", "³", "⁴", "⁵", "⁶", "⁷", "⁸", "⁹")
  exp_digits <- strsplit(as.character(exp_abs), "")[[1]]
  exp_str <- paste(sapply(exp_digits, function(d) {
    superscript_digits[as.integer(d) + 1]
  }), collapse = "")
  paste0(base, "×10", exp_sign, exp_str)
}#scientific-format func

df$`P-value`[4:6] <-sapply(df$`P-value`[4:6], format_scientific, digits = 2)
df$`P-value`[10:12] <-sapply(df$`P-value`[10:12], format_scientific, digits = 2)


df$`P-value`[is.na(df$`P-value`)] <- ' '
df$` `[is.na(df$` `)] <- ' '
df$`OR (95% CI)`[is.na(df$`OR (95% CI)`)] <- ' '
df$`Case`[is.na(df$`Case`)] <- ' '
df$`Control`[is.na(df$`Control`)] <- ' '

df$HR <- as.numeric(df$OR)
df$`L-OR` <- as.numeric(df$`L-OR`)
df$`H-OR` <- as.numeric(df$`H-OR`)


forest(
  data=df[, c(1,2,8,9,10,6,7)],#plot col in order
  est = df$OR,
  lower=df$`L-OR`,
  upper = df$`H-OR`,
  ci_column = 5,#chose which col to put CI
  ref_line = 1,
  xlim = c(0.5, 1.1),
  ticks_at = c(0.5,0.7, 0.9, 1.0, 1.1),
  sizes = 0.6,
  theme = forest_theme(
    base_family = "serif",
    base_size = 11,
    ci_pch = 15,
    ci_col = "black",
    ci_lwd = 1.5,
    refline_gp = gpar(
      col = "black",
      lty = "solid", 
      lwd = 1.2 
    )
  )
)
#view the plot in rstudio or store with ggsave() in .pdf format 
