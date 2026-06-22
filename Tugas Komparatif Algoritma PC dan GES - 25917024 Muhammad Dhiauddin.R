# Script R diekstrak secara utuh dari pc_algorithm_vs_ges.rmd
# (Mencakup penyesuaian output paths ke folder image/Rhistory)
# ==========================================================

knitr::opts_chunk$set(
  echo       = TRUE,
  warning    = FALSE,
  message    = FALSE,
  fig.align  = "center",
  fig.width  = 7,
  fig.height = 5,
  out.width  = "85%",
  fig.path   = "output/image/"
)
options(scipen = 999, digits = 4)

# Create directories if they don't exist
dir.create("output/image/", showWarnings = FALSE, recursive = TRUE)
dir.create("output/Rhistory/", showWarnings = FALSE, recursive = TRUE)
try(savehistory("output/Rhistory/.Rhistory"), silent = TRUE)

# ── Jalankan blok ini SATU KALI untuk menginstal semua dependensi ──────────────
install.packages(c("bnlearn", "igraph", "dagitty",
                   "ggplot2", "dplyr", "corrplot", "knitr"))

# Rgraphviz (opsional, alternatif visualisasi via Bioconductor)
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(c("Rgraphviz", "graph"))

library(bnlearn)
library(igraph)
library(dagitty)
library(ggplot2)
library(dplyr)
library(corrplot)
library(knitr)
set.seed(42)

var_df <- data.frame(
  No         = 1:9,
  Variabel   = c("Pregnancies", "Glucose", "BloodPressure", "SkinThickness",
                  "Insulin", "BMI", "DiabetesPedigreeFunction", "Age", "Outcome"),
  Tipe       = c("Kontinu", "Kontinu", "Kontinu", "Kontinu",
                  "Kontinu", "Kontinu", "Kontinu", "Kontinu", "Biner Diskrit"),
  Deskripsi  = c(
    "Jumlah kehamilan",
    "Glukosa plasma 2 jam pasca tes toleransi oral (mg/dL)",
    "Tekanan darah diastolik (mm Hg)",
    "Ketebalan lipatan kulit trisep (mm)",
    "Kadar insulin serum 2 jam (µU/mL)",
    "Indeks Massa Tubuh (kg/m²)",
    "Skor riwayat diabetes keluarga",
    "Usia (tahun)",
    "Diagnosis diabetes (1 = Positif, 0 = Negatif)"
  )
)
kable(var_df, caption = "Deskripsi Variabel Dataset Pima Indians Diabetes",
      col.names = c("No", "Variabel", "Tipe", "Deskripsi"))

# Pastikan file diabetes.csv berada di direktori yang tepat
diabetes_raw <- read.csv("dataset/diabetes.csv", stringsAsFactors = FALSE)

# Konversi semua kolom integer menjadi numeric untuk menghindari error di bnlearn
diabetes_raw[] <- lapply(diabetes_raw, function(x) if(is.integer(x)) as.numeric(x) else x)

cat("Dimensi dataset:", nrow(diabetes_raw), "baris x",
    ncol(diabetes_raw), "kolom\n")

# Menampilkan tipe data dalam format tabel yang rapi
tipe_data_df <- data.frame(
  Variabel = names(diabetes_raw),
  Tipe_Data = sapply(diabetes_raw, class),
  row.names = NULL
)
knitr::kable(tipe_data_df, caption = "Tipe Data Setiap Kolom", row.names = FALSE)

# Transpose tabel summary agar variabel menjadi baris, bukan kolom (menghindari kepanjangan ke kanan)
desc_stats <- t(round(do.call(cbind, lapply(diabetes_raw, summary)), 2))
knitr::kable(desc_stats, caption = "Statistik Deskriptif Awal (Sebelum Preprocessing)", row.names = TRUE)

par(mfrow = c(3, 3), mar = c(3, 3, 2, 1))
num_cols <- names(diabetes_raw)
for (col in num_cols) {
  if (col == "Outcome") {
    barplot(table(diabetes_raw[[col]]),
            main = col, col = c("#AED6F1", "#E74C3C"),
            names.arg = c("Negatif (0)", "Positif (1)"), cex.main = 0.9)
  } else {
    hist(diabetes_raw[[col]], main = col, col = "#AED6F1",
         border = "white", xlab = "", cex.main = 0.9)
  }
}
par(mfrow = c(1, 1))

zero_cols <- c("Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI")

# Salin dataset dan ganti 0 dengan NA
diabetes_na <- diabetes_raw
for (col in zero_cols) {
  diabetes_na[[col]][diabetes_na[[col]] == 0] <- NA
}

# Tampilkan jumlah missing values
mv_df <- data.frame(
  Variabel   = names(diabetes_na),
  Jumlah_NA  = colSums(is.na(diabetes_na)),
  Persen_NA  = round(colSums(is.na(diabetes_na)) / nrow(diabetes_na) * 100, 1)
)
kable(mv_df, caption = "Jumlah dan Persentase Missing Values per Variabel",
      col.names = c("Variabel", "Jumlah NA", "Persen (%)"),
      row.names = FALSE)

# Lakukan Median Imputation
diabetes_imp <- diabetes_na
medians_used <- numeric(length(zero_cols))
names(medians_used) <- zero_cols

for (col in zero_cols) {
  med_val <- median(diabetes_imp[[col]], na.rm = TRUE)
  medians_used[col] <- med_val
  diabetes_imp[[col]][is.na(diabetes_imp[[col]])] <- med_val
}

cat("Nilai Median yang Digunakan untuk Imputasi:\n")
medians_df <- data.frame(Variabel = names(medians_used), Median = medians_used, row.names = NULL)
knitr::kable(medians_df, caption = "Nilai Median yang Digunakan untuk Imputasi")
cat("\nVerifikasi - Total NA setelah imputasi:",
    sum(is.na(diabetes_imp)), "\n")

# Salin data imputed untuk proses diskretisasi
diabetes_disc_prep <- diabetes_imp

# Konversi Outcome ke faktor terlebih dahulu (agar tidak ikut didiskretisasi)
diabetes_disc_prep$Outcome <- factor(
  diabetes_disc_prep$Outcome,
  levels = c(0, 1),
  labels = c("Negatif", "Positif")
)

# Diskretisasi semua variabel numerik menjadi 3 bin (kuantil)
# Menggunakan metode 'interval' untuk menghindari error zero-length intervals
# akibat banyaknya data yang bernilai persis sama hasil imputasi median.
disc_data <- bnlearn::discretize(diabetes_disc_prep,
                                  method = "interval",
                                  breaks  = 3)

# Tampilkan tipe data setelah diskretisasi dalam format tabel
tipe_disc_df <- data.frame(
  Variabel = names(disc_data),
  Tipe_Data = sapply(disc_data, class),
  row.names = NULL
)
knitr::kable(tipe_disc_df, caption = "Tipe Data Setelah Diskretisasi")

# Tampilkan level dalam bentuk tabel agar rapi (tidak berantakan di layar)
levels_df <- data.frame(
  Variabel = names(disc_data),
  Level_Kategori = sapply(names(disc_data), function(col) paste(levels(disc_data[[col]]), collapse = " | ")),
  row.names = NULL
)
knitr::kable(levels_df, caption = "Level Variabel Setelah Diskretisasi")

# Transpose tabel agar rapi dan muat di halaman
post_stats <- t(round(sapply(diabetes_imp[, -9], function(x) {
  c(Min    = min(x),
    Q1     = quantile(x, 0.25),
    Median = median(x),
    Mean   = mean(x),
    Q3     = quantile(x, 0.75),
    Max    = max(x),
    SD     = sd(x))
}), 2))
knitr::kable(post_stats, caption = "Statistik Deskriptif Setelah Imputasi (Variabel Kontinu)", row.names = TRUE)

outcome_counts <- table(diabetes_imp$Outcome)
outcome_pct    <- round(prop.table(outcome_counts) * 100, 1)
barplot(outcome_counts,
        col     = c("#AED6F1", "#E74C3C"),
        names.arg = c(paste0("Negatif (0)\n", outcome_pct[1], "%"),
                      paste0("Positif (1)\n",  outcome_pct[2], "%")),
        main    = "Distribusi Outcome (Diagnosis Diabetes)",
        ylab    = "Frekuensi",
        border  = "white")
legend("topright",
       legend = c(paste0("Negatif: n = ", outcome_counts[1]),
                  paste0("Positif: n = ", outcome_counts[2])),
       fill   = c("#AED6F1", "#E74C3C"), bty = "n")

cor_mat <- cor(diabetes_imp[, -9])
corrplot(cor_mat,
         method      = "color",
         type        = "upper",
         addCoef.col = "black",
         number.cex  = 0.65,
         tl.cex      = 0.75,
         col         = colorRampPalette(c("#D73027", "#FFFFFF", "#1A9850"))(200),
         title       = "Matriks Korelasi (Variabel Kontinu)",
         mar         = c(0, 0, 2, 0))

# Fungsi plot CPDAG menggunakan igraph
# Edge berarah  -> ditampilkan dengan warna gelap dan anak panah
# Edge tidak berarah - ditampilkan merah putus-putus tanpa anak panah
plot_cpdag <- function(net, title = "CPDAG") {

  node_names <- nodes(net)
  n          <- length(node_names)
  dir_a      <- directed.arcs(net)
  undir_a    <- undirected.arcs(net)

  # Akumulasi daftar edge
  e_from <- integer(0); e_to <- integer(0)
  e_col  <- character(0); e_lty <- integer(0); e_arr <- integer(0)

  # Edge berarah
  if (nrow(dir_a) > 0) {
    e_from <- c(e_from, match(dir_a[, "from"], node_names))
    e_to   <- c(e_to,   match(dir_a[, "to"],   node_names))
    e_col  <- c(e_col,  rep("#1A5276", nrow(dir_a)))
    e_lty  <- c(e_lty,  rep(1L,        nrow(dir_a)))
    e_arr  <- c(e_arr,  rep(2L,        nrow(dir_a)))   # anak panah ke depan
  }

  # Edge tidak berarah - ambil pasangan unik saja
  if (nrow(undir_a) > 0) {
    pairs_m <- unique(t(apply(undir_a, 1L, sort)))
    if (!is.matrix(pairs_m)) pairs_m <- matrix(pairs_m, nrow = 1L)
    e_from <- c(e_from, match(pairs_m[, 1L], node_names))
    e_to   <- c(e_to,   match(pairs_m[, 2L], node_names))
    e_col  <- c(e_col,  rep("#C0392B", nrow(pairs_m)))
    e_lty  <- c(e_lty,  rep(2L,        nrow(pairs_m)))
    e_arr  <- c(e_arr,  rep(0L,        nrow(pairs_m)))   # tanpa anak panah
  }

  # Buat igraph
  g <- make_empty_graph(n = n, directed = TRUE)
  V(g)$name <- node_names

  if (length(e_from) > 0) {
    g               <- add_edges(g, as.vector(rbind(e_from, e_to)))
    E(g)$color      <- e_col
    E(g)$lty        <- e_lty
    E(g)$arrow.mode <- e_arr
  }

  set.seed(123)
  lo <- layout_with_fr(g)

  plot(g,
       layout             = lo,
       vertex.color       = "#AED6F1",
       vertex.frame.color = "#1A5276",
       vertex.shape       = "circle",
       vertex.size        = 30,
       vertex.label       = V(g)$name,
       vertex.label.cex   = 0.72,
       vertex.label.color = "black",
       vertex.label.font  = 2L,
       edge.arrow.size    = 0.55,
       edge.width         = 1.8,
       main               = title)

  legend("bottomright",
         legend = c("Edge Berarah (->)", "Edge Tidak Berarah (-)"),
         col    = c("#1A5276", "#C0392B"),
         lty    = c(1L, 2L),
         lwd    = 2,
         cex    = 0.75,
         bg     = "white",
         box.lty = 0)
}

# Fungsi ringkasan edge
summarize_edges <- function(net, label = "") {
  dir_a   <- directed.arcs(net)
  undir_a <- undirected.arcs(net)
  n_dir   <- nrow(dir_a)
  n_undir <- if (nrow(undir_a) > 0) nrow(undir_a) / 2L else 0L
  cat(sprintf("\n=== %s ===\n", label))
  cat(sprintf("Edge berarah    : %d\n", n_dir))
  cat(sprintf("Edge tdk berarah: %d\n", n_undir))
  cat(sprintf("Total edge      : %d\n\n", n_dir + n_undir))

  if (n_dir > 0) {
    cat("Edge berarah:\n"); print(dir_a)
  }
  if (n_undir > 0) {
    pairs_m <- unique(t(apply(undir_a, 1L, sort)))
    if (!is.matrix(pairs_m)) pairs_m <- matrix(pairs_m, nrow = 1L)
    cat("\nEdge tidak berarah (pasangan unik):\n"); print(pairs_m)
  }
  invisible(list(n_dir = n_dir, n_undir = n_undir))
}

set.seed(42)
pc_01  <- pc.stable(disc_data, alpha = 0.01, test = "mi", undirected = FALSE)
info01 <- summarize_edges(pc_01, "PC-Stable (alpha = 0.01)")
plot_cpdag(pc_01, title = expression(paste("CPDAG: PC-Stable  (", alpha, " = 0.01)")))

set.seed(42)
pc_05  <- pc.stable(disc_data, alpha = 0.05, test = "mi", undirected = FALSE)
info05 <- summarize_edges(pc_05, "PC-Stable (alpha = 0.05)")
plot_cpdag(pc_05, title = expression(paste("CPDAG: PC-Stable  (", alpha, " = 0.05)")))

set.seed(42)
# Mencari DAG terbaik menggunakan Hill-Climbing dengan skor BIC
hc_dag <- hc(disc_data, score = "bic")

# Mengonversi DAG menjadi CPDAG
hc_cpdag <- cpdag(hc_dag)
info_hc <- summarize_edges(hc_cpdag, "Hill-Climbing (BIC)")
plot_cpdag(hc_cpdag, title = "CPDAG: Hill-Climbing (Score = BIC)")

# Menghitung skor BIC untuk pembandingan (Lebih tinggi/mendekati 0 = Lebih baik)
# score() membutuhkan DAG. Kita buat fungsi tryCatch agar tidak error jika cextend gagal
safe_score <- function(cpdag_net, data) {
  res <- tryCatch(score(cextend(cpdag_net), data, type = "bic"), error = function(e) NA)
  return(res)
}

score_pc01 <- safe_score(pc_01, disc_data)
score_pc05 <- safe_score(pc_05, disc_data)
score_hc   <- score(hc_dag, disc_data, type = "bic")

sens_df <- data.frame(
  Kriteria = c(
    "Metode Pendekatan",
    "Parameter / Skor",
    "Total Edge",
    "Edge Berarah (->)",
    "Edge Tidak Berarah (-)",
    "Skor BIC Graf"
  ),
  Alpha_001 = c("Constraint-Based", "α = 0.01",
                info01$n_dir + info01$n_undir,
                info01$n_dir, info01$n_undir, round(score_pc01, 2)),
  Alpha_005 = c("Constraint-Based", "α = 0.05",
                info05$n_dir + info05$n_undir,
                info05$n_dir, info05$n_undir, round(score_pc05, 2)),
  HC_BIC = c("Score-Based", "BIC",
             info_hc$n_dir + info_hc$n_undir,
             info_hc$n_dir, info_hc$n_undir, round(score_hc, 2))
)
kable(sens_df,
      caption = "Perbandingan Analisis: PC-Stable (Sensitivity) vs Hill-Climbing",
      col.names = c("Kriteria", "PC-Stable (α=0.01)", "PC-Stable (α=0.05)", "Hill-Climbing"),
      row.names = FALSE)

dag_expert <- dagitty('dag {
  Pregnancies              [pos="0,2"]
  Glucose                  [pos="1.5,1"]
  BloodPressure            [pos="3,2"]
  SkinThickness            [pos="4,2"]
  Insulin                  [pos="4,1"]
  BMI                      [pos="2,2.5"]
  DiabetesPedigreeFunction [pos="4,3"]
  Age                      [pos="0,3"]
  Outcome                  [pos="2,0"]

  Age -> Pregnancies
  Age -> BMI
  Age -> Glucose
  BMI -> Outcome
  BMI -> SkinThickness
  Glucose -> Outcome
  Glucose -> Insulin
  DiabetesPedigreeFunction -> Outcome
  BloodPressure -> Outcome
  Pregnancies -> Outcome
}')

# Tampilkan ancestor / descendant BMI untuk memeriksa struktur
cat("Ancestors dari BMI:", paste(ancestors(dag_expert, "BMI"), collapse = ", "), "\n")
cat("Descendants dari BMI:", paste(descendants(dag_expert, "BMI"), collapse = ", "), "\n\n")

# Identifikasi Backdoor Adjustment Set minimal
adj_bmi <- adjustmentSets(dag_expert,
                           exposure = "BMI",
                           outcome  = "Outcome",
                           type     = "minimal")
cat("Minimal Backdoor Adjustment Set untuk P(Outcome | do(BMI)):\n")
print(adj_bmi)

adj_gluc <- adjustmentSets(dag_expert,
                            exposure = "Glucose",
                            outcome  = "Outcome",
                            type     = "minimal")
cat("\nMinimal Backdoor Adjustment Set untuk P(Outcome | do(Glucose)):\n")
print(adj_gluc)

# Cek identifiability menggunakan ada/tidaknya adjustment set
cat("Efek P(Outcome|do(BMI)) identifiable via backdoor?",
    length(adj_bmi) > 0, "\n")
cat("Efek P(Outcome|do(Glucose)) identifiable via backdoor?",
    length(adj_gluc) > 0, "\n")

# Tentukan adjustment variable dari set minimal pertama
adj_vars_bmi  <- as.character(unlist(adj_bmi[1]))
adj_vars_gluc <- as.character(unlist(adj_gluc[1]))
cat("\nVariabel penyesuaian untuk BMI    :", paste(adj_vars_bmi, collapse = ", "), "\n")
cat("Variabel penyesuaian untuk Glucose:", paste(adj_vars_gluc, collapse = ", "), "\n")

# Data regresi: gunakan data imputed (kontinu), Outcome sebagai 0/1 numerik
diabetes_reg <- diabetes_imp   # Outcome masih 0/1 numerik

# ── MODEL BMI ──────────────────────────────────────────────────────────────────
# Model 1: Naif - tanpa adjustment
m_bmi_naive <- glm(Outcome ~ BMI,
                    data   = diabetes_reg,
                    family = binomial(link = "logit"))

# Model 2: Adjusted - backdoor criterion (misal: Age sebagai confounders)
fml_bmi_adj <- as.formula(
  paste("Outcome ~ BMI +", paste(adj_vars_bmi, collapse = " + "))
)
m_bmi_adj <- glm(fml_bmi_adj,
                  data   = diabetes_reg,
                  family = binomial(link = "logit"))

# Model 3: Full adjusted - tambahan kontrol Glucose (mediator/confounders potensial)
m_bmi_full <- glm(Outcome ~ BMI + Age + Pregnancies + Glucose,
                   data   = diabetes_reg,
                   family = binomial(link = "logit"))

cat("── Koefisien Model Naif (BMI) ──\n")
print(round(summary(m_bmi_naive)$coefficients, 4))

cat("\n── Koefisien Model Adjusted (BMI + Backdoor) ──\n")
print(round(summary(m_bmi_adj)$coefficients, 4))

# Fungsi bantu hitung OR dan CI (Wald-based)
ci_or <- function(model, var) {
  b   <- coef(model)[var]
  se  <- summary(model)$coefficients[var, "Std. Error"]
  data.frame(
    OR      = round(exp(b), 4),
    CI_low  = round(exp(b - 1.96 * se), 4),
    CI_high = round(exp(b + 1.96 * se), 4),
    p_value = round(summary(model)$coefficients[var, "Pr(>|z|)"], 4),
    AIC     = round(AIC(model), 2)
  )
}

or_bmi_naive <- ci_or(m_bmi_naive, "BMI")
or_bmi_adj   <- ci_or(m_bmi_adj,   "BMI")
or_bmi_full  <- ci_or(m_bmi_full,  "BMI")

or_df <- rbind(
  cbind(Model = "Naif (BMI saja)",                        or_bmi_naive),
  cbind(Model = paste0("Adjusted (BMI + ", paste(adj_vars_bmi, collapse="+"), ")"),
        or_bmi_adj),
  cbind(Model = "Full Adj. (BMI + Age + Pregnancies + Glucose)", or_bmi_full)
)

kable(or_df,
      caption = "Perbandingan Odds Ratio BMI terhadap Outcome: Naif vs. Adjusted",
      col.names = c("Model", "OR (BMI)", "95% CI (Bawah)", "95% CI (Atas)",
                    "p-value", "AIC"),
      row.names = FALSE)

# ── MODEL GLUCOSE ─────────────────────────────────────────────────────────────
m_gluc_naive <- glm(Outcome ~ Glucose,
                     data   = diabetes_reg,
                     family = binomial(link = "logit"))

fml_gluc_adj <- as.formula(
  paste("Outcome ~ Glucose +", paste(adj_vars_gluc, collapse = " + "))
)
m_gluc_adj <- glm(fml_gluc_adj,
                   data   = diabetes_reg,
                   family = binomial(link = "logit"))

or_gluc_naive <- ci_or(m_gluc_naive, "Glucose")
or_gluc_adj   <- ci_or(m_gluc_adj,   "Glucose")

or_gluc_df <- rbind(
  cbind(Model = "Naif (Glucose saja)",         or_gluc_naive),
  cbind(Model = paste0("Adjusted (Glucose + ",
                        paste(adj_vars_gluc, collapse="+"), ")"), or_gluc_adj)
)
kable(or_gluc_df,
      caption = "Perbandingan Odds Ratio Glucose terhadap Outcome: Naif vs. Adjusted",
      col.names = c("Model", "OR (Glucose)", "95% CI (Bawah)", "95% CI (Atas)",
                    "p-value", "AIC"),
      row.names = FALSE)

