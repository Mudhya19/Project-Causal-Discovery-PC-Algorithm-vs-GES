# Causal Discovery on Pima Indians Diabetes Database: PC-Stable vs Hill-Climbing

Repositori ini memuat proyek akhir analisis *Causal Discovery* yang mengimplementasikan perbandingan algoritma berbasis konstrain (*Constraint-based*) dan algoritma berbasis skor (*Score-based*) pada **Pima Indians Diabetes Database**.

## 📌 Deskripsi Proyek
Tujuan utama dari proyek ini adalah untuk menemukan struktur kausal secara *data-driven* dari data observasional medis. Pertanyaan kausal utama yang dijawab adalah:
> *"Apakah Indeks Massa Tubuh (BMI) dan kadar glukosa darah secara kausal mempengaruhi risiko diabetes (Outcome), atau hubungan tersebut dikonfound oleh variabel lain seperti usia (Age) dan riwayat kehamilan (Pregnancies)?"*

## 🚀 Pendekatan dan Metodologi
1. **Pra-pemrosesan Data**: Penanganan *missing values* (median imputation) dan diskretisasi (*quantile-based*) yang diwajibkan untuk *constraint-based testing*.
2. **Algoritma Causal Discovery**:
   - **PC-Stable Algorithm** (Constraint-based) dengan uji *Sensitivity Analysis* pada ambang batas $\alpha = 0.01$ dan $\alpha = 0.05$ menggunakan *G-Squared / Mutual Information test*.
   - **Hill-Climbing** (Score-based, sebagai alternatif *Greedy Equivalence Search* / GES) dengan pemanfaatan optimasi skor *Bayesian Information Criterion* (BIC).
3. **Inferensi Kausal Formal**: 
   - Evaluasi kelayakan asumsi *Causal Markov*, *Faithfulness*, dan *Causal Sufficiency*.
   - Analisis *identifiability* via *Backdoor Criterion* menggunakan paket `dagitty`.
   - Estimasi efek menggunakan perbandingan *Naive Logistic Regression* vs *Backdoor Adjusted Logistic Regression*.

## 🛠️ Teknologi yang Digunakan
- **Bahasa Pemrograman:** R ($\ge$ 4.1.0)
- **Library Utama:** `bnlearn`, `igraph`, `dagitty`, `ggplot2`, `dplyr`, `corrplot`, `knitr`

## 📁 Struktur Repositori
- `dataset/` - Berisi data mentah `diabetes.csv`.
- `docs/` - Memuat berkas panduan tugas, laporan PDF final, dan presentasi PPTX.
- `output/` - Memuat *output* grafik (`image/`) dan riwayat sistem R (`Rhistory/`).
- `pc_algorithm_vs_ges.rmd` - *Notebook* RMarkdown sentral yang memuat kode utuh, hasil analisis, beserta narasinya.
- `pc_algorithm_vs_ges.html` - *Notebook* hasil rentetan kompilasi *Knit* agar analisis lebih mudah dibaca di *browser*.
- `pc_algorithm_vs_ges.R` - *Script* R murni (*source code*) yang diekstrak langsung dari *notebook*.

## ⚙️ Cara Menjalankan (Reproducibility)
1. Klon repositori ini: 
   ```bash
   git clone https://github.com/Mudhya19/Project-Causal-Discovery-PC-Algorithm-vs-GES.git
   ```
2. Buka proyek ini di RStudio.
3. *Install* semua dependensi terkait jika Anda belum memilikinya dengan menjalankan perintah di baris pertama *script*.
4. Anda dapat mengeksekusi *script* murni secara berurutan pada `pc_algorithm_vs_ges.R`, atau cukup menekan tombol **Knit** pada file `pc_algorithm_vs_ges.rmd` untuk mereproduksi laporan HTML dan memunculkan grafik baru.

---
**Identitas Peneliti:**  
**Muhammad Dhiauddin** (25917024)  
*Sains Data - Profesional*
