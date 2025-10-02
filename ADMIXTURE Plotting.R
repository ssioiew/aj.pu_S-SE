# ------------------------------------------------------
# File: ADMIXTURE Plotting.R
# Purpose: Brief description of the script
# Author: Pirada Naewkam
# Date: 2025-09-25
#
# Description:
#   Detailed description or steps
#
# Input: input files or data
# Output: output files or results
# ------------------------------------------------------


# แบ่งตาม region ----------------------------------------------------------

# Load library
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

# Parameters
k_values <- 8:13

# Paths
q_dir <- "aligned_results"
fam_path <- "modern_ancient_full_geno.fam"
metadata_path <- "SK_PL_31-july-E_SE_no_redundance.xlsx"

# Paths for missingness data
missingness_file_path <- "missingness_report.imiss"
missingness_data <- read.table(missingness_file_path, header = TRUE, stringsAsFactors = FALSE) %>% 
  select(`Genetic ID` = IID, F_MISS) %>% 
  mutate(
    `Genetic ID` = case_when(
      `Genetic ID` == "human1" ~ "Human1",
      `Genetic ID` == "human2" ~ "Human2",
      TRUE ~ `Genetic ID` # ถ้าไม่ใช่ก็คงค่าเดิมไว้
    ),
    F_MISS_percent = sprintf("%.2f%%", F_MISS * 100)
  ) %>% 
  as_tibble()

# Load fam and metadata
fam <- read.table(fam_path, header = FALSE, stringsAsFactors = FALSE)
fam[fam$V2 == "human1", 2] <- "Human1"
fam[fam$V2 == "human2", 2] <- "Human2"
colnames(fam) <- c("FID", "Genetic ID", "Father ID", "Mother ID", "Sex", "Phenotype")

metadata <- read_xlsx(metadata_path) %>%
  select(`Genetic ID`, `Full Date`, `Language family`, `Group ID`, `Political Entity`)

# time slot for label
df_date <- read_xlsx(metadata_path) %>%
  select(`Genetic ID`, `Full Date`) %>%
  rename("ori full date" = `Full Date`) %>%
  mutate(`ori full date` = ifelse(`ori full date` == "present", "present",
                                  sub("\\(.*", "", `ori full date`) %>%
                                    trimws()
  )
  )

# Merge fam and metadata together
fam_merged <- fam %>%
  left_join(metadata, by = "Genetic ID") %>%
  left_join(df_date, by = "Genetic ID") %>%
  select(-`Full Date`)


# Formatting Data ---------------------------------------------------------

# Define region groups
china_ids <- fam_merged$`Group ID`[fam_merged$`Political Entity` %in% "China"]
mainland_ids <- fam_merged$`Group ID`[fam_merged$`Political Entity` %in% c("Thailand","Cambodia","Myanmar","Laos","Vietnam")]
island_ids <- fam_merged$`Group ID`[fam_merged$`Political Entity` %in% c("Indonesia","Philippines","Singapore","Brunei","Malaysia")]
eastasia_ids <- fam_merged$`Group ID`[fam_merged$`Political Entity` %in% c("Mongolia","Taiwan","South Korea","Japan")]
southasia_ids <- fam_merged$`Group ID`[fam_merged$`Political Entity` %in% c("India","Pakistan","Bangladesh","Nepal","Sri Lanka")]
oceania_ids <- fam_merged$`Group ID`[fam_merged$`Political Entity` %in% "Papua New Guinea"]
europe_ids <- fam_merged$`Group ID`[fam_merged$`Political Entity` %in% c("Russia", "USA")]
africa_ids <- fam_merged$`Group ID`[fam_merged$`Political Entity` %in% "Nigeria"]

region_order <- c("Human1", "Human2", "NW China", "NE China", "Central China", "South China",
                  "Mainland SEA", "Island SEA", "South Asia", "Oceania", "European", "Africa", "Other")

# Process fam_merged_final
fam_merged_final <- fam_merged %>%
  mutate(
    Region = case_when(
      `Political Entity` == "Human1" ~ "Human1",
      `Political Entity` == "Human2" ~ "Human2",
      
      `Group ID` %in% c("Bonan.HO", "Qiang.HO", "Dongxiang.HO", "Yugur.HO", "Tibetan.HO",
                        "Uyghur.DG", "Uyghur.HO", "Uyghur.SG", "Kazakh_China.HO",
                        "Kyrgyz_China.HO", "Salar.HO", "Xibo.DG", "Xibo.HO",
                        "Tu.DG", "Tu.HO", "Tibetan_Yunnan.HO") ~ "NW China",
      
      `Group ID` %in% c("Oroqen.DG", "Oroqen.HO", "Daur.HO", "Daur.DG", "Hezhen.DG", "Hezhen.HO",
                        "Mongola.DG", "Mongola.HO") | `Group ID` %in% eastasia_ids ~ "NE China",
      
      `Group ID` %in% c("Han.DG", "Han.HO", "Tujia.DG", "Tujia.HO", "CHB.DG", "CHB.SG",
                        "China_Lahu.DG", "China_Lahu.HO", "She.DG", "She.HO") ~ "Central China",
      
      `Group ID` %in% c("Mulam.HO", "Li.HO", "Dong.HO", "Gelao.HO", "CHS.DG", "CHS.SG",
                        "CDX.SG", "CDX.DG", "Miao.DG", "Miao.HO", "Yi.DG", "Yi.HO",
                        "Dai.HO", "Dai.DG", "Naxi.DG", "Naxi.HO", "Maonan.HO", "Zhuang.HO") ~ "South China",
      
      `Group ID` %in% mainland_ids  ~ "Mainland SEA",
      `Group ID` %in% island_ids    ~ "Island SEA",
      `Group ID` %in% southasia_ids ~ "South Asia",
      `Group ID` %in% oceania_ids   ~ "Oceania",
      `Group ID` %in% europe_ids    ~ "European",
      `Group ID` %in% africa_ids    ~ "Africa",
      
      TRUE ~ "Other"
    ),
    Region = factor(Region, levels = region_order),   # ถ่าไม่ทำเป็น factor มันจะเรียงแบบอักษรศาสตร์ทันที
    
    SampleType = case_when(`ori full date` == "present" ~ "Modern",
                           TRUE ~ "Ancient"),
    SampleType = factor(SampleType, levels = c("Ancient", "Modern")),
    
    GID_LanSampleType = case_when(
      `Genetic ID` %in% c("Human1", "Human2") ~ paste0("**", `Genetic ID`, "**"),
      SampleType == "Ancient" ~ paste0("**", `Group ID`, "_", `Language family`, "**"),
      SampleType == "Modern" ~ paste0(`Group ID`, "_", `Language family`)
    ),
    
  ) %>%
  arrange(Region, SampleType, `Political Entity`, GID_LanSampleType) %>%
  mutate(SampleOrder = row_number()) %>%
  distinct() %>%
  as_tibble()

# กรองเหลือแค่ modern population เท่านั้น
modern_only_sorted <- fam_merged_final %>%
  filter(SampleType == "Modern") %>%
  
  # จัดเรียงตาม Region, Political Entity, Group ID (ใช้ลำดับเดิมจาก fam_merged_final)
  arrange(Region, `Political Entity`, `Group ID`) %>%
  mutate(SampleOrder = row_number())

# กำหนด constant color ก่อนเข้า function
ancestry_colors <- c(
  "#FF0000",
  "#FFFF00",
  "#80FF00",
  "#0000FF",
  "#FF00FF",
  "#800080",
  "#00FA9A",
  "#1E90FF",
  "#FF4500",
  "#008080",
  "#8A2BE2",
  "#4B0082",
  "#00FFFF",
  "#8B4513",
  "#FF1493",
  "#FFD700",
  "#808080",
  "#6B8E23",
  "#87CEEB",
  "#FF6347"
)


# สร้าง ggplot แต่ละค่า k ตาม region -------------------------------------

create_ggplot <- function(k) {
  q_file <- file.path(q_dir, sprintf("modern_ancient_full_geno.%d.Q", k))
  q_data <- read.table(q_file, header = FALSE)
  
  q_subset <- cbind(fam[, "Genetic ID", drop = FALSE], q_data)
  colnames(q_subset)[2:(k+1)] <- paste0("Ancestry", 1:k)
  
  df <- fam_merged_final %>%
    left_join(q_subset, by = "Genetic ID") %>%
    select(SampleOrder, `Genetic ID`, Region, `Group ID`, starts_with("Ancestry"))
  
  df_long <- df %>%
    pivot_longer(
      cols = starts_with("Ancestry"),
      names_to = "Ancestry",
      values_to = "Proportion"
    )
  
  # ทำให้ลำดับใน Legend ถูกต้องเสมอ (Ancestry1, 2, 3, ...)
  df_long$Ancestry <- factor(df_long$Ancestry, levels = rev(paste0("Ancestry", 1:k)))
  
  region_boundaries <- fam_merged_final %>%
    group_by(Region) %>%
    summarise(start = min(SampleOrder), end = max(SampleOrder), mid = (start + end) / 2) %>%
    ungroup()
  
  # plotting
  ggplot(df_long, aes(x = SampleOrder, y = Proportion, fill = Ancestry)) +
    geom_bar(stat = "identity", width = 1) +
    geom_vline(
      data = region_boundaries,
      aes(xintercept = end + 0.5),
      color = "black",
      linewidth = 0.7
    ) +
    scale_x_continuous(breaks = region_boundaries$mid, labels = region_boundaries$Region, expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_manual(
      values = rev(ancestry_colors[1:k]),
      breaks = rev(paste0("Ancestry", 1:k))
    ) +
    
    guides(fill = guide_legend(reverse = TRUE)) +
    
    labs(title = paste("K =", k), x = "Region", y = "Ancestry Proportion") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      axis.title = element_text(size = 12),
      panel.grid = element_blank(),
      legend.position = "right",
      legend.title = element_text(face = "bold")
    )
}

# เก็บ ggplot แต่ละค่า k ผ่าน create_ggplot function ไว้ใน list
ggplot_list <- lapply(k_values, create_ggplot)

# รวมกราฟ ggplot ในภาพเดียวแบบแนวตั้ง
combined_ggplot <- grid.arrange(grobs = ggplot_list, ncol = 1, heights = rep(1, length(k_values)))

# บันทึกรูปภาพ
ggsave(
  filename = "C:/Users/patch/Desktop/structure_k2-5.png",
  plot = combined_ggplot,
  width = 80,
  height = 6 * length(k_values),
  dpi = 300,
  limitsize = FALSE,
  device = "png"
)

cat("Your plot is ready on the Desktop! 🎉")


# สร้าง ggplot ด้วย custom k ตาม region -----------------------------------

k_new <- 5
custom_plot <- create_ggplot(k_new)

# บันทึกรูปภาพ
ggsave(
  filename = file.path("C:/Users/patch/Desktop/", paste0("structureK_", k_new, ".png")),
  plot = custom_plot,
  width = 100,
  height = 6,
  dpi = 300,
  limitsize = FALSE,
  device = "png",
  bg = "white"
)

cat("Your plot is ready on the Desktop! 🎉")


# พล็อต base R barplot ---------------------------------------------

#  กำหนด output ไฟล์ภาพใหญ่รวมทุก K
png("C:/Users/patch/Desktop/barK_2-5.png",
    width = 42000,
    height = 1000 * length(k_values),   # สูงเพิ่มตามจำนวน K
    res = 300)  
par(mfrow = c(length(k_values), 1), mar = c(6, 5, 4, 8))  # ขยาย margin ขวาให้พอใส่ legend

for (k in k_values) {
  q_file <- file.path(q_dir, sprintf("modern_ancient_full_geno.%d.Q", k))
  q_data <- read.table(q_file, header = FALSE)
  mat <- t(as.matrix(q_data))
  
  # ตั้งชื่อ column ให้ตรงกับ sample ก่อนที่จะ sort
  colnames(mat) <- fam$`Genetic ID`
  
  # ใช้ชื่อ sample เดิมที่ตรงกับ .fam
  ordered_ids <- modern_only_sorted$`Genetic ID`
  
  # เรียง sample_ids ตาม fam_merged_final ที่มี SampleOrder แล้ว
  label_ids <- modern_only_sorted$GID_LanSampleType
  
  # เลือก column และเรียงตามลำดับ sample_ids (ซึ่งเป็นลำดับที่เราต้องการ)
  mat <- mat[, ordered_ids, drop = FALSE]
  
  barplot_heights <- barplot(
    mat,
    col = ancestry_colors,
    border = "black",
    names.arg = label_ids,
    las = 2,
    cex.names = 0.4,
    main = paste("Base R barplot with borders: K =", k),
    ylab = "Ancestry Proportion",
    space = 0
  )
  
  # เพิ่ม legend ทางขวาของกราฟ
  legend(
    x = max(barplot_heights) + 5,
    y = 1,
    legend = paste0("Ancestry", 1:k),
    fill = ancestry_colors,
    border = "black",
    cex = 0.6,
    bty = "n"
  )
}

dev.off()
cat("Your plot is ready on the Desktop! 🎉")


# พล็อต base R barplot ด้วย custom k --------------------------------------

k_new <- 5

png(file.path("C:/Users/patch/Desktop/", paste0("barK", k_new, ".png")),
    width = 48000,
    height = 900,   # สูงเพิ่มตามจำนวน K
    res = 300)  

par(mfrow = c(1, 1), mar = c(4, 5, 4, 8))  # ขยาย margin ขวาให้พอใส่ legend

for (k in k_new) {
  q_file <- file.path(q_dir, sprintf("modern_ancient_full_geno.%d.Q", k_new))
  q_data <- read.table(q_file, header = FALSE)
  mat <- t(as.matrix(q_data))
  
  # ตั้งชื่อ column ให้ตรงกับ sample ก่อนที่จะ sort
  colnames(mat) <- fam$`Genetic ID`
  
  # ใช้ชื่อ sample เดิมที่ตรงกับ .fam
  ordered_ids <- fam_merged_final$`Genetic ID`
  
  # เรียง sample_ids ตาม fam_merged_final ที่มี SampleOrder แล้ว
  label_ids <- fam_merged_final$GID_LanSampleType
  
  # เลือก column และเรียงตามลำดับ sample_ids (ซึ่งเป็นลำดับที่เราต้องการ)
  mat <- mat[, ordered_ids, drop = FALSE]
  
  barplot_heights <- barplot(
    mat,
    col = ancestry_colors,
    border = "black",
    names.arg = label_ids,
    las = 2,
    cex.names = 0.2,
    main = paste("Base R barplot with borders: K =", k_new),
    ylab = "Ancestry Proportion",
    space = 0
  )
  
  # เพิ่ม legend ทางขวาของกราฟ
  legend(
    x = max(barplot_heights) + 5,
    y = 1,
    legend = paste0("Ancestry", 1:k),
    fill = ancestry_colors,
    border = "black",
    cex = 0.8,
    bty = "n"
  )
}

dev.off()
cat("Your plot is ready on the Desktop! 🎉")


# เอา Ancient ขึ้นมาข้างหน้าให้หมด ----------------------------------------------------------

# กรองมาแค่ ancient sample จาก SampleType
ancient_only <- fam_merged_final %>%
  filter(SampleType == "Ancient") %>%
  
  # จัด label ใน GID_LanSampleType ใหม่
  mutate(
    GID_LanSampleType = case_when(
      `Genetic ID` %in% c("Human1", "Human2") ~ `Group ID`,
      TRUE ~ paste0(`Group ID`, " (", `ori full date`, ")")
    )
  ) %>%
  arrange() %>% 
  
  # เรียงลำดับ SampleOrder ใหม่
  mutate(SampleOrder = row_number()) %>%
  distinct()


# sort แค่ Ancient ใหม่ ให้เป็นระบบ ---------------------------------------

get_start_year <- function(date_string) {
  
  # ถ้า date_string เป็น NA หรือ "present" ให้ถือว่าเป็นปีใหม่สุด
  if (is.na(date_string) || date_string == "present") {
    return(2024)
  }
  
  # ดึงเฉพาะส่วนที่เป็นตัวเลขออกมา (จัดการกับเคสที่ไม่มีตัวเลข)
  year_str <- regmatches(date_string, regexpr("^[0-9]+", date_string))
  
  # ถ้าไม่เจอตัวเลขเลย (เช่น "Unknown") ให้กำหนดค่า default (เช่น ปีเก่ามากๆ)
  if (length(year_str) == 0) {
    
    # สำหรับ "Unknown (8000-1 BCE)" จะถูกจัดเป็น Neolithic_Early อยู่แล้ว
    # การให้ค่าเป็น -8000 จะช่วยให้เรียงลำดับได้ถูกต้อง
    if (grepl("Unknown", date_string)) {
      return(-8000)
    }
    return(NA) # หรือค่าอื่นที่เหมาะสม
  }
  
  year_val <- as.numeric(year_str)
  
  # ถ้าเป็น BCE ให้เป็นค่าลบ
  if (grepl("BCE", date_string, ignore.case = TRUE)) {
    return(-year_val)
  } else {
    return(year_val)
  }
}

# 1. กรองเฉพาะ Ancient และสร้างคอลัมน์ใหม่ ๆ เพื่อใช้จัดเรียง
# Sort order is chronological (Oldest to Newest): Jomon (~10000-300 BCE) -> Hoabinhian (~12000-2000 BCE)
# -> Neolithic (~2500-500 BCE) -> Bronze Age (~1500-500 BCE) -> Iron Age (~500 BCE - 500 CE) -> Historical (after ~500 CE)
ancient_only_sorted <- fam_merged_final %>%
  filter(SampleType == "Ancient") %>%
  
  # 1. นำข้อมูล missingness มารวมกับตารางนี้
  left_join(missingness_data, by = "Genetic ID") %>%
  
  # 2. สร้างคอลัมน์สำหรับจัดเรียงและสร้าง Label ใหม่
  mutate(
    SortGroup = case_when(
      `Genetic ID` == "Human1" ~ "01_Human1",
      `Genetic ID` == "Human2" ~ "02_Human2",
      grepl("Jomon", `Group ID`) ~ "03_Jomon",
      grepl("Hoabinhian", `Group ID`) ~ "04_Hoabinhian",
      grepl("Unknown", `Group ID`) ~ "05_Neolithic_Early",
      grepl("historical", `Group ID`) ~ "09_Historical",
      grepl("_IA_|_IA$|^IA_", `Group ID`) ~ "08_IronAge",
      grepl("_BA_|_BA$|^BA_|_DongSon", `Group ID`) ~ "07_BronzeAge",
      grepl("_LN_|_LN$|^LN_|_N_|_N$|^N_|_HaLong|RedSlipped", `Group ID`) ~ "06_Neolithic_Late",
      TRUE ~ "99_Other"
    ),
    
    StartDate = sapply(`ori full date`, get_start_year),
    
    # สร้าง PlotLabel ที่มีดีไซน์แบบ Professional
    # ใช้ case_when เพื่อจัดการทุกเงื่อนไขในที่เดียว
    PlotLabel = case_when(
      # เงื่อนไขที่ 1: ถ้าเป็น Human1 หรือ Human2
      `Genetic ID` %in% c("Human1", "Human2") ~ paste0(`Group ID`, " (", `ori full date`, " | ", F_MISS_percent, ")"),
      
      # เงื่อนไขที่ 2: ถ้าไม่ใช่ Human1/2 แต่ไม่มีข้อมูล missingness
      is.na(F_MISS_percent) ~ paste0(`Group ID`, " (", `ori full date`, ")"),
      
      # เงื่อนไขที่ 3 (TRUE): กรณีอื่นๆ ทั้งหมด (มีข้อมูล missingness)
      TRUE ~ paste0(`Group ID`, " (", `ori full date`, " | ", F_MISS_percent, ")")
    )
  ) %>%
  
  # 4. จัดเรียงและสร้าง SampleOrder (เหมือนเดิม)
  arrange(SortGroup, StartDate) %>%
  mutate(SampleOrder = row_number())


# พล็อต base R barplot แค่ ancient (new sort) -----------------------------

# กำหนดค่า k ที่ต้องการพล็อตทั้งหมด
k_values <- 8:13

# กำหนด output ไฟล์ภาพใหญ่รวมทุก K
png(file.path("C:/Users/patch/Desktop/", "barK_2-5_ancient_sorted.png"),
    width = 2000,
    height = 1000 * length(k_values),   # สูงเพิ่มตามจำนวน K
    res = 300)  

# ตั้งค่าให้วาดหลายกราฟในแนวตั้ง (จำนวนแถว = จำนวน k, จำนวนคอลัมน์ = 1)
par(mfrow = c(length(k_values), 1), mar = c(11, 5, 4, 8))

# เริ่ม loop เพื่อวาดกราฟสำหรับแต่ละค่า k
for (k in k_values) {
  
  # --- ส่วนนี้เหมือนกับโค้ด custom k ของคุณทุกประการ ---
  
  q_file <- file.path(q_dir, sprintf("modern_ancient_full_geno.%d.Q", k))
  q_data <- read.table(q_file, header = FALSE)
  mat <- t(as.matrix(q_data))
  
  colnames(mat) <- fam$`Genetic ID`
  
  # ใช้ DataFrame ที่จัดเรียงใหม่แล้ว (ancient_only_sorted)
  ordered_ids <- ancient_only_sorted$`Genetic ID`
  label_ids <- ancient_only_sorted$PlotLabel
  
  # เลือกและเรียงคอลัมน์ของ matrix ตามลำดับใหม่
  mat <- mat[, ordered_ids, drop = FALSE]
  
  # วาด barplot
  barplot(
    mat,
    col = ancestry_colors[1:k], # ใช้สีตามจำนวน k ในแต่ละรอบ
    border = "black",
    names.arg = label_ids,
    las = 2,
    cex.names = 0.4,
    main = paste("Ancient sample plotting for K =", k, "(Cultural/Chronological Sort)"),
    ylab = "Ancestry Proportion",
    space = 0
  )
  
  # เพิ่ม legend
  legend("right",
         inset = c(-0.15, 0),
         xpd = TRUE,
         legend = paste0("Ancestry ", 1:k),
         fill = ancestry_colors[1:k], # ใช้สีตามจำนวน k ในแต่ละรอบ
         cex = 0.8,
         bty = "n"
  )           
}

# ปิดการเขียนไฟล์ภาพ
dev.off()

cat("Your plot with all K values is ready on the Desktop! 🎉")


# พล็อต base R barplot ด้วย custom k แค่ ancient (new sort) --------

k_new <- 5

png(file.path("C:/Users/patch/Desktop/", paste0("barK", k_new,"_ancient_sorted.png")),
    width = 2500,
    height = 1500,
    res = 300)  

par(mfrow = c(1, 1), mar = c(9, 5, 4, 8))

for (k in k_new) {
  q_file <- file.path(q_dir, sprintf("modern_ancient_full_geno.%d.Q", k_new))
  q_data <- read.table(q_file, header = FALSE)
  mat <- t(as.matrix(q_data))
  
  colnames(mat) <- fam$`Genetic ID`
  
  # ใช้ DataFrame ที่จัดเรียงใหม่แล้ว (ancient_only_sorted)
  ordered_ids <- ancient_only_sorted$`Genetic ID`
  label_ids <- ancient_only_sorted$PlotLabel
  
  # เลือกและเรียงคอลัมน์ของ matrix ตามลำดับใหม่
  mat <- mat[, ordered_ids, drop = FALSE]
  
  barplot_heights <- barplot(
    mat,
    col = ancestry_colors,
    border = "black",
    names.arg = label_ids,
    las = 2,
    cex.names = 0.4,
    main = paste("Ancient sample plotting for K =", k_new, "(Cultural/Chronological Sort)"),
    ylab = "Ancestry Proportion",
    space = 0
  )
  
  legend("right",
         inset = c(-0.15, 0),
         xpd = TRUE,
         legend = paste0("Ancestry ", 1:k),
         fill = ancestry_colors,
         cex = 0.8,
         bty = "n"
  )           
}

dev.off()
cat("Your plot is ready on the Desktop! 🎉")

