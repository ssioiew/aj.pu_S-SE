# ------------------------------------------------------
# File: align_admixter_colors.R
# Purpose: Align ADMIXTURE Q-files with a robust anchor population check.
# Author: Pirada Naewkam
# Date: 2025-08-29
#
# Description:
#   Detailed description or steps
#
# Input: input files or data
# Output: output files or results
# ------------------------------------------------------


# Step 0: Setup All Environments ------------------------------------------

# Load All Libraries
library(readxl)
library(dplyr)

# Parameters
K_VALUES <- 2:10
BASENAME <- "modern_ancient_no_maniri"
Q_DIR <- "week2"

# Paths
FAM_PATH <- file.path(Q_DIR, paste0(BASENAME, ".fam"))
METADATA_PATH <- "SK_PL_31-july-E_SE_no_redundance.xlsx"

# Create output directory
aligned_q_dir <- file.path(Q_DIR, "aligned_results")
dir.create(aligned_q_dir, showWarnings = FALSE) # สร้าง folder ใหม่ให้เลย

# Load Metadata with the correct column name handling

# Load fam file
fam <- read.table(FAM_PATH, header = FALSE, stringsAsFactors = FALSE,
                  col.names = c("FID", "Genetic ID", "Father ID", "Mother ID", "Sex", "Phenotype"),
                  check.names = FALSE)

# Load metadata
metadata <- read_xlsx(METADATA_PATH) %>% 
  select(`Genetic ID`, `Group ID`)

# Merge fam file with metadata
fam_meta <- left_join(fam, metadata, by = "Genetic ID")

# Define anchor population
ANCHOR_GROUP_IDS <- "Mongol.HO"

# Add a check to ensure anchors are found
anchor_indices <- which(fam_meta$`Group ID` %in% ANCHOR_GROUP_IDS)

if (length(anchor_indices) == 0) {
  stop("FATAL ERROR: No individuals from the anchor population (Southern China) were found. Please check your 'Genetic ID' columns for mismatches or Group IDs in METADATA_PATH.")
} else {
  cat(sprintf("Successfully found %d individuals from the anchor population.\n", length(anchor_indices)))
}


# Step 2: Align the First K file (K=2) ------------------------------------

# กำหนด k แรกสุด
first_k <- min(K_VALUES)
q_file_path_first <- file.path(Q_DIR, sprintf("%s.%d.Q", BASENAME, first_k))
q_data_first <- read.table(q_file_path_first)

# Calculate mean proportions within the verified anchor population
anchor_means <- colMeans(q_data_first[anchor_indices, ], na.rm = TRUE)

# Determine the new column order
anchor_component_col <- which.max(anchor_means)
other_component_col <- setdiff(1:first_k, anchor_component_col)
initial_order <- c(anchor_component_col, other_component_col)

# Reorder the K=2 data
aligned_q_first <- q_data_first[, initial_order]

# Save the correctly oriented K=2 file
aligned_q_path_first <- file.path(aligned_q_dir, basename(q_file_path_first))
write.table(aligned_q_first, file = aligned_q_path_first, sep = " ", row.names = FALSE, col.names = FALSE)

cat(sprintf("Intelligently aligned K=%d. Anchor component is now Column 1. Original column order was %s.\n",
            first_k, paste(initial_order, collapse=",")))


# Step 3: Loop and Align the Rest of the K Values -------------------------

# สร้างฟังก์ชันเพื่อลูป และจัดเรียงค่า component k ที่เพิ่มเข้ามาในแต่ละรอบ
for (k in (first_k + 1):max(K_VALUES)) {
  ref_k <- k - 1
  ref_q_path <- file.path(aligned_q_dir, sprintf("%s.%d.Q", BASENAME, ref_k))
  ref_q <- read.table(ref_q_path)
  
  target_q_path <- file.path(Q_DIR, sprintf("%s.%d.Q", BASENAME, k))
  target_q <- read.table(target_q_path)
  
  cor_matrix <- cor(ref_q, target_q)
  
  new_order <- numeric(k)
  used_cols <- c()
  
  for (i in 1:ref_k) {
    cor_vector <- cor_matrix[i, ]
    cor_vector[used_cols] <- -1
    best_match <- which.max(cor_vector)
    new_order[i] <- best_match
    used_cols <- c(used_cols, best_match)
  }
  
  new_component_col <- setdiff(1:k, used_cols)
  new_order[k] <- new_component_col
  
  aligned_target_q <- target_q[, new_order]
  
  aligned_q_path_new <- file.path(aligned_q_dir, sprintf("%s.%d.Q", BASENAME, k))
  write.table(aligned_target_q, file = aligned_q_path_new, sep = " ", row.names = FALSE, col.names = FALSE)
  
  cat(sprintf("Aligned K=%d. New column order: %s\n", k, paste(new_order, collapse=", ")))
}

cat("All Q files have been intelligently aligned and saved in:", aligned_q_dir, "\n")

