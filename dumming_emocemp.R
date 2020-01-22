## DUMMING

# Clinical Onset ----------------------------------
# Split variable into multiple collumns
clinical_cha <- as.character(emocemp$clinica_onset)
clinical_split <- str_split_fixed(clinical_cha, "," , 7)
clinical_df <- as.data.frame(clinical_split)

# Transform to numeric and change NAs
clinical_df$V2 <- as.character(clinical_df$V2)
clinical_df$V2 <- as.numeric(clinical_df$V2)
clinical_df$V2[is.na(clinical_df$V2)] <- 0 # Change NA for 0

clinical_df$V1 <- as.character(clinical_df$V1)
clinical_df$V1 <- as.numeric(clinical_df$V1)
clinical_df$V1[is.na(clinical_df$V1)] <- 0 # Change NA for 0

clinical_df$V3 <- as.character(clinical_df$V3)
clinical_df$V3 <- as.numeric(clinical_df$V3)
clinical_df$V3[is.na(clinical_df$V3)] <- 0 # Change NA for 0

clinical_df$V4 <- as.character(clinical_df$V4)
clinical_df$V4 <- as.numeric(clinical_df$V4)
clinical_df$V4[is.na(clinical_df$V4)] <- 0 # Change NA for 0

clinical_df$V5 <- as.character(clinical_df$V5)
clinical_df$V5 <- as.numeric(clinical_df$V5)
clinical_df$V5[is.na(clinical_df$V5)] <- 0 # Change NA for 0

clinical_df$V6 <- as.character(clinical_df$V6)
clinical_df$V6 <- as.numeric(clinical_df$V6)
clinical_df$V6[is.na(clinical_df$V6)] <- 0 # Change NA for 0

clinical_df$V7 <- as.character(clinical_df$V7)
clinical_df$V7 <- as.numeric(clinical_df$V7)
clinical_df$V7[is.na(clinical_df$V7)] <- 0 # Change NA for 0


# Create dummie variables for each clinical condition
clinical_df <- clinical_df %>%
        mutate(neurite_b = ifelse(clinical_df$V1 == 1, 1, 0)) %>%
        
        mutate(neurite_u = ifelse(clinical_df$V1 == 2, 1, 0) |
                       ifelse(clinical_df$V2 == 2,1,0) ) %>%
        
        mutate(mielite_parcial = ifelse(clinical_df$V1 == 3, 1, 0) | 
                       ifelse(clinical_df$V2 == 3,1,0) |
                       ifelse(clinical_df$V3 == 3,1,0) |
                       ifelse(clinical_df$V4 == 3,1,0) |
                       ifelse(clinical_df$V5 == 3,1,0) |
                       ifelse(clinical_df$V6 == 3,1,0) |
                       ifelse(clinical_df$V7 == 3,1,0) ) %>%
        
        mutate(adem = ifelse(clinical_df$V1 == 5, 1, 0) | 
                       ifelse(clinical_df$V2 == 5,1,0) |
                       ifelse(clinical_df$V3 == 5,1,0) |
                       ifelse(clinical_df$V4 == 5,1,0) |
                       ifelse(clinical_df$V5 == 5,1,0) |
                       ifelse(clinical_df$V6 == 5,1,0) |
                       ifelse(clinical_df$V7 == 5,1,0) ) %>%
        
        mutate(mielite_transversa = ifelse(clinical_df$V1 == 4, 1, 0) | 
                       ifelse(clinical_df$V2 == 4,1,0) |
                       ifelse(clinical_df$V3 == 4,1,0) |
                       ifelse(clinical_df$V4 == 4,1,0) |
                       ifelse(clinical_df$V5 == 4,1,0) |
                       ifelse(clinical_df$V6 == 4,1,0) |
                       ifelse(clinical_df$V7 == 4,1,0) ) %>%
        
        mutate(romboencefalite = ifelse(clinical_df$V1 == 6, 1, 0) | 
                       ifelse(clinical_df$V2 == 6,1,0) |
                       ifelse(clinical_df$V3 == 6,1,0) |
                       ifelse(clinical_df$V4 == 6,1,0) |
                       ifelse(clinical_df$V5 == 6,1,0) |
                       ifelse(clinical_df$V6 == 6,1,0) |
                       ifelse(clinical_df$V7 == 6,1,0) ) %>%
        
        mutate(outros_clinic = ifelse(clinical_df$V1 == 7, 1, 0) | 
                       ifelse(clinical_df$V2 == 7,1,0) |
                       ifelse(clinical_df$V3 == 7,1,0) |
                       ifelse(clinical_df$V4 == 7,1,0) |
                       ifelse(clinical_df$V5 == 7,1,0) |
                       ifelse(clinical_df$V6 == 7,1,0) |
                       ifelse(clinical_df$V7 == 7,1,0) )
# Change logical operators for numeric
clinical_df$neurite_b <- as.numeric(clinical_df$neurite_b)
clinical_df$neurite_u <- as.numeric(clinical_df$neurite_u)
clinical_df$mielite_parcial <- as.numeric(clinical_df$mielite_parcial)
clinical_df$mielite_transversa <- as.numeric(clinical_df$mielite_transversa)
clinical_df$adem <- as.numeric(clinical_df$adem)
clinical_df$outros_clinic <- as.numeric(clinical_df$outros_clinic)
clinical_df$romboencefalite <- as.numeric(clinical_df$romboencefalite)
#Subset dummie
clinical_dummie <- clinical_df[, 8:14]
# Create NO and Mielite
clinical_dummie <- clinical_dummie %>%
        mutate(neurite_all = neurite_b + neurite_u) %>%
        mutate(mielite_all = mielite_parcial + mielite_transversa)
# Exclude clinical onset and bind clinical dummie
emocemp_tmp <- emocemp[, -15]
emocemp <- cbind(emocemp_tmp, clinical_dummie)
# Change dummie to factors
emocemp[, 83:91] <- lapply(emocemp[, 83:91], factor)


# ---------------------------------------------------
# Acute phase treatment -----------------------------
# ---------------------------------------------------

# Splitting
tto_a_cha <- as.character(emocemp$tto_fa)
tto_a_split <- str_split_fixed(tto_a_cha, "," , 5)
tto_a_df <- as.data.frame(tto_a_split, stringsAsFactors = FALSE)

# Zero NAs
tto_a_df$V1[is.na(tto_a_df$V1)] <- 0
tto_a_df$V2[is.na(tto_a_df$V2)] <- 0
tto_a_df$V3[is.na(tto_a_df$V3)] <- 0
tto_a_df$V4[is.na(tto_a_df$V4)] <- 0
tto_a_df$V5[is.na(tto_a_df$V5)] <- 0

# Dummie columns
tto_a_df <- tto_a_df %>%
        mutate(ivmp= ifelse(tto_a_df$V1 == 1, 1, 0) | 
                       ifelse(tto_a_df$V2 == 1,1,0) |
                       ifelse(tto_a_df$V3 == 1,1,0) |
                       ifelse(tto_a_df$V4 == 1,1,0) |
                       ifelse(tto_a_df$V5 == 1,1,0) ) %>%
        
        mutate(corticoide_vo = ifelse(tto_a_df$V1 == 2, 1, 0) | 
                       ifelse(tto_a_df$V2 == 2,1,0) |
                       ifelse(tto_a_df$V3 == 2,1,0) |
                       ifelse(tto_a_df$V4 == 2,1,0) |
                       ifelse(tto_a_df$V5 == 2,1,0) ) %>%
        
        mutate(igiv= ifelse(tto_a_df$V1 == 3, 1, 0) | 
                       ifelse(tto_a_df$V2 == 3,1,0) |
                       ifelse(tto_a_df$V3 == 3,1,0) |
                       ifelse(tto_a_df$V4 == 3,1,0) |
                       ifelse(tto_a_df$V5 == 3,1,0) ) %>%
        
        mutate(plex = ifelse(tto_a_df$V1 == 4, 1, 0) | 
                       ifelse(tto_a_df$V2 == 4,1,0) |
                       ifelse(tto_a_df$V3 == 4,1,0) |
                       ifelse(tto_a_df$V4 == 4,1,0) |
                       ifelse(tto_a_df$V5 == 4,1,0) ) %>%
        
        mutate(nenhum_tto_fa = ifelse(tto_a_df$V1 == 5, 1, 0) | 
                       ifelse(tto_a_df$V2 == 5,1,0) |
                       ifelse(tto_a_df$V3 == 5,1,0) |
                       ifelse(tto_a_df$V4 == 5,1,0) |
                       ifelse(tto_a_df$V5 == 5,1,0) )
# Convert to numeric
tto_a_df$ivmp <- as.numeric(tto_a_df$ivmp)
tto_a_df$corticoide_vo <- as.numeric(tto_a_df$corticoide_vo)
tto_a_df$igiv <- as.numeric(tto_a_df$igiv)
tto_a_df$plex <- as.numeric(tto_a_df$plex)
tto_a_df$nenhum_tto_fa <- as.numeric(tto_a_df$nenhum_tto_fa)

# Subset and create dummie
tto_a_df[,6:10] <- lapply(tto_a_df[,6:10], factor)
tto_dummie <- tto_a_df[,6:10]
#Incorporate do dataset
emocemp_tmp <- emocemp[, -62]
emocemp <- cbind(emocemp_tmp, tto_dummie)


# DMD ---------------------------------------------------
dmd_cha <- as.character(emocemp$dmd)
dmd_num <- as.numeric(dmd_cha)
dmd_num[is.na(dmd_num)] <- 0
emocemp$dmd <- dmd_num
emocemp$dmd <- sapply(emocemp$dmd, factor)

# -----------------------------------------------------
# Neuroimaging analysis
# -----------------------------------------------------


# Brain image -----------------------------------------------------
# Change to strings and create df
bimage_cha <- as.character(emocemp$brain_mr)
bimage_split <- str_split_fixed(bimage_cha, "," , 15)
bimage_df <- as.data.frame(bimage_split)

# Create dummie
bimage_df$V1 <- as.character(bimage_df$V1)
bimage_df$V1 <- as.numeric(bimage_df$V1)
bimage_df$V1[is.na(bimage_df$V1)] <- 0 # Change NA for 0

bimage_df$V2 <- as.character(bimage_df$V2)
bimage_df$V2 <- as.numeric(bimage_df$V2)
bimage_df$V2[is.na(bimage_df$V2)] <- 0 # Change NA for 0

bimage_df$V3 <- as.character(bimage_df$V3)
bimage_df$V3 <- as.numeric(bimage_df$V3)
bimage_df$V3[is.na(bimage_df$V3)] <- 0 # Change NA for 0

bimage_df$V4 <- as.character(bimage_df$V4)
bimage_df$V4 <- as.numeric(bimage_df$V4)
bimage_df$V4[is.na(bimage_df$V4)] <- 0 # Change NA for 0

bimage_df$V5 <- as.character(bimage_df$V5)
bimage_df$V5 <- as.numeric(bimage_df$V5)
bimage_df$V5[is.na(bimage_df$V5)] <- 0 # Change NA for 0

bimage_df$V6 <- as.character(bimage_df$V6)
bimage_df$V6 <- as.numeric(bimage_df$V6)
bimage_df$V6[is.na(bimage_df$V6)] <- 0 # Change NA for 0

bimage_df$V7 <- as.character(bimage_df$V7)
bimage_df$V7 <- as.numeric(bimage_df$V7)
bimage_df$V7[is.na(bimage_df$V7)] <- 0 # Change NA for 0

bimage_df$V8 <- as.character(bimage_df$V8)
bimage_df$V8 <- as.numeric(bimage_df$V8)
bimage_df$V8[is.na(bimage_df$V8)] <- 0 # Change NA for 0

bimage_df$V9 <- as.character(bimage_df$V9)
bimage_df$V9 <- as.numeric(bimage_df$V9)
bimage_df$V9[is.na(bimage_df$V9)] <- 0 # Change NA for 0

bimage_df$V10 <- as.character(bimage_df$V10)
bimage_df$V10 <- as.numeric(bimage_df$V10)
bimage_df$V10[is.na(bimage_df$V10)] <- 0 # Change NA for 0

bimage_df$V11 <- as.character(bimage_df$V11)
bimage_df$V11 <- as.numeric(bimage_df$V11)
bimage_df$V11[is.na(bimage_df$V11)] <- 0 # Change NA for 0

bimage_df$V12 <- as.character(bimage_df$V12)
bimage_df$V12 <- as.numeric(bimage_df$V12)
bimage_df$V12[is.na(bimage_df$V12)] <- 0 # Change NA for 0

bimage_df$V13<- as.character(bimage_df$V13)
bimage_df$V13 <- as.numeric(bimage_df$V13)
bimage_df$V13[is.na(bimage_df$V13)] <- 0 # Change NA for 0

bimage_df$V14 <- as.character(bimage_df$V14)
bimage_df$V14 <- as.numeric(bimage_df$V14)
bimage_df$V14[is.na(bimage_df$V14)] <- 0 # Change NA for 0

bimage_df$V15 <- as.character(bimage_df$V15)
bimage_df$V15 <- as.numeric(bimage_df$V15)
bimage_df$V15[is.na(bimage_df$V15)] <- 0 # Change NA for 0

# Creating new columns
bimage_df <- bimage_df %>%
        mutate(uma_ou_mais_perpendicular_cc = ifelse(bimage_df$V1 == 1, 1, 0) | 
                       ifelse(bimage_df$V2 == 1,1,0) |
                       ifelse(bimage_df$V3 == 1,1,0) |
                       ifelse(bimage_df$V4 == 1,1,0) |
                       ifelse(bimage_df$V5 == 1,1,0) |
                       ifelse(bimage_df$V6 == 1,1,0) |
                       ifelse(bimage_df$V7 == 1,1,0) |
                       ifelse(bimage_df$V8 == 1,1,0) |
                       ifelse(bimage_df$V9 == 1,1,0) |
                       ifelse(bimage_df$V10 == 1,1,0) |
                       ifelse(bimage_df$V11 == 1,1,0) |
                       ifelse(bimage_df$V12 == 1,1,0) |
                       ifelse(bimage_df$V13 == 1,1,0) |
                       ifelse(bimage_df$V14 == 1,1,0) |
                       ifelse(bimage_df$V15 == 1,1,0) ) %>%
        
        mutate(apenas_bem_delimit = ifelse(bimage_df$V1 == 2, 1, 0) | 
                       ifelse(bimage_df$V2 == 2,1,0) |
                       ifelse(bimage_df$V3 == 2,1,0) |
                       ifelse(bimage_df$V4 == 2,1,0) |
                       ifelse(bimage_df$V5 == 2,1,0) |
                       ifelse(bimage_df$V6 == 2,1,0) |
                       ifelse(bimage_df$V7 == 2,1,0) |
                       ifelse(bimage_df$V8 == 2,1,0) |
                       ifelse(bimage_df$V9 == 2,1,0) |
                       ifelse(bimage_df$V10 == 2,1,0) |
                       ifelse(bimage_df$V11 == 2,1,0) |
                       ifelse(bimage_df$V12 == 2,1,0) |
                       ifelse(bimage_df$V13 == 2,1,0) |
                       ifelse(bimage_df$V14 == 2,1,0) |
                       ifelse(bimage_df$V15 == 2,1,0) ) %>%
        
        mutate(cinco_ou_mais_hipert2 = ifelse(bimage_df$V1 == 3, 1, 0) | 
                       ifelse(bimage_df$V2 == 3,1,0) |
                       ifelse(bimage_df$V3 == 3,1,0) |
                       ifelse(bimage_df$V4 == 3,1,0) |
                       ifelse(bimage_df$V5 == 3,1,0) |
                       ifelse(bimage_df$V6 == 3,1,0) |
                       ifelse(bimage_df$V7 == 3,1,0) |
                       ifelse(bimage_df$V8 == 3,1,0) |
                       ifelse(bimage_df$V9 == 3,1,0) |
                       ifelse(bimage_df$V10 == 3,1,0) |
                       ifelse(bimage_df$V11 == 3,1,0) |
                       ifelse(bimage_df$V12 == 3,1,0) |
                       ifelse(bimage_df$V13 == 3,1,0) |
                       ifelse(bimage_df$V14 == 3,1,0) |
                       ifelse(bimage_df$V15 == 3,1,0) ) %>%
        
        mutate(duas_ou_mais_peri = ifelse(bimage_df$V1 == 4, 1, 0) | 
                       ifelse(bimage_df$V2 == 4,1,0) |
                       ifelse(bimage_df$V3 == 4,1,0) |
                       ifelse(bimage_df$V4 == 4,1,0) |
                       ifelse(bimage_df$V5 == 4,1,0) |
                       ifelse(bimage_df$V6 == 4,1,0) |
                       ifelse(bimage_df$V7 == 4,1,0) |
                       ifelse(bimage_df$V8 == 4,1,0) |
                       ifelse(bimage_df$V9 == 4,1,0) |
                       ifelse(bimage_df$V10 == 4,1,0) |
                       ifelse(bimage_df$V11 == 4,1,0) |
                       ifelse(bimage_df$V12 == 4,1,0) |
                       ifelse(bimage_df$V13 == 4,1,0) |
                       ifelse(bimage_df$V14 == 4,1,0) |
                       ifelse(bimage_df$V15 == 4,1,0) ) %>%
        
        mutate(uma_tronco = ifelse(bimage_df$V1 == 5, 1, 0) | 
                       ifelse(bimage_df$V2 == 5,1,0) |
                       ifelse(bimage_df$V3 == 5,1,0) |
                       ifelse(bimage_df$V4 == 5,1,0) |
                       ifelse(bimage_df$V5 == 5,1,0) |
                       ifelse(bimage_df$V6 == 5,1,0) |
                       ifelse(bimage_df$V7 == 5,1,0) |
                       ifelse(bimage_df$V8 == 5,1,0) |
                       ifelse(bimage_df$V9 == 5,1,0) |
                       ifelse(bimage_df$V10 == 5,1,0) |
                       ifelse(bimage_df$V11 == 5,1,0) |
                       ifelse(bimage_df$V12 == 5,1,0) |
                       ifelse(bimage_df$V13 == 5,1,0) |
                       ifelse(bimage_df$V14 == 5,1,0) |
                       ifelse(bimage_df$V15 == 5,1,0) ) %>%
        
        mutate(black_holes = ifelse(bimage_df$V1 == 6, 1, 0) | 
                       ifelse(bimage_df$V2 == 6,1,0) |
                       ifelse(bimage_df$V3 == 6,1,0) |
                       ifelse(bimage_df$V4 == 6,1,0) |
                       ifelse(bimage_df$V5 == 6,1,0) |
                       ifelse(bimage_df$V6 == 6,1,0) |
                       ifelse(bimage_df$V7 == 6,1,0) |
                       ifelse(bimage_df$V8 == 6,1,0) |
                       ifelse(bimage_df$V9 == 6,1,0) |
                       ifelse(bimage_df$V10 == 6,1,0) |
                       ifelse(bimage_df$V11 == 6,1,0) |
                       ifelse(bimage_df$V12 == 6,1,0) |
                       ifelse(bimage_df$V13 == 6,1,0) |
                       ifelse(bimage_df$V14 == 6,1,0) |
                       ifelse(bimage_df$V15 == 6,1,0) ) %>%
        
        mutate(difusas_bilat = ifelse(bimage_df$V1 == 7, 1, 0) | 
                       ifelse(bimage_df$V2 == 7,1,0) |
                       ifelse(bimage_df$V3 == 7,1,0) |
                       ifelse(bimage_df$V4 == 7,1,0) |
                       ifelse(bimage_df$V5 == 7,1,0) |
                       ifelse(bimage_df$V6 == 7,1,0) |
                       ifelse(bimage_df$V7 == 7,1,0) |
                       ifelse(bimage_df$V8 == 7,1,0) |
                       ifelse(bimage_df$V9 == 7,1,0) |
                       ifelse(bimage_df$V10 == 7,1,0) |
                       ifelse(bimage_df$V11 == 7,1,0) |
                       ifelse(bimage_df$V12 == 7,1,0) |
                       ifelse(bimage_df$V13 == 7,1,0) |
                       ifelse(bimage_df$V14 == 7,1,0) |
                       ifelse(bimage_df$V15 == 7,1,0) ) %>%
        
        mutate(uma_ou_mais_justa = ifelse(bimage_df$V1 == 8, 1, 0) | 
                       ifelse(bimage_df$V2 == 8,1,0) |
                       ifelse(bimage_df$V3 == 8,1,0) |
                       ifelse(bimage_df$V4 == 8,1,0) |
                       ifelse(bimage_df$V5 == 8,1,0) |
                       ifelse(bimage_df$V6 == 8,1,0) |
                       ifelse(bimage_df$V7 == 8,1,0) |
                       ifelse(bimage_df$V8 == 8,1,0) |
                       ifelse(bimage_df$V9 == 8,1,0) |
                       ifelse(bimage_df$V10 == 8,1,0) |
                       ifelse(bimage_df$V11 == 8,1,0) |
                       ifelse(bimage_df$V12 == 8,1,0) |
                       ifelse(bimage_df$V13 == 8,1,0) |
                       ifelse(bimage_df$V14 == 8,1,0) |
                       ifelse(bimage_df$V15 == 8,1,0) ) %>%
        
        mutate(uma_ou_mais_peri = ifelse(bimage_df$V1 == 9, 1, 0) | 
                       ifelse(bimage_df$V2 == 9,1,0) |
                       ifelse(bimage_df$V3 == 9,1,0) |
                       ifelse(bimage_df$V4 == 9,1,0) |
                       ifelse(bimage_df$V5 == 9,1,0) |
                       ifelse(bimage_df$V6 == 9,1,0) |
                       ifelse(bimage_df$V7 == 9,1,0) |
                       ifelse(bimage_df$V8 == 9,1,0) |
                       ifelse(bimage_df$V9 == 9,1,0) |
                       ifelse(bimage_df$V10 == 9,1,0) |
                       ifelse(bimage_df$V11 == 9,1,0) |
                       ifelse(bimage_df$V12 == 9,1,0) |
                       ifelse(bimage_df$V13 == 9,1,0) |
                       ifelse(bimage_df$V14 == 9,1,0) |
                       ifelse(bimage_df$V15 == 9,1,0) ) %>%
        
        mutate(uma_ou_mais_infra = ifelse(bimage_df$V1 == 10, 1, 0) | 
                       ifelse(bimage_df$V2 == 10,1,0) |
                       ifelse(bimage_df$V3 == 10,1,0) |
                       ifelse(bimage_df$V4 == 10,1,0) |
                       ifelse(bimage_df$V5 == 10,1,0) |
                       ifelse(bimage_df$V6 == 10,1,0) |
                       ifelse(bimage_df$V7 == 10,1,0) |
                       ifelse(bimage_df$V8 == 10,1,0) |
                       ifelse(bimage_df$V9 == 10,1,0) |
                       ifelse(bimage_df$V10 == 10,1,0) |
                       ifelse(bimage_df$V11 == 10,1,0) |
                       ifelse(bimage_df$V12 == 10,1,0) |
                       ifelse(bimage_df$V13 == 10,1,0) |
                       ifelse(bimage_df$V14 == 10,1,0) |
                       ifelse(bimage_df$V15 == 10,1,0) ) %>%
        
        mutate(realce_anel_inc = ifelse(bimage_df$V1 == 11, 1, 0) | 
                       ifelse(bimage_df$V2 == 11,1,0) |
                       ifelse(bimage_df$V3 == 11,1,0) |
                       ifelse(bimage_df$V4 == 11,1,0) |
                       ifelse(bimage_df$V5 == 11,1,0) |
                       ifelse(bimage_df$V6 == 11,1,0) |
                       ifelse(bimage_df$V7 == 11,1,0) |
                       ifelse(bimage_df$V8 == 11,1,0) |
                       ifelse(bimage_df$V9 == 11,1,0) |
                       ifelse(bimage_df$V10 == 11,1,0) |
                       ifelse(bimage_df$V11 == 11,1,0) |
                       ifelse(bimage_df$V12 == 11,1,0) |
                       ifelse(bimage_df$V13 == 11,1,0) |
                       ifelse(bimage_df$V14 == 11,1,0) |
                       ifelse(bimage_df$V15 == 11,1,0) ) %>%
        
        mutate(outro_realce = ifelse(bimage_df$V1 == 12, 1, 0) | 
                       ifelse(bimage_df$V2 == 12,1,0) |
                       ifelse(bimage_df$V3 == 12,1,0) |
                       ifelse(bimage_df$V4 == 12,1,0) |
                       ifelse(bimage_df$V5 == 12,1,0) |
                       ifelse(bimage_df$V6 == 12,1,0) |
                       ifelse(bimage_df$V7 == 12,1,0) |
                       ifelse(bimage_df$V8 == 12,1,0) |
                       ifelse(bimage_df$V9 == 12,1,0) |
                       ifelse(bimage_df$V10 == 12,1,0) |
                       ifelse(bimage_df$V11 == 12,1,0) |
                       ifelse(bimage_df$V12 == 12,1,0) |
                       ifelse(bimage_df$V13 == 12,1,0) |
                       ifelse(bimage_df$V14 == 12,1,0) |
                       ifelse(bimage_df$V15 == 12,1,0) ) %>%
        
        mutate(nenhum_brm = ifelse(bimage_df$V1 == 13, 1, 0) | 
                       ifelse(bimage_df$V2 == 13,1,0) |
                       ifelse(bimage_df$V3 == 13,1,0) |
                       ifelse(bimage_df$V4 == 13,1,0) |
                       ifelse(bimage_df$V5 == 13,1,0) |
                       ifelse(bimage_df$V6 == 13,1,0) |
                       ifelse(bimage_df$V7 == 13,1,0) |
                       ifelse(bimage_df$V8 == 13,1,0) |
                       ifelse(bimage_df$V9 == 13,1,0) |
                       ifelse(bimage_df$V10 == 13,1,0) |
                       ifelse(bimage_df$V11 == 13,1,0) |
                       ifelse(bimage_df$V12 == 13,1,0) |
                       ifelse(bimage_df$V13 == 13,1,0) |
                       ifelse(bimage_df$V14 == 13,1,0) |
                       ifelse(bimage_df$V15 == 13,1,0) ) %>%
        
        mutate(nao_realizada_brm = ifelse(bimage_df$V1 == 14, 1, 0) | 
                       ifelse(bimage_df$V2 == 14,1,0) |
                       ifelse(bimage_df$V3 == 14,1,0) |
                       ifelse(bimage_df$V4 == 14,1,0) |
                       ifelse(bimage_df$V5 == 14,1,0) |
                       ifelse(bimage_df$V6 == 14,1,0) |
                       ifelse(bimage_df$V7 == 14,1,0) |
                       ifelse(bimage_df$V8 == 14,1,0) |
                       ifelse(bimage_df$V9 == 14,1,0) |
                       ifelse(bimage_df$V10 == 14,1,0) |
                       ifelse(bimage_df$V11 == 14,1,0) |
                       ifelse(bimage_df$V12 == 14,1,0) |
                       ifelse(bimage_df$V13 == 14,1,0) |
                       ifelse(bimage_df$V14 == 14,1,0) |
                       ifelse(bimage_df$V15 == 14,1,0) ) %>%
        
        mutate(outras_brm = ifelse(bimage_df$V1 == 15, 1, 0) | 
                       ifelse(bimage_df$V2 == 15,1,0) |
                       ifelse(bimage_df$V3 == 15,1,0) |
                       ifelse(bimage_df$V4 == 15,1,0) |
                       ifelse(bimage_df$V5 == 15,1,0) |
                       ifelse(bimage_df$V6 == 15,1,0) |
                       ifelse(bimage_df$V7 == 15,1,0) |
                       ifelse(bimage_df$V8 == 15,1,0) |
                       ifelse(bimage_df$V9 == 15,1,0) |
                       ifelse(bimage_df$V10 == 15,1,0) |
                       ifelse(bimage_df$V11 == 15,1,0) |
                       ifelse(bimage_df$V12 == 15,1,0) |
                       ifelse(bimage_df$V13 == 15,1,0) |
                       ifelse(bimage_df$V14 == 15,1,0) |
                       ifelse(bimage_df$V15 == 15,1,0) )

# Change logical operators for numeric
bimage_df$uma_ou_mais_perpendicular_cc <- as.numeric(bimage_df$uma_ou_mais_perpendicular_cc)
bimage_df$apenas_bem_delimit <- as.numeric(bimage_df$apenas_bem_delimit)
bimage_df$cinco_ou_mais_hipert2 <- as.numeric(bimage_df$cinco_ou_mais_hipert2)
bimage_df$duas_ou_mais_peri <- as.numeric(bimage_df$duas_ou_mais_peri)
bimage_df$uma_ou_mais_justa <- as.numeric(bimage_df$uma_ou_mais_justa)
bimage_df$uma_ou_mais_peri <- as.numeric(bimage_df$uma_ou_mais_peri)
bimage_df$uma_ou_mais_infra <- as.numeric(bimage_df$uma_ou_mais_infra)
bimage_df$uma_tronco <- as.numeric(bimage_df$uma_tronco)
bimage_df$black_holes <- as.numeric(bimage_df$black_holes)
bimage_df$difusas_bilat <- as.numeric(bimage_df$difusas_bilat)
bimage_df$realce_anel_inc<- as.numeric(bimage_df$realce_anel_inc)
bimage_df$outro_realce <- as.numeric(bimage_df$outro_realce)
bimage_df$nenhum_brm <- as.numeric(bimage_df$nenhum_brm )
bimage_df$nao_realizada_brm <- as.numeric(bimage_df$nao_realizada_brm)
bimage_df$outras_brm <- as.numeric(bimage_df$outras_brm)

#Subset dummie
bimg_dummie <- bimage_df[, 16:30]
# Exclude clinical onset and bind clinical dummie
col_unwanted <- "brain_rm"
emocemp_tmp = emocemp[,-39]
emocemp <- cbind(emocemp_tmp, bimg_dummie)
# Change dummie to factors
emocemp[, 91:105] <- lapply(emocemp[, 91:105], factor)


# Spinal image -------------------------------------------------------------
simage_cha <- as.character(emocemp$spinal_rm)
simage_split <- str_split_fixed(simage_cha, "," , 13)
simage_df <- as.data.frame(simage_split)

# Create dummie
simage_df$V1 <- as.character(simage_df$V1)
simage_df$V1 <- as.numeric(simage_df$V1)
simage_df$V1[is.na(simage_df$V1)] <- 0 # Change NA for 0

simage_df$V2 <- as.character(simage_df$V2)
simage_df$V2 <- as.numeric(simage_df$V2)
simage_df$V2[is.na(simage_df$V2)] <- 0 # Change NA for 0

simage_df$V3 <- as.character(simage_df$V3)
simage_df$V3 <- as.numeric(simage_df$V3)
simage_df$V3[is.na(simage_df$V3)] <- 0 # Change NA for 0

simage_df$V4 <- as.character(simage_df$V4)
simage_df$V4 <- as.numeric(simage_df$V4)
simage_df$V4[is.na(simage_df$V4)] <- 0 # Change NA for 0

simage_df$V5 <- as.character(simage_df$V5)
simage_df$V5 <- as.numeric(simage_df$V5)
simage_df$V5[is.na(simage_df$V5)] <- 0 # Change NA for 0


simage_df$V6 <- as.character(simage_df$V6)
simage_df$V6 <- as.numeric(simage_df$V6)
simage_df$V6[is.na(simage_df$V6)] <- 0 # Change NA for 0

simage_df$V7 <- as.character(simage_df$V7)
simage_df$V7 <- as.numeric(simage_df$V7)
simage_df$V7[is.na(simage_df$V7)] <- 0 # Change NA for 0

simage_df$V8 <- as.character(simage_df$V8)
simage_df$V8 <- as.numeric(simage_df$V8)
simage_df$V8[is.na(simage_df$V8)] <- 0 # Change NA for 0

simage_df$V9 <- as.character(simage_df$V9)
simage_df$V9 <- as.numeric(simage_df$V9)
simage_df$V9[is.na(simage_df$V9)] <- 0 # Change NA for 0

simage_df$V10 <- as.character(simage_df$V10)
simage_df$V10 <- as.numeric(simage_df$V10)
simage_df$V10[is.na(simage_df$V10)] <- 0 # Change NA for 0

simage_df$V11 <- as.character(simage_df$V11)
simage_df$V11 <- as.numeric(simage_df$V11)
simage_df$V11[is.na(simage_df$V11)] <- 0 # Change NA for 0

simage_df$V12 <- as.character(simage_df$V12)
simage_df$V12 <- as.numeric(simage_df$V12)
simage_df$V12[is.na(simage_df$V12)] <- 0 # Change NA for 0

simage_df$V13 <- as.character(simage_df$V13)
simage_df$V13 <- as.numeric(simage_df$V13)
simage_df$V13[is.na(simage_df$V13)] <- 0 # Change NA for 0

# Creating new columns
simage_df <- simage_df %>%
        mutate(mielite_trans_mr = ifelse(simage_df$V1 == 1, 1, 0) | 
                       ifelse(simage_df$V2 == 1,1,0) |
                       ifelse(simage_df$V3 == 1,1,0) |
                       ifelse(simage_df$V4 == 1,1,0) |
                       ifelse(simage_df$V5 == 1,1,0) |
                       ifelse(simage_df$V6 == 1,1,0) |
                       ifelse(simage_df$V7 == 1,1,0) |
                       ifelse(simage_df$V8 == 1,1,0) |
                       ifelse(simage_df$V9 == 1,1,0) |
                       ifelse(simage_df$V10 == 1,1,0) |
                       ifelse(simage_df$V11 == 1,1,0) |
                       ifelse(simage_df$V12 == 1,1,0) |
                       ifelse(simage_df$V13 == 1,1,0) ) %>%
        
        mutate(mielite_centromedular = ifelse(simage_df$V1 == 2, 1, 0) | 
                       ifelse(simage_df$V2 == 2,1,0) |
                       ifelse(simage_df$V3 == 2,1,0) |
                       ifelse(simage_df$V4 == 2,1,0) |
                       ifelse(simage_df$V5 == 2,1,0) |
                       ifelse(bimage_df$V6 == 2,1,0) |
                       ifelse(simage_df$V7 == 2,1,0) |
                       ifelse(simage_df$V8 == 2,1,0) |
                       ifelse(simage_df$V9 == 2,1,0) |
                       ifelse(simage_df$V10 == 2,1,0) |
                       ifelse(simage_df$V11 == 2,1,0) |
                       ifelse(simage_df$V12 == 2,1,0) |
                       ifelse(simage_df$V13 == 2,1,0) ) %>%
        
        mutate(mielite_perif = ifelse(simage_df$V1 == 3, 1, 0) | 
                       ifelse(simage_df$V2 == 3,1,0) |
                       ifelse(simage_df$V3 == 3,1,0) |
                       ifelse(simage_df$V4 == 3,1,0) |
                       ifelse(simage_df$V5 == 3,1,0) |
                       ifelse(simage_df$V6 == 3,1,0) |
                       ifelse(simage_df$V7 == 3,1,0) |
                       ifelse(simage_df$V8 == 3,1,0) |
                       ifelse(simage_df$V9 == 3,1,0) |
                       ifelse(simage_df$V10 == 3,1,0) |
                       ifelse(simage_df$V11 == 3,1,0) |
                       ifelse(simage_df$V12 == 3,1,0) |
                       ifelse(simage_df$V13 == 3,1,0) ) %>%
        
        mutate(mielite_menos3 = ifelse(simage_df$V1 == 4, 1, 0) | 
                       ifelse(simage_df$V2 == 4,1,0) |
                       ifelse(simage_df$V3 == 4,1,0) |
                       ifelse(simage_df$V4 == 4,1,0) |
                       ifelse(simage_df$V5 == 4,1,0) |
                       ifelse(simage_df$V6 == 4,1,0) |
                       ifelse(simage_df$V7 == 4,1,0) |
                       ifelse(simage_df$V8 == 4,1,0) |
                       ifelse(simage_df$V9 == 4,1,0) |
                       ifelse(simage_df$V10 == 4,1,0) |
                       ifelse(simage_df$V11 == 4,1,0) |
                       ifelse(simage_df$V12 == 4,1,0) |
                       ifelse(simage_df$V13 == 4,1,0) ) %>%
        
        mutate(letm = ifelse(simage_df$V1 == 5, 1, 0) | 
                       ifelse(simage_df$V2 == 5,1,0) |
                       ifelse(simage_df$V3 == 5,1,0) |
                       ifelse(simage_df$V4 == 5,1,0) |
                       ifelse(simage_df$V5 == 5,1,0) |
                       ifelse(simage_df$V6 == 5,1,0) |
                       ifelse(simage_df$V7 == 5,1,0) |
                       ifelse(simage_df$V8 == 5,1,0) |
                       ifelse(simage_df$V9 == 5,1,0) |
                       ifelse(simage_df$V10 == 5,1,0) |
                       ifelse(simage_df$V11 == 5,1,0) |
                       ifelse(simage_df$V12 == 5,1,0) |
                       ifelse(simage_df$V13 == 5,1,0) ) %>%
        
        mutate(mielite_cerv = ifelse(simage_df$V1 == 6, 1, 0) | 
                       ifelse(simage_df$V2 == 6,1,0) |
                       ifelse(simage_df$V3 == 6,1,0) |
                       ifelse(simage_df$V4 == 6,1,0) |
                       ifelse(simage_df$V5 == 6,1,0) |
                       ifelse(simage_df$V6 == 6,1,0) |
                       ifelse(simage_df$V7 == 6,1,0) |
                       ifelse(simage_df$V8 == 6,1,0) |
                       ifelse(simage_df$V9 == 6,1,0) |
                       ifelse(simage_df$V10 == 6,1,0) |
                       ifelse(simage_df$V11 == 6,1,0) |
                       ifelse(simage_df$V12 == 6,1,0) |
                       ifelse(simage_df$V13 == 6,1,0) ) %>%
        
        mutate(mielite_dorsal = ifelse(simage_df$V1 == 7, 1, 0) | 
                       ifelse(simage_df$V2 == 7,1,0) |
                       ifelse(simage_df$V3 == 7,1,0) |
                       ifelse(simage_df$V4 == 7,1,0) |
                       ifelse(simage_df$V5 == 7,1,0) |
                       ifelse(simage_df$V6 == 7,1,0) |
                       ifelse(simage_df$V7 == 7,1,0) |
                       ifelse(simage_df$V8 == 7,1,0) |
                       ifelse(simage_df$V9 == 7,1,0) |
                       ifelse(simage_df$V10 == 7,1,0) |
                       ifelse(simage_df$V11 == 7,1,0) |
                       ifelse(simage_df$V12 == 7,1,0) |
                       ifelse(simage_df$V13 == 7,1,0) ) %>%
        
        mutate(mielite_lombo = ifelse(simage_df$V1 == 8, 1, 0) | 
                       ifelse(simage_df$V2 == 8,1,0) |
                       ifelse(simage_df$V3 == 8,1,0) |
                       ifelse(simage_df$V4 == 8,1,0) |
                       ifelse(simage_df$V5 == 8,1,0) |
                       ifelse(simage_df$V6 == 8,1,0) |
                       ifelse(simage_df$V7 == 8,1,0) |
                       ifelse(simage_df$V8 == 8,1,0) |
                       ifelse(simage_df$V9 == 8,1,0) |
                       ifelse(simage_df$V10 == 8,1,0) |
                       ifelse(simage_df$V11 == 8,1,0) |
                       ifelse(simage_df$V12 == 8,1,0) |
                       ifelse(simage_df$V13 == 8,1,0) ) %>%
        
        mutate(nenhuma_spinalmr = ifelse(simage_df$V1 == 9, 1, 0) | 
                       ifelse(simage_df$V2 == 9,1,0) |
                       ifelse(simage_df$V3 == 9,1,0) |
                       ifelse(simage_df$V4 == 9,1,0) |
                       ifelse(simage_df$V5 == 9,1,0) |
                       ifelse(simage_df$V6 == 9,1,0) |
                       ifelse(simage_df$V7 == 9,1,0) |
                       ifelse(simage_df$V8 == 9,1,0) |
                       ifelse(simage_df$V9 == 9,1,0) |
                       ifelse(simage_df$V10 == 9,1,0) |
                       ifelse(simage_df$V11 == 9,1,0) |
                       ifelse(simage_df$V12 == 9,1,0) |
                       ifelse(simage_df$V13 == 9,1,0) ) %>%
        
        mutate(nao_realizada_srm = ifelse(simage_df$V1 == 10, 1, 0) | 
                       ifelse(simage_df$V2 == 10,1,0) |
                       ifelse(simage_df$V3 == 10,1,0) |
                       ifelse(simage_df$V4 == 10,1,0) |
                       ifelse(simage_df$V5 == 10,1,0) |
                       ifelse(simage_df$V6 == 10,1,0) |
                       ifelse(simage_df$V7 == 10,1,0) |
                       ifelse(simage_df$V8 == 10,1,0) |
                       ifelse(simage_df$V9 == 10,1,0) |
                       ifelse(simage_df$V10 == 10,1,0) |
                       ifelse(simage_df$V11 == 10,1,0) |
                       ifelse(simage_df$V12 == 10,1,0) |
                       ifelse(simage_df$V13 == 10,1,0) ) %>%
        
        mutate(realce_medular = ifelse(simage_df$V1 == 11, 1, 0) | 
                       ifelse(simage_df$V2 == 11,1,0) |
                       ifelse(simage_df$V3 == 11,1,0) |
                       ifelse(simage_df$V4 == 11,1,0) |
                       ifelse(simage_df$V5 == 11,1,0) |
                       ifelse(simage_df$V6 == 11,1,0) |
                       ifelse(simage_df$V7 == 11,1,0) |
                       ifelse(simage_df$V8 == 11,1,0) |
                       ifelse(simage_df$V9 == 11,1,0) |
                       ifelse(simage_df$V10 == 11,1,0) |
                       ifelse(simage_df$V11 == 11,1,0) |
                       ifelse(simage_df$V12 == 11,1,0) |
                       ifelse(simage_df$V13 == 11,1,0) ) %>%
        
        mutate(bright_spot = ifelse(simage_df$V1 == 12, 1, 0) | 
                       ifelse(simage_df$V2 == 12,1,0) |
                       ifelse(simage_df$V3 == 12,1,0) |
                       ifelse(simage_df$V4 == 12,1,0) |
                       ifelse(simage_df$V5 == 12,1,0) |
                       ifelse(simage_df$V6 == 12,1,0) |
                       ifelse(simage_df$V7 == 12,1,0) |
                       ifelse(simage_df$V8 == 12,1,0) |
                       ifelse(simage_df$V9 == 12,1,0) |
                       ifelse(simage_df$V10 == 12,1,0) |
                       ifelse(simage_df$V11 == 12,1,0) |
                       ifelse(simage_df$V12 == 12,1,0) |
                       ifelse(simage_df$V13 == 12,1,0) ) %>%
        
        mutate(outras_srm = ifelse(simage_df$V1 == 13, 1, 0) | 
                       ifelse(simage_df$V2 == 13,1,0) |
                       ifelse(simage_df$V3 == 13,1,0) |
                       ifelse(simage_df$V4 == 13,1,0) |
                       ifelse(simage_df$V5 == 13,1,0) |
                       ifelse(simage_df$V6 == 13,1,0) |
                       ifelse(simage_df$V7 == 13,1,0) |
                       ifelse(simage_df$V8 == 13,1,0) |
                       ifelse(simage_df$V9 == 13,1,0) |
                       ifelse(simage_df$V10 == 13,1,0) |
                       ifelse(simage_df$V11 == 13,1,0) |
                       ifelse(simage_df$V12 == 13,1,0) |
                       ifelse(simage_df$V13 == 13,1,0) )

simage_df$mielite_centromedular <- as.numeric(simage_df$mielite_centromedular)
simage_df$mielite_trans_mr <- as.numeric(simage_df$mielite_trans_mr)
simage_df$mielite_perif <- as.numeric(simage_df$mielite_perif)
simage_df$mielite_menos3 <- as.numeric(simage_df$mielite_menos3)
simage_df$letm <- as.numeric(simage_df$letm)
simage_df$mielite_cerv <- as.numeric(simage_df$mielite_cerv)
simage_df$mielite_dorsal <- as.numeric(simage_df$mielite_dorsal)
simage_df$mielite_lombo <- as.numeric(simage_df$mielite_lombo)
simage_df$realce_medular <- as.numeric(simage_df$realce_medular)
simage_df$nenhuma_spinalmr <- as.numeric(simage_df$nenhuma_spinalmr)
simage_df$bright_spot <- as.numeric(simage_df$bright_spot)
simage_df$outras_srm <- as.numeric(simage_df$outras_srm)
simage_df$nao_realizada_srm <- as.numeric(simage_df$nao_realizada_srm)

#Subset dummie
simg_dummie <- simage_df[, 14:26]
# Exclude clinical onset and bind clinical dummie

emocemp <- cbind(emocemp, simg_dummie)
# Change dummie to factors
emocemp[, 110:122] <- lapply(emocemp[, 110:122], factor)



# Orbital image ------------------------------------------------------------------
oimage_cha <- as.character(emocemp$orbit_rm)
oimage_split <- str_split_fixed(oimage_cha, "," , 8)
oimage_df <- as.data.frame(oimage_split, stringsAsFactors = FALSE)

# Create dummie
oimage_df$V1 <- as.character(oimage_df$V1)
oimage_df$V1 <- as.numeric(oimage_df$V1)
oimage_df$V1[is.na(oimage_df$V1)] <- 0 # Change NA for 0

oimage_df$V2 <- as.character(oimage_df$V2)
oimage_df$V2 <- as.numeric(oimage_df$V2)
oimage_df$V2[is.na(oimage_df$V2)] <- 0 # Change NA for 0

oimage_df$V3 <- as.character(oimage_df$V3)
oimage_df$V3 <- as.numeric(oimage_df$V3)
oimage_df$V3[is.na(oimage_df$V3)] <- 0 # Change NA for 0

oimage_df$V4 <- as.character(oimage_df$V4)
oimage_df$V4 <- as.numeric(oimage_df$V4)
oimage_df$V4[is.na(oimage_df$V4)] <- 0 # Change NA for 0

oimage_df$V5 <- as.character(oimage_df$V5)
oimage_df$V5 <- as.numeric(oimage_df$V5)
oimage_df$V5[is.na(oimage_df$V5)] <- 0 # Change NA for 0


oimage_df$V6 <- as.character(oimage_df$V6)
oimage_df$V6 <- as.numeric(oimage_df$V6)
oimage_df$V6[is.na(oimage_df$V6)] <- 0 # Change NA for 0

oimage_df$V7 <- as.character(oimage_df$V7)
oimage_df$V7 <- as.numeric(oimage_df$V7)
oimage_df$V7[is.na(oimage_df$V7)] <- 0 # Change NA for 0

oimage_df$V8 <- as.character(oimage_df$V8)
oimage_df$V8 <- as.numeric(oimage_df$V8)
oimage_df$V8[is.na(oimage_df$V8)] <- 0 # Change NA for 0


# Creating new columns
oimage_df <- oimage_df %>%
        mutate(hipersinal_retrob = ifelse(oimage_df$V1 == 1, 1, 0) | 
                       ifelse(oimage_df$V2 == 1,1,0) |
                       ifelse(oimage_df$V3 == 1,1,0) |
                       ifelse(oimage_df$V4 == 1,1,0) |
                       ifelse(oimage_df$V5 == 1,1,0) |
                       ifelse(oimage_df$V6 == 1,1,0) |
                       ifelse(oimage_df$V7 == 1,1,0) |
                       ifelse(oimage_df$V8 == 1,1,0) ) %>%
        
        mutate(hipersinal_quiasma = ifelse(oimage_df$V1 == 2, 1, 0) | 
                       ifelse(oimage_df$V2 == 2,1,0) |
                       ifelse(oimage_df$V3 == 2,1,0) |
                       ifelse(oimage_df$V4 == 2,1,0) |
                       ifelse(oimage_df$V5 == 2,1,0) |
                       ifelse(oimage_df$V6 == 2,1,0) |
                       ifelse(oimage_df$V7 == 2,1,0) |
                       ifelse(oimage_df$V8 == 2,1,0) ) %>%
        
        mutate(hipersinal_media = ifelse(oimage_df$V1 == 3, 1, 0) | 
                       ifelse(oimage_df$V2 == 3,1,0) |
                       ifelse(oimage_df$V3 == 3,1,0) |
                       ifelse(oimage_df$V4 == 3,1,0) |
                       ifelse(oimage_df$V5 == 3,1,0) |
                       ifelse(oimage_df$V6 == 3,1,0) |
                       ifelse(oimage_df$V7 == 3,1,0) |
                       ifelse(oimage_df$V8 == 3,1,0) ) %>%
        
        mutate(hipersinal_extensa = ifelse(oimage_df$V1 == 4, 1, 0) | 
                       ifelse(oimage_df$V2 == 4,1,0) |
                       ifelse(oimage_df$V3 == 4,1,0) |
                       ifelse(oimage_df$V4 == 4,1,0) |
                       ifelse(oimage_df$V5 == 4,1,0) |
                       ifelse(oimage_df$V6 == 4,1,0) |
                       ifelse(oimage_df$V7 == 4,1,0) |
                       ifelse(oimage_df$V8 == 4,1,0) ) %>%
        
        mutate(realce_orbit = ifelse(oimage_df$V1 == 5, 1, 0) | 
                       ifelse(oimage_df$V2 == 5,1,0) |
                       ifelse(oimage_df$V3 == 5,1,0) |
                       ifelse(oimage_df$V4 == 5,1,0) |
                       ifelse(oimage_df$V5 == 5,1,0) |
                       ifelse(oimage_df$V6 == 5,1,0) |
                       ifelse(oimage_df$V7 == 5,1,0) |
                       ifelse(oimage_df$V8 == 5,1,0) ) %>%
        
        mutate(nenhuma_orbitrm = ifelse(oimage_df$V1 == 6, 1, 0) | 
                       ifelse(oimage_df$V2 == 6,1,0) |
                       ifelse(oimage_df$V3 == 6,1,0) |
                       ifelse(oimage_df$V4 == 6,1,0) |
                       ifelse(oimage_df$V5 == 6,1,0) |
                       ifelse(oimage_df$V6 == 6,1,0) |
                       ifelse(oimage_df$V7 == 6,1,0) |
                       ifelse(oimage_df$V8 == 6,1,0) ) %>%
        
        mutate(nao_realizada_orbitrm = ifelse(oimage_df$V1 == 7, 1, 0) | 
                       ifelse(oimage_df$V2 == 7,1,0) |
                       ifelse(oimage_df$V3 == 7,1,0) |
                       ifelse(oimage_df$V4 == 7,1,0) |
                       ifelse(oimage_df$V5 == 7,1,0) |
                       ifelse(oimage_df$V6 == 7,1,0) |
                       ifelse(oimage_df$V7 == 7,1,0) |
                       ifelse(oimage_df$V8 == 7,1,0) ) %>%
        
        mutate(outras_orbitrm = ifelse(oimage_df$V1 == 8, 1, 0) | 
                       ifelse(oimage_df$V2 == 8,1,0) |
                       ifelse(oimage_df$V3 == 8,1,0) |
                       ifelse(oimage_df$V4 == 8,1,0) |
                       ifelse(oimage_df$V5 == 8,1,0) |
                       ifelse(oimage_df$V6 == 8,1,0) |
                       ifelse(oimage_df$V7 == 8,1,0) |
                       ifelse(oimage_df$V8 == 8,1,0) )

 

oimage_df$hipersinal_retrob <- as.numeric(oimage_df$hipersinal_retrob)
oimage_df$hipersinal_quiasma <- as.numeric(oimage_df$hipersinal_quiasma)
oimage_df$hipersinal_extensa <- as.numeric(oimage_df$hipersinal_extensa)
oimage_df$realce_orbit <- as.numeric(oimage_df$realce_orbit)
oimage_df$nenhuma_orbitrm <- as.numeric(oimage_df$nenhuma_orbitrm)
oimage_df$nao_realizada_orbitrm <- as.numeric(oimage_df$nao_realizada_orbitrm)
oimage_df$outras_orbitrm <- as.numeric(oimage_df$outras_orbitrm)
oimage_df$hipersinal_media <- as.numeric(oimage_df$hipersinal_media)


#Subset dummie
oimg_dummie <- oimage_df[, 9:15]
# Bind orbital image
emocemp <- cbind(emocemp, oimg_dummie)
emocemp[, 123:129] <- lapply(emocemp[, 123:129], factor)
