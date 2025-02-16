# Load necessary package
library(dplyr)

# 1. Function to Translate DNA Sequence to Protein
dna_to_protein <- function(dna_seq) {
  codon_table <- list(
    "ATA"="I", "ATC"="I", "ATT"="I", "ATG"="M",
    "ACA"="T", "ACC"="T", "ACG"="T", "ACT"="T",
    "AAC"="N", "AAT"="N", "AAA"="K", "AAG"="K",
    "AGC"="S", "AGT"="S", "AGA"="R", "AGG"="R",
    "CTA"="L", "CTC"="L", "CTG"="L", "CTT"="L",
    "CCA"="P", "CCC"="P", "CCG"="P", "CCT"="P",
    "CAC"="H", "CAT"="H", "CAA"="Q", "CAG"="Q",
    "CGA"="R", "CGC"="R", "CGG"="R", "CGT"="R",
    "GTA"="V", "GTC"="V", "GTG"="V", "GTT"="V",
    "GCA"="A", "GCC"="A", "GCG"="A", "GCT"="A",
    "GAC"="D", "GAT"="D", "GAA"="E", "GAG"="E",
    "GGA"="G", "GGC"="G", "GGG"="G", "GGT"="G",
    "TCA"="S", "TCC"="S", "TCG"="S", "TCT"="S",
    "TTC"="F", "TTT"="F", "TTA"="L", "TTG"="L",
    "TAC"="Y", "TAT"="Y", "TAA"="*", "TAG"="*", "TGA"="*"
  )
  
  protein_seq <- ""
  for (i in seq(1, nchar(dna_seq)-2, by=3)) {
    codon <- substr(dna_seq, i, i+2)
    protein_seq <- paste0(protein_seq, codon_table[[codon]])
  }
  
  return(protein_seq)
}

# 2. Function to Simulate Logistic Growth with Randomized Lag & Exponential Phase
simulate_logistic_growth <- function(time, K, r, lag_range = c(5, 15), exp_range = c(10, 30)) {
  lag_phase <- sample(seq(lag_range[1], lag_range[2]), 1)
  exp_phase <- sample(seq(exp_range[1], exp_range[2]), 1)
  
  pop <- numeric(length(time))
  pop[1] <- 0.01 * K  # Initial population size
  
  for (t in 2:length(time)) {
    if (time[t] < lag_phase) {
      pop[t] <- pop[t-1]  # No growth in lag phase
    } else if (time[t] < (lag_phase + exp_phase)) {
      pop[t] <- pop[t-1] + r * pop[t-1] * (1 - pop[t-1] / K)  # Exponential growth
    } else {
      pop[t] <- pop[t-1] + r * pop[t-1] * (1 - pop[t-1] / K)  # Logistic growth
    }
  }
  
  return(data.frame(Time=time, Population=pop))
}

# 3. Generate 100 Growth Curves and Store in a DataFrame
generate_growth_curves <- function(n_curves=100, time_range=seq(0, 100, by=1)) {
  all_curves <- list()
  for (i in 1:n_curves) {
    K <- sample(seq(50, 500, by=50), 1)  # Random carrying capacity
    r <- runif(1, 0.1, 1)  # Random growth rate
    curve <- simulate_logistic_growth(time_range, K, r)
    curve$Curve_ID <- i
    all_curves[[i]] <- curve
  }
  
  return(bind_rows(all_curves))
}

# 4. Function to Determine Time to Reach 80% of Carrying Capacity
time_to_reach_80 <- function(growth_curve, K) {
  target_population <- 0.8 * K
  time_reached <- growth_curve$Time[which.min(abs(growth_curve$Population - target_population))]
  return(time_reached)
}

# 5. Function to Calculate Hamming Distance
hamming_distance <- function(str1, str2) {
  len1 <- nchar(str1)
  len2 <- nchar(str2)
  
  if (len1 < len2) {
    str1 <- paste0(str1, strrep(" ", len2 - len1))
  } else if (len2 < len1) {
    str2 <- paste0(str2, strrep(" ", len1 - len2))
  }
  
  sum(unlist(strsplit(str1, "")) != unlist(strsplit(str2, "")))
}

# ---- TESTING ----

# DNA Translation Example
print(dna_to_protein("ATGGCCATTGTAATGGGCCGCTGAAAGGGTGCCCGATAG"))

# Generate Growth Curves
growth_data <- generate_growth_curves(100)
print(head(growth_data))

# Test Time to Reach 80%
test_curve <- simulate_logistic_growth(seq(0, 100, by=1), K=300, r=0.2)
print(time_to_reach_80(test_curve, 300))

# Hamming Distance Example
print(hamming_distance("SlackUser", "TwitterHandle"))

write.csv(growth_data, "growth_data.csv", row.names = FALSE)
write.csv(test_curve, "test_curve.csv", row.names = FALSE)





