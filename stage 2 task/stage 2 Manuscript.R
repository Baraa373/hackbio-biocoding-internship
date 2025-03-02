# Create unique identifier column in both datasets
sift_df <- sift_df %>% mutate(specific_Protein_aa = paste(Protein, Amino_acid, sep = "_"))
foldx_df <- foldx_df %>% mutate(specific_Protein_aa = paste(Protein, Amino_acid, sep = "_"))

# Check the updated datasets
head(sift_df$specific_Protein_aa)
head(foldx_df$specific_Protein_aa)
# Merge SIFT and FoldX datasets using 'specific_Protein_aa'
merged_df <- merge(sift_df, foldx_df, by = "specific_Protein_aa")

# View the merged dataset
head(merged_df)
# Find mutations that are both functionally and structurally deleterious
deleterious_mutations <- merged_df %>%
  filter(SIFT_score < 0.05 & FoldX_score > 2)

# View the first few deleterious mutations
head(deleterious_mutations)
# Extract the first amino acid (original) from 'Amino_acid.x' column
deleterious_mutations <- deleterious_mutations %>%
  mutate(original_aa = substr(Amino_acid.x, 1, 1))

# View the updated dataset
head(deleterious_mutations)

# Create a frequency table for amino acids
aa_frequency <- table(deleterious_mutations$original_aa)

# Convert to a data frame
aa_freq_df <- as.data.frame(aa_frequency)
colnames(aa_freq_df) <- c("Amino_Acid", "Count")

# View the frequency table sorted by count
aa_freq_df <- aa_freq_df %>% arrange(desc(Count))
head(aa_freq_df)

ggplot(aa_freq_df, aes(x = reorder(Amino_Acid, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Frequency of Deleterious Amino Acid Mutations",
       x = "Amino Acid",
       y = "Count")
ggplot(aa_freq_df, aes(x = "", y = Count, fill = Amino_Acid)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_minimal() +
  labs(title = "Amino Acid Mutation Distribution") +
  theme(axis.text.x = element_blank())

aa_freq_df[1, ]

high_occurrence_aa <- aa_freq_df %>% filter(Count > 100)
print(high_occurrence_aa)
write.csv(deleterious_mutations, "deleterious_mutations.csv", row.names = FALSE)

write.csv(aa_freq_df, "amino_acid_frequency.csv", row.names = FALSE)

write.csv(high_occurrence_aa, "high_occurrence_amino_acids.csv", row.names = FALSE)









