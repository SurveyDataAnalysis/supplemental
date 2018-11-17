# Read in full Leuven study data from CSV file
Leuven_Full_Data <- read.csv(file.choose())
Leuven_Subset <- Leuven_Full_Data[1:50, c("bads_prorated_ar_pomp","bads_prorated_si_pomp")]
show(Leuven_Subset)
Subset_Int <- data.frame(Leuven_Subset)

# Scatterplot of depression & rumination data
# This code defines the scatterplot
scatterplot <- ggplot(Subset_Int, aes(bads_prorated_ar_pomp, bads_prorated_si_pomp)) + 
  geom_point(alpha = 1 / 10) +
  theme_classic() +
  scale_x_continuous(limits = c(40, 100)) +
  scale_y_continuous(limits = c(0, 100)) +
  geom_smooth(method = 'lm', se = FALSE) +
  xlab("Avoidance-Rumination") +
  ylab("Social Impairment")
scatterplot
