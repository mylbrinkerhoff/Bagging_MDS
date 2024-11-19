#-------------------------------------------------------------------------------
#
# 08_feature_interpretation.R
#
# Generating a Bagging decission tree
#
# M. Brinkerhoff  * UCSC  * 2024-08-16 (F)
#
#-------------------------------------------------------------------------------

# plotting the variable importance
vip::vip(slz_bag2, num_features = 21, geom = "point", all_permutations = T)

# Extract variable importance scores
vi_scores <- vip::vi(slz_bag2)

# Create a Lollipop chart of variable importance scores
variable_importance <- ggplot(vi_scores, aes(x = reorder(Variable,
Importance), y = Importance)) +
  geom_segment(aes(xend = Variable, yend = 0)) +
  geom_point() +
  coord_flip() +
  labs(title = "Variable Importance", 
       x = "Variable", 
       y = "Importance (Gini Index)") +
  theme_bw()
variable_importance

# Save the plot as an image in the figs folder
ggsave(filename = "figs/vip.eps",
    plot = variable_importance,
    width = 6, height = 3, dpi = 300, units = "in")

ggsave(filename = "figs/vip.png",
    plot = variable_importance,
    width = 6, height = 3, dpi = 300, units = "in")

ggsave(filename = "figs/vip_large.png",
    plot = variable_importance,
    width = 16, height = 8, dpi = 600, units = "in")
