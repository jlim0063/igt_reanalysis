# Load required packages
require(ggplot2)
require(hrbrthemes)
require(plotrix)

# Plot choices for one participant in one condition -----------------------
pt_ix   <- 1
cond_ix <- "pre"

## Subset participant's trial data
pt_subset <- trial_data[subject==pt_ix & condition==cond_ix]


choice_chart <- ggplot(data = pt_subset, aes(x = trial_no, y = ptresp)) +
  geom_line(alpha =.5) + geom_point(size = 2,  aes(color = factor(ptresp))) +  
  ggpubr::theme_pubr() +
  # ggthemes::theme_hc() + 
  scale_fill_manual(values=c( "#9999CC", "#999999", "red", "green"))+
  guides(color = "none") + 
  labs(title = paste("Choices for subject", pt_ix, "in", cond_ix, "condition"), x = "Trial", y = "Choice")+
  ylim(c(1, 4)) + 
  theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) 

random_four <- sort(sample(1:40, size = 4))
ggplot(data = trial_data[subject %in% random_four & condition=="pre"], aes(x = trial_no, y = ptresp)) +
  geom_line(alpha =.5) + geom_point(size = 2,  aes(color = factor(ptresp))) +
  ggpubr::theme_pubr() +
  scale_color_manual(values=c("#744253", "#228CDB", "#C44536", "#33CA7F"))+
  guides(color = "none") +
  labs(title = paste0('Choice patterns for subjects ', random_four[1], ", ", random_four[2], ", ",
                     random_four[3], " and ", random_four[4]))+
  theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) +
  ylim(c(1, 4)) + 
  facet_wrap(~ subject, ncol = 2)


# Return a specific subject's parameter values ----------------------------

pt_ix <- 22
if (pt_ix %in% 1:n_pt){
  message(paste0(
    "Risk-seeking (rho): ",      round(rho_list[pt_ix], 3), "\n", 
    "Loss aversion (lambda): ",  round(lambda_list[pt_ix],3), "\n", 
    "Learning rate (eta): ",     round(eta_pre_list[pt_ix],3), "\n", 
    "Inv. temperature (beta): ", round(beta_list[pt_ix],3), "\n", 
    "Consistency (C): ",         round(C_list[pt_ix],3), "\n"
  ))
}


