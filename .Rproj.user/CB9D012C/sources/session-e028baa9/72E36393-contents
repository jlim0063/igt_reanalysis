# Load required packages
require(ggplot2)
require(hrbrthemes)
require(plotrix)
require(ggpubr)

# Load extra fonts
extrafont::loadfonts(quiet=TRUE)

# Plot choices for one participant in one condition -----------------------
pt_ix   <- 40
cond_ix <- "pre"

## Subset participant's trial data
pt_subset <- trial_data[subject==pt_ix & condition==cond_ix]


choice_chart <- ggplot(data = pt_subset, aes(x = trial_no, y = ptresp)) +
  geom_line(alpha = .5) + 
  geom_point(size = 3,  aes(color = factor(ptresp))) +  
  theme_pubr() +
  scale_color_manual(values=c("#744253", "#228CDB", "#C44536", "#33CA7F"))+
  guides(color = "none") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) + 
  labs(title = paste("Choice pattern for subject", pt_ix), x = "Trial No.", y = "Choice")+
  theme(title = element_text(size = 18), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) 

# Randomly sample 4 participants and plot their choice patterns in a facet grid. 
random_four <- sort(sample(trial_data$subject %>% unique, size = 4))

ggplot(data = trial_data[subject %in% random_four & condition=="pre"], aes(x = trial_no, y = ptresp)) +
  geom_line(alpha =.5) + 
  geom_point(size = 2,  aes(color = factor(ptresp))) +
  theme_pubr() +
  scale_color_manual(values=c("#744253", "#228CDB", "#C44536", "#33CA7F"))+
  guides(color = "none") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) + 
  labs(title = "Choice patterns for 4 subjects", x = "Trial", y = "Choice")+
  theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) +
  facet_wrap(~ subject, ncol = 2)


# Plot fluctuations in exploit/explore weights ------------------------------------------

exploit_A <- rep(0.00, nrow(trial_data))
exploit_B <- rep(0.00, nrow(trial_data))
exploit_C <- rep(0.00, nrow(trial_data))
exploit_D <- rep(0.00, nrow(trial_data))
explore_A <- rep(0.00, nrow(trial_data))
explore_B <- rep(0.00, nrow(trial_data))
explore_C <- rep(0.00, nrow(trial_data))
explore_D <- rep(0.00, nrow(trial_data))

exploit.dt <- data.table(trial_data$subject, trial_data$condition, trial_data$trial_no, exploit_A, exploit_B, exploit_C, exploit_D)
explore.dt <- data.table(trial_data$subject, trial_data$condition, trial_data$trial_no, explore_A, explore_B, explore_C, explore_D)

## Pull exploit/explore weights into separate datatables
## This will take a while depending on the length of trial_data
for (i in 1:nrow(trial_data)){
  exploit_weights <- unlist(trial_data[i, "exploit_weights"])
  explore_weights <- unlist(trial_data[i, "explore_weights"])
  for(j in 1:4){
    exploit.dt[i, names(exploit.dt)[3+j]] <- exploit_weights[j]
    explore.dt[i, names(explore.dt)[3+j]] <- explore_weights[j]
  }
}
names(exploit.dt)[1:3] <- c("subject", "condition", "trial_no")
names(explore.dt)[1:3] <- c("subject", "condition", "trial_no") 


## Create individual datatable for specified participant and condition
deck <- rep(c("A", "B", "C", "D"), each = 100)
trial_no <- rep(1:100, 4)
indiv_weights.long <- data.table(
  deck,
  trial_no,
  c(exploit.dt[subject==pt_ix & condition == cond_ix]$exploit_A, exploit.dt[subject==pt_ix & condition == cond_ix]$exploit_B,
    exploit.dt[subject==pt_ix & condition == cond_ix]$exploit_C, exploit.dt[subject==pt_ix & condition == cond_ix]$exploit_D),
  c(explore.dt[subject==pt_ix & condition == cond_ix]$explore_A, explore.dt[subject==pt_ix & condition == cond_ix]$explore_B,
    explore.dt[subject==pt_ix & condition == cond_ix]$explore_C, explore.dt[subject==pt_ix & condition == cond_ix]$explore_D)
)
names(indiv_weights.long)[3:4] <- c("exploit" , "explore")

exploit_chart <- ggplot(data= indiv_weights.long[trial_no <= 20], aes(x = trial_no, y = exploit, color = deck)) +
  geom_line(linewidth = .8) + theme_pubr() +
  ylim(c(-5, 5)) +
  scale_x_continuous(breaks = seq(0, 20, by = 1))+
  labs(title = paste("Exploit weights across trials for subject", pt_ix, ", trials 1-20"), color = "Deck") + 
  xlab("Trial") + ylab("Exploit weight") +
  theme(aspect.ratio = .8,
    plot.title   = element_text(size = 20, family = "Times New Roman"),
    axis.title.x = element_text(size = 14, family = "Times New Roman"),
    axis.title.y = element_text(size = 14, family = "Times New Roman"),
    legend.title = element_text(size = 12, family = "Times New Roman"),
    legend.text  = element_text(size = 12, family = "Times New Roman")
  )

text <- paste("Participant's exploration bonus parameter:", round(unique(trial_data[subject==pt_ix]$true_phi),3))
explore_chart <- ggplot(data= indiv_weights.long[trial_no <= 20], aes(x = trial_no, y = explore, color = deck)) +
  geom_line(linewidth = .8) + ggpubr::theme_pubr() +
  scale_x_continuous(breaks = seq(0, 20, by = 1))+
  # annotate("text", label = text, x = 10, y = -.8, size = 5 )+
  ylim(c(-3, 3)) +
  labs(title = paste("Explore weights across trials for subject", pt_ix, ", trials 1-20"), color = "Deck") + 
  xlab("Trial") + ylab("Explore weight") +
  theme(aspect.ratio = .8,
        plot.title   = element_text(size = 14, family = "Times New Roman"),
        axis.title.x = element_text(size = 14, family = "Times New Roman"),
        axis.title.y = element_text(size = 14, family = "Times New Roman"),
        legend.title = element_text(size = 12, family = "Times New Roman"),
        legend.text  = element_text(size = 12, family = "Times New Roman")
  )


# Plot SE index -----------------------------------------------------------

n_pt <- unique(trial_data$subject) %>% length

## Create SE marker for each participant
trial_data[, seq_exp := as.integer()]

for (i in 1:n_pt){
  for (cond_x in unique(trial_data[subject==i]$condition)){
    ## isolate participant's subset of data, in the respective condition
    temp <- trial_data[subject == i & condition == cond_x]
    
    ## trial level loop
    for (j in 1:n_trials){
      if (j == 1){
        next
      } else if (j %in% 2:3){
        ## record previous 1 to 3 trial responses
        se_marker <- ifelse(length(unique(temp[1:j]$ptresp)) == j, 1, 0)
      } else {
        ## record previous 4 trial responses
        se_marker <- ifelse(length(unique(temp[(j-3):j]$ptresp)) == 4, 1, 0)
      }
      
      ## write to temp datatable
      trial_data[subject==i & condition==cond_x & trial_no==j, "seq_exp"] <- se_marker
    }
  }
}

# Create data table of mean seq_exp (thus, an SE index across trials) by averaging across trial number
se.table <- trial_data[, .(mean(seq_exp), std.error(seq_exp)), by = trial_no]
names(se.table) <- c("trial_no", "SE_index", "SE_index_stderr")
se.table[, upper_CI := SE_index + (1.96*SE_index_stderr)]
se.table[, lower_CI := SE_index - (1.96*SE_index_stderr)]

# Plot SE index across trials
se_index_chart <- ggplot(data = se.table, aes(x = trial_no, y = SE_index)) + 
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), fill = '#90EE90', alpha = .35) + 
  geom_line( color = "#06402B", linewidth = .8) + 
  # geom_hline(yintercept = .05, linetype = "dashed") + 
  labs(title = "SE index in a simulated dataset of N=40", x = "Trial", y = "SE index")  +   
  scale_x_continuous(breaks = seq(0, 100, by = 10)) + scale_y_continuous(breaks = seq(0, 1, by = .1))+
  ggpubr::theme_pubclean() +
  theme(aspect.ratio = .8,
        plot.title   = element_text(size = 14, family = "EB Garamond SemiBold"),
        axis.title.x = element_text(size = 14, family = "EB Garamond"),
        axis.title.y = element_text(size = 14, family = "EB Garamond"))
