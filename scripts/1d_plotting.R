# Load required packages
require(ggplot2)
require(ggsignif)
require(hrbrthemes)
require(plotrix)
require(data.table)
require(extrafont)

extrafont::loadfonts()

# Read csv with trial-level data
load(here::here("data", "actual", "Killgore2007_GUM4", "Killgore2007_GUM4_merged.Rdata"))

# Read csv with modelled parameter data
igt_data.pars <- as.data.table(read.csv(here::here("data/actual/Killgore2007_GUM4", "Killgore2007_GUM4_with_parameters.csv")))

igt_data.pars[67, "session"] <- 3
igt_data.pars$session <- factor(igt_data.pars$session, levels = c(1, 2, 3), labels = c("Baseline", "51h TSD", "75h TSD"))

## Default text formatting and color palette for graphs
default_text <- element_text(size = 16, family = "Averia Serif Libre Light")
color_palette <- c("#62BEC1", "#FFE1A8", "#E26D5C")


# Plot choices for one participant in one session -----------------------
pt_ix <- sample(unique(igt_data$PtID), size = 1)
sesh_ix <- sample(unique(igt_data[PtID==pt_ix]$session), size = 1)

## Subset participant's trial data
pt_subset <- igt_data[PtID==pt_ix & session==sesh_ix]

ggplot(data = pt_subset, aes(x = Trial, y = choice)) +
  geom_line(alpha =.5) + geom_point(size = 2,  aes(color = factor(choice))) +  
  ggpubr::theme_pubr()+ 
  scale_fill_manual(values=c( "#9999CC", "#999999", "red", "green"))+
  guides(color = "none") + 
  labs(title = paste("Choices for subject", pt_ix, "in Session", sesh_ix), x = "Trial", y = "Choice") +
  theme(axis.title.x =default_text, axis.title.y =default_text) 

choice_chart <- ggplot(data = pt_subset, aes(x = Trial, y = choice)) +
  geom_line(alpha =.5) + geom_point(size = 2,  aes(color = factor(choice))) +  
  ggpubr::theme_pubr()+ 
  scale_fill_manual(values=c( "#9999CC", "#999999", "red", "green"))+
  guides(color = "none") + 
  labs(title = paste("Choices for subject", pt_ix, "in Session", sesh_ix), x = "Trial", y = "Choice") +
  theme(axis.title.x =default_text, axis.title.y =default_text) 

# Parameter value boxplots ------------------------------------------------

## Theta
plot.VSE.theta <- ggplot(data = igt_data.pars[!is.na(drug)], aes(x = session, y = VSE_theta,  fill = factor(drug))) +
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(shape = factor(drug)), position = position_jitterdodge(dodge.width = .75), size = 1.5) +
  scale_shape_manual(values = c(15, 16)) +
  ggpubr::theme_pubr()+ 
  labs(title = "Value Sensitivity", fill = "Drug group", shape = "Drug group",  x = "Session", y = "θ")+
  theme(axis.title.x =default_text, axis.title.y =default_text) 

## Gamma
plot.VSE.gamma <- ggplot(data = igt_data.pars[!is.na(drug)], aes(x = session, y = VSE_gamma,  fill = factor(drug))) +
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(shape = factor(drug)), position = position_jitterdodge(dodge.width = .75), size = 1.5) +
  scale_shape_manual(values = c(15, 16)) +
  ggpubr::theme_pubr()+
  labs(title = "Decay Rate", fill = "Drug group", shape = "Drug group", x = "Session", y = "γ")+
  theme(axis.title.x =default_text, axis.title.y =default_text) 

## Phi
plot.VSE.phi <- ggplot(data = igt_data.pars[!is.na(drug)], aes(x = session, y = VSE_phi,  fill = factor(drug))) +
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(shape = factor(drug)), position = position_jitterdodge(dodge.width = .75), size = 1.5) +
  scale_shape_manual(values = c(15, 16)) +
  ggpubr::theme_pubr()+
  labs(title = "Exploration Bonus", fill = "Drug group", shape = "Drug group", x = "Session", y = "φ")+
  theme(axis.title.x =default_text, axis.title.y =default_text) 

## Eta
plot.VSE.eta <- ggplot(data = igt_data.pars[!is.na(drug)], aes(x = session, y = VSE_eta,  fill = factor(drug))) +
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(shape = factor(drug)), position = position_jitterdodge(dodge.width = .75), size = 1.5) +
  scale_shape_manual(values = c(15, 16)) +
  ggpubr::theme_pubr() + 
  labs(title = "Exploration Update", fill = "Drug group", shape = "Drug group", x = "Session", y = "η")+
  theme(axis.title.x =default_text, axis.title.y =default_text) 

## C
plot.VSE.C <- ggplot(data = igt_data.pars[!is.na(drug)], aes(x = session, y = VSE_C,  fill = factor(drug))) +
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(shape = factor(drug)), position = position_jitterdodge(dodge.width = .75), size = 1.5) +
  scale_shape_manual(values = c(15, 16)) +
  ggpubr::theme_pubr() + 
  labs(title = "Consistency", fill = "Drug group", shape = "Drug group", x = "Session", y = "C")+
  theme(axis.title.x =default_text, axis.title.y =default_text) 

ggpubr::ggarrange(plot.VSE.theta, plot.VSE.gamma, plot.VSE.phi, plot.VSE.eta, plot.VSE.C
                  , ncol = 3, nrow = 2, common.legend = T)

# Plots of models without interaction (just session predictor) ------------

plot.VSE.theta <- ggplot(data = igt_data.pars, aes(x = session, y = VSE_theta, fill = session)) +
  geom_boxplot(width = .5, outlier.shape = NA) +
  geom_point(position = position_jitterdodge(jitter.width = .5), size = 1.5, alpha = .6) +
  scale_fill_manual(values = color_palette) + 
  ggpubr::theme_pubr()+ 
  guides(fill = "none") + 
  labs(title = "Value Sensitivity", x = "Session", y = "θ", fill = "Session") + 
  theme(title =  element_text(size = 17, family = "Averia Serif Libre", face = "bold"),
        axis.title.x = default_text,
        axis.title.y = element_text(size = 16, family = "serif")
        ) 

plot.VSE.gamma <- ggplot(data = igt_data.pars, aes(x = session, y = VSE_gamma, fill = session)) +
  geom_boxplot(width = .5, outlier.shape = NA) + 
  geom_point(position = position_jitterdodge(jitter.width = .5), size = 1.5, alpha = .6) +
  scale_fill_manual(values = color_palette) + 
  ggpubr::theme_pubr()+ 
  guides(fill = "none") + 
  labs(title = "Decay Rate", x = "Session", y = "γ", fill = "Session")+
  theme(title =  element_text(size = 17, family = "Averia Serif Libre", face = "bold"),
        axis.title.x = default_text,
        axis.title.y = element_text(size = 16, family = "serif")
  ) 

plot.VSE.phi <- ggplot(data = igt_data.pars, aes(x = session, y = VSE_phi, fill = session)) +
  geom_boxplot(width = .5, outlier.shape = NA) + 
  geom_point(position = position_jitterdodge(jitter.width = .5), size = 1.5, alpha = .6) +
  scale_fill_manual(values = color_palette) + 
  ggpubr::theme_pubr()+ 
  guides(fill = "none") + 
  labs(title = "Exploration Bonus", x = "Session", y = "φ", fill = "Session")+
  theme(title =  element_text(size = 17, family = "Averia Serif Libre", face = "bold"),
        axis.title.x = default_text,
        axis.title.y = element_text(size = 16, family = "serif")
  ) 

plot.VSE.eta <- ggplot(data = igt_data.pars, aes(x = session, y = VSE_eta, fill = session)) +
  geom_boxplot(width = .5, outlier.shape = NA) + 
  geom_point(position = position_jitterdodge(jitter.width = .5), size = 1.5, alpha = .6) +
  scale_fill_manual(values = color_palette) + 
  ggpubr::theme_pubr()+ 
  guides(fill = "none") + 
  labs(title = "Exploration Update", x = "Session", y = "η", fill = "Session")+
  theme(title =  element_text(size = 17, family = "Averia Serif Libre", face = "bold"),
        axis.title.x = default_text,
        axis.title.y = element_text(size = 16, family = "serif")
  ) 

plot.VSE.C <- ggplot(data = igt_data.pars, aes(x = session, y = VSE_C, fill = session)) +
  geom_boxplot(width = .5, outlier.shape = NA) + 
  geom_point(position = position_jitterdodge(jitter.width = .5), size = 1.5, alpha = .6) +
  scale_fill_manual(values = color_palette) + 
  ggpubr::theme_pubr()+ 
  guides(fill = "none") + 
  labs(title = "Consistency", x = "Session", y = "C", fill = "Session")+
  theme(title =  element_text(size = 17, family = "Averia Serif Libre", face = "bold"),
        axis.title.x = default_text, axis.title.y = default_text) 

ggpubr::ggarrange(plot.VSE.theta, plot.VSE.gamma, plot.VSE.phi, plot.VSE.eta, plot.VSE.C, ncol = 3, nrow = 2)


# SE Index ----------------------------------------------------------------

n_sessions <- unique(igt_data$stan_index) %>% length

## Create SE marker for each participant
trial_data[, seq_exp := as.integer()]

for (i in 1:n_sessions){
  ## isolate session data
  temp <- igt_data[stan_index == i]
  n_trials <- max(temp$Trial)
    ## trial level loop
  for (j in 1:n_trials){
    if (j == 1){
      next
    } else if (j %in% 2:3){
      ## record previous 1 to 3 trial responses
      se_marker <- ifelse(length(unique(temp[1:j]$choice)) == j, 1, 0)
    } else {
      ## record previous 4 trial responses
      se_marker <- ifelse(length(unique(temp[(j-3):j]$choice)) == 4, 1, 0)
    }
    
    ## write to temp datatable
    igt_data[stan_index == i & Trial == j, "seq_exp"] <- se_marker
  }
}

# Create data table of mean seq_exp (thus, an SE index across trials) by averaging across trial number
se.table <- igt_data[, .(mean(seq_exp), std.error(seq_exp)), by = Trial]
names(se.table) <- c("trial_no", "SE_index", "SE_index_stderr")
se.table[, upper_CI := SE_index + (1.96*SE_index_stderr)]
se.table[, lower_CI := SE_index - (1.96*SE_index_stderr)]

## Make sure there are no values less than 0 for lower CI
se.table[, lower_CI := ifelse(lower_CI<0, .000001, lower_CI)]

# Plot SE index across trials
se_index_chart <- ggplot(data = se.table, aes(x = trial_no, y = SE_index)) + 
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), fill = '#000080', alpha = .15) + 
  geom_path( color = "#06402B", linewidth = .8, lineend = "round") + 
  labs(title = "SE index", x = "Trial", y = "SE index")  +   
  scale_x_continuous(limits = c(1, 100), n.breaks = 20) + 
  scale_y_continuous(limits = c(0, .45), n.breaks = 9) +
  # geom_hline(yintercept = .05, linetype = "dashed") + 
  ggpubr::theme_pubr() +
  theme(aspect.ratio = .3,
        plot.title   = element_text(size = 18, family = "Averia Serif Libre", face = "bold"),
        axis.title.x = element_text(size = 16, family = "Averia Serif Libre Light"),
        axis.title.y = element_text(size = 16, family = "Averia Serif Libre Light"))


