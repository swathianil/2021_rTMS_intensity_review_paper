# Code used for generating figures and performing descriptive statistics.

library(ProjectTemplate)
load.project()

#-------
# total number of studies
data %>% dplyr::distinct(pdf_name) %>% nrow()

# total number of protocols
n_prot <- data %>% nrow()

# unique protocols
data %>% dplyr::filter(typ == "rTMS" & !is.na(frq)) %>% 
  dplyr::mutate(uniqe_rTMS = paste(typ, frq, si_app, si_pct, sep = "_")) %>% 
  dplyr::pull(uniqe_rTMS) %>% unique(.) %>% length()

# unspecified motor threshold
data <- data %>% dplyr::mutate(si_app = dplyr::case_when((si_app == "MT") ~ "uMT",
                                                         TRUE ~ si_app))
step1 <- step1 %>% dplyr::mutate(si_app = dplyr::case_when((si_app == "MT") ~ "uMT",
                                                           TRUE ~ si_app))
step2 <- step2 %>% dplyr::mutate(si_app = dplyr::case_when((si_app == "MT") ~ "uMT",
                                                           TRUE ~ si_app))
#-------
# Stimulation intensity selection approaches (overview)
data %>% count(si_app)

# threshold-based procedures
data %>% count(si_app) %>% 
  dplyr::filter(stringr::str_detect(si_app, pattern = "RMT|AMT|uMT|PT|FL|TT")) %>% 
  dplyr::summarize(sum(n)) %>% dplyr::pull() / n_prot * 100

# fixed
data %>% count(si_app) %>% 
  dplyr::filter(stringr::str_detect(si_app, pattern = "FXD")) %>% 
  dplyr::summarize(sum(n)) %>% dplyr::pull() / n_prot * 100

# electric field
data %>% count(si_app) %>% 
  dplyr::filter(stringr::str_detect(si_app, pattern = "EF")) %>% 
  dplyr::summarize(sum(n)) %>% dplyr::pull() / n_prot * 100

# NA
data %>% count(si_app) %>% 
  dplyr::filter(is.na(si_app)) %>% 
  dplyr::summarize(sum(n)) %>% dplyr::pull() / n_prot * 100

# motor threshold
data %>% count(si_app) %>% 
  dplyr::filter(stringr::str_detect(si_app, pattern = "RMT|AMT|uMT")) %>% 
  dplyr::summarize(sum(n)) %>% dplyr::pull() / n_prot * 100

data %>% count(si_app) %>% 
  dplyr::filter(stringr::str_detect(si_app, pattern = "RMT")) %>% 
  dplyr::summarize(sum(n)) %>% dplyr::pull() / n_prot * 100

data %>% count(si_app) %>% 
  dplyr::filter(stringr::str_detect(si_app, pattern = "AMT")) %>% 
  dplyr::summarize(sum(n)) %>% dplyr::pull() / n_prot * 100

data %>% count(si_app) %>% 
  dplyr::filter(stringr::str_detect(si_app, pattern = "^uMT")) %>% 
  dplyr::summarize(sum(n)) %>% dplyr::pull() / n_prot * 100

# method of limit or MLTH
data %>% count(th_strat) %>% 
  dplyr::filter(stringr::str_detect(th_strat, pattern = "ML")) %>% 
  dplyr::summarize(sum(n)) %>% dplyr::pull() / n_prot * 100

data %>% count(th_strat) %>% 
  dplyr::filter(stringr::str_detect(th_strat, pattern = "^CITE")) %>% 
  dplyr::summarize(sum(n)) %>% dplyr::pull() / n_prot * 100

data %>% count(th_strat) %>% 
  dplyr::filter(stringr::str_detect(th_strat, pattern = "MLTH|MTAT|PEST|5STEP")) %>% 
  dplyr::summarize(sum(n)) %>% dplyr::pull() / n_prot * 100

data %>% 
  dplyr::filter((si_app == "RMT" | si_app == "AMT" | si_app == "uMT") & is.na(th_strat)) %>% 
  count(th_strat) %>% dplyr::summarize(sum(n)) %>% dplyr::pull() / n_prot * 100

# electrodes or visual 
data %>% 
  dplyr::filter((si_app == "RMT" | si_app == "AMT" | si_app == "uMT") & (th_meas == "E")) %>% 
  count(th_meas) %>% dplyr::summarize(sum(n)) %>% dplyr::pull() / n_prot * 100

data %>% 
  dplyr::filter((si_app == "RMT" | si_app == "AMT" | si_app == "uMT") & (th_meas == "V")) %>% 
  count(th_meas) %>% dplyr::summarize(sum(n)) %>% dplyr::pull() / n_prot * 100

data %>% 
  dplyr::filter((si_app == "RMT" | si_app == "AMT" | si_app == "uMT") & is.na(th_meas)) %>% 
  count(th_meas) %>% dplyr::summarize(sum(n)) %>% dplyr::pull() / n_prot * 100

# Less commmon approaches
data %>% 
  dplyr::filter(si_app == "PT") %>% 
  count(si_app) %>% dplyr::summarize(sum(n)) %>% dplyr::pull() / n_prot * 100

data %>% 
  dplyr::filter(si_app == "FL") %>% 
  count(si_app) %>% dplyr::summarize(sum(n)) %>% dplyr::pull() / n_prot * 100

data %>% 
  dplyr::filter(si_app == "TT") %>% 
  count(si_app) %>% dplyr::summarize(sum(n)) %>% dplyr::pull() / n_prot * 100

# treemap
sum_table <- data %>% count(si_app)
sum_table <- sum_table %>% dplyr::mutate(group = dplyr::case_when((si_app == "FXD") ~ "FXD",
                                                                  (si_app == "EF") ~ "EF",
                                                                  is.na(si_app) ~ "NR",
                                                                  TRUE ~ as.character("TH")),
                                         si_app = dplyr::case_when(is.na(si_app) ~ "NR",
                                                                   TRUE ~ si_app))

sum_table <- sum_table %>% dplyr::arrange(group, si_app)
sum_table <- sum_table %>% dplyr::mutate(si_app = factor(si_app, levels = c("RMT", "AMT", "uMT", "PT", "FL", "TT", "FXD", "EF", "NR")))
sum_table <- sum_table %>% dplyr::mutate(pct = 100 * (n / sum(n)))

ggplot(sum_table, aes(area = n, label = paste(si_app, paste(round(pct, 1), "%", sep = " "), sep = "\n"), fill = si_app, subgroup = group)) +
  treemapify::geom_treemap(color = "gray25") +
  treemapify::geom_treemap_text(color = "black", place = "centre") +
    treemapify::geom_treemap_subgroup_border(color = "white", size = 2) +
  scale_fill_manual(values = c("gray65", "gray75", "gray85", "#F8B196", "#99B898", "black", "#56B4E9", "#E69F00", "#800080"),
                    guide = guide_legend(title = "Approaches", title.position = "top")) +
  theme(legend.background = element_rect(size = 0.4, linetype = "solid", color = "black"))

ggsave("graphs/treemap.png", plot = last_plot(), 
       width = 160, height = 90, units = "mm", dpi = 300)
sum_table %>% dplyr::group_by(group) %>% dplyr::summarize(sum(pct)) %>% dplyr::ungroup()
sum_table %>% dplyr::filter(si_app == "AMT" | si_app == "RMT" | si_app == "uMT") %>% dplyr::summarize(sum(pct)) %>% dplyr::ungroup()


# treemap for step 1
sum_table1 <- step1 %>% count(si_app)
sum_table1 <- sum_table1 %>% dplyr::mutate(group = dplyr::case_when((si_app == "FXD") ~ "FXD",
                                                                    (si_app == "EF") ~ "EF",
                                                                     is.na(si_app) ~ "NR",
                                                                     TRUE ~ as.character("TH")),
                                           si_app = dplyr::case_when(is.na(si_app) ~ "NR",
                                                                     TRUE ~ si_app))

sum_table1 <- sum_table1 %>% dplyr::arrange(group, si_app)
sum_table1 <- sum_table1 %>% dplyr::mutate(si_app = factor(si_app, levels = c("RMT", "AMT", "uMT", "PT", "FL", "TT", "FXD", "EF", "NR")))
sum_table1 <- sum_table1 %>% dplyr::mutate(pct = 100 * (n / sum(n)))

ggplot(sum_table1, aes(area = n, label = paste(si_app, paste(round(pct, 1), "%", sep = " "), sep = "\n"), fill = si_app, subgroup = group)) +
  treemapify::geom_treemap(color = "gray25") +
  treemapify::geom_treemap_text(color = "black", place = "centre") +
  treemapify::geom_treemap_subgroup_border(color = "white", size = 2) +
  scale_fill_manual(values = c("gray65", "gray75", "gray85", "#F8B196", "#99B898", "#56B4E9", "#800080"),
                    guide = guide_legend(title = "Approaches", title.position = "top")) +
  theme(legend.background = element_rect(size = 0.4, linetype = "solid", color = "black"))

ggsave("graphs/treemap_s1.png", plot = last_plot(), 
       width = 160, height = 90, units = "mm", dpi = 300)

# treemap for step 2
sum_table2 <- step2 %>% count(si_app)
sum_table2 <- sum_table2 %>% dplyr::mutate(group = dplyr::case_when((si_app == "FXD") ~ "FXD",
                                                                    (si_app == "EF") ~ "EF",
                                                                    is.na(si_app) ~ "NR",
                                                                    TRUE ~ as.character("TH")),
                                           si_app = dplyr::case_when(is.na(si_app) ~ "NR",
                                                                     TRUE ~ si_app))

sum_table2 <- sum_table2 %>% dplyr::arrange(group, si_app)
sum_table2 <- sum_table2 %>% dplyr::mutate(si_app = factor(si_app, levels = c("RMT", "AMT", "uMT", "PT", "FL", "TT", "FXD", "EF", "NR")))
sum_table2 <- sum_table2 %>% dplyr::mutate(pct = 100 * (n / sum(n)))

ggplot(sum_table2, aes(area = n, label = paste(si_app, paste(round(pct, 1), "%", sep = " "), sep = "\n"), fill = si_app, subgroup = group)) +
  treemapify::geom_treemap(color = "gray25") +
  treemapify::geom_treemap_text(color = "black", place = "centre") +
  treemapify::geom_treemap_subgroup_border(color = "white", size = 2) +
  scale_fill_manual(values = c("gray65", "gray75", "gray85", "#F8B196", "#99B898", "black", "#56B4E9", "#E69F00", "#800080"),
                    guide = guide_legend(title = "Approaches", title.position = "top")) +
  theme(legend.background = element_rect(size = 0.4, linetype = "solid", color = "black"))

ggsave("graphs/treemap_s2.png", plot = last_plot(), 
       width = 160, height = 90, units = "mm", dpi = 300)

#-------
# Motor threshold approach
mt <- data %>% 
  dplyr::filter(stringr::str_detect(si_app, pattern = "^RMT|^AMT|^uMT") & !is.na(si_pct))  
dim(mt)[1]/n_prot *100 

mt %>% count(si_app)

xbreaks <- seq(from = min(mt$si_pct, na.rm = T), to = max(mt$si_pct, na.rm = T), by = 20)
f1 <- ggplot(data = mt, aes(x = si_pct)) +
  geom_histogram(color = "black", fill = "grey75", binwidth = 10) +
  geom_vline(xintercept = 100, color = "black", linetype = "dashed", size = 0.5) +
  scale_x_continuous(breaks = xbreaks) +
  labs(x = "Motor threshold [percent]", y = "Nr. of protocols") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", angle = 45),
        axis.text.y = element_text(colour = "black"))

f2 <- ggplot(data = mt, aes(x = si_pct)) +
  geom_histogram(color = "black", fill = "grey75", binwidth = 10) +
  geom_vline(xintercept = 100, color = "black", linetype = "dashed", size = 0.5) +
  scale_x_continuous(breaks = xbreaks) +
  facet_wrap(si_app ~ .)  +
  labs(x = "Motor threshold [percent]", y = "Nr. of protocols") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", angle = 45),
        axis.text.y = element_text(colour = "black"))

f3<-ggarrange(f1, f2,
              labels = c("A", "B"),
              widths=c(0.3, 0.7),
              nrow = 1, ncol = 2,
              common.legend = FALSE)

ggsave("graphs/MT_intensity.png", plot = f3, 
       width = 210, height = 60, units = "mm", dpi = 300)


# Non-patterned protocols
data %>% count(typ)
data %>% dplyr::filter(typ == "rTMS") %>% count(frq) %>% dplyr::arrange(-n)

n_patterned <- data %>% 
  dplyr::filter(typ == "rTMS" & stringr::str_detect(si_app, pattern = "^RMT|^uMT|^AMT") & !is.na(si_pct)) %>% 
  dplyr::filter(frq %in% c(1, 5, 10, 15, 20, 25)) %>%
  dplyr::mutate(frq_label = as.factor(stringr::str_c(frq, " Hz"))) %>%
  dplyr::mutate(frq_label = factor(frq_label, levels = c("1 Hz", "5 Hz", "10 Hz", "15 Hz", "20 Hz", "25 Hz")))
  
xbreaks <- seq(from = min(n_patterned$si_pct, na.rm = T), to = max(n_patterned$si_pct, na.rm = T), by = 20)
ggplot2::ggplot(data = n_patterned, aes(x = si_pct)) +
  geom_histogram(color = "black", fill = "grey75", binwidth = 10) +
  geom_vline(xintercept = 100, color = "black", linetype = "dashed", size = 0.5) +
  scale_x_continuous(breaks = xbreaks) +
  facet_wrap(frq_label ~ .) +
  labs(x = "Motor threshold [percent]", y = "Nr. of protocols") +
  theme_bw() +
  theme(text = element_text(size = 9),
        axis.text.x = element_text(colour = "black", angle = 45),
        axis.text.y = element_text(colour = "black"),
        strip.text.x = element_text(color = "black"), 
        strip.background = element_rect(color = "black", size = 0.5, linetype = "solid"))

ggsave("graphs/conventional.png", plot = last_plot(), 
       width = 120, height = 70, units = "mm", dpi = 300)  
n_patterned %>% count(frq)

# patterned protocols
data %>% dplyr::filter(stringr::str_detect(typ, pattern = "TBS$")) %>% dplyr::select(typ, si_app, si_pct)

patterned <- data %>% dplyr::filter(stringr::str_detect(typ, pattern = "cTBS|iTBS") & stringr::str_detect(si_app, pattern = "^RMT|^AMT|^uMT") & !is.na(si_pct)) 

xbreaks <- seq(from = 70, to = 110, by = 10)
ggplot2::ggplot(data = patterned, aes(x = si_pct)) +
  geom_histogram(color = "black", fill = "grey75", binwidth = 10) +
  geom_vline(xintercept = 100, color = "black", linetype = "dashed", size = 0.5) +
  scale_x_continuous(breaks = xbreaks) +
  facet_wrap(typ ~ .) +
  labs(x = "Motor threshold [percent]", y = "Nr. of protocols") +
  theme_bw()

xbreaks <- seq(from = 70, to = 120, by = 10)
ggplot2::ggplot(data = patterned %>% dplyr::filter(stringr::str_detect(si_app, pattern = "RMT|AMT")), aes(x = si_pct)) +
  geom_histogram(color = "black", fill = "grey75", binwidth = 10) +
  geom_vline(xintercept = 100, color = "black", linetype = "dashed", size = 0.5) +
  scale_x_continuous(breaks = xbreaks) +
  facet_grid(typ ~ si_app) +
  labs(x = "Motor threshold [percent]", y = "Nr. of protocols") +
  theme_bw() +
  theme(text = element_text(size = 9),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        strip.text.x = element_text(color = "black"), 
        strip.background.x = element_rect(color = "black", size = 0.5, linetype = "solid"))

ggsave("graphs/tbs.png", plot = last_plot(), 
       width = 90, height = 70, units = "mm", dpi = 300)

patterned %>% count(typ) %>% dplyr::arrange(-n) 

# stacked barplot about the motor threshold
sb <- data %>% dplyr::filter((frq == 1 | frq == 5 | frq == 10 | typ == "cTBS" | typ == "iTBS") & (si_app == "AMT" | si_app == "RMT" | si_app == "uMT" | si_app == "FXD")) %>% 
  dplyr::mutate(label = dplyr::case_when(frq == 1 ~ "1 Hz",
                                         frq == 10 ~ "10 Hz",
                                         frq == 5 ~ "5 Hz",
                                         typ == "cTBS" ~ "cTBS",
                                         typ == "iTBS" ~ "iTBS",
                                         TRUE ~ "check")) %>% 
  dplyr::group_by(label) %>% count(si_app) %>% dplyr::ungroup() %>% 
  dplyr::mutate(label = factor(label, levels = c("cTBS", "iTBS", "10 Hz", "5 Hz", "1 Hz")),
                si_app = factor(si_app, levels = c("FXD", "uMT", "AMT", "RMT")))

ggplot(data = sb, aes(x = label, y = n, fill = si_app)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("#005b96", "gray70", "gray55", "gray40")) +
  coord_flip() +
  labs(x = "", y = "Nr. of protocols", fill = "Approach") +
  theme_bw() +
  theme(text = element_text(size = 9),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.89, 0.4),
        legend.background = element_rect(colour = 'black', size = 0.4, linetype = 'solid'))
ggsave("graphs/mt_fxd.png", plot = last_plot(), 
       width = 120, height = 60, units = "mm", dpi = 300)

# Reproducible full 
rep1 <- data %>% dplyr::filter(!is.na(typ) & !is.na(si_app) & !is.na(th_strat) & 
                               !is.na(th_meas) & !is.na(mt_uV) & !is.na(mso) & 
                               !is.na(stim_co) & !is.na(stim_mo) & !is.na(coil_mo)) 
rep2 <- data %>% dplyr::filter(!is.na(typ) & si_app == "FXD" & !is.na(mso) & 
                               !is.na(stim_co) & !is.na(stim_mo) & !is.na(coil_mo)) 

# Reproducible stimulation intensity
rep1 <- data %>% dplyr::filter(!is.na(typ) & !is.na(mso) & 
                               !is.na(stim_co) & !is.na(stim_mo) & !is.na(coil_mo)) 
rep2 <- data %>% dplyr::filter(!is.na(typ) & si_app == "FXD" & !is.na(mso) & 
                               !is.na(stim_co) & !is.na(stim_mo) & !is.na(coil_mo)) 

# Not reported
data %>% dplyr::filter(is.na(si_app)) %>% count()
data %>% dplyr::filter(is.na(si_pct) & is.na(si_pct_min) & si_app != "FXD") %>% count() 

data %>% dplyr::filter(is.na(mso)) %>% count()
data %>% dplyr::filter(is.na(stim_co)) %>% count()
data %>% dplyr::filter(is.na(stim_mo)) %>% count()
data %>% dplyr::filter(is.na(coil_mo)) %>% count()
data %>% dplyr::filter(is.na(coil_shp) & is.na(coil_mo)) %>% count()

# Not reported 
from_year = 2011 #1991 #2001 #2011
to_year =  2020 #2000 #2010 #2020
get_percent <- function(input, total){
  percent = round((input/total) * 100, 2)
  return(percent)
}

data_sub <- data %>% dplyr::rowwise() %>% dplyr::mutate(yop = stringr::str_split(pdf_name, '_') %>% unlist() %>% .[2]) %>%
  dplyr::filter(yop %in% (from_year:to_year))
# intensity selection approach
data_sub %>% dplyr::filter(is.na(si_app)) %>% count()
get_percent(data_sub %>% dplyr::filter(is.na(si_app)) %>% count() %>% sum(), dim(data_sub)[1])
# Stimulation intensity
data_sub %>% dplyr::filter(is.na(si_pct) & is.na(si_pct_min) & si_app != "FXD") %>% count() 
get_percent(data_sub %>% dplyr::filter(is.na(si_pct) & is.na(si_pct_min) & si_app != "FXD") %>% count() %>% sum(), dim(data_sub)[1])
# Device output
data_sub %>% dplyr::filter(is.na(mso)) %>% count()
get_percent(data_sub %>% dplyr::filter(is.na(mso)) %>% count() %>% sum(), dim(data_sub)[1])
# Stimulator company
data_sub %>% dplyr::filter(is.na(stim_co)) %>% count()
get_percent(data_sub %>% dplyr::filter(is.na(stim_co)) %>% count() %>% sum(), dim(data_sub)[1])
# Stimulator model
data_sub %>% dplyr::filter(is.na(stim_mo)) %>% count()
get_percent(data_sub %>% dplyr::filter(is.na(stim_mo)) %>% count() %>% sum(), dim(data_sub)[1])
# Coil model
data_sub %>% dplyr::filter(is.na(coil_mo)) %>% count()
get_percent(data_sub %>% dplyr::filter(is.na(coil_mo)) %>% count() %>% sum(), dim(data_sub)[1])
# Coil model and shape
data_sub %>% dplyr::filter(is.na(coil_shp) & is.na(coil_mo)) %>% count()
get_percent(data_sub %>% dplyr::filter(is.na(coil_shp) & is.na(coil_mo)) %>% count() %>% sum(), dim(data_sub)[1])


  

