Sys.setenv(LANGUAGE = 'en')
options(error = traceback,
        repos = c(CRAN = "https://cran.r-project.org")
)

# install.packages("reshape")
# install.packages("ggsci")
# install.packages("ggplot2")
# install.packages("reshape2")
# install.packages("tidyr")
# install.packages("ggnewscale")

library(ggplot2)
library(scales)

library(ggsci)
# library(reshape2)
library(tidyr)
library(ggnewscale)

img_width <- 1024 * 1.2
# img_height <- 768 * 0.65
img_height <- 768 * 0.50
res <- 120
# scale_factor <- 4
scale_factor <- 8

# read filename from command line
args <- commandArgs(trailingOnly = TRUE)
filename <- args[1]

# read data
dat <- read.table(filename,
                  header = T,
                  sep = ",",
)

# dat[with(dat, order(network, heuristic, r_auc, no_removal)),]
# dat [!duplicated(dat[c("network", "heuristic")]),]

N_grid <- 4941
N_crime <- 754
N_coll <- 4158
N_ph <- 2000


dat$no_removal_frac <- NA

idxs <- which(dat$network == "Collaboration")
dat$no_removal_frac[idxs] <- dat$no_removal[idxs] / N_coll
idxs <- which(dat$network == "Crime")
dat$no_removal_frac[idxs] <- dat$no_removal[idxs] / N_crime
idxs <- which(dat$network == "PH")
dat$no_removal_frac[idxs] <- dat$no_removal[idxs] / N_ph
idxs <- which(dat$network == "Grid")
dat$no_removal_frac[idxs] <- dat$no_removal[idxs] / N_grid


dat$is_reinserted <- 0
dat[which(dat$heuristic == "GDMR"),]$is_reinserted <- 1
dat[which(dat$heuristic == "GNDR"),]$is_reinserted <- 1
dat[which(dat$heuristic == "CoreGDM"),]$is_reinserted <- 1
dat[which(dat$heuristic == "CoreHD"),]$is_reinserted <- 1
dat[grep("+R", dat$heuristic),]$is_reinserted <- 1
dat[grep("CollectiveInfluence", dat$heuristic),]$is_reinserted <- 1
dat[grep("_reinsertion", dat$heuristic),]$is_reinserted <- 1

# distinguish between target methods for the comment and others
dat$is_highlighted <- 0.01
dat[grep("entanglement", dat$heuristic),]$is_highlighted <- 0.09


# relabel methods
dat[which(dat$heuristic == "CollectiveInfluenceL2"),]$heuristic <- "CI_l_2"
dat[which(dat$heuristic == "network_entanglement_large"),]$heuristic <- "NE_large"
dat[which(dat$heuristic == "network_entanglement_mid"),]$heuristic <- "NE_mid"
dat[which(dat$heuristic == "network_entanglement_small"),]$heuristic <- "NE_small"
dat[which(dat$heuristic == "vertex_entanglement"),]$heuristic <- "VE"

dat[which(dat$heuristic == "GNDR"),]$heuristic <- "GND + R"
dat[which(dat$heuristic == "GDMR"),]$heuristic <- "GDM + R"
dat[which(dat$heuristic == "network_entanglement_large_reinsertion"),]$heuristic <- "NE_large + R"
dat[which(dat$heuristic == "network_entanglement_mid_reinsertion"),]$heuristic <- "NE_mid + R"
dat[which(dat$heuristic == "network_entanglement_small_reinsertion"),]$heuristic <- "NE_small + R"
dat[which(dat$heuristic == "vertex_entanglement_reinsertion"),]$heuristic <- "VE + R"


color_palette <- pal_aaas()(10)

# https://color-hex.org/color-palettes/189
my_colors <- c(
  "CI_l_2" = color_palette[1],
  "CoreHD" = color_palette[2],
  "EI_s1" = color_palette[3],
  "GDM" = color_palette[4],
  "GND" = color_palette[5],
  "NE_mid" = "#55d0ff",
  "NE_large" = "#ccf9ff",
  "NE_small" = "#0080bf",
  "VE" = "red4",
  # )
  #
  # my_colors2 <- c(
  "GDM + R" = color_palette[4],
  "GND + R" = color_palette[5],
  "NE_mid + R" = "#55d0ff",
  "NE_large + R" = "#ccf9ff",
  "NE_small + R" = "#0080bf",
  "VE + R" = "red4"
)

idxs <- which(dat$is_reinserted == 0)

# Convert to long format (columns lcc.norm and slcc.norm are melted into a single column)
# dat_melted <- melt(dat,
#                    id.vars=c("network",
#                                   "heuristic",
#                              "no_removal_frac",
#                              "is_reinserted",
#                              "is_highlighted"
#                    ),
#                    measure.vars=c("lcc.norm",
#                                   "slcc.norm"
#                    ),
#                    variable.name="cc_type",
#                    value.name="cc_size"
# )

dat_melted <- pivot_longer(dat,
                           cols = c(lcc.norm, slcc.norm),
                           names_to = "cc_type",
                           values_to = "cc_size"
)
# Show the melted data
print(head(dat_melted))


# p_noR <- ggplot(dat[idxs,], aes(no_removal_frac, lcc.norm, color=heuristic)) +
#       theme_bw() + theme(legend.position="bottom", panel.grid=element_blank(), legend.text=element_text(size=12)) +
#       geom_line(aes(size=is_highlighted, alpha=is_highlighted)) +
#       facet_wrap(.~network, scales="free", ncol=4) +
#       #scale_color_aaas(name="Method") +
#       #scale_linetype(types = unique(dat[idxs,]$linetype)) +
#       scale_alpha(range=c(0.5,1)) +
#       scale_size(range=c(0.3,1)) +
#       scale_color_manual(name="", values = my_colors) +
#       guides(size = "none", alpha="none") +
#       xlab("Fraction of removed nodes") + ylab("Relative size of the LCC") +
#       ggtitle("Without Reinsertion")
#
#
# png("comparison_no_reinsertion.png", width=img_width * scale_factor, height=img_height * scale_factor, res=res * scale_factor)
# print(p_noR)
# dev.off()

# Plot the melted data
non_reinserted_idxs <- which(dat_melted$is_reinserted == 0)
non_reinserted_dat <- dat_melted[non_reinserted_idxs,]

lcc_idxs <- which(non_reinserted_dat$cc_type == "lcc.norm")
slcc_idxs <- which(non_reinserted_dat$cc_type == "slcc.norm")

# reinserted_dat[lcc_idxs,]$line_size <- reinserted_dat[lcc_idxs,]$is_highlighted * 1
# reinserted_dat[slcc_idxs,]$line_size <- 0.2
p_noR <- ggplot(non_reinserted_dat, aes(x = no_removal_frac,
                                        color = heuristic,
                                        stroke = cc_type,
)) +
  # Plot both LCC and SLCC with different y-axis and linetype in the same geom_line

  geom_line(aes(y = cc_size,
                alpha = is_highlighted,
                linetype = factor(cc_type),
                size = is_highlighted,
                # linewidth = is_highlighted,
  ),

  ) +

  scale_alpha(range = c(0.5, 1)) +
  scale_size(range = c(0.3, 0.8)) +

  scale_y_continuous(name = "Connected Component size (ratio)",
                     # sec.axis = sec_axis(~.,
                     #                     name = "SLCC",
                     #                     labels = label_number(accuracy = 0.1),
                     # ),
                     labels = label_number(accuracy = 0.1),

  ) +

  scale_color_manual(name = "",
                     values = my_colors,
  ) +
  scale_linetype_manual(name = "Component",
                        values = c("solid", "dashed"),
                        labels = c("Largest", "Secondary Largest")
  ) +
  facet_wrap(. ~ network, scales = "free", ncol = 4) +

  guides(
    colour = guide_legend(
      title = "Method",
      # position = "bottom",
      direction = "horizontal",
      nrow = 1,
      title.position = "left",
    ),

    linetype = guide_legend(
      title = "Component",
      # position = "bottom",
      direction = "horizontal",
      title.position = "left",
      nrow = 1,
    ),
    size = "none",
    alpha = "none",
    linewidth = "none",
  ) +
  new_scale_color() +
  xlab("Fraction of removed nodes") +
  ylab("Relative size of the CC") +
  ggtitle("Without Reinsertion") +

  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.box = "vertical",
    legend.margin = unit(0.1, "cm")
  )


pdf("comparison_no_reinsertion.pdf", width = img_width / 100, height = img_height / 100)
# png("comparison_with_reinsertion.png", width=img_width * scale_factor, height=img_height * scale_factor, res=res * scale_factor)
print(p_noR)
dev.off()


# Plot the melted data
non_reinserted_idxs <- which(dat_melted$is_reinserted == 0)
non_reinserted_dat <- dat_melted[non_reinserted_idxs,]

lcc_idxs <- which(non_reinserted_dat$cc_type == "lcc.norm")
slcc_idxs <- which(non_reinserted_dat$cc_type == "slcc.norm")

# reinserted_dat[lcc_idxs,]$line_size <- reinserted_dat[lcc_idxs,]$is_highlighted * 1
# reinserted_dat[slcc_idxs,]$line_size <- 0.2
p_noR <- ggplot(non_reinserted_dat, aes(x = no_removal_frac,
                                        color = heuristic,
                                        stroke = cc_type,
)) +
  # Plot both LCC and SLCC with different y-axis and linetype in the same geom_line

  geom_line(aes(y = cc_size,
                size = is_highlighted,
                alpha = is_highlighted,
                linetype = factor(cc_type)
  )) +

  scale_alpha(range = c(0.5, 1)) +
  scale_size(range = c(0.3, 0.8)) +

  # scale_y_continuous(name = "LCC", sec.axis = sec_axis(~., name = "SLCC")) +
  scale_y_continuous(name = "Connected Component size (ratio)",
                     # sec.axis = sec_axis(~.,
                     #                     name = "SLCC",
                     #                     labels = label_number(accuracy = 0.1),
                     # ),
                     labels = label_number(accuracy = 0.1),

  ) +
  scale_color_manual(name = "",
                     values = my_colors,
  ) +
  scale_linetype_manual(name = "Component",
                        values = c("solid", "dashed"),
                        labels = c("Largest", "Secondary Largest")
  ) +
  # Log scale
  scale_x_continuous(trans = "log10") +
  facet_wrap(. ~ network, scales = "free", ncol = 4) +

  guides(
    colour = guide_legend(
      title = "Method",
      # position = "bottom",
      direction = "horizontal",
      nrow = 1,
      title.position = "left",
    ),

    linetype = guide_legend(
      title = "Component",
      # position = "bottom",
      direction = "horizontal",
      title.position = "left",
      nrow = 1,
    ),
    size = "none",
    alpha = "none",
    linewidth = "none",

  ) +
  new_scale_color() +
  xlab("Fraction of removed nodes") +
  ylab("Relative size of the CC") +
  ggtitle("Without Reinsertion") +

  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.box = "vertical",
    legend.margin = unit(0.1, "cm")
  )


pdf("comparison_no_reinsertion_log.pdf", width = img_width / 100, height = img_height / 100)
# png("comparison_with_reinsertion.png", width=img_width * scale_factor, height=img_height * scale_factor, res=res * scale_factor)
print(p_noR)
dev.off()


# idxs <- which(dat$is_reinserted==1)
# p_yesR <- ggplot(dat[idxs,], aes(x=no_removal_frac, color=heuristic)) +
#       # Plot both LCC and SLCC with different y-axis and linetype in the same geom_line
#       geom_line(aes(y=lcc.norm, size=is_highlighted, alpha=is_highlighted, linetype="solid"), linetype = "solid") +
#       geom_line(aes(y=slcc.norm, size=0.2, alpha=is_highlighted, linetype="dashed"), linetype = "dashed") +
#
#       # geom_line(aes(y=lcc.norm, size=is_highlighted, alpha=is_highlighted), linetype = "solid") +
#       # geom_line(aes(y=slcc.norm, size=0.2, alpha=is_highlighted), linetype = "dashed") +
#
#       #scale_color_aaas(name="Method") +
#       #scale_linetype(types = unique(dat[idxs,]$linetype)) +
#       scale_alpha(range=c(0.5,1)) +
#       scale_size(range=c(0.3,1)) +
#
#       scale_y_continuous(name="LCC", sec.axis = sec_axis(~., name = "SLCC")) +
#
#       scale_color_manual(name="", values = my_colors2) +
#       scale_linetype_manual(name = "",
#                             values = c("solid", "dashed"),
#                             labels = c("LCC", "SLCC")
#                             ) +
#
#       facet_wrap(.~network, scales="free", ncol=4) +
#
#       guides(size = "none", alpha="none") +
#       xlab("Fraction of removed nodes") +
#       ylab("Relative size of the CC") +
#       ggtitle("With Reinsertion") +
#
#       theme_bw() +
#       theme(legend.position="bottom", panel.grid=element_blank(), legend.text=element_text(size=12))


# Plot the melted data
reinserted_idxs <- which(dat_melted$is_reinserted == 1)
reinserted_dat <- dat_melted[reinserted_idxs,]

lcc_idxs <- which(reinserted_dat$cc_type == "lcc.norm")
slcc_idxs <- which(reinserted_dat$cc_type == "slcc.norm")

# reinserted_dat[lcc_idxs,]$line_size <- reinserted_dat[lcc_idxs,]$is_highlighted * 1
# reinserted_dat[slcc_idxs,]$line_size <- 0.2
p_yesR <- ggplot(reinserted_dat, aes(x = no_removal_frac,
                                     color = heuristic,
                                     stroke = cc_type,
)) +
  # Plot both LCC and SLCC with different y-axis and linetype in the same geom_line

  geom_line(aes(y = cc_size,
                size = is_highlighted,
                alpha = is_highlighted,
                linetype = factor(cc_type)
  )) +

  scale_alpha(range = c(0.5, 1)) +
  scale_size(range = c(0.3, 0.8)) +

  # scale_y_continuous(name = "LCC", sec.axis = sec_axis(~., name = "SLCC")) +
  scale_y_continuous(name = "Connected Component size (ratio)",
                     # sec.axis = sec_axis(~.,
                     #                     name = "SLCC",
                     #                     labels = label_number(accuracy = 0.1),
                     # ),
                     labels = label_number(accuracy = 0.1),

  ) +
  scale_color_manual(name = "",
                     values = my_colors,
  ) +
  scale_linetype_manual(name = "Component",
                        values = c("solid", "dashed"),
                        labels = c("Largest", "Secondary Largest")
  ) +
  facet_wrap(. ~ network, scales = "free", ncol = 4) +

  guides(
    colour = guide_legend(
      title = "Method",
      # position = "bottom",
      direction = "horizontal",
      nrow = 1,
      title.position = "left",
    ),

    linetype = guide_legend(
      title = "Component",
      # position = "bottom",
      direction = "horizontal",
      title.position = "left",
      nrow = 1,
    ),
    size = "none",
    alpha = "none",
    linewidth = "none",

  ) +
  new_scale_color() +
  xlab("Fraction of removed nodes") +
  ylab("Relative size of the CC") +
  ggtitle("With Reinsertion") +

  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.box = "vertical",
    legend.margin = unit(0.1, "cm")
  )


pdf("comparison_with_reinsertion.pdf", width = img_width / 100, height = img_height / 100)
# png("comparison_with_reinsertion.png", width=img_width * scale_factor, height=img_height * scale_factor, res=res * scale_factor)
print(p_yesR)
dev.off()

p_yesR <- ggplot(reinserted_dat, aes(x = no_removal_frac,
                                     color = heuristic,
                                     stroke = cc_type,)
) +
  # Plot both LCC and SLCC with different y-axis and linetype in the same geom_line

  geom_line(aes(y = cc_size,
                size = is_highlighted,
                alpha = is_highlighted,
                linetype = factor(cc_type),
  )) +

  scale_alpha(range = c(0.5, 1)) +
  scale_size(range = c(0.3, 0.8)) +

  # scale_y_continuous(name = "LCC", sec.axis = sec_axis(~., name = "SLCC")) +
  scale_y_continuous(name = "Connected Component size (ratio)",
                     # sec.axis = sec_axis(~.,
                     #                     name = "SLCC",
                     #                     labels = label_number(accuracy = 0.1),
                     # ),
                     labels = label_number(accuracy = 0.1),

  ) +
  scale_color_manual(name = "",
                     values = my_colors,
  ) +
  scale_linetype_manual(name = "Component",
                        values = c("solid", "dashed"),
                        labels = c("Largest", "Secondary Largest")
  ) +
  # Log scale
  scale_x_continuous(trans = "log10") +
  facet_wrap(. ~ network, scales = "free", ncol = 4) +

  guides(
    colour = guide_legend(
      title = "Method",
      # position = "bottom",
      direction = "horizontal",
      nrow = 1,
      title.position = "left",
    ),

    linetype = guide_legend(
      title = "Component",
      # position = "bottom",
      direction = "horizontal",
      title.position = "left",
      nrow = 1,
    ),
    size = "none",
    alpha = "none",
    linewidth = "none",

  ) +
  new_scale_color() +
  xlab("Fraction of removed nodes") +
  ylab("Relative size of the CC") +
  ggtitle("With Reinsertion") +

  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.box = "vertical",
    legend.margin = unit(0.1, "cm")
  )


pdf("comparison_with_reinsertion_log.pdf", width = img_width / 100, height = img_height / 100)
# png("comparison_with_reinsertion.png", width=img_width * scale_factor, height=img_height * scale_factor, res=res * scale_factor)
print(p_yesR)
dev.off()

# Retrieve VE+R and all non-reinserted methods
reinserted_ve_all_others <- dat_melted[dat_melted$heuristic == "VE + R" | dat_melted$is_reinserted == 0,]
print(unique(reinserted_ve_all_others$heuristic))

p_yesR <- ggplot(reinserted_ve_all_others,
                 aes(x = no_removal_frac,
                     color = heuristic,
                     stroke = cc_type,
                 )
) +
  # Plot both LCC and SLCC with different y-axis and linetype in the same geom_line

  geom_line(aes(y = cc_size,
                size = is_highlighted,
                alpha = is_highlighted,
                linetype = factor(cc_type),
  )) +

  scale_alpha(range = c(0.5, 1)) +
  scale_size(range = c(0.3, 0.8)) +

  # scale_y_continuous(name = "LCC", sec.axis = sec_axis(~., name = "SLCC")) +
  scale_y_continuous(name = "Connected Component size (ratio)",
                     # sec.axis = sec_axis(~.,
                     #                     name = "SLCC",
                     #                     labels = label_number(accuracy = 0.1),
                     # ),
                     labels = label_number(accuracy = 0.1),

  ) +
  scale_color_manual(name = "",
                     values = my_colors,
  ) +
  scale_linetype_manual(name = "Component",
                        values = c("solid", "dashed"),
                        labels = c("Largest", "Secondary Largest")
  ) +
  # # Log scale
  # scale_x_continuous(trans = "log10") +
  facet_wrap(. ~ network, scales = "free", ncol = 4) +

  guides(
    colour = guide_legend(
      title = "Method",
      # position = "bottom",
      direction = "horizontal",
      nrow = 1,
      title.position = "left",
    ),

    linetype = guide_legend(
      title = "Component",
      # position = "bottom",
      direction = "horizontal",
      title.position = "left",
      nrow = 1,
    ),
    size = "none",
    alpha = "none",
    linewidth = "none",

  ) +
  new_scale_color() +
  xlab("Fraction of removed nodes") +
  ylab("Relative size of the CC") +
  ggtitle("VE + R vs. Non-reinserted") +

  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.box = "vertical",
    legend.margin = unit(0.1, "cm")
  )


pdf("comparison_with_reinsertion_VEr_others.pdf", width = img_width / 100, height = img_height / 100)
# png("comparison_with_reinsertion.png", width=img_width * scale_factor, height=img_height * scale_factor, res=res * scale_factor)
print(p_yesR)
dev.off()

p_yesR <- ggplot(reinserted_ve_all_others,
                 aes(x = no_removal_frac,
                     color = heuristic,
                     stroke = cc_type,
                 )
) +
  # Plot both LCC and SLCC with different y-axis and linetype in the same geom_line

  geom_line(aes(y = cc_size,
                size = is_highlighted,
                alpha = is_highlighted,
                linetype = factor(cc_type),
  )) +

  scale_alpha(range = c(0.5, 1)) +
  scale_size(range = c(0.3, 0.8)) +

  # scale_y_continuous(name = "LCC",
  #                    sec.axis = sec_axis(~., name = "SLCC"),
  #                    labels = label_number(accuracy = 0.1),
  # ) +
  scale_y_continuous(name = "Connected Component size (ratio)",
                     # sec.axis = sec_axis(~.,
                     #                     name = "SLCC",
                     #                     labels = label_number(accuracy = 0.1),
                     # ),
                     labels = label_number(accuracy = 0.1),

  ) +

  scale_color_manual(name = "",
                     values = my_colors,
  ) +
  scale_linetype_manual(name = "Component",
                        values = c("solid", "dashed"),
                        labels = c("Largest", "Secondary Largest")
  ) +
  # Log scale
  scale_x_continuous(trans = "log10") +
  facet_wrap(. ~ network, scales = "free", ncol = 4) +

  guides(
    colour = guide_legend(
      title = "Method",
      # position = "bottom",
      direction = "horizontal",
      nrow = 1,
      title.position = "left",
    ),

    linetype = guide_legend(
      title = "Component",
      # position = "bottom",
      direction = "horizontal",
      title.position = "left",
      nrow = 1,
    ),
    size = "none",
    alpha = "none",
    linewidth = "none",

  ) +
  new_scale_color() +
  xlab("Fraction of removed nodes") +
  ylab("Relative size of the CC") +
  ggtitle("VE + R vs. Non-reinserted") +

  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.box = "vertical",
    legend.margin = unit(0.1, "cm")
  )


pdf("comparison_with_reinsertion_VEr_others_log.pdf", width = img_width / 100, height = img_height / 100)
# png("comparison_with_reinsertion.png", width=img_width * scale_factor, height=img_height * scale_factor, res=res * scale_factor)
print(p_yesR)
dev.off()