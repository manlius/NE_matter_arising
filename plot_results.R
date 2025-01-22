Sys.setenv(LANGUAGE='en')
options(error=traceback)

library(ggplot2)
library(ggsci)

img_width <- 1024 * 1.2
img_height <- 768 * 0.65
res <- 120
scale_factor <- 4

# read filename from command line
args <- commandArgs(trailingOnly=TRUE)
filename <- args[1]

# read data
dat_heu <- read.table(filename, header=T, sep=",")

# dat_heu[with(dat_heu, order(network, heuristic, r_auc, no_removal)),]
# dat_heu [!duplicated(dat_heu[c("network", "heuristic")]),]

N_grid <- 4941
N_crime <- 754
N_coll <- 4158
N_ph <- 2000


dat_heu$no_removal_frac <- NA

idxs <- which(dat_heu$network=="Collaboration")
dat_heu$no_removal_frac[idxs] <- dat_heu$no_removal[idxs]/N_coll
idxs <- which(dat_heu$network=="Crime")
dat_heu$no_removal_frac[idxs] <- dat_heu$no_removal[idxs]/N_crime
idxs <- which(dat_heu$network=="PH")
dat_heu$no_removal_frac[idxs] <- dat_heu$no_removal[idxs]/N_ph
idxs <- which(dat_heu$network=="Grid")
dat_heu$no_removal_frac[idxs] <- dat_heu$no_removal[idxs]/N_grid


dat_heu$is_reinserted <- 0
dat_heu[which(dat_heu$heuristic=="GDMR"),]$is_reinserted <- 1
dat_heu[which(dat_heu$heuristic=="GNDR"),]$is_reinserted <- 1
dat_heu[grep("_reinsertion", dat_heu$heuristic),]$is_reinserted <- 1

dat <- dat_heu

# distinguish between target methods for the comment and others
dat$is_highlighted <- 0
dat[grep("entanglement", dat$heuristic),]$is_highlighted <- 1

# relabel methods
dat[which(dat$heuristic=="CollectiveInfluenceL2"),]$heuristic <- "CI_l_2"
dat[which(dat$heuristic=="network_entanglement_large"),]$heuristic <- "NE_large"
dat[which(dat$heuristic=="network_entanglement_mid"),]$heuristic <- "NE_mid"
dat[which(dat$heuristic=="network_entanglement_small"),]$heuristic <- "NE_small"
dat[which(dat$heuristic=="vertex_entanglement"),]$heuristic <- "VE"

dat[which(dat$heuristic=="GNDR"),]$heuristic <- "GND + R"
dat[which(dat$heuristic=="GDMR"),]$heuristic <- "GDM + R"
dat[which(dat$heuristic=="network_entanglement_large_reinsertion"),]$heuristic <- "NE_large + R"
dat[which(dat$heuristic=="network_entanglement_mid_reinsertion"),]$heuristic <- "NE_mid + R"
dat[which(dat$heuristic=="network_entanglement_small_reinsertion"),]$heuristic <- "NE_small + R"
dat[which(dat$heuristic=="vertex_entanglement_reinsertion"),]$heuristic <- "VE + R"


color_palette <- pal_aaas()(10)

# https://color-hex.org/color-palettes/189
my_colors <- c("CI_l_2" = color_palette[1],
               "CoreHD" = color_palette[2],
               "EI_s1" = color_palette[3],
               "GDM" = color_palette[4],
               "GND" = color_palette[5],
               "NE_mid" = "#55d0ff",
               "NE_large" = "#ccf9ff",
               "NE_small" = "#0080bf",
               "VE" = "red4"
               )

idxs <- which(dat$is_reinserted==0)
p_noR <- ggplot(dat[idxs,], aes(no_removal_frac, lcc.norm, color=heuristic)) + 
      theme_bw() + theme(legend.position="bottom", panel.grid=element_blank(), legend.text=element_text(size=12)) + 
      geom_line(aes(size=is_highlighted, alpha=is_highlighted)) + 
      facet_wrap(.~network, scales="free", ncol=4) +
      #scale_color_aaas(name="Method") +
      #scale_linetype(types = unique(dat[idxs,]$linetype)) +
      scale_alpha(range=c(0.5,1)) +
      scale_size(range=c(0.3,1)) +
      scale_color_manual(name="", values = my_colors) +
      guides(size = "none", alpha="none") +
      xlab("Fraction of removed nodes") + ylab("Relative size of the LCC") +
      ggtitle("Without Reinsertion")


png("comparison_no_reinsertion.png", width=img_width * scale_factor, height=img_height * scale_factor, res=res * scale_factor)
print(p_noR)
dev.off()

my_colors2 <- c("GDM + R" = color_palette[4],
               "GND + R" = color_palette[5],
               "NE_mid + R" = "#55d0ff",
               "NE_large + R" = "#ccf9ff",
               "NE_small + R" = "#0080bf",
               "VE + R" = "red4"
               )

idxs <- which(dat$is_reinserted==1)
p_yesR <- ggplot(dat[idxs,], aes(no_removal_frac, lcc.norm, color=heuristic)) + 
      theme_bw() + theme(legend.position="bottom", panel.grid=element_blank(), legend.text=element_text(size=12)) + 
      geom_line(aes(size=is_highlighted, alpha=is_highlighted)) + 
      facet_wrap(.~network, scales="free", ncol=4) +
      #scale_color_aaas(name="Method") +
      #scale_linetype(types = unique(dat[idxs,]$linetype)) +
      scale_alpha(range=c(0.5,1)) +
      scale_size(range=c(0.3,1)) +
      scale_color_manual(name="", values = my_colors2) +
      guides(size = "none", alpha="none") +
      xlab("Fraction of removed nodes") + ylab("Relative size of the LCC") +
      ggtitle("With Reinsertion")


png("comparison_with_reinsertion.png", width=img_width * scale_factor, height=img_height * scale_factor, res=res * scale_factor)
print(p_yesR)
dev.off()

