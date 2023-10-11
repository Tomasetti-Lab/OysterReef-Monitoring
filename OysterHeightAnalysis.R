#####################################################################################################################
#####################################################################################################################
### R Code for Tomasetti-Lab, originally written by GRousselet/rogme
### Adapted by Stephen Tomasetti
### For Project: Oyster Reef Control of Carbonate Chemistry 
### Article: https://doi.org/10.1111/gcb.16960
#####################################################################################################################


# Good script for comparing distributions of length measurements
# Number of measurements do not need to be equal in each group
# Package 'rogme' by GRousselet - on Github: https://github.com/GRousselet/rogme
# Need "devtools" package to download R package from github
# githubinstall("packagename")

# Needed libraries
library(rogme)
library(tidyverse)

# Eliminate NANs in existing oyster data for each group
c1<-oylength21$ctl1
c1<-c1[!is.na(c1)]

r2<-oylength21$reef2
r2<-r2[!is.na(r2)]

r1<-oylength21$reef1
r1<-r1[!is.na(r1)]

# Compare lengths of different groups
rfcompare <- mkt2(r1, r2)
c1tor1 <- mkt2(c1, r1)
c1tor2 <- mkt2(c1, r2)

# Make scatterplots (must do for each comparison)
ps <- plot_scat2(data = c1tor2,
                 formula = obs ~ gr,
                 xlabel = "",
                 ylabel = "Oyster Height (mm)",
                 alpha = 1,
                 shape = 21,
                 size = 3,
                 colour = "grey10",
                 fill = "grey90") #> scatterplots
ps

# Flip the coordinates (I like the flipped look)
ps <- ps + coord_flip()
ps

# Compute shift function
sf <- shifthd_pbci(data = c1tor2, formula = obs ~ gr, nboot = 1000, q = c(.1,.25,.5,.75,.9))


#> Plot shift function
psf <- plot_sf(sf, plot_theme = 2)
psf
#> Warning: Using alpha for a discrete variable is not advised.
#> Warning: Using alpha for a discrete variable is not advised.

#> add labels for deciles 1 & 9
psf <- add_sf_lab(psf, sf, 
                  y_lab_nudge = .1, 
                  text_size = 4)
psf

# Change axis labels
psf[[1]] <- psf[[1]] +  labs(x = "sand oyster quantile heights (mm)",
                             y = "sand - reef 1 quantile differences (mm)")
psf[[1]]

# Make combined plot
shift_fig <- plot_scat2(c1tor2,
                xlabel = "",
                ylabel = "heights (mm)",
                alpha = 1,
                shape = 21,
                size = 3,
                colour = "grey10",
                fill = "grey90") #> scatterplots
shift_fig <- plot_hd_links(shift_fig, sf[[1]],
                   q_size = 1,
                   md_size = 1.5,
                   add_rect = TRUE,
                   rect_alpha = 0.1,
                   rect_col = "grey50",
                   add_lab = TRUE,
                   text_size = 5) #> superimposed deciles + rectangle
shift_fig


# Flip if you want
shift_fig <- shift_fig + coord_flip() #> flip axes
shift_fig

