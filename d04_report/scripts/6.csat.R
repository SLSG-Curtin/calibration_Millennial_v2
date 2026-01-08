library(ggplot2)

Cdef_all <- cbind(cdef_loCI,cdef_mean,cdef_upCI)
#cdef_loCI

Cdef_all <- Cdef_all[,c(1:6,7:8,17:18,27:28,10)]
names(Cdef_all)[7:12] <- c("loCI_bound","loCI_deficit","mean_bound","mean_deficit","upCI_bound","upCI_deficit")


Cdef_all$MAOC_tha <- with(Cdef_all,mean_bound-mean_deficit)

Cdef_all_update <- merge(Cdef_all,carbon_finefraction_covariates[,c(4,5,7,11,12,15,71,72)],by="x")


Cdef_all_update$Qmax <- with(Cdef_all_update,0.3*bd030w_aver*1000*fine_frac.x*0.86/100)
Cdef_all_update$MAOC_theory <- with(Cdef_all_update,C*bd030w_aver*30)
Cdef_all_update$g <- with(Cdef_all_update,MAOC_tha/MAOC_030)
Cdef_all_update$b_g <- with(Cdef_all_update,MAOC_tha/MAOC_perc/30)

p <- ggplot() +
  geom_point(
    data = data.frame(x = df_muresk$fine_frac, y = df_muresk$MAOC_perc),
    mapping = aes(x, y),
    colour = "grey"
  ) +
  geom_point(
    data = data.frame(x = df_muresk$fine_frac, y = df_muresk$Qmax_perc),
    mapping = aes(x, y),
    colour = "blue"
  ) +
  geom_line(
    data = boundary_pdf,
    mapping = aes(
      x = X_fit,
      # y = y_fit_min
      y = y_fit_mean - 2 * y_fit_sd
    ),
    colour = "red", linetype = "dotted"
  ) +
  geom_line(
    data = boundary_pdf,
    mapping = aes(x = X_fit, y = y_fit_mean),
    colour = "red"
  ) +
  geom_line(
    data = boundary_pdf,
    # mapping = aes(x = X_fit, y = y_fit_max),
    mapping = aes(
      x = X_fit,
      # y = y_fit_max
      y = y_fit_mean + 2 * y_fit_sd
    ),
    colour = "red", linetype = "dotted"
  ) +
  labs(
    x = "Clay-silt (%)",
    y = "MAOC, %"
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank()
  )

p

asc = c("So","Te","Ka","Ca","Ru","Ch","Ve","Or-Po","Hy","Ku","De","Fe")
asc_new = c("Sodosol","Tenosol","Kandosol","Calcarosol","Rudosol","Chromosol","Vertosol","Organosol-Podsol","Hydrosol","Kurosol","Dermosol","Ferrosol")


library(ggplot2)

# Create a named vector for facet labeling
asc_labels <- setNames(asc_new, asc)

ggplot(Cdef_all_update) +
  geom_point(aes(x = fine_frac.x, y = MAOC_tha), alpha = 0.2, shape = 21, colour = 'grey40',size = 1) +
  geom_ribbon(aes(x = fine_frac.x, ymin = loCI_bound, 
                  ymax = upCI_bound), fill = "pink", alpha = 0.2, show.legend = FALSE) +	
  geom_smooth(aes(x = fine_frac.x, y = Qmax), method = "lm", colour = "red", fill = "lightblue", size = 0.5, se = FALSE, alpha = 0.3) +
  geom_line(aes(x = fine_frac.x, y = upCI_bound), colour = "red", linetype = "dotted", size = 0.5) +
  geom_line(aes(x = fine_frac.x, y = mean_bound), colour = "black", size = 0.5) +
  geom_line(aes(x = fine_frac.x, y = loCI_bound), colour = "red", linetype = "dotted", size = 0.5) +
  # scale_fill_manual(values = c("#4daf4a", "#377eb8", "#e41a1c"), 
  # scale_colour_brewer(palette = "Set1",aesthetics = c("colour", "fill")) +  # need this so that lines and ribbon same colour	
  labs(x ="Fine fraction (%)", y = expression(MAOC~(Mg~ha^-1))) +		
  facet_wrap(~ asc, scales = "free", labeller = labeller(asc = asc_labels)) +
  theme_bw() +
  theme(panel.border = element_rect(fill = NA, colour = "grey40", size = 1), 
        legend.position ='none',
        aspect.ratio = 1,
        strip.background =element_rect(colour="white", fill = "white"),
        strip.text = element_text(colour = 'black', size = 11),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 11),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 10))
