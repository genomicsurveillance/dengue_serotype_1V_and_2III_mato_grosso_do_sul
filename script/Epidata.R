
library("gdata")
library("tidyverse")
library("dplyr")
library("RColorBrewer")
library("gridExtra")
library("ggnewscale")
library("hrbrthemes")
library("scales")

denv_epi <- fread("data/epidata/MS-Epidata.txt", header = TRUE)
denv_serotype <- fread("data/epidata/MS-Serotypes-MG.txt", header = TRUE)

denv_epi$data <- as.Date(cut(as.Date(paste(denv_epi$year, denv_epi$ew, 1, sep="-"), "%Y-%U-%u"), breaks = "week",start.on.monday = FALSE))

denv_epi$month <- as.Date(paste0(format(denv_epi$data, "%Y-%m"), "-01"))
denv_epi_agre <- denv_epi %>% 
  group_by(month, State)  %>% 
  dplyr::summarise(count = sum(total)) %>%
  drop_na()

denv_epi_agre$incidence <- (denv_epi_agre$count / 2756700) * 100000

denv_serotype$data <- as.Date(cut(as.Date(paste(denv_serotype$ANO_SEMANA_EPIDEMIOLOGICA, denv_serotype$SEMANA_EPIDEMIOLOGICA, 1, sep="-"), "%Y-%U-%u"), breaks = "week",start.on.monday = FALSE))

denv_serotype_agre <- denv_serotype %>% 
  group_by(ANO_SEMANA_EPIDEMIOLOGICA, SOROTIPO)  %>% 
  dplyr::summarise(count = sum(TOTAL)) %>%
  drop_na()

denv_ano_agre <- denv_serotype %>% 
  group_by(ANO_SEMANA_EPIDEMIOLOGICA)  %>% 
  dplyr::summarise(count = sum(TOTAL)) %>%
  drop_na()

denv_serotype_agre$ANO_SEMANA_EPIDEMIOLOGICA <- factor(denv_serotype_agre$ANO_SEMANA_EPIDEMIOLOGICA, levels=names(table(denv_serotype_agre$ANO_SEMANA_EPIDEMIOLOGICA)))

denv_ano_agre$aesx <- paste0(denv_ano_agre$ANO_SEMANA_EPIDEMIOLOGICA, "\n(n = ", denv_ano_agre$count, ")")
denv_ano_agre$ANO_SEMANA_EPIDEMIOLOGICA <- as.factor(denv_ano_agre$ANO_SEMANA_EPIDEMIOLOGICA)
denv_serotype_agrefinal <- left_join(denv_serotype_agre, denv_ano_agre, by=c("ANO_SEMANA_EPIDEMIOLOGICA"))


p0 <- ggplot(denv_epi_agre , aes(x=month, y=(incidence))) +  
  geom_bar(stat="identity", fill="#7fcdbb") +
  theme_classic()+
  theme(legend.position = "bottom")+
  labs(x="Year",y="100 (%)",fill="",title = "") +
  scale_x_date(labels = label_date_short(format = c("%Y")),expand = c(0,0), breaks = "1 year") +
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.text=element_text(size=10),
        text=element_text(size=10),
        title = element_text(size=12))
p0

p1 <- ggplot(denv_serotype_agrefinal , aes(x=aesx, y=count.x, fill=SOROTIPO)) +  
  geom_bar(position = "fill", stat="identity") +
  scale_y_continuous(label = c("0%", "25%", "50%", "75%", "100%"))+
  scale_fill_manual("", values = c("Dengue 1" = "#00adbe", "Dengue 2" = "#0067a1", 
                                   "Dengue 3" = "#c05980", "Dengue 4" = "#f37e8d")) +
  theme_classic()+
  theme(legend.position = "bottom")+
  labs(x="Year",y="Proportion (%)",fill="",title = "") +
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.text=element_text(size=10),
        text=element_text(size=10),
        title = element_text(size=12))
p1

p2 <- ggplot(denv_serotype_agre , aes(x=ANO_SEMANA_EPIDEMIOLOGICA, y=count, group=SOROTIPO, colour=SOROTIPO)) +  
  geom_line(linewidth=1) +
  scale_color_manual("", values = c("Dengue 1" = "#00adbe", "Dengue 2" = "#0067a1", 
                                   "Dengue 3" = "#c05980", "Dengue 4" = "#f37e8d")) +
  theme_classic()+
  theme(legend.position = "bottom")+
  labs(x="Year",y="Cases number per serotype",fill="",title = "") +
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.text=element_text(size=10),
        text=element_text(size=10),
        title = element_text(size=12))
p2

p <- arrangeGrob(p0, p2, ncol=1)
ggsave(filename = paste0("test.pdf"), plot = p, device = "pdf", dpi = 300, width = 17, height = 12)


