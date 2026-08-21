#Catch to biomass ratio plot (uses total biomass age 6+ from the assessment and the projections)
#Borrowing code from file.path(codedir,"CallPlots.R") to read in data from a run.
#' Plot the catch to biomass ratio for harvest projection SAFE
#'
#' @param plot_dir 
#' @param proj_output 
#' @param assess_output 
#' @param LastProjYr 
#'
#' @returns
#' @export
#'
#' @examples
plot_catch_biomass_ratio<-function(plot_dir,proj_output, assess_output,LastProjYr = 2028) {

  #rundir<-"C:/Users/carey.mcgilliard/Work/FlatfishAssessments/2023/BSAI_NRS/Partial_Run_2023/"
  .OVERLAY <-TRUE
  .THEME<- theme_few()

#Grab the total biomass from projection years
proj_results.df<-proj_output %>%
  dplyr::select(c(Year,GM_Biom,Catch_Assump,ABC_HM)) %>%
  dplyr::rename(TotBiom = GM_Biom,Catches = Catch_Assump) %>%
  dplyr::mutate(Ratio = Catches/TotBiom, Type = "Projection") %>%
  dplyr::select(c(Year,Ratio,Type))

all.df<-data.frame(Year = assess_output$TotBiom[,1],
                            TotBiom = assess_output$TotBiom[,2],
                            Catches = assess_output$Obs_catch) %>%
  dplyr::mutate(Ratio = Catches/TotBiom,Type = "Historical") %>%
  dplyr::select(c(Year,Ratio,Type)) %>%
  dplyr::bind_rows(proj_results.df) %>%
  dplyr::filter(Year<=LastProjYr) %>%
  dplyr::group_by(Type)

p2<-all.df %>% ggplot(aes(x = Year,y = Ratio,group = Type, color = Type)) + 
  geom_line(linewidth = 1.3) + theme_classic() +
  scale_color_manual(values = c("black","cadetblue")) + ylim(c(0,0.2)) +
  ylab("Catch to Age 6+ Total Biomass Ratio") +
  theme(axis.text.x=element_text(size=12), axis.text.y = element_text(size=12))

ggsave(filename = file.path(plot_dir,"catch_biomass_ratio.png"),p2)
return(p2)
}