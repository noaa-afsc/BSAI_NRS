library(dplyr)
library(ggplot2)

#' Plot survey biomass
#'
#' @param in_file this is the survey biomass to read in with sql developer column names
#' @param out_dir directory where the plot should be saved
#'
#' @returns
#' @export
#'
#' @examples
#' surv_bio_plot(in_file = "C:/Users/carey.mcgilliard/Work/FlatfishAssessments/2026/bsai_nrs/data/output/bs_srv_biomdat.csv", out_dir = "C:/Users/carey.mcgilliard/Work/FlatfishAssessments/2026/bsai_nrs/plots")
surv_bio_plot<-function(in_file,out_dir,stock = "bsai_nrs") {
  #species <-"BSAI_NRS"
  #in_dir <-file.path("C:\\Users\\carey.mcgilliard\\Work\\FlatfishAssessments\\2023",species)
  
  #survey biomass prior to 1996 is unidentified rock sole.
  #Survey biomass with confidence intervals plot:
  biomass.df<-read.csv(in_file)
  
  biomass.df<-biomass.df %>% 
    dplyr::rename_with(tolower) %>%
    dplyr::mutate(cv = sqrt(varbio)/biomass) %>%
    dplyr::mutate(sd_log_pred = sqrt(log(cv^2+1))) %>%
    dplyr::mutate(lci = exp(log(biomass)-qnorm(0.975)*sd_log_pred), uci = exp(log(biomass)+qnorm(0.975)*sd_log_pred))
  
  p<-biomass.df %>% ggplot2::ggplot(aes(year,biomass)) + 
    ggplot2::geom_ribbon(aes(ymin = lci, ymax = uci), fill = "cadetblue", alpha = 0.3) +
    ggplot2::geom_line(color = "black",linewidth = 1) +
    ggplot2::scale_x_continuous(breaks = seq(min(biomass.df$year), max(biomass.df$year), by = 5)) + 
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x=element_text(size=12), axis.text.y = element_text(size=12)) +
    ggplot2::xlab("Year") +
    ggplot2::ylab("Biomass (mt)")
  
  ggplot2::ggsave(filename = file.path(out_dir,paste0(stock,"_survey_biomass_plot.png")),p)
  return(p)
}
