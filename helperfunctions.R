# Functions from Sam


model_plotter<-function(model, data, parameter){
  data<-data%>%
    filter(!is.na(.data[[parameter]]))%>%
    mutate(Residuals=resid(model),
           Fitted=predict(model))

  units<-case_when(
    parameter=="Temperature" ~ " (°C)",
    parameter=="Salinity_l" ~ " (log)",
    parameter=="Secchi_l" ~ " (log)"
  )
  parameter_label<-case_when(parameter=="Secchi_l" ~ "secchi depth",
                             parameter=="Salinity_l" ~ "salinity",
                             parameter=="Temperature" ~ "temperature")

  p_hist<-ggplot(data, aes(x=Residuals))+
    geom_histogram()+
    xlab(paste0("Residuals", units))+
    theme_bw()

  p_res_fit<-ggplot(data, aes(x=Residuals, y=Fitted))+
    geom_point()+
    ylab(paste0("Predicted ", parameter_label, units))+
    xlab(paste0("Residuals", units))+
    theme_bw()

  p_obs_fit<-ggplot(data, aes(x=.data[[parameter]], y=Fitted))+
    geom_point()+
    geom_abline(slope=1, intercept=0, color="red")+
    ylab(paste0("Predicted ", parameter_label, units))+
    xlab(paste0("Observed ", parameter_label, units))+
    theme_bw()

  out<-(p_hist+plot_layout(ncol=1))+(p_res_fit+p_obs_fit+plot_layout(ncol=2))+plot_layout(nrow=2, widths=c(1, 0.5, 0.5))

  return(out)
}

result_plotter<-function(contrast, variable, xlabel){
  contrasts<-plot(contrast$emmeans, comparisons = T, by=variable, plotit=F)%>%
    mutate(across(c(lcmpl, rcmpl), ~ if_else(is.na(.x), the.emmean, .x)))

  ggplot(contrasts, aes(y=Drought, x=the.emmean))+
    geom_pointrange(aes(xmin=lcmpl, xmax=rcmpl))+
    facet_wrap(~.data[[variable]], scales="free_x", ncol=1)+
    xlab(xlabel)+
    theme_bw()
}

partial.r2<-function(ANOVA, factor){
  r2<-ANOVA[factor, "Sum Sq"]/(ANOVA[factor, "Sum Sq"]+ ANOVA["Residuals", "Sum Sq"])
  return(r2)
}
