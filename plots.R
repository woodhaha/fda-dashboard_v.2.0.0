library(rCharts)
#library(dygraphs)
#library(stringi)
theme_drug_plots <- function(...) {
  theme(panel.grid.minor.y = element_blank(),
        axis.title.x = element_text(size = 15),
        plot.title = element_text(size = 18),
        legend.position = "top",
        ...)
}

output$reports <- renderChart2({
  if(is.null(input$drug))
    return()
  isolate({
    d <- tbl_df(dates_received()) %>% filter(time >= as.POSIXct(as.Date(time,format='%Y-%m-%d')))
    mo <- strftime(d$time, "%m")
    yr <- strftime(d$time, "%Y")
    dd <- data.frame(mo, yr, log10(d$count),d$drug)
    colnames(dd)[3] <- "count"
    dd.agg <- aggregate(count ~ mo + yr + d.drug, dd, FUN = sum)
    dd.agg$date <- as.POSIXct(paste(dd.agg$yr, dd.agg$mo, "01", sep = "-"))
    print(head(dd.agg))
    plt <- nPlot(count ~ date ,data = dd.agg , group="d.drug",type="lineChart")
    plt$xAxis(
      tickFormat =   "#!
         function(d) {return d3.time.format('%Y-%m-%d')(new Date(d*1000));}
          !#",
  rotateLabels = -90
     )
    plt$addParams(width = 1000, height =500,title = "Adverse Events Counts")
    plt$set(dom = 'graph')
    return(plt)
    
#     p <- ggplot(tbl_df(dates_received()) %>%
#                   filter(time >= as.POSIXct("2004-01-01 00:00:00")),
#                 aes(x = time,
#                     y = count,
#                     colour = drug)) +
#       geom_point(alpha = 0.5) +
#       geom_smooth(method = 'gam',
#                   formula = y ~ s(x,
#                                   bs = 'ps'),
#                   se = F,
#                   size = 2) +
#       scale_color_colorblind(
#         name = "Drug(s)",
#         guide = guide_legend(ncol = 2,
#                              override.aes = list(size = 5))) +
#       scale_x_datetime(breaks = pretty_breaks(10)) +
#       scale_y_continuous(breaks = pretty_breaks(10),
#                          labels = comma) +
#       theme_light(base_size = 20,base_family = "Helvetica") +
#       theme_hc()+
#       scale_colour_hc()+
#       theme_drug_plots(axis.text.x = element_text(size = 18,
#                                                   angle = 90,
#                                                   vjust = 0.5)) +
#       ylab("Adverse Events") +
#       xlab("")
# 
# 
#     if(input$log_scale) {
#       # http://stackoverflow.com/posts/22227846/revisions
#       base_breaks <- function(n = 10){
#         function(x) {
#           axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
#         }
#       }
#       p <- p + scale_y_log10(breaks = base_breaks(),
#                              labels = prettyNum)
#     }
# 
#     print(p)
  })


# output$downloadPlot3 <- downloadHandler(
#       filename = function() { paste('perweek.pdf') },
#       content = function(file) {
#           ggsave(file,p,width=10, height=10)
#       }
# )
})

output$pc_reports <- renderChart2({
    if(is.null(input$pharm))
        return()
    isolate({
      
      d <- tbl_df(pc_dates_received()) %>% 
        filter(time >= as.POSIXct(as.Date(time,format='%Y-%m-%d')))
      print (head(d))
      mo <- strftime(d$time, "%m")
      yr <- strftime(d$time, "%Y")
      dd <- data.frame(mo, yr, log10(d$count),d$pharm)
      print(head(dd))
      colnames(dd)[3] <- "count"
      dd.agg <- aggregate(count ~ mo + yr + d.pharm, dd, FUN = sum)
      dd.agg$date <- as.POSIXct(paste(dd.agg$yr, dd.agg$mo, "01", sep = "-"))
      
      #dd.agg$pharm <- stri_replace_all(dd.agg$d.pharm, " ", fixed="+")      
      plt <- nPlot(count ~ date ,data = dd.agg , group="d.pharm",type="lineChart")
      plt$xAxis(
        tickFormat =   "#!
         function(d) {return d3.time.format('%Y-%m-%d')(new Date(d*1000));}
          !#",
        rotateLabels = -90
      )
      plt$addParams(width = 1000, height = 500,title = "Adverse Events Counts")
      plt$set(dom = 'graph1')
      return(plt)
        
#         p <- ggplot(tbl_df(pc_dates_received()) %>%
#                         filter(time >= as.POSIXct("2004-01-01 00:00:00")),
#                     aes(x = time,
#                         y = count,
#                         colour = pharm),plot.margin = unit(c(1.5,1.5,1.5,1.5), "cm")) +
#             geom_point(alpha = 0.5) +
#             geom_smooth(method = 'gam',
#                         formula = y ~ s(x,
#                                         bs = 'ps'),
#                         se = F,
#                         size = 2) +
#             scale_color_colorblind(
#                 name = "Pharmacological Class",
#                 guide = guide_legend(ncol = 2,
#                                      override.aes = list(size = 5))) +
#             scale_x_datetime(breaks = pretty_breaks(10)) +
#             scale_y_continuous(breaks = pretty_breaks(10),
#                                labels = comma) +
#             theme_light(base_size = 20,base_family = "Helvetica") +
#             theme_hc()+
#             scale_colour_hc()+
#             theme_drug_plots(axis.text.x = element_text(size = 18,
#                                                         angle = 90,
#                                                         vjust = 0.5)) +
#             ylab("Adverse Events") +
#             xlab("")
#         
#         
#         if(input$log_scale) {
#             # http://stackoverflow.com/posts/22227846/revisions
#             base_breaks <- function(n = 10){
#                 function(x) {
#                     axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
#                 }
#             }
#             p <- p + scale_y_log10(breaks = base_breaks(),
#                                    labels = prettyNum)
#         }
#         
#         print(p)
    })
#     output$downloadPlot4 <- downloadHandler(
#         filename = function() { paste('pc_perweek.pdf') },
#         content = function(file) {
#             ggsave(file,p,width=10, height=10)
#         }
#     )
})



output$ages <- renderPlot({
  if(is.null(input$drug))
    return()
  isolate({
      
    d <- ages() %>%
      filter(term < 150) %>%  # sometimes ages are coded wrong like 15,000
      group_by(drug) %>%
      mutate(total = sum(count)) %>%
      ungroup %>%
      mutate(share = (count / total)*100)

    p <- ggplot(d,
                aes(x = term,
                    y = share,
                    colour = drug)) +
      geom_point() +
      geom_smooth(method = 'gam',
                  formula = y ~ s(x,
                                  bs = 'ps'),
                  se = F,
                  size = 1) +
      scale_color_colorblind(name = "Drug(s)",
                             guide = guide_legend(ncol = 2,
                                                  override.aes = list(size = 5))) +
      scale_x_continuous(breaks = pretty_breaks(10)) +
      scale_y_continuous(breaks = pretty_breaks(10),
                         labels = percent) +
      theme_light(base_size = 20) +
      theme_hc()+
      scale_colour_hc()+    
      theme_drug_plots(axis.text.x = element_text(size = 18),
                       axis.title.y = element_text(vjust = 0.8)) +
      ylab("% of adverse events (by drug)") +
      xlab("Patient Age (at report)")


    print(p)
   })
  output$downloadPlot1 <- downloadHandler(
      filename = function() { paste('ages.pdf') },
      content = function(file) {
          ggsave(file,p,width=8, height=8)
      }
  )
})

output$outcome_plot <- renderChart2({
  if(is.null(input$drug))
    return()
  isolate({
    d <- tbl_df(
      melt(outcomes(),
           "Outcome")) %>%
      group_by(variable) %>%
      mutate(total_report_count = sum(value),
             share = (value / total_report_count)*100) %>%
      ungroup
    print (head(d))
    plt <- nPlot(share ~ Outcome, group = 'variable', data = d, type = 'multiBarChart')
    plt$addParams(width = 600, height = 400,title = "Bar plots Drug Outcomes")
    plt$chart(reduceXTicks = FALSE)
    plt$xAxis(staggerLabels = TRUE)
    return(plt)
#     p <- ggplot(
#       data = d,
#       aes(x = Outcome,
#           y = share,
#           fill = factor(variable))) +
#       geom_bar(stat = "identity",
#                position = "dodge") +
#       scale_fill_colorblind(
#         name = "Drug(s)",
#         guide = guide_legend(ncol = 2,
#                              override.aes = list(size = 5))) +
#       scale_y_continuous(breaks = pretty_breaks(10),
#                          labels = percent) +
#       theme_light(base_size = 20) +
#         theme_hc()+
#         scale_colour_hc()+
#       theme_drug_plots(axis.text.x = element_text(size = 15,
#                                                   angle = 15,
#                                                   vjust = 1, hjust = 1,
#                                                   colour = "black")) +
#       ylab("% of outcomes") +
#       xlab("")
#      
# 
#     print(p)
  })
  output$downloadPlot2 <- downloadHandler(
      filename = function() { paste('outcome.pdf') },
      content = function(file) {
          ggsave(file,p,width=10, height=10)
      }
  )
})
