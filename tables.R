output$reports_by_week <- renderDataTable({
  if(is.null(input$drug))
    return()
  isolate({
  as.data.frame(
    tbl_df(
      dcast(
        dates_received() %>%
          mutate(Week = format(time, "%Y-%m-%d"),
                 Week = as.POSIXct(Week)) %>%
          select(Week, `Adverse Events` = count),
        Week ~ drug,
        value.var = "Adverse Events"
      )
    )  %>%
      arrange(desc(Week))
  )
  })
}, options = list(searching = FALSE,
                  bLengthChange = I("false")))


output$pc_reports_by_week <- renderDataTable({
    if(is.null(input$pharm))
        return()
    isolate({
        as.data.frame(
            tbl_df(
                dcast(
                    pc_dates_received() %>%
                        mutate(Week = format(time, "%Y-%m-%d"),
                               Week = as.POSIXct(Week)) %>%
                        select(Week, `Adverse Events` = count),
                    Week ~ pharm,
                    value.var = "Adverse Events"
                )
            )  %>%
                arrange(desc(Week))
        )
    })
}, options = list(searching = FALSE,
                  bLengthChange = I("false")))

output$outcomes <- renderDataTable({
  if(is.null(input$drug))
    return()
  tbl_df(
    outcomes()) %>%
    arrange(Outcome)
}, options = list(searching = FALSE,
                  paging = FALSE,
                  bLengthChange = I("false"))
)

output$outcome_shares <- renderDataTable({
  if(is.null(input$drug))
    return()
  isolate({
  dcast(
    tbl_df(
      melt(outcomes(),
           "Outcome")) %>%
      group_by(variable) %>%
      mutate(total_report_count = sum(value),
             share = value / total_report_count) %>%
      ungroup %>%
      mutate(share = percent(share)) %>%
      select(Outcome, variable, share),
    Outcome ~ variable,
    value.var = "share")
  })
}, options = list(searching = FALSE,
                  paging = TRUE,
                  bLengthChange = I("false"))
)

output$reactions <- renderDataTable({
  if(is.null(input$drug))
    return()
  isolate({
  reactionoutcomes()
  })
}, options = list(searching = TRUE,
                  bLengthChange = I("false")))

## Added By Abhik Seal
output$indications <- renderDataTable({
    if(is.null(input$drug))
        return()
    indicationoutcome()
}, options = list(searching = TRUE,
                  bLengthChange = I("false")))

output$deaths <- renderText({
  paste(
    comma((tbl_df(
      fda_query("/drug/event.json") %>%
        fda_api_key("4kdJ2pgx9MCYScE11gXrhfJIp7jIfxv7UsbrkUti") %>% 
        fda_count("patient.reaction.reactionoutcome") %>%
        fda_exec()) %>%
        filter(term == 5))$count),
    "deaths")
})

# Added By Abhik Seal
output$all_reactions <- renderDataTable({
    if(is.null(input$pharm))
        return()
    isolate({
    classreaction()
    })
}, options = list(searching = TRUE,
                  bLengthChange = I("false")))

output$moa_reactions <- renderDataTable({
    if(is.null(input$moa))
        return()
    isolate({
    moaReaction()
    })
}, options = list(searching = TRUE,
                  bLengthChange = I("false")))

output$classIndication <- renderDataTable({
    if(is.null(input$pharm))
        return()
    isolate({
    classIndication()
    })
}, options = list(searching =TRUE,
                  bLengthChange = I("false")))

output$moaIndication <- renderDataTable({
    if(is.null(input$moa))
        return()
    isolate({
    moaIndication()
    })
}, options = list(searching = TRUE,
                  bLengthChange = I("false")))


output$ddi_reactions <- renderDataTable({
    
    if(is.null(input$dr))
        return()
    isolate({
       ddiReact()
    })
}, options = list(searching =TRUE,
                  bLengthChange = I("false")))

output$dr_reactions <- renderDataTable({
    
    if(is.null(input$dr))
        return()
    isolate({
        drReact()
    })
}, options = list(searching =TRUE,
                  bLengthChange = I("false")))

#output$ss <- renderDataTable({
    
#    if(is.null(input$ss))
#        return()
#    isolate({
#        simSearch()
#    })
#}, options = list(searching =TRUE,
#                  bLengthChange = I("false")))

# output$dstat_reactions <- renderDataTable({
#     
#     if(is.null(input$dstat))
#         return()
#     isolate({
#         dstatReact()
#     })
# }, options = list(searching =TRUE,
#                   bLengthChange = I("false")))


# output$chemClassReaction <- renderDataTable({
#     if(is.null(input$chemicalClass))
#         return()
#     isolate({
#         chemicalClassReaction()
#     })
# }, options = list(searching = TRUE,
#                   bLengthChange = I("false")))
# 
# 
# 
# 
# output$chemClassIndication <- renderDataTable({
#     if(is.null(input$chemicalClass))
#         return()
#     isolate({
#         chemicalClassIndication()
#     })
# }, options = list(searching = TRUE,
#                   bLengthChange = I("false")))


ages_for_table <- reactive({
    ages() %>%
        rename(Age = term) %>%
        mutate(Age = cut(Age,
                         breaks = seq(0, 120,
                                      5),
                         include.lowest = TRUE)) %>%
        group_by(drug) %>%
        mutate(total_report_count = sum(count),
               share = round(count / total_report_count, 4)) %>%
        ungroup %>%
        mutate(share = share)
})

output$age_shares <- renderDataTable({
    if(is.null(input$drug))
        return()
    isolate({
        tbl_df(
            dcast(
                ages_for_table() %>%
                    select(Age, drug, share),
                Age ~ drug,
                value.var = "share", fill = 0,
                fun.aggregate = sum)) %>%
            mutate_each(funs(percent), -c(1))
    })
}, options = list(searching = FALSE,
                  paging = FALSE,
                  LengthChange = I("false"))
)


output$ages_counts <- renderDataTable({
    if(is.null(input$drug))
        return()
    isolate({
        dcast(
            ages_for_table() %>%
                select(Age, drug, count),
            Age ~ drug,
            value.var = "count", fill = 0,
            fun.aggregate = sum)
    })
}, options = list(searching = FALSE,
                  paging = FALSE,
                  LengthChange = I("false"))
)