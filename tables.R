output$reports_by_week <- renderDataTable({
    if(is.null(input$drug))
        return()
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
}, options = list(searching = FALSE,
                  bLengthChange = I("false")))


## Added by Christopher Peters
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

## Added by Christopher Peters
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
}, options = list(searching = FALSE,
                  paging = TRUE,
                  bLengthChange = I("false"))
)

output$reactions <- renderDataTable({
    if(is.null(input$drug))
        return()
    reactionoutcomes()
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
    classreaction()
}, options = list(searching = TRUE,
                  bLengthChange = I("false")))

output$moa_reactions <- renderDataTable({
    if(is.null(input$moa))
        return()
    moaReaction()
}, options = list(searching = TRUE,
                  bLengthChange = I("false")))

output$classIndication <- renderDataTable({
    if(is.null(input$pharm))
        return()
    classIndication()
}, options = list(searching = TRUE,
                  bLengthChange = I("false")))

output$moaIndication <- renderDataTable({
    if(is.null(input$moa))
        return()
    moaIndication()
}, options = list(searching = TRUE,
                  bLengthChange = I("false")))