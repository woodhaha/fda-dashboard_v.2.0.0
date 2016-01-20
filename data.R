query_drug <- function(drug) {
    fda_query("/drug/event.json") %>%
        fda_api_key("4kdJ2pgx9MCYScE11gXrhfJIp7jIfxv7UsbrkUti") %>% 
        fda_filter("patient.drug.openfda.generic_name",
                   drug)
}

query_allpharm <- function(pharm) {
    fda_query("/drug/event.json") %>%
        fda_api_key("4kdJ2pgx9MCYScE11gXrhfJIp7jIfxv7UsbrkUti") %>% 
        fda_filter("patient.drug.openfda.pharm_class_epc",
                   pharm)
}

query_moa <- function(moa) {
    fda_query("/drug/event.json") %>%
        fda_api_key("4kdJ2pgx9MCYScE11gXrhfJIp7jIfxv7UsbrkUti") %>% 
        fda_filter("patient.drug.openfda.pharm_class_moa",
                   moa)
}
count_fda <- function(variable, ...) {
    
    dots <- unlist(list(...))
    print (dots)
    #   validate(
    #     need(length(dots) < 5,
    #          message = "Only up to four drugs allowed for now!"),
    #     errorClass = "too-many-warning"
    #   )
    
    do.call(rbind,
            lapply(dots, FUN = function(input_drug) {
                tryCatch(
                    tbl_df(
                        query_drug(input_drug) %>%
                            fda_api_key("4kdJ2pgx9MCYScE11gXrhfJIp7jIfxv7UsbrkUti") %>%   
                            fda_count(variable) %>%
                            fda_exec()
                    ) %>%
                        mutate(drug = input_drug),
                    error = function(e) {
                        stop("Oops, API limit hit, please try again shortly!")
                    })
            }))
}
# Added By Abhik Seal
count_pharm <- function(variable, ...) {
    
    dots <- unlist(list(...))
    print (dots)
    #   validate(
    #     need(length(dots) < 5,
    #          message = "Only up to four drugs allowed for now!"),
    #     errorClass = "too-many-warning"
    #   )
    dots <- gsub(" ", "+",dots)
    dots <- gsub("[EPC]","",dots)
    dots <- gsub("\\[|\\]","",dots)
    do.call(rbind,
            lapply(dots, FUN = function(input_pharm) {
                tryCatch(
                    tbl_df(
                        query_allpharm(input_pharm) %>% 
                            fda_count(variable) %>%
                            fda_exec()
                    ) %>%
                        mutate(pharm = input_pharm),
                    error = function(e) {
                        stop("Oops, API limit hit, please try again shortly!")
                    })
            }))
}
count_moa <- function(variable, ...) {
    
    dots <- unlist(list(...))
    print (dots)
    #   validate(
    #     need(length(dots) < 5,
    #          message = "Only up to four drugs allowed for now!"),
    #     errorClass = "too-many-warning"
    #   )
    dots <- gsub(" ", "+",dots)
    #dots <- gsub("[MoA]","",dots)
    #dots <- gsub("\\[|\\]","",dots)
    do.call(rbind,
            lapply(dots, FUN = function(input_moa) {
                tryCatch(
                    tbl_df(
                        query_moa(input_moa) %>% 
                            fda_api_key("4kdJ2pgx9MCYScE11gXrhfJIp7jIfxv7UsbrkUti") %>% 
                            fda_count(variable) %>%
                            fda_exec()
                    ) %>%
                        mutate(moa = input_moa),
                    error = function(e) {
                        stop("Oops, API limit hit, please try again shortly!")
                    })
            }))
}


dates_received <- reactive({
    if(is.null(input$drug))
        return()
    isolate({
        d <- count_fda(variable = "receivedate",
                       input$drug)
        
        d <- d %>%
            mutate(time = as.POSIXct(time,
                                     format = "%Y%m%d"))
        
        # One possible extention is to add a grouping
        # toggle like: month, week, day
        d <- d %>%
            mutate(time = cut.POSIXt(time, 'week',
                                     start.on.monday = F)) %>%
            group_by(drug, time) %>%
            summarise(count = sum(count)) %>%
            mutate(time = as.character(time),
                   time = as.POSIXct(time))
        
        # There are trade-offs in terms of counting weeks
        # without adverse events as 0 or NA.
        # Splines work in log-scale if weeks without events
        # are set to NA. In general, counting 0s are
        # important for distributional modeling and even smoothing.
        # For many drugs 0 counts are rare in given weeks.
        
        #   date_sequence <- data.frame(
        #     time = as.POSIXct(
        #       seq(min(d$time),
        #           max(d$time),
        #           by = "week"),
        #       format = "%Y-%m-%d"))
        #
        #   d <- left_join(date_sequence,
        #                  d) %>%
        #     mutate(count = ifelse(is.na(count), 0, count))
        
        return(d)
    })
})

ages <- reactive({
    if(is.null(input$drug))
        return()
    isolate({
        count_fda(variable = "patient.patientonsetage",
                  input$drug)
    })
})

outcomes <- reactive({
    if(is.null(input$drug))
        return()
    isolate({
        as.data.frame(
            tbl_df(
                dcast(
                    count_fda(variable = "patient.reaction.reactionoutcome",
                              input$drug),
                    term ~ drug,
                    value.var = "count"
                )
            ) %>%
                mutate(term = sapply(term, function(x) {
                    switch(x,
                           "Recovered/resolved",
                           "Recovering/resolving",
                           "Not recovered/not resolved",
                           "Recovered/resolved with sequelae",
                           "Fatal",
                           "Unknown")})) %>%
                rename(Outcome = term)
        )
    })
})


reactionoutcomes <- reactive({
    if(is.null(input$drug))
        return()
    isolate({
        dcast(
            count_fda(variable = "patient.reaction.reactionmeddrapt",
                      input$drug),
            term ~ drug, value.var = "count")
    })
})

#Added By Abhik Seal
indicationoutcome <- reactive({
    if(is.null(input$drug))
        return()
    isolate({
        dcast(
            count_fda(variable = "patient.drug.drugindication",
                      input$drug),
            term ~ drug, value.var = "count")
    })
})

classreaction <- reactive({
    if(is.null(input$pharm))
        return()
    isolate({
        dcast(
            count_pharm(variable = "patient.reaction.reactionmeddrapt.exact",
                        input$pharm),
            term ~ pharm, value.var = "count")
    })
})

moaReaction <- reactive({
    if(is.null(input$moa))
        return()
    isolate({
        dcast(
            count_moa(variable = "patient.reaction.reactionmeddrapt.exact",
                      input$moa),
            term ~ moa, value.var = "count")
    })
})

classIndication <- reactive({
    if(is.null(input$pharm))
        return()
    isolate({
        dcast(
            count_pharm(variable = "patient.drug.drugindication.exact",
                        input$pharm),
            term ~ pharm, value.var = "count")
    })
})


moaIndication <- reactive({
    if(is.null(input$moa))
        return()
    isolate({
        dcast(
            count_moa(variable = "patient.drug.drugindication.exact",
                      input$moa),
            term ~ moa, value.var = "count")
    })
})