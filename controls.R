output$drugs <- renderUI({
    selectInput("drug", label = "Choose drugs (generic names)",
                choices = (tbl_df(
                    fda_query("/drug/event.json") %>%
                        #fda_api_key("4kdJ2pgx9MCYScE11gXrhfJIp7jIfxv7UsbrkUti") %>% 
                        fda_count("patient.drug.openfda.generic_name") %>%                   
                        fda_limit(1000) %>%
                        fda_exec()) %>%
                        arrange(term) %>%
                        filter(nchar(term) > 4))$term,
                selected = c("oxycodone"), multiple = T)
})


output$pharmclass <- renderUI({
    selectInput("pharm", label = "Choose Pharmacological Class",
                choices = (tbl_df(
                    fda_query("/drug/event.json") %>%
                        fda_api_key("4kdJ2pgx9MCYScE11gXrhfJIp7jIfxv7UsbrkUti") %>% 
                        fda_count("patient.drug.openfda.pharm_class_epc.exact") %>%                   
                        fda_limit(1000) %>%
                        fda_exec()) %>%
                        arrange(term) %>%
                        filter(nchar(term) > 3))$term,
                selected = c("Benzodiazepine"), multiple = T)
})

output$moa <- renderUI({
    selectInput("moa", label = "Choose MOA",
                choices = (tbl_df(
                    fda_query("/drug/event.json") %>%
                        fda_api_key("4kdJ2pgx9MCYScE11gXrhfJIp7jIfxv7UsbrkUti") %>% 
                        fda_count("patient.drug.openfda.pharm_class_moa") %>%                   
                        fda_limit(1000) %>%
                        fda_exec()) %>%
                        arrange(term) %>%
                        filter(nchar(term) > 3))$term,
                selected = c("Cyclooxygenase Inhibitors"), multiple = T)
})

output$data_retrieval_button <- renderUI({
  actionButton("run_button",
               label = "Retrieve data",
               class="btn btn-warning")
})
