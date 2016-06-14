## get the drugs from dictionary

drugs_dict <- read.csv("drug_dictionary.csv",skip=1,row.names=1)
ddi_name <- rownames(read.csv("DDI_DRUGNAMES.csv",header=FALSE,row.names=1))
# output$drugs <- renderUI({
#   selectInput("drug", label = "Choose drugs (generic names)",
#               choices = (tbl_df(
#                 
#                 
#                 fda_query("/drug/event.json") %>%
#                   #fda_api_key("4kdJ2pgx9MCYScE11gXrhfJIp7jIfxv7UsbrkUti") %>% 
#                   fda_count("patient.drug.openfda.generic_name") %>%                   
#                   fda_limit(1000) %>%
#                   fda_exec()) %>%
#                   arrange(term) %>%
#                   filter(nchar(term) > 4))$term,
#               selected = c("oxycodone"), multiple = T)
# })

output$drugs <- renderUI({
  selectInput("drug", label = "Search drugs",
              choices = rownames(drugs_dict),
              selected = "", multiple = T,width='40%')
})


output$data_retrieval_button <- renderUI({
    actionButton("run_button",
                 label = "Retrieve data",
                 class="btn btn-warning")
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

output$dr <- renderUI({
    selectInput("dr", label = "Choose Drug to Search:" ,multiple=F,
                choices = ddi_name)
})

#output$ss <- renderUI({
#    textInput("ss", "Enter Your SMILES : " ,"CC(=O)Oc1ccccc1C(=O)O")
#})

# output$dstat <- renderUI({
#     selectInput("dstat", label = "Choose Drug to get Reaction Stats",
#                 choices = rownames(read.csv("BRANDNAMES.csv",header=FALSE,row.names=1)))
# })

# output$chemicalClass <- renderUI({
#     selectInput("chemicalClass", label = "Choose Chemical Class",
#                 choices = (tbl_df(
#                     fda_query("/drug/event.json") %>%
#                         fda_api_key("4kdJ2pgx9MCYScE11gXrhfJIp7jIfxv7UsbrkUti") %>% 
#                         fda_count("patient.drug.openfda.pharm_class_cs.exact") %>%                   
#                         fda_limit(1000) %>%
#                         fda_exec()) %>%
#                         arrange(term) %>%
#                         filter(nchar(term) > 3))$term,
#                 selected = c("Benzodiazepines"), multiple = T)
# })