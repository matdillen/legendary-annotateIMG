library(shiny)
library(tidyverse)
library(shinyjs)
library(magrittr)

source("fun.R")

#data import
heroes=read_csv2('data/heroes.csv')
schemes=read_csv2('data/schemes.csv')
villains=read_csv2('data/villains.csv')
henchmen=read_csv2('data/henchmen.csv')
masterminds=read_csv2('data/masterminds.csv')

heroes$id = paste0(heroes$Hero," (",heroes$Set,")")
heroes$uni = heroes$id
heroes$keywords = ""
heroes$seq = seq(1,dim(heroes)[1])
schemes$id = schemes$Name
schemes$keywords = ""
schemes$seq = seq(1,dim(schemes)[1])
villains$id = villains$Group
villains$keywords = ""
villains$seq = seq(1,dim(villains)[1])
henchmen$id = henchmen$Name
henchmen$keywords = ""
henchmen$seq = seq(1,dim(henchmen)[1])
masterminds$id = ifelse(is.na(masterminds$MM),masterminds$Name,masterminds$MM)
masterminds$keywords = ""
masterminds$seq = seq(1,dim(masterminds)[1])

#format data as list
src = list(heroes,schemes,villains,henchmen,masterminds)
names(src) = c("heroes","schemes","villains","henchmen","masterminds")

#format a list of heroes with proper ids
#arrange by abc and add an empty initial value
herolist = distinct(src$heroes,uni)
herolist %<>% arrange(uni)
heroaslist = as.list(t(herolist$uni))
names(heroaslist) = herolist$uni

#format a list of henchmen
henchlist = distinct(src$henchmen,Name)
henchlist %<>% arrange(Name)
henchaslist = as.list(t(henchlist$Name))
names(henchaslist) = henchlist$Name

#format a list of villains
villist = distinct(src$villains,Group)
villist %<>% arrange(Group)
vilaslist = as.list(t(villist$Group))
names(vilaslist) = villist$Group

#format a list of masterminds
mmlist = distinct(filter(src$masterminds,!is.na(MM)),MM)
mmlist %<>% arrange(MM)
mmaslist = as.list(t(mmlist$MM))
names(mmaslist) = mmlist$MM

#format a list of schemes
schlist = distinct(src$schemes,Name)
schlist %<>% arrange(Name)
schemaslist = as.list(t(schlist$Name))
names(schemaslist) = schlist$Name

srcaslist = list(heroaslist,schemaslist,vilaslist,henchaslist,mmaslist)
names(srcaslist) = names(src)

types = names(src)
typeslist = as.list(types)
names(typeslist) = types

imgsize = read_tsv("data/sizeinfo.txt")
keywords = read_tsv("data/keywords-tts.txt",col_names = F)
kwlist = pull(keywords,X1)
kwlist = as.list(t(kwlist))
names(kwlist) = keywords$X1


ui <- fluidPage(
    useShinyjs(),
    sidebarLayout(sidebarPanel(
        fluidRow(
            column(
                selectizeInput("cards",
                               "Card text",
                               heroaslist),
                width=6),
            column(
                selectizeInput("type",
                               "Card type",
                               typeslist),
                width=6
            )),
        actionButton("nextc","Next"),
        hidden(selectizeInput("keywords1",
                      "Keywords",
                      kwlist,
                      multiple=T)),
        hidden(selectizeInput("keywords1",
                              "Keywords",
                              kwlist,
                              multiple=T)),
        hidden(selectizeInput("keywords2",
                              "Keywords",
                              kwlist,
                              multiple=T)),
        hidden(selectizeInput("keywords3",
                              "Keywords",
                              kwlist,
                              multiple=T)),
        hidden(selectizeInput("keywords4",
                              "Keywords",
                              kwlist,
                              multiple=T)),
        hidden(selectizeInput("keywords5",
                              "Keywords",
                              kwlist,
                              multiple=T)),
        hidden(selectizeInput("keywords6",
                              "Keywords",
                              kwlist,
                              multiple=T)),
        hidden(selectizeInput("keywords7",
                              "Keywords",
                              kwlist,
                              multiple=T)),
        hidden(selectizeInput("keywords8",
                              "Keywords",
                              kwlist,
                              multiple=T)),
        hidden(selectizeInput("keywords9",
                              "Keywords",
                              kwlist,
                              multiple=T)),
        actionButton("save",
                     "Save Edit"),
        width = 6
    ),
    mainPanel(
        HTML("<BR>"),
        uiOutput("images"),
        width = 6
    ))
)

server <- shinyServer(function(input, output, session) {
    
    tt = reactiveValues(data = src)
    
    observeEvent(input$type,{
        filterlist = srcaslist[input$type][[1]]
        updateSelectizeInput(session, "cards",choices=filterlist)
    })
    
    #Next button for easier processing the whole file
    observeEvent(input$nextc,{
        part = srcaslist[input$type]
        id = grep(input$cards,
                  names(part[[1]]),
                  fixed=T)
        nextn = names(part[[1]])[id+1]
        updateSelectizeInput(session,"cards",selected = nextn)
    })
    
    #render the images
    output$images <- renderUI({
        imgGen(input$cards,input$type,src,imgsize)
    })
    
    #show keyword inputs
    
    observeEvent(input$cards,{
        data = filter(isolate(tt)$data[input$type][[1]],
                      id==input$cards)
        
        if (input$type=="heroes") {
            omit = data %>%
                filter(duplicated(Split),
                       !is.na(Split),
                       !grepl("T",Split))
        } else if (input$type=="masterminds") {
            omit = data %>% 
                filter(!is.na(T)|
                       !is.na(Epic))
        } else {
            omit = tibble(seq=NA)
        }
        for (i in 1:10) {
            inpname = paste0("keywords",i)
            if (i > dim(data)[1]) {
                updateSelectizeInput(session,inpname,selected = "")
                hide(inpname)
            } else if (data$seq[i]%in%omit$seq) {
                updateSelectizeInput(session,inpname,selected = "")
                hide(inpname)
            } else {
                updateSelectizeInput(session,inpname,selected = strsplit(data$keywords[i],split="\\|")[[1]])
                show(inpname)
                if (input$type=="henchmen"&input$cards=="Mandarin's Rings") {
                    updateSelectizeInput(session,inpname,label = data$NameSpecific[i])
                } else {
                    updateSelectizeInput(session,inpname,label = data$Name[i])
                }
            }
        }
    })
    
    #save to the temporary data in the session
    #and also to file
    #currently saves to file in a different directory than the source file
    observeEvent(input$save,{
        k = isolate(tt$data[input$type][[1]])
        datatosave = filter(k,
                      id==input$cards)
        if (input$type=="heroes") {
            datatosave %<>% filter(!duplicated(Split)|
                                is.na(Split)|
                                grepl("T",Split))
        }
        for (i in 1:dim(datatosave)[1]) {
            datatosave$keywords[i] = paste(input[[paste0("keywords",i)]],collapse="|")
        }
        k %<>%
            mutate(keywords=replace(keywords,
                                id==input$cards,
                                datatosave$keywords))
        write_tsv(k,paste0(input$type,"-keywords.txt"),na="")
        tt$data[input$type][[1]] = k
    })
    
})

shinyApp(ui=ui,server=server)