
### Changes the type of columns of the current table
###   Dependencies on specific packages: dplyr, tidyr.
###   Dependencies in generated code: dplyr.

page_reclass <- list(

  ui = function() ..ui(page="reclass", control=TRUE),


  server = function(input, output, session) {

    ..vServer(input,output,"reclass")

    output$reclass.control <- renderUI({
      .IGoR$state$meta
      output$reclass.comment <- renderText("")
      if (..isNotEmpty(input$main.data))
        fluidRow(
          column(width=6, ..select.ui("reclass",buttons.title=..s2(.IGoR$Z$reclass$reclass))),
          column(width=6,
            ..load.ui("reclass",input$main.data),
            box(width='100%',
              column(width=6,
                selectizeInput("reclass.class.out",..s2(.IGoR$Z$reclass$class),
                               choices=c('' %>% {names(.)<- .IGoR$Z$reclass$class.none;.},
                                       ..Znames("reclass","class",c("factor","as.character","as.double","as.integer","as.logical","as.Date")))
              ) ),
              column(width=6,
                checkboxInput("reclass.short",..s5(.IGoR$Z$reclass$short),TRUE),
                uiOutput("reclass.empty")
        ) ) ) )
    })

    output$reclass.empty <- renderUI(
      if (..isEQ(input$reclass.class.out,"factor")
        &&(length(input$reclass.type)>0)
        &&((input$reclass.type!=2)
         ||((input$reclass.type==2)
          &&((..isEQ(input$reclass.class,"character")&&!input$reclass.drop)
           ||(..isNE(input$reclass.class,"character")&&input$reclass.drop)))))
        checkboxInput("reclass.empty",..s5(.IGoR$Z$reclass$empty),TRUE)
    )

    output$reclass.command2 <- renderUI(
      ..textarea("reclass", "mutate(column=as...(column))", 4,
        if (!is.null(input$reclass.type)&&..isNotEmpty(input$reclass.class.out)) {
          na <- if ((input$reclass.class.out=="factor")
                  &&((input$reclass.type!=2)
                   ||((input$reclass.type==2)
                    &&((..isEQ(input$reclass.class,"character")&&!input$reclass.drop)
                     ||(..isNE(input$reclass.class,"character")&&input$reclass.drop))))
                  &&..isTRUE(input$reclass.empty)) ", exclude=''" else ""
          ..command2(
            "mutate",
            if (!..isTRUE(input$reclass.short)) {
              old <- ..name(..select.columns(input,output,"reclass"))
              if (length(old)==0) paste0("() # ",.IGoR$Z$reclass$nop)
              else glue("({..collapse0(paste0(old,'=',input$reclass.class.out,'(',old,na,')'))})")
            }
            else
              paste0(
                if (input$reclass.type==2)
                  if (input$reclass.drop)
                       glue("_if(Negate(is.{input$reclass.class}), ")
                  else glue("_if(is.{input$reclass.class}, ")
                else
                if (input$reclass.type==3)
                  if (input$reclass.drop)
                       "_at(c(), "
                  else "_all("
                else glue("_at({..select(input,'reclass',vars=TRUE)}, "),
                input$reclass.class.out,
                na,
                ")"
          )   )
        }
    ) )

    observeEvent(input$reclass.command2,
      ..try(input,output,"reclass",
        function(x) {
          a <- Map(class, ..data(input))
          b <- Map(class, x)
          d <- purrr::map2_lgl(a,b,identical)
          m <- length(d[!d])  # Changed columns
          d <- purrr::map2_lgl(a[!d], b[!d],
                 function(old,new) ("factor" %in% old)                     # possibly length(old)>1
                                  &(new %not in% c("factor","character"))  # allways length(new)==1
               )
          n <- length(d[d])   # Changed columns from factor to not factor or character
          if (m==0) .IGoR$Z$reclass$msg.nop
          else paste0(
            sprintf(.IGoR$Z$reclass$msg.result,m),
            if (n>0) paste0("\n",.IGoR$Z$reclass$msg.factor)
          )
        }
    ) )

  }
)

