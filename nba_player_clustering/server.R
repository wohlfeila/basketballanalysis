shinyServer(function(input, output) {
    
    js <- "
    function(el, x, inputName){
      var id = el.getAttribute('id');
      var gd = document.getElementById(id);
      var d3 = Plotly.d3;
      Plotly.plot(id).then(attach);
        function attach() {
          var xaxis = gd._fullLayout.xaxis;
          var yaxis = gd._fullLayout.yaxis;
          var l = gd._fullLayout.margin.l;
          var t = gd._fullLayout.margin.t;
          var coordinates = [null, null]
    
          gd.addEventListener('click', function(evt) {
            var coordinates = [xaxis.p2c(evt.x - l), yaxis.p2c(evt.y - t)];
            Shiny.setInputValue(inputName, coordinates);
          });
        };
  }
  "

    years_reactive <- reactive({
        select_years <- str_split(input$years_select, " ") %>% unlist()
        
        if(input$years_select == "All"){
            select_years <- years
        } else {
            select_years <- str_split(input$years_select, " ") %>% unlist()
        }
        
        tmp <- nba_stats %>% 
            filter(year %in% select_years)
        
        return(tmp)
    })
            
    kmeans_reactive <- reactive({
        tmp <- kmeans_calc(years_reactive(), input$num_cluster)
        return(tmp)
    })
            
    tmp <- reactive({
        
        color_list <- randomcoloR::randomColor(count = input$num_cluster)
        
        p <- plot_ly(data = kmeans_reactive(),
                     x = ~PC1,
                     y = ~PC2,
                     color = ~.cluster,
                     colors = color_list,
                     text = ~players,
                     textposition = "auto",
                     hoverinfo = "text") %>%
            add_markers(size = 2) %>%
            layout(showlegend = FALSE)
        
        p <- htmlwidgets::appendContent(p, htmltools::tags$input(id='inputText', value='Kareem Abdul-Jabbar', '', size = 40), 
                                        htmltools::tags$button(id='buttonSearch', 'Search'))
        
        p <- htmlwidgets::appendContent(p, htmltools::tags$script(HTML(
            'document.getElementById("buttonSearch").addEventListener("click", function()
          {        
              var i = 0;
             var j = 0;
              var found = [];
              var myDiv = document.getElementsByClassName("js-plotly-plot")[0]
              var data = JSON.parse(document.querySelectorAll("script[type=\'application/json\']")[0].innerHTML);
              for (i = 0 ;i < data.x.data.length; i += 1) {
                for (j = 0; j < data.x.data[i].text.length; j += 1) {
                  if (data.x.data[i].text[j].indexOf(document.getElementById("inputText").value) !== -1) {
                    found.push({curveNumber: i, pointNumber: j});
                  }
                }
              }
              Plotly.Fx.hover(myDiv, found);
            }  
         );')))
        
        p
    })
    
    output$player_clusters <- renderUI({
        # Change when the "update" button is pressed...
        input$refresh_clusters
        
        # ...but not for anything else
        isolate({
            withProgress({
                setProgress(message = "Processing...")
                
        saveWidget(tmp(), "p1.html")
        tags$iframe(seamless="seamless", src= "nba_player_clustering/p1.html", width=1000, height=800)
    })
        })
    })

})
