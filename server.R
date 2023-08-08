function(input, output)  {
  
  #PAGE1    
  #row1-----------------------------------------------------------------------------------------------------------  
  output$numsui <- renderValueBox({
    num_sui <-  suicide %>%
      group_by(continent)  %>%
      summarise(jumlah=sum(suicides_numbers)) %>% 
      summarise(freq=sum(jumlah)) %>% 
      pull(freq)
    
    valueBox(
      value = comma(num_sui),
      subtitle = "Number of Suicides Recorded",
      color = "red",
      icon = icon("exclamation-triangle"),
      width = 12
    )
  })
  
  output$ratesui <- renderValueBox({
    rate_sui <- suicide %>%
      group_by(continent)  %>%
      summarise(ratarata=mean(suicides_rate)) %>% 
      summarise(freq=round(mean(ratarata),1)) %>% 
      pull(round(freq,1))
    
    valueBox(
      value = rate_sui,
      subtitle = "Number of Suicides per 100k Population",
      icon = icon("exclamation-triangle"),
      color = "red",
      width = 12
    )
  })
  
  output$ratemaxcont <- renderValueBox({
    rate_max_cont <- suicide %>%
      group_by(continent) %>%
      summarise(freq=mean(suicides_rate)) %>%
      arrange(desc(freq)) %>% 
      head(1) %>% 
      pull(continent)
    
    valueBox(
      value = rate_max_cont,
      subtitle = "Continent With The Highest Suicides/100k Population",
      icon = icon("exclamation"),
      color = "red",
      width = 12
    )
  })
  
  output$ratemaxcountry <- renderValueBox({
    rate_max_country <- suicide %>% 
      group_by(country) %>% 
      summarise(freq=mean(suicides_rate)) %>% 
      arrange(desc(freq)) %>% 
      head(1) %>% 
      pull(country)
    
    valueBox(
      value = rate_max_country,
      subtitle = "Country With The Highest Suicides/100k Population",
      icon = icon("exclamation"),
      color = "red",
      width = 12
    )
  })
  
  #row2-----------------------------------------------------------------------------------------------------------
  
  output$lineContinent <- renderPlotly({
    continent_line_plot <-
      ggplot(data = continent_line_data,
             aes(x = year, y = rate,
                 group = continent, color = continent,
                 text = label)) +
      geom_line(lwd = 0.75) +
      geom_point(size = 1) +
      scale_x_continuous(breaks = seq(1985, 2016, by = 5),
                         limits = c(1985, 2016)) +
      labs(x = "Year", y = "Suicide per 100,000 population",
           title = "<b>Suicide Rates Over the Years</b>",
           color = "") +
      theme_minimal()
    
    ggplotly(continent_line_plot, tooltip = "text")
  })
  
  #row3---------------------------------------------------------------------------------------------------   
  
  reactive_continent <- reactive({
    suicide %>%
      filter(continent == input$continentrate)
  })
  
  output$numsuicont <- renderValueBox({
    num_sui_cont <- reactive_continent() %>%
      group_by(continent)  %>%
      summarise(jumlah=sum(suicides_numbers)) %>% 
      summarise(freq=sum(jumlah)) %>% 
      pull(freq)
    
    valueBox(
      value = comma(num_sui_cont),
      subtitle = "Number of Suicides",
      
      color = "purple",
      width = 12
    )
    
  })
  
  output$ratesuicont <- renderValueBox({
    
    rate_sui_cont <- reactive_continent () %>%
      group_by(continent) %>%
      summarise(freq= round(mean(suicides_rate),1)) %>%
      pull(freq)
    
    valueBox(
      value = rate_sui_cont,
      subtitle = "Suicides per 100k Population",
      
      color = "purple",
      width = 12
    )
  })
  
  output$suitimeline <- renderPlotly({
    sui_timeline <- reactive_continent () %>%
      group_by(year,continent) %>%
      summarise(freq = sum(suicides_numbers)) %>%
      mutate(
        label = glue(
          "Year Recorded: {year}
                     Continent: {continent}
                     Number of Suicides: {comma(freq)}"
        )
      ) %>%
      ggplot(aes(
        x = year,
        y = freq,
        text = label,
        group = 1
      )) +
      geom_line(aes(col=continent)) + geom_point() + geom_area(fill="#6c9ed4",alpha=0.4)+
      scale_y_continuous(labels=comma)+
      labs(title = "",
           x = "Year",
           y = "Number of Suicides") +
      theme_bw()
    
    ggplotly(sui_timeline, tooltip = "text") %>% layout(showlegend = F)
  })
  
  output$suiyearly <- renderPlotly({
    suicideyear <- reactive_continent () %>%
      group_by(year,continent) %>%
      summarise(suicidesrate = round(mean(suicides_rate),1)) %>%
      mutate(
        label = glue(
          "Year Recorded: {year}
                     Continent: {continent}
                     Avg Number of Suicides/100k Population: {suicidesrate}"
        )
      ) %>%
      ggplot(aes(
        x = year,
        y = suicidesrate,
        text = label,
        group = 1
      )) +
      geom_line(aes(col=continent)) + geom_point() + geom_area(fill="#54e88f",alpha=0.4)+
      labs(title = "",
           x = "Year",
           y = "Avg Number of Suicides/100k Population") +
      theme_bw()
    
    ggplotly(suicideyear, tooltip = "text") %>% layout(showlegend = F)
    
  })
  
  #Row4------------------------------------------------------------------------------------------------------------    
  
  reactive_country <- reactive({
    suicide %>%
      filter(country == input$countryrate)
  })
  
  output$numsuicountry <- renderValueBox({
    num_sui_country <- reactive_country() %>%
      group_by(country)  %>%
      summarise(jumlahcountry=sum(suicides_numbers)) %>% 
      summarise(freq=sum(jumlahcountry)) %>% 
      pull(freq)
    
    valueBox(
      value = comma(num_sui_country),
      subtitle = "Number of Suicides",
      color = "purple",
      width = 12
    )
  })
  
  output$ratesuicountry <- renderValueBox({
    rate_sui_country <- reactive_country () %>%
      group_by(country) %>%
      summarise(freq= round(mean(suicides_rate),1)) %>%
      pull(freq)
    
    valueBox(
      value = rate_sui_country,
      subtitle = "Suicides per 100k Population",
      color = "purple",
      width = 12
    )
    
  })
  
  output$suitimelinecountry <- renderPlotly({
    
    sui_timeline_country <- reactive_country () %>%
      group_by(year,country) %>%
      summarise(freq = sum(suicides_numbers)) %>%
      mutate(
        label = glue(
          "Year Recorded: {year}
                     Country: {country}
                     Number of Suicides: {comma(freq)}"
        )
      ) %>%
      ggplot(aes(
        x = year,
        y = freq,
        text = label,
        group = 1
      )) +
      geom_line(aes(col="green")) + geom_point() + geom_area(fill="#6c9ed4",alpha=0.4)+
      scale_y_continuous(labels=comma)+
      labs(title = "",
           x = "Year",
           y = "Number of Suicides") +
      theme_bw()
    
    ggplotly(sui_timeline_country, tooltip = "text") %>% layout(showlegend = F)
    
  })
  
  output$suiage <- renderPlotly({
    sui_age <- reactive_country() %>%
      group_by(year,country) %>%
      summarise(freq = round(mean(suicides_rate),1)) %>%
      mutate(
        label = glue(
          "Year Recorded: {year}
                     Country: {country}
                     Avg Number of Suicides/100k Population: {freq}"
        )
      ) %>%
      ggplot(aes(
        x = year,
        y = freq,
        text = label,
        group = 1
      )) +
      geom_line(aes(col="green")) + geom_point() + geom_area(fill="#54e88f",alpha=0.4)+
      labs(title = "",
           x = "Year",
           y = "Avg Number of Suicides/100k Population") +
      theme_bw()
    ggplotly(sui_age, tooltip = "text") %>% layout(showlegend = F)
  })
  
  
  #PAGE2
  #Row1--------------------------------------------------------------------------------------------------------------------------------------------
  output$scatterGDP <- renderPlotly({
    scatter_gdp_plot <- 
      ggplot(data = scatter_data,
             aes(x = gdp_per_cap, y = rate,
                 frame = year, text = label_gdp)) +
      geom_point(aes(size = total_pop,
                     color = continent), alpha = 0.5) +
      scale_x_continuous(labels = dollar_format(prefix = '$')) +
      labs(x = "GDPPC",
           y = "Suicide per 100,000 population",
           title = "<b>Suicide Rates vs GDPPC Over the Years</b>") +
      guides(size = FALSE) +
      theme_minimal()
    
    ggplotly(scatter_gdp_plot, tooltip = "text") %>%
      layout(legend = list(orientation = "h",
                           xanchor = "center",
                           x = 0.5,
                           y = 1.2),
             margin = list(t = 70),
             title = list(x = 0,
                          y = 0.99)) %>% 
      animation_opts() %>% 
      animation_slider(
        currentvalue = list(prefix = "Year ")
      )
  })
  
  output$scatterHDI <- renderPlotly({
    scatter_hdi_plot <-
      ggplot(data = scatter_data %>% filter(!is.na(HDI)),
             aes(x = HDI, y = rate,
                 frame = year, text = label_hdi)) +
      geom_point(aes(size = total_pop,
                     color = continent), alpha = 0.5) +
      scale_x_continuous(labels = comma) +
      labs(x = "HDI",
           y = "Suicide per 100,000 population",
           title = "<b>Suicide Rates vs HDI Over the Years</b>") +
      guides(size = FALSE) +
      theme_minimal()
    
    ggplotly(scatter_hdi_plot, tooltip = "text") %>%
      layout(legend = list(orientation = "h",
                           xanchor = "center",
                           x = 0.5,
                           y = 1.2),
             margin = list(t = 70),
             title = list(x = 0,
                          y = 0.99)) %>% 
      animation_opts() %>% 
      animation_slider(
        currentvalue = list(prefix = "Year ")
      )
  })
  
  #Page3---------------------------------------------------------------------------------------------------------------------------------------------   
  reactive_petabundir <- reactive({
    suicide %>%
      filter(tahun == input$peta_bundir)
  })
  
  output$totalcountry <- renderValueBox({
    total_country <-  reactive_petabundir() %>%
      group_by(country)  %>%
      summarise(sui=sum(suicides_numbers)) %>% 
      group_by(country) %>% 
      summarise(freq=n()) %>% 
      count() %>% 
      pull(n)
    
    valueBox(
      value = total_country,
      subtitle = "Countries ",
      color = "red",
      icon = icon("globe"),
      width = 12
    )
    
  })
  
  output$totalnumyear <- renderValueBox({
    total_numyear <-  reactive_petabundir() %>%
      group_by(continent)  %>%
      summarise(sui=sum(suicides_numbers)) %>% 
      summarise(freq=sum(sui)) %>% 
      pull(freq)
    
    valueBox(
      value = comma(total_numyear),
      subtitle = "Number of Suicides ",
      color = "red",
      icon = icon("globe"),
      width = 12
    )
    
  })
  output$totalrateyear <- renderValueBox({
    total_rateyear <-  reactive_petabundir() %>%
      group_by(continent)  %>%
      summarise(sui=mean(suicides_rate)) %>% 
      summarise(freq=mean(sui)) %>% 
      pull(freq)
    
    valueBox(
      value = round(total_rateyear,1),
      subtitle = "Number of Suicides/100K Population ",
      color = "red",
      icon = icon("globe"),
      width = 12
    )
    
  })
  
  output$petamapsui <- renderPlotly({
    petasui <- reactive_petabundir() %>% group_by(country) %>% summarise(freq=round(sum(suicides_numbers),2)) %>% 
      mutate(country_code=countrycode(country,origin="country.name",destination="iso3c"))
    
    l <- list(color = toRGB("grey"), width = 0.5)
    g <- list(
      resolution=200,
      showcoastlines=T, coastlinecolor="RebeccaPurple",
      showland=F, landcolor="grey85",
      showocean=T, oceancolor="white",
      showlakes=T, lakecolor="Lightblue",
      showrivers=T, rivercolor="Lightblue",
      projection = list(type = 'natural earth', scale = 1)
    )
    plot_ly(petasui, z = petasui$freq , text = petasui$country ,locations = petasui$country_code,  type = 'choropleth',
            color = petasui$freq, colors = 'YlGnBu', marker = list(line = l ),colorbar = list(title = "Number of Suicides")) %>% 
      layout(geo=g)
  })
  
  output$totaldemographic <- renderPlotly({
    total_demographic <- reactive_petabundir () %>%
      group_by(sex,group_age) %>% 
      summarise(freq=sum(suicides_numbers)) %>% 
      mutate(label=glue(
        "Age Group: {group_age}
                 Number of Sucides by {sex}: {comma(freq)}
                 year: {input$peta_bundir}
          "
      )) %>% 
      ggplot(aes(x=sex,y=freq,text=label)) + geom_bar(aes(col=group_age,fill=group_age),stat="identity",width=0.5) +
      scale_y_continuous(labels=comma)+
      theme_bw() +
      labs(y="Number of Suicides",
           x="",
           fill=""
           
      )
    ggplotly(total_demographic, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0.1, y = -0.1))
  })
  
  #Page4-------------------------------------------------------------------------------------------------------------------------------------------    
  output$datasuicide <- renderDataTable({suicide})
  
  
}