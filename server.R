
server <- function(input, output) {
  
  #TABSET 1 Figures
  #Figure 1: Map  Plot
  output$overallMapPlot <- renderPlotly({
    
    count_type <-ifelse(input$cases_v_deaths_1,"Cases","Deaths")
    
    # Code to select which values (cases vs deaths) to display in map based on user selection
    filtered_df <- jhu_state_confirmed_cases %>%
      mutate(scaled_case_count = (case_count/(max(case_count))),
             scaled_death_count = (death_count/(max(death_count)))) %>%
      mutate(selected_count = case_when(input$cases_v_deaths_1 ~case_count ,
                                        !input$cases_v_deaths_1 ~ death_count),
             selected_scaled_count = case_when(input$cases_v_deaths_1 ~scaled_case_count ,
                                               !input$cases_v_deaths_1 ~ scaled_death_count)) %>%
      # filter(long>-140) %>%
      # filter(lat>25) %>%
      filter(!str_detect(province_state,"Unassigned")) %>%
      filter(selected_count>0) %>%
      filter(date_fmt==input$end_date) %>%
      crossing(alpha=0.35) %>%
      select(province_state,long,lat,selected_scaled_count,selected_count,alpha) %>%
      bind_rows(tibble(province_state="US",
                       long=-140,
                       lat=30,
                       selected_scaled_count=0,
                       selected_count=0,
                       alpha=1))
    
    # filtered_df <- ifelse(nrow(filtered_df)==0,
    #                       tibble(long=120,
    #                              lat=30,
    #                              selected_scaled_count=3,
    #                              selected_count=0,
    #                              alpha=1,
    #                              province_state="US"),
    #                       filtered_df)
    
    
    
    # Code to control relative sizes of points    
    max_case_count = max(filtered_df$selected_count)
    max_size_point =   (max_case_count/ifelse(input$cases_v_deaths_1,
                                              300000,
                                              30000)) * 35
    min_size_point =   ifelse(input$cases_v_deaths_1,
                              1,
                              0.5)
    
    # ggplot map plot    
    map_plot_aug <- ggplot()+
      geom_point(data=tibble(long=-100,
                             lat=38,
                             selected_scaled_count=1.5),
                 aes(x=long,
                     y=lat,
                     size=selected_scaled_count))+
      geom_sf(data=states,
              fill="white")+
      geom_point(data=filtered_df,
                 mapping=aes(x=long,
                             y=lat,
                             alpha=alpha,
                             group=province_state,
                             size=selected_scaled_count,
                             text= paste0("County/State: ",province_state,
                                          "<br>",count_type,": ",selected_count)),
                 fill="red",
                 #alpha=0.35,
                 shape=21)+
      scale_size_continuous(breaks = function(x) c(10,
                                                   100,
                                                   1000)/max_case_count,
                            labels= function(x) round_half_up(x*max_case_count),
                            range=c(min_size_point,min_size_point+max_size_point))+
      scale_alpha_identity()+
      scale_x_continuous(expand=expand_scale(mult=c(0.1,0.1)))+
      scale_y_continuous(expand=expand_scale(mult=c(0.1,0.1)))+
      labs(title="Geographic Distribution",
           x="",
           y="",
           size=paste0("# of ",count_type)) +
      coord_sf(xlim=c(-122,-70),
               ylim=c(26,49))+
      theme_bw()+
      theme(legend.position = c(0.5,0.5),
            title=element_text(size=14),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank())
    
    # convert map ggplot to map plotly
    ggplotly(map_plot_aug,
             tooltip = "text") %>%
      config(displayModeBar = F) %>%
      layout(xaxis=list(fixedrange=TRUE)) %>%
      layout(yaxis=list(fixedrange=TRUE)) %>%
      layout(title = list(text = paste0("Map: Total ",count_type,
                                        '<br>',
                                        '<sup>',
                                        paste0("Date: ",format(input$end_date,format="%m/%d/%y")),
                                        '</sup>')))
  })
  
  #Figure 2: Overall Line Plot
  output$overallLinePlot <- renderPlot({
    
    quant_type <-ifelse(input$cum_v_daily_1,"Total","Daily")
    count_type <-ifelse(input$cases_v_deaths_1,"Cases","Deaths")
    
    # Code to select which value to display on overall line plot based on user selection
    filtered_df  <-jhu_global_combined %>% 
      mutate(cum_v_daily_1 = input$cum_v_daily_1,
             cases_v_deaths_1 = input$cases_v_deaths_1,
             y_total = case_when(cases_v_deaths_1 & cum_v_daily_1 ~ case_total,
                                 cases_v_deaths_1 & !cum_v_daily_1 ~ case_total_daily,
                                 !cases_v_deaths_1 & cum_v_daily_1 ~ death_total,
                                 !cases_v_deaths_1 & !cum_v_daily_1 ~ death_total_daily))
    
    # ggplot overall line plot
    plot_line_ov_time <-filtered_df %>%
      ggplot(aes(x=date_fmt,
                 y=y_total,
                 color=date_fmt_num))+
      geom_line(size=3)+
      geom_vline(xintercept=as.numeric(input$end_date),
                 size=2,
                 alpha=0.5)+
      geom_line(size=3)+
      geom_label(data=filtered_df %>%
                   filter(date_fmt == input$end_date),
                 aes(label=y_total),
                 size=7,
                 fontface="bold")+
      scale_y_continuous(
        labels = function(x) ifelse(x>=1000,
                                    paste0(x/1000,"k"),
                                    x),
        expand = expand_scale(mult=c(0.07,0.11))
      )+
      scale_x_date(date_breaks = "2 week",
                   labels= scales::date_format("%m/%d"),
                   expand = expand_scale(mult=c(0.05,0.15)),
                   limits=c(min(jhu_global_combined$date_fmt),max(jhu_global_combined$date_fmt)))+
      scale_color_gradient(low="black",high="red")+
      labs(title=paste0(quant_type," ",count_type," Over Time"),
           subtitle=paste0("Date: ",format(input$end_date,format="%m/%d/%y")),
           x="",
           y=paste0("# of ",quant_type," ",count_type)) +
      guides(color=F)+
      theme_bw()+
      theme(title = element_text(size=20),
            axis.text = element_text(face="bold",size=14),
            axis.title = element_text(face="bold",size=14),
            plot.subtitle = element_text(size=16))
    plot_line_ov_time
    
  })
  
  #TABSET 2 Figures - Total Case Count
  #Figure 1: State Line Plot
  output$stateLinePlotly <- renderPlotly({
    quant_type <-ifelse(input$cum_v_daily_2,"Total","Daily")
    count_type <-case_when(input$cases_v_deaths_2 & input$raw_v_adjusted_2 ~"Cases",
                           input$cases_v_deaths_2 & !input$raw_v_adjusted_2 ~"Cases per 100k",
                           !input$cases_v_deaths_2 & input$raw_v_adjusted_2 ~"Deaths",
                           !input$cases_v_deaths_2 & !input$raw_v_adjusted_2 ~"Deaths per 100k")
    
    # Code to select which values to present on state line plot based on user selection 
    filtered_df  <-jhu_state_sum_confirmed_cases %>% 
      filter(!is.na(state_abb)) %>%
      mutate(cum_v_daily = input$cum_v_daily_2,
             cases_v_deaths = input$cases_v_deaths_2,
             raw_v_adjusted = input$raw_v_adjusted_2,
             y_total = case_when(cases_v_deaths & cum_v_daily & raw_v_adjusted ~ case_total,
                                 cases_v_deaths & cum_v_daily & !raw_v_adjusted ~ case_total_per_100k,
                                 cases_v_deaths & !cum_v_daily & raw_v_adjusted~ case_total_daily,
                                 cases_v_deaths & !cum_v_daily &  !raw_v_adjusted~ case_total_daily_per_100k,
                                 !cases_v_deaths & cum_v_daily & raw_v_adjusted ~ death_total,
                                 !cases_v_deaths & cum_v_daily & !raw_v_adjusted ~ death_total_per_100k,
                                 !cases_v_deaths & !cum_v_daily & raw_v_adjusted ~ death_total_daily,
                                 !cases_v_deaths & !cum_v_daily & !raw_v_adjusted ~ death_total_daily_per_100k)) %>%
      filter("All States" %in% input$states_selected_2 |
               is.null(input$states_selected_2)|
               state_fullname %in% input$states_selected_2)
    
    min_max_df <-filtered_df %>%
      filter(date_fmt <= input$end_date_2) %>%
      summarise(min_value=0,
                max_value=max(c(y_total,10),na.rm=T))
    
    # build df object that allows plotly to highlight entire line for each state
    state_plotly_df <- highlight_key(filtered_df,
                                     ~state_abb)
    
    # ggplot state line plot code
    state_ov_time_line_plot <-  state_plotly_df %>%
      ggplot(aes(x=date_fmt,
                 y=y_total,
                 color=color_pal))+
      geom_vline(xintercept=as.numeric(input$end_date_2),
                 size=1,
                 alpha=0.5)+
      geom_line(size=1)+
      geom_point(aes(text=paste0("<b>State: ",state_abb,
                                 "<br>Date: ",format(date_fmt,format="%m/%d"),
                                 "<br>",count_type,": ",y_total,"</b>")))+
      scale_color_identity()+
      scale_size_continuous(range=c(4,6))+
      scale_y_continuous(label = function(x) ifelse(x>=1000,
                                                    paste0(x/1000,"k"),
                                                    x),
                         expand = expand_scale(mult=c(0.07,0.11))
                         
      )+
      scale_x_date(date_breaks = "2 week",
                   labels= scales::date_format("%m/%d"),
                   expand = expand_scale(mult=c(0,0.04)),
                   limits=c(as.Date("2020-02-24"),max(jhu_state_sum_confirmed_cases$date_fmt+2.5)))+
      labs(title=paste0(quant_type," Cases Over Time"),
           x="",
           y=paste0("# of ", quant_type," ", count_type)) +
      guides(color=F,
             size=F)+
      coord_cartesian(ylim=c(0,min_max_df$max_value))+
      theme_bw()+
      theme(axis.text =  element_text(size=12,
                                      face="bold"),
            axis.title=element_text(size=12,face="bold"),
            title=element_text(size=12),
            legend.position = "none")
    
    #convert  state line ggplot to state line plotly
    ggplotly(state_ov_time_line_plot,
             tooltip = "text") %>%
      config(displayModeBar = F) %>%
      hide_colorbar() %>%
      layout(xaxis=list(fixedrange=TRUE)) %>%
      layout(yaxis=list(fixedrange=TRUE)) %>%
      layout(title = list(text = paste0(quant_type," ",count_type," Over Time",
                                        '<br>',
                                        '<sup>',
                                        paste0("Date: ",format(input$end_date_2,format="%m/%d/%y")),
                                        '</sup>'))) %>%
      highlight(on="plotly_hover",
                off="plotly_doubleclick",
                color=NULL)
    
  })
  
  #Figure 2: State Bar Plots
  output$stateBarPlot <- renderPlot({
    quant_type <-ifelse(input$cum_v_daily_2,"Total","Daily")
    count_type <-case_when(input$cases_v_deaths_2 & input$raw_v_adjusted_2 ~"Cases",
                           input$cases_v_deaths_2 & !input$raw_v_adjusted_2 ~"Cases per 100k",
                           !input$cases_v_deaths_2 & input$raw_v_adjusted_2 ~"Deaths",
                           !input$cases_v_deaths_2 & !input$raw_v_adjusted_2 ~"Deaths per 100k")
    
    # Code to select which values to present on state bar plot based on user selection 
    state_bar_plot_df <- jhu_state_sum_confirmed_cases %>%
      filter("All States" %in% input$states_selected_2 |
               is.null(input$states_selected_2)|
               state_fullname %in% input$states_selected_2) %>%
      filter(date_fmt == input$end_date_2,
             !is.na(state_abb)) %>%
      mutate(cum_v_daily = input$cum_v_daily_2,
             cases_v_deaths = input$cases_v_deaths_2,
             raw_v_adjusted = input$raw_v_adjusted_2,
             y_total = case_when(cases_v_deaths & cum_v_daily & raw_v_adjusted ~ case_total,
                                 cases_v_deaths & cum_v_daily & !raw_v_adjusted ~ case_total_per_100k,
                                 cases_v_deaths & !cum_v_daily & raw_v_adjusted~ case_total_daily,
                                 cases_v_deaths & !cum_v_daily &  !raw_v_adjusted~ case_total_daily_per_100k,
                                 !cases_v_deaths & cum_v_daily & raw_v_adjusted ~ death_total,
                                 !cases_v_deaths & cum_v_daily & !raw_v_adjusted ~ death_total_per_100k,
                                 !cases_v_deaths & !cum_v_daily & raw_v_adjusted ~ death_total_daily,
                                 !cases_v_deaths & !cum_v_daily & !raw_v_adjusted ~ death_total_daily_per_100k)) %>%
      arrange(desc(y_total),state_abb) %>%
      slice(1:20) %>% 
      mutate(state_abb_x=factor(state_abb,levels=unique(state_abb)))
    
    bar_subtitle <- ifelse("All States" %in% input$states_selected_2 |
                             is.null(input$states_selected_2),
                           "Top 20 States on ",
                           "Selected States on ")
    
    # ggplot bar plot   
    state_bar_plot <- state_bar_plot_df %>%
      ggplot(aes(x=state_abb_x,
                 y=y_total,
                 fill=color_pal))+
      geom_bar(stat="identity")+
      geom_text(aes(y=y_total+(max(y_total)*0.005),
                    label=ifelse(y_total>=1e5,
                                 paste0(formatC(y_total/1000,digits=1,format="f"),"k"),
                                 y_total)),
                hjust="left",
                size=4.5)+
      scale_fill_identity()+
      scale_y_continuous(labels=function(x) ifelse(x>=1000,
                                                   paste0(x/1000,"k"),
                                                   x),
                         expand=expand_scale(mult=c(0,0.12))
      )+
      scale_x_discrete(limits=rev(levels(state_bar_plot_df$state_abb_x)))+
      labs(title=paste0(quant_type," ",count_type),
           subtitle=paste0(bar_subtitle,format(input$end_date_2,format="%m/%d/%y")),
           x="",
           y=paste0("# of ",quant_type," ",count_type)) +
      guides(color=F,
             fill=F,
             size=F)+
      coord_flip()+
      theme_bw()+
      theme(axis.text.x =  element_text(size=20,face="bold"),
            axis.text.y =  element_text(size=16,
                                        face="bold"),
            title=element_text(size=20),
            plot.subtitle = element_text(size=16))
    
    state_bar_plot
  })
  
  
  
  # TABSET #3 Figures
  
  output$stateTestingLinePlotly <- renderPlotly({
    
    # Code to select which values to present on state line plot based on user selection
    quant_type <-ifelse(input$cum_v_daily_3,"Total","Daily")
    count_type <- case_when(input$raw_v_adjusted_3 ~ "Tests",
                            !input$raw_v_adjusted_3 ~ "Tests per 100k")
    
    filtered_df <- covid_tracking_states_w_pop %>%
      filter(!is.na(state_abb)) %>%
      mutate(cum_v_daily = input$cum_v_daily_3,
             raw_v_adjusted = input$raw_v_adjusted_3,
             y_total = case_when(cum_v_daily & raw_v_adjusted ~ test_total,
                                 cum_v_daily & !raw_v_adjusted ~ test_total_per_100k,
                                 !cum_v_daily & raw_v_adjusted ~ test_total_daily,
                                 !cum_v_daily & !raw_v_adjusted ~ test_total_daily_per_100k)) %>%
      filter("All States" %in% input$states_selected_3 |
               is.null(input$states_selected_3)|
               state_fullname %in% input$states_selected_3)
    
    min_max_df <-filtered_df %>%
      filter(date_fmt <= input$end_date_3) %>%
      summarise(min_value=0,
                max_value=max(c(y_total,10),na.rm=T))
    
    # build df object that allows plotly to highlight entire line for each state
    covid_tracking_states_w_pop_plotly_df <- highlight_key(filtered_df ,~state_abb)
    
    # ggplot state line plot code
    p3_1 <- covid_tracking_states_w_pop_plotly_df %>%
      ggplot(aes(x=date_fmt,group=state_abb,color=color_pal,y=y_total))+
      geom_line(size=1)+
      geom_vline(xintercept=as.numeric(input$end_date_3),
                 size=1,
                 alpha=0.5)+
      geom_point(aes(text=paste0("<b>State: ",state_abb,
                                 "<br>Date: ",format(date_fmt,format="%m/%d"),
                                 "<br>",count_type,": ",y_total,
                                 "<br>Grade: ", grade,"</b>")))+
      scale_color_identity()+
      scale_x_date(breaks="1 week",
                   labels= scales::date_format("%m/%d"),
                   limits = c(min(covid_tracking_states_w_pop$date_fmt),
                              max(covid_tracking_states_w_pop$date_fmt)))+
      scale_y_continuous(
        labels=function(x)ifelse(x>=1000,
                                 paste0(x/1000,"k"),
                                 x)
      )+
      labs(title=paste0(quant_type," Over Time"),
           x="",
           y=paste0("# of ",quant_type," ",count_type))+
      guides(color=F)+
      coord_cartesian(#xlim=c(min_max_df$min_date,min_max_df$max_date),
        ylim=c(0,min_max_df$max_value))+
      theme_bw()+
      theme(legend.position = "none")
    
    #convert state line ggplot to state line plotly
    p3_1 %>%  
      ggplotly(tooltip = "text") %>%
      config(displayModeBar = F) %>%
      hide_colorbar() %>%
      layout(xaxis=list(fixedrange=TRUE)) %>%
      layout(yaxis=list(fixedrange=TRUE)) %>%
      layout(title = list(text = paste0(quant_type," ",count_type," Over Time",
                                        '<br>',
                                        '<sup>',
                                        paste0("By State"),
                                        '</sup>'
      ))) %>%
      highlight(on="plotly_hover",
                off="plotly_doubleclick",
                color=NULL)
    
  })  
  
  output$stateTestingPositiveVTotalScatter <- renderPlotly({
    
    # build df object that allows plotly to highlight entire line for each state 
    covid_tracking_states_w_pop_plotly_df <- highlight_key(covid_tracking_states_w_pop %>%
                                                             filter(!is.na(state_abb)) %>%
                                                             filter(date_fmt == input$end_date_3) %>%
                                                             filter(total!=0,
                                                                    positive!=0) %>%
                                                             filter("All States" %in% input$states_selected_3 |
                                                                      is.null(input$states_selected_3)|
                                                                      state_fullname %in% input$states_selected_3),
                                                           ~state_abb)
    # ggplot scatter plot
    p3_2 <- covid_tracking_states_w_pop_plotly_df %>%
      ggplot(aes(x=total,y=positive,color=color_pal)) +
      geom_point(aes(text=paste0("<b>State: ",state_abb,
                                 "<br>Positive:<br>", 
                                 pct_positive,"%",
                                 " (",positive,"/", total,")",
                                 "<br>Grade: ", grade,"</b>")),
                 size=3)+
      labs(title="% Positive vs. Total Tests (log-log)",
           x = "Total Tests",
           y = "Positive Cases") +
      scale_color_identity()+
      scale_y_log10(limits = c(10,max(covid_tracking_states_w_pop$positive,na.rm=T)),
                    labels = function(x) ifelse(x>=1000,
                                                paste0(x/1000,"k"),
                                                x))+
      scale_x_log10(limits = c(10,max(covid_tracking_states_w_pop$total)),
                    labels = function(x) ifelse(x>=1000,
                                                paste0(x/1000,"k"),
                                                x))+
      theme_bw()+
      theme(legend.position = "none")
    
    # convert scatterplot ggplot to plotly
    p3_2 %>% 
      ggplotly(tooltip = "text") %>%
      config(displayModeBar = F) %>%
      hide_colorbar() %>%
      layout(xaxis=list(fixedrange=TRUE)) %>%
      layout(yaxis=list(fixedrange=TRUE)) %>%
      layout(title = list(text = paste0("# Positive vs. Total Tests (log-log)",
                                        '<br>',
                                        '<sup>',
                                        paste0("Date: ",input$end_date_3),
                                        '</sup>'))) %>%
      highlight(on="plotly_hover",
                off="plotly_doubleclick",
                color=NULL)
    
  })
  
  output$stateTestingPctPositiveVTotalScatter <- renderPlotly({
    # build df object that allows plotly to highlight entire line for each state
    covid_tracking_states_w_pop_plotly_df <- highlight_key(covid_tracking_states_w_pop %>%
                                                             filter(!is.na(state_abb)) %>%
                                                             filter(date_fmt == input$end_date_3) %>%
                                                             filter(total!=0,
                                                                    positive!=0) %>%
                                                             filter("All States" %in% input$states_selected_3 |
                                                                      is.null(input$states_selected_3)|
                                                                      state_fullname %in% input$states_selected_3),~state_abb)
    # ggplot scatter plot
    p3_3 <- covid_tracking_states_w_pop_plotly_df %>%
      ggplot(aes(x=total,y=pct_positive,color=color_pal)) +
      geom_point(aes(text=paste0("<b>State: ",state_abb,
                                 "<br>Positive:<br>", 
                                 pct_positive,"%",
                                 " (",positive,"/", total,")",
                                 "<br>Grade: ", grade,"</b>")),
                 size=3)+
      scale_color_identity()+
      scale_y_continuous(breaks=seq(0,100,10),
                         limits=c(0,max(covid_tracking_states_w_pop$pct_positive)))+
      scale_x_continuous(limits = c(10,max(covid_tracking_states_w_pop$total)),
                         labels = function(x) ifelse(x>=1000,
                                                     paste0(x/1000,"k"),
                                                     x))+
      labs(title="% Positive Tests vs. Total Tests",
           x = "Total Tests",
           y = "% Positive Tests") +
      theme_bw()+
      theme(legend.position = "none")
    
    # convert scatterplot ggplot to plotly
    p3_3 %>% 
      ggplotly(tooltip = "text") %>%
      config(displayModeBar = F) %>%
      hide_colorbar() %>%
      layout(xaxis=list(fixedrange=TRUE)) %>%
      layout(yaxis=list(fixedrange=TRUE)) %>%
      layout(title = list(text = paste0("% Positive vs. Total Tests",
                                        '<br>',
                                        '<sup>',
                                        paste0("Date: ",input$end_date_3),
                                        '</sup>'))) %>%
      highlight(on="plotly_hover",
                off="plotly_doubleclick",
                color=NULL)
    
  })
}

