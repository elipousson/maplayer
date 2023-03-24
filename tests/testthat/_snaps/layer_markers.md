# layer_markers works

    Code
      ggplot2::summarise_layout(ggplot2::ggplot_build(plot))
    Output
      # A tibble: 1 x 10
        panel   row   col vars                  xmin     xmax   ymin   ymax xscale    
        <fct> <dbl> <dbl> <list>               <dbl>    <dbl>  <dbl>  <dbl> <list>    
      1 1         1     1 <named list [0]> -9403413.  -8.39e6 4.02e6 4.38e6 <SclCntnP>
      # ... with 1 more variable: yscale <list>

