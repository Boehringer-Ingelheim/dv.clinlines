# create_main_plot() returns a plot that displays events per subject and respresents milestones as bullets and periods as horizontal lines__spec_ids{plot_specs$events;plot_specs$event_types}

    $plot
    
    $height
    [1] 50
    
    $filter_event
    [1] "Treatment Start" "Adverse Events" 
    
    $time_range
    [1] "2012-08-01 00:00:00 UTC" "2014-01-01 01:00:00 UTC"
    

# create_main_plot() returns a plot that scales the x-axis either as date or as numeric__spec_ids{sidebar_specs$date_day}

    $plot
    
    $height
    [1] 50
    
    $filter_event
    [1] "Treatment Start" "Adverse Events" 
    
    $time_range
    [1] "2012-08-01 00:00:00 UTC" "2015-01-01 01:00:00 UTC"
    

---

    $plot
    
    $height
    [1] 50
    
    $filter_event
    [1] "Treatment Start" "Adverse Events" 
    
    $time_range
    [1]  0 50
    

# create_main_plot() returns a plot that sorts the y-axis either alphanumerically or by earliest event__spec_ids{sidebar_specs$sorting}

    $plot
    
    $height
    [1] 50
    
    $filter_event
    [1] "Treatment Start" "Adverse Events" 
    
    $time_range
    [1] "2012-08-01 00:00:00 UTC" "2015-01-01 01:00:00 UTC"
    

---

    $plot
    
    $height
    [1] 50
    
    $filter_event
    [1] "Treatment Start" "Adverse Events" 
    
    $time_range
    [1] "2012-08-01 00:00:00 UTC" "2015-01-01 01:00:00 UTC"
    

# create_main_plot() returns a plot that determines vertical space per subject according to user settings__spec_ids{sidebar_specs$boxheight}

    $plot
    
    $height
    [1] 30
    
    $filter_event
    [1] "Treatment Start" "Adverse Events" 
    
    $time_range
    [1] "2012-08-15 00:00:00 UTC" "2015-01-01 01:00:00 UTC"
    

---

    $plot
    
    $height
    [1] 130
    
    $filter_event
    [1] "Treatment Start" "Adverse Events" 
    
    $time_range
    [1] "2012-08-15 00:00:00 UTC" "2015-01-01 01:00:00 UTC"
    

# create_main_plot() returns a plot that indicates dose changes visually__spec_ids{plot_specs$drug_admin_event}

    $plot
    
    $height
    [1] 50
    
    $filter_event
    [1] "Treatment Start"     "Drug Administration"
    
    $time_range
    [1] "2012-08-15 00:00:00 UTC" "2015-01-01 01:00:00 UTC"
    

