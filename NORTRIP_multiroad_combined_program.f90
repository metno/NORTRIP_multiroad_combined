
    !This program combines the multiroad preprocessor and NORTRIP fortran codes
    !Data produced in the preprocessor is written to intermediate arrays to avoid
    !Writing and reading large files
    
    program NORTRIP_multiroad_combined_program
    
    use NORTRIP_definitions
    use NORTRIP_multiroad_index_definitions
    use NORTRIP_multiroad_combined_definitions
    
    implicit none
    
    Write(*,*) 'Starting NORTRIP_multiroad_combined_program'

    NORTRIP_multiroad_combined_flag=.true.
    NORTRIP_preprocessor_combined_flag=NORTRIP_multiroad_combined_flag
    NORTRIP_fortran_combined_flag=NORTRIP_multiroad_combined_flag
 
    Write(*,*) 'Starting NORTRIP_multiroad_combined_program'
  
    call NORTRIP_multiroad_control_64bit
    
    if (NORTRIP_multiroad_combined_flag) then
        
    !Transfer data
    !----------------------------------------------------------------------
    !Transfer the name of the info file provided in the command line. Even though not used
    commandline_filename=pathfilename_info
    
    !Setting to blank means it runs for the input data period
    start_date_save_str=''
    end_date_save_str=''
    
    !Transfer pathname data and open log file for NORTRIP
    call transfer_preprocessor_to_combined_pathnames
    call transfer_combined_to_NORTRIP_pathnames
        
    !Transfer metadata
    call transfer_preprocessor_to_combined_metadata
    call transfer_combined_to_NORTRIP_metadata
    
    !Transfer init (simple initialisation)
    call transfer_preprocessor_to_combined_initialdata
    call transfer_combined_to_NORTRIP_initialdata
    
    call transfer_preprocessor_to_combined_datedata
    call transfer_combined_to_NORTRIP_datedata

    call transfer_preprocessor_to_combined_trafficdata
    call transfer_combined_to_NORTRIP_trafficdata
    
    call transfer_preprocessor_to_combined_meteodata
    call transfer_combined_to_NORTRIP_meteodata

    call transfer_preprocessor_to_combined_airqualitydata
    call transfer_combined_to_NORTRIP_airqualitydata

    !Make a routine, like the ones above, for this
    if (.not.allocated(activity_data)) allocate(activity_data(num_activity_index,n_time,0:n_roads))
    activity_data=0.
    
    endif
    
    call NORTRIP_fortran_control_v2
    
    end program NORTRIP_multiroad_combined_program
    
