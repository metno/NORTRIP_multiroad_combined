
    !This program combines the multiroad preprocessor and NORTRIP fortran codes
    !Data produced in the preprocessor is written to intermediate arrays to avoid
    !Writing and reading large files
    !cd D:\uEMEP\NORTRIP_Norway>
    !D:\uEMEP\NORTRIP_multiroad_combined\Fortran\binary\NORTRIP_multiroad_combined_v1.exe .\config\NORTRIP_multiroad_config_Norway.txt "2018,04,12,01" "2018,04,14,00"
    
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

    !Make a routine, like the ones above, for this but for now simply set all activity to 0 and say it is available
    if (.not.allocated(activity_data)) allocate(activity_data(num_activity_index,n_time,0:n_roads))
    activity_data=0.
    available_activity_data=.true.
    
    endif
    
    call NORTRIP_fortran_control_v2
    
    end program NORTRIP_multiroad_combined_program
    
    !----------------------------------------------------------------------
    !Stop error index in NORTRIP
    !----------------------------------------------------------------------
    !1 Stop because init file does not have the same number of roads as the current running model
    !2 Stop because init file IO fails, probably because there is an error in the file
    !3 Stop because init file contains NaNs
    !4 ERROR reading emission week dynamic file
    !5 NORTRIP_read_main_inputs ERROR: Number of hours is 0 or less
    !6 read_NORTRIP_multiroad_pathnames ERROR: "pathfilename_mainfile" does not exist
    !7 NORTRIP_multiroad_read_receptor_data ERROR reading road receptor link file:
    !8 ERROR: Meteo netcdf file still does not exist
    !9 ERROR: Meteo netcdf file still does not exist
    !10 ERROR reading EF traffic file
    !11 ERROR: Static road link file 1 does not exist
    !12 ERROR: Static road link file 2 does not exist:
    !13 ERROR reading road link file
    !14 ERROR: Too many links found in traffic file (NORTRIP_multiroad_save_metadata)
    !15 ERROR: Number of marked links not the same as existing links (NORTRIP_multiroad_save_metadata):
    !16 ERROR: Static road link file ascii does not exist:
    !17 ERROR reading road link file:
    !18 ERROR: Dynamic traffic data file does not exist
    !19 ERROR reading road week dynamic traffic file
    !20 ERROR: Path for airquality file does not exist
    !21 ERROR: Info path does not exist
    !22 ERROR: Initialdata path does not exist:
    !23 ERROR: Path to metadata file does not exist:
    !24 ERROR: meteo_data_type not properly defined
    !25 ERROR: Input time start or stop date not found in meteo data
    !26 ERROR: Path for saving meteo data does not exist
    !27 ' ERROR: Path does not exist: ', trim(pathname_traffic)
    !28 ' ERROR: Path does not exist: ', trim(pathname_traffic)
    !29 ERROR reading main Epiode file:
    !30 ERROR: Meteo netcdf file does not exist: (NORTRIP_read_t2m500yr_netcdf v2)
    !31 'ERROR: File '//trim(temp_name)//' does not exist.' Metadata reading
    !32 ERROR: File '//trim(temp_name)//' does not exist.' Metadata reading
    !33 'ERROR: File '//trim(temp_name)//' does not exist.' Initial zip
    !34 'ERROR: File '//trim(temp_name_zip)//' does not exist.' airquality zip
    !35 ERROR: Number of dates in meteo input file (',n_date,') not the same as in traffic input file (',n_time,').
    !36 ERROR: Number of dates in airquality input file (',n_date,') not the same as in traffic input file (',n_time,').
    !37 ERROR: Main NORTRIP input file does not exist.
    !38 ERROR reading meteo file
    
    
    
