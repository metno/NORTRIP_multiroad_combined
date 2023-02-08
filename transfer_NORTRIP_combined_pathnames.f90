
    subroutine transfer_preprocessor_to_combined_pathnames
    
    use NORTRIP_multiroad_index_definitions
    use NORTRIP_multiroad_combined_definitions
    
    implicit none       
    
    !Initialise some values as in NORTRIP_multiroad_save_info_file
    path_inputdata=path_inputdata_for_NORTRIP
    path_outputfig='Not_used'
    path_ospm='Not_used'
    path_fortran='Not_used'
    filename_inputdata=filename_NORTRIP_data
    filename_outputdata=filename_NORTRIP_template
    path_init=pathname_init_in
    path_init_out=pathname_init_out
    filename_init=filename_init_in
    filename_output_roadmeteo=trim(filename_NORTRIP_template)//'_roadmeteo.txt'
    
    !Open the log file when it is transferred back
    
    !Transfoer the file and path names
    transfer_filename_log=filename_log_NORTRIP
    transfer_path_inputparam=path_inputparam
    transfer_filename_inputparam=filename_inputparam
    transfer_path_inputdata=path_inputdata
    transfer_filename_inputdata=filename_inputdata
    transfer_path_outputdata=path_outputdata
    transfer_filename_outputdata=filename_outputdata

    transfer_path_init=path_init
    transfer_path_init_out=path_init_out
    transfer_filename_init=filename_init
    transfer_path_output_emis=path_output_emis
    transfer_filename_output_emis=filename_output_emis
    transfer_filename_output_grid_emis=filename_output_grid_emis
    transfer_path_output_roadmeteo=path_output_roadmeteo
    transfer_filename_output_roadmeteo=filename_output_roadmeteo
 
    transfer_path_outputfig=path_outputfig
    transfer_path_ospm=path_ospm
    transfer_path_fortran=path_fortran
    transfer_path_fortran_output=path_fortran_output

    transfer_finished_file_append=multi_finished_file_append
    
    end subroutine transfer_preprocessor_to_combined_pathnames
    
    subroutine transfer_combined_to_NORTRIP_pathnames
    
    use NORTRIP_definitions
    use NORTRIP_multiroad_combined_definitions
    
    implicit none       
    
    
    !Open the log file when it is transferred back
    !Read log file name
    filename_log=transfer_filename_log
    
    !If no log file then write to screen
    if (filename_log.eq.'') then
        unit_logfile=0
    endif

    if (unit_logfile.gt.0) then
        write(*,'(A)') 'Writing to log file' 
  	    write(*,'(A)') '================================================================'
    endif
   
    !Open log file for the first time
    call open_logfile
   
	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Transfering model path and file names to NORTRIP (transfer_combined_to_NORTRIP_pathnames)'
	write(unit_logfile,'(A)') '================================================================' 

    
    !Transfer the file and path names
    path_inputparam=transfer_path_inputparam
    filename_inputparam=transfer_filename_inputparam
    path_inputdata=transfer_path_inputdata
    filename_inputdata=transfer_filename_inputdata
    path_outputdata=transfer_path_outputdata
    filename_outputdata=transfer_filename_outputdata

    path_init=transfer_path_init
    path_init_out=transfer_path_init_out
    filename_init=transfer_filename_init
    path_output_emis=transfer_path_output_emis
    filename_output_emis=transfer_filename_output_emis
    filename_output_grid_emis=transfer_filename_output_grid_emis
    path_output_roadmeteo=transfer_path_output_roadmeteo
    filename_output_roadmeteo=transfer_filename_output_roadmeteo
 
    path_outputfig=transfer_path_outputfig
    path_ospm=transfer_path_ospm
    path_fortran=transfer_path_fortran
    path_fortran_output=transfer_path_fortran_output

    finished_file_append=transfer_finished_file_append

    end subroutine transfer_combined_to_NORTRIP_pathnames