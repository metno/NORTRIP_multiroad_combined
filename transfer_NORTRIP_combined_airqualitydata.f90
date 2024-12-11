subroutine transfer_preprocessor_to_combined_airqualitydata
    
    use NORTRIP_multiroad_index_definitions
    use NORTRIP_multiroad_combined_definitions
    
    implicit none

    integer t,i,jj
    real conversion

	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Transfer multiroad airquality data (transfer_preprocessor_to_combined_airqualitydata)'
	write(unit_logfile,'(A)') '================================================================'
            
    transfer_n_time=n_hours_input
    transfer_n_roads=n_save_links
    
    if (.not.allocated(transfer_airquality_data)) allocate(transfer_airquality_data(transfer_num_airquality_index,transfer_n_time,0:transfer_n_roads))
    
    !Write header
    write(unit_logfile,'(19a)') 'Road_number',achar(9),'Time_index',achar(9) &
        ,'Year',achar(9),'Month',achar(9),'Day',achar(9),'Hour',achar(9),'Minute',achar(9) &
        ,'EP_emis',achar(9),'NOX_emis',achar(9),'Disp_fac'
    
    !To be set as part of the input data
    !start_dim_nc(time_index)=6
    !end_dim_nc(time_index)=30

    !Conversion of g/s/m to g/km/hr
    if (index(calculation_type,'episode')) then
        conversion=1000.*3600.
    elseif (index(calculation_type,'road weather').gt.0.or.index(calculation_type,'uEMEP').gt.0.or.index(calculation_type,'Avinor').gt.0.or.index(calculation_type,'gridded').gt.0) then
        !Does not read in dynamic data
        conversion=1.
    else
        write(unit_logfile,'(a)') ' No valid calculation type, will not save any exhaust emissions'
        conversion=0.
    endif
    
    do jj=1,n_save_links
        i=save_links(jj)
        if ((inputdata_int_rl(savedata_rl_index,i).eq.1.and.use_only_special_links_flag.ge.1) &
            .or.(use_only_special_links_flag.eq.0).or.(use_only_special_links_flag.eq.2)) then
        
            do t=1,n_hours_input
                
                transfer_airquality_data(:,t,jj)=transfer_nodata      
                transfer_airquality_data(transfer_EP_emis_index,t,jj)=airquality_data(EP_emis_index,t,i)*conversion
                transfer_airquality_data(transfer_NOX_emis_index,t,jj)=airquality_data(NOX_emis_index,t,i)*conversion
                transfer_airquality_data(transfer_f_conc_index,t,jj)=airquality_data(f_conc_index,t,i)        

            enddo
        endif
    enddo
   
    transfer_available_airquality_data(transfer_EP_emis_index)=.true.
    transfer_available_airquality_data(transfer_NOX_emis_index)=.true.
    transfer_available_airquality_data(transfer_f_conc_index)=.true.
    
    end subroutine transfer_preprocessor_to_combined_airqualitydata
    
    subroutine transfer_combined_to_NORTRIP_airqualitydata
    
    use NORTRIP_definitions
    use NORTRIP_multiroad_combined_definitions
    
    implicit none

    if (.not.allocated(airquality_data)) allocate(airquality_data(num_airquality_index,n_time,0:n_roads))

    airquality_data=transfer_airquality_data
    available_airquality_data=transfer_available_airquality_data
    
    if (allocated(transfer_airquality_data)) deallocate(transfer_airquality_data)
    
    end subroutine transfer_combined_to_NORTRIP_airqualitydata