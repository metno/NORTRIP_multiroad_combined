subroutine transfer_preprocessor_to_combined_datedata
    
    use NORTRIP_multiroad_index_definitions
    use NORTRIP_multiroad_combined_definitions
    
    implicit none

    integer t

	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Transfer multiroad date file (transfer_preprocessor_to_combined_datedata)'
	write(unit_logfile,'(A)') '================================================================'
            
    transfer_n_time=n_hours_input
    transfer_available_date_data=.true.
    if (.not.allocated(transfer_date_data)) allocate(transfer_date_data(transfer_num_date_index,transfer_n_time))
    
        do t=1,n_hours_input
                    
                transfer_date_data(transfer_year_index,t)=date_data(year_index,t)
                transfer_date_data(transfer_month_index,t)=date_data(month_index,t)
                transfer_date_data(transfer_day_index,t)=date_data(day_index,t)
                transfer_date_data(transfer_hour_index,t)=date_data(hour_index,t)
                transfer_date_data(transfer_minute_index,t)=date_data(minute_index,t)
   ! write(*,*) transfer_date_data(1:5,t)
        enddo
        
    
    end subroutine transfer_preprocessor_to_combined_datedata
    
    subroutine transfer_combined_to_NORTRIP_datedata
    
    use NORTRIP_definitions
    use NORTRIP_multiroad_combined_definitions
    
    implicit none

    n_time=transfer_n_time
    
    if (.not.allocated(date_data)) allocate(date_data(num_date_index,n_time))
    if (.not.allocated(date_str)) allocate(date_str(4,n_time))
    
    date_data=transfer_date_data
    available_date_data=transfer_available_date_data
    
    if (allocated(transfer_date_data)) deallocate(transfer_date_data)
    
    end subroutine transfer_combined_to_NORTRIP_datedata
