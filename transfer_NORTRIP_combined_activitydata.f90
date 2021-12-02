subroutine transfer_preprocessor_to_combined_activitydata
    
    use NORTRIP_multiroad_index_definitions
    use NORTRIP_multiroad_combined_definitions
    
    implicit none


	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Transfer multiroad activity file (transfer_preprocessor_to_combined_activitydata)'
	write(unit_logfile,'(A)') '================================================================'
    
    transfer_n_roads=n_save_links
    transfer_n_time=n_hours_input

    !transfer_available_activity_input_data=.true.
    
    if (.not.allocated(transfer_activity_input_data)) allocate(transfer_activity_input_data(num_activity_input_index,transfer_n_time,0:transfer_n_roads))
    
    transfer_available_activity_data=multi_available_activity_data
    
    !This routine places non-chronological activity data into the transfer_activity_data array if there is available data (testing only for date)
    !Sets availability to true and data to 0 if not available
    if (multi_available_activity_data(year_index)) then
        call process_multi_NORTRIP_activity_inputdata
    else
        transfer_activity_input_data=0
        transfer_available_activity_data=.false.
    endif
    
    end subroutine transfer_preprocessor_to_combined_activitydata
    
    subroutine transfer_combined_to_NORTRIP_activitydata
    
    use NORTRIP_definitions
    use NORTRIP_multiroad_combined_definitions
    
    implicit none
    
    if (.not.allocated(activity_input_data)) allocate(activity_input_data(num_activity_input_index,n_time,0:n_roads))
    if (.not.allocated(activity_data)) allocate(activity_data(num_activity_input_index,n_time,0:n_roads))
    
    activity_input_data=transfer_activity_input_data

    !Set this to 0 as it is not used in this form but filled during calculation
    activity_data=0
    available_activity_data=transfer_available_activity_data
    
    if (allocated(transfer_activity_input_data)) deallocate(transfer_activity_input_data)
    
    end subroutine transfer_combined_to_NORTRIP_activitydata


    !----------------------------------------------------------------------
    subroutine process_multi_NORTRIP_activity_inputdata

    use NORTRIP_multiroad_index_definitions
    use NORTRIP_multiroad_combined_definitions
    
    implicit none

    integer i_road
    integer n_input_activity,n_output_activity
    integer match_count
    logical match_found
    integer i,j
    integer, allocatable :: match_link(:),match_time(:)
    
    !Open log file
    !if (unit_logfile.gt.0) then
    !    open(unit_logfile,file=filename_log,status='old',position='append')
    !endif

 	write(unit_logfile,'(A)') '================================================================'
 	write(unit_logfile,'(A)') 'Processing activity data'
   
    transfer_n_roads=n_save_links
    if (allocated(multi_activity_input_data)) then
        n_input_activity=size(multi_activity_input_data,2)
    else
        n_input_activity=0
    endif
    
    n_output_activity=size(transfer_activity_input_data,2)
    
 	write(unit_logfile,'(A)') '----------------------------------------------------------------'
    if (.not.allocated(match_link)) allocate(match_link(transfer_n_roads))
    if (.not.allocated(match_time)) allocate(match_time(n_output_activity))
    match_link=0
    match_time=0
    
    transfer_activity_input_data=nodata_activity
    match_count=0
    
        !First set to 0 any road that is mentioned at all, so it is no longer nodata_activity
        do i_road=1,transfer_n_roads
        do j=1,n_input_activity

            if (inputdata_int_rl(id_rl_index,save_links(i_road)).eq.int(multi_activity_input_data(activity_roadID_index,j))) then
                
                !If a road is matched at all then it is assumed to have data all the time. Will be written over in the next time loop
                transfer_activity_input_data(:,:,i_road)=0.
                
                match_link(i_road)=1

            endif
        enddo
        enddo
    
        write(unit_logfile,'(A,5i)') 'Number of activity road links matched without valid dates: ',sum(match_link)

        match_link=0

        !Now put in data if a date is found
        do i_road=1,transfer_n_roads
        do j=1,n_input_activity
            match_found=.false.

            if (inputdata_int_rl(id_rl_index,save_links(i_road)).eq.int(multi_activity_input_data(activity_roadID_index,j))) then
                
            do i=1,n_output_activity
                !write(*,*)j,i,inputdata_int_rl(id_rl_index,save_links(i_road)),int(multi_activity_input_data(activity_roadID_index,j))
                !Matches date and road ID. If no road ID has been read (nodata) then apply to all roads for that date
                if (date_data(year_index,i).eq.int(multi_activity_input_data(activity_year_index,j)).and. &
                    date_data(month_index,i).eq.int(multi_activity_input_data(activity_month_index,j)).and. &
                    date_data(day_index,i).eq.int(multi_activity_input_data(activity_day_index,j)).and. &
                    date_data(hour_index,i).eq.int(multi_activity_input_data(activity_hour_index,j)).and. &
                    date_data(minute_index,i).eq.int(multi_activity_input_data(activity_minute_index,j))) then
                    !date_data(minute_index,i).eq.int(multi_activity_input_data(activity_minute_index,j)).and. &
                    !inputdata_int_rl(id_rl_index,save_links(i_road).eq.int(multi_activity_input_data(activity_roadID_index,j)))) then
                
                    !There should be one unique date and road link value. If none is found then it will retain the nodata_activity value
                    transfer_activity_input_data(:,i,i_road)=multi_activity_input_data(:,j)
                    match_count=match_count+1
                    match_found=.true.
                    match_link(i_road)=1
                    match_time(i)=1
                            !if (multi_activity_input_data(activity_hour_index,j).eq.1.and.date_data(hour_index,i).eq.1.and.inputdata_int_rl(id_rl_index,save_links(i_road)).eq.699005) then
                            !    write(*,*) 'First hour transferred: ',sum(multi_activity_input_data(:,j)),inputdata_int_rl(id_rl_index,save_links(i_road))
                            !    write(*,*) multi_activity_input_data(activity_hour_index,j),j,date_data(hour_index,i),i
                            !endif
                            
                endif             
            
            enddo
            endif
            if (match_found) then
                !write(unit_logfile,'(A,5i12)') 'Match for : ',int(multi_activity_input_data(activity_year_index:activity_hour_index,j)),int(multi_activity_input_data(activity_roadID_index,j))
                !write(*,*) transfer_activity_input_data(:,:,i_road)
            endif
        enddo
        enddo
    
        write(unit_logfile,'(A,5i)') 'Number of activity dates available : ',n_input_activity
        write(unit_logfile,'(A,5i)') 'Number of activity matched: ',match_count
        write(unit_logfile,'(A,5i)') 'Number of activity road links matched: ',sum(match_link)
        write(unit_logfile,'(A,5i)') 'Number of activity times matched: ',sum(match_time)
    
    !Close log file
    !if (unit_logfile.gt.0) then
    !    close(unit_logfile,status='keep')
    !endif

    end subroutine process_multi_NORTRIP_activity_inputdata
!----------------------------------------------------------------------