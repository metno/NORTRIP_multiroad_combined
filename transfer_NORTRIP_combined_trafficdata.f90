subroutine transfer_preprocessor_to_combined_trafficdata
    
    use NORTRIP_multiroad_index_definitions
    use NORTRIP_multiroad_combined_definitions
    
    implicit none

    integer t,i,jj

	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Transfer multiroad traffic file (transfer_preprocessor_to_combined_trafficdata)'
	write(unit_logfile,'(A)') '================================================================'
            
    transfer_n_time=n_hours_input
    transfer_n_roads=n_save_links
    transfer_available_traffic_data=.true.
    write(*,*) transfer_n_time,transfer_n_roads
    
    if (.not.allocated(transfer_traffic_data)) allocate(transfer_traffic_data(transfer_num_traffic_index,transfer_n_time,0:transfer_n_roads))
    
    write(unit_logfile,'(35a)') 'N(total)',achar(9),'N(he)',achar(9),'N(li)' &
        ,achar(9),'N(st,he)',achar(9),'N(st,li)' &
        ,achar(9),'N(wi,he)',achar(9),'N(wi,li)' &
        ,achar(9),'N(su,he)',achar(9),'N(su,li)' &
        ,achar(9),'V_veh(he)',achar(9),'V_veh(li)'
    
    do jj=1,n_save_links
        i=save_links(jj)
        if ((inputdata_int_rl(savedata_rl_index,i).eq.1.and.use_only_special_links_flag.ge.1) &
            .or.(use_only_special_links_flag.eq.0).or.(use_only_special_links_flag.eq.2)) then
            do t=1,n_hours_input
                    
                transfer_traffic_data(transfer_N_total_index,t,i)=traffic_data(N_total_index,t,i)
                transfer_traffic_data(transfer_N_v_index(transfer_he),t,i)=traffic_data(N_v_index(he),t,i)
                transfer_traffic_data(transfer_N_v_index(transfer_li),t,i)=traffic_data(N_v_index(li),t,i)
                transfer_traffic_data(transfer_N_t_v_index(transfer_st,transfer_he),t,i)=traffic_data(N_t_v_index(st,he),t,i)
                transfer_traffic_data(transfer_N_t_v_index(transfer_st,transfer_li),t,i)=traffic_data(N_t_v_index(st,li),t,i)
                transfer_traffic_data(transfer_N_t_v_index(transfer_wi,transfer_he),t,i)=traffic_data(N_t_v_index(wi,he),t,i)
                transfer_traffic_data(transfer_N_t_v_index(transfer_wi,transfer_li),t,i)=traffic_data(N_t_v_index(wi,li),t,i)
                transfer_traffic_data(transfer_N_t_v_index(transfer_su,transfer_he),t,i)=traffic_data(N_t_v_index(su,he),t,i)
                transfer_traffic_data(transfer_N_t_v_index(transfer_su,transfer_li),t,i)=traffic_data(N_t_v_index(su,li),t,i)
                transfer_traffic_data(transfer_V_he_index,t,i)=traffic_data(V_he_index,t,i)
                transfer_traffic_data(transfer_V_li_index,t,i)=traffic_data(V_li_index,t,i)
        
                !write(*,*) jj,i,t,transfer_traffic_data(transfer_N_total_index,t,i)
            enddo
        endif
    enddo
    
            
    
    end subroutine transfer_preprocessor_to_combined_trafficdata
    
    subroutine transfer_combined_to_NORTRIP_trafficdata
    
    use NORTRIP_definitions
    use NORTRIP_multiroad_combined_definitions
    
    implicit none

    if (.not.allocated(traffic_data)) allocate(traffic_data(num_traffic_index,n_time,0:n_roads))
    
    traffic_data=transfer_traffic_data
    available_traffic_data=transfer_available_traffic_data
    
    if (allocated(transfer_traffic_data)) deallocate(transfer_traffic_data)
    
    end subroutine transfer_combined_to_NORTRIP_trafficdata