subroutine transfer_preprocessor_to_combined_initialdata
    
    use NORTRIP_multiroad_index_definitions
    use NORTRIP_multiroad_combined_definitions
    
    implicit none
    
    integer i,t,jj
    integer unit_in
    integer exists
    
    real, allocatable :: M2_dust_road(:),M2_sand_road(:),M2_salt_road_1(:),M2_salt_road_2(:)
    real, allocatable ::  water_road(:),snow_road(:),ice_road(:)
    !real, allocatable ::  P2_fugitive(:)
    real adt_factor,speed_factor
    real month_scale(12)
    data month_scale /0.7, 0.9, 1.0, 0.8, 0.5, 0.2, 0.1, 0.05, 0.03, 0.02, 0.1, 0.4/
    
    allocate (M2_dust_road(n_roadlinks))
    allocate (M2_sand_road(n_roadlinks))
    allocate (M2_salt_road_1(n_roadlinks))
    allocate (M2_salt_road_2(n_roadlinks))
    allocate (water_road(n_roadlinks))
    allocate (snow_road(n_roadlinks))
    allocate (ice_road(n_roadlinks))

    allocate (transfer_M2_dust_road(n_roadlinks))
    allocate (transfer_M2_sand_road(n_roadlinks))
    allocate (transfer_M2_salt_road_1(n_roadlinks))
    allocate (transfer_M2_salt_road_2(n_roadlinks))
    allocate (transfer_water_road(n_roadlinks))
    allocate (transfer_snow_road(n_roadlinks))
    allocate (transfer_ice_road(n_roadlinks))

    M2_dust_road=5.00  !        	(g/m2)  
    M2_sand_road=0.00!        	(g/m2)      
    M2_salt_road_1=0.00 !       	(g/m2)      
    M2_salt_road_2=0.00!        	(g/m2)      
    water_road=0.05!        	(mm)        
    snow_road=0.00!        	(mm.w.e)    
    ice_road=0.00!        	(mm.w.e)    
    
    !Calculate road dust according to the monthly distribution for each road. Assumes the speed is the same throughout    
    do jj=1,n_save_links
        i=save_links(jj)
        speed_factor=max(1.+(50.-traffic_data(V_li_index,1,i))/50.,0.2)
        adt_factor=min(inputdata_rl(adt_rl_index,i)/inputdata_int_rl(nlanes_rl_index,i)/20000.*4.,5.)
        M2_dust_road(i)=200.*month_scale(date_data(month_index,1))*max_stud_fraction(li)/100.*adt_factor*speed_factor
        if (inputdata_int_rl(roadstructuretype_rl_index,i).eq.tunnel_roadtype) then
            M2_dust_road(i)=0.
        endif
    enddo
    
    
    write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Transfering initial model values for startup (NORTRIP_multiroad_save_initialdata)'
	write(unit_logfile,'(A)') '================================================================'
    write(unit_logfile,'(a,2f12.1)') ' Maximum and minimum initial dust layer',maxval(M2_dust_road),minval(M2_dust_road)

    !write(unit_in,'(a32,a,<n_save_links>f12.4)') 'Road index',achar(9),inputdata_int_rl(roadindex_rl_index,save_links(1:n_save_links))
    do i=1,n_save_links
        transfer_M2_dust_road(i)=M2_dust_road(save_links(i))
        transfer_M2_sand_road(i)=M2_sand_road(save_links(i))
        transfer_M2_salt_road_1(i)=M2_salt_road_1(save_links(i))
        transfer_M2_salt_road_2(i)=M2_salt_road_2(save_links(i))
        transfer_water_road(i)=water_road(save_links(i))
        transfer_snow_road(i)=snow_road(save_links(i))
        transfer_ice_road(i)=ice_road(save_links(i))
    enddo
    transfer_long_rad_in_offset=long_rad_in_offset
    transfer_RH_offset=RH_offset
    transfer_T_a_offset=T_a_offset
    
    
    deallocate (M2_dust_road)
    deallocate (M2_sand_road)
    deallocate (M2_salt_road_1)
    deallocate (M2_salt_road_2)
    deallocate (water_road)
    deallocate (snow_road)
    deallocate (ice_road)
    
    
end subroutine transfer_preprocessor_to_combined_initialdata
    
subroutine transfer_combined_to_NORTRIP_initialdata
    
    use NORTRIP_definitions
    use NORTRIP_multiroad_combined_definitions
    
    implicit none
    
    real, allocatable :: M_road_init_temp(:,:)
    
  	write(unit_logfile,'(A)') '================================================================'
    write(unit_logfile,'(A)') 'Transfering road initial conditions data (transfer_combined_to_NORTRIP_initialdata)' 
  	write(unit_logfile,'(A)') '================================================================'
 
    !Allocate and initialise arrays for the initial conditions
    if (.not.allocated(M_road_init)) then
        allocate (M_road_init(num_source_all,num_size,num_track,0:n_roads))
        M_road_init=0.0
    endif
    if (.not.allocated(M_road_init_temp)) then
        allocate (M_road_init_temp(num_source_all,0:n_roads))
        M_road_init_temp=0.0
    endif

    if (.not.allocated(g_road_init)) then
        allocate (g_road_init(num_moisture,num_track,0:n_roads))
        g_road_init=0.0
    endif
    
    M_road_init_temp(road_index,1:n_roads)=transfer_M2_dust_road
    M_road_init_temp(sand_index,1:n_roads)=transfer_M2_sand_road
    M_road_init_temp(salt_index(1),1:n_roads)=transfer_M2_salt_road_1
    M_road_init_temp(salt_index(2),1:n_roads)=transfer_M2_salt_road_2
    g_road_init(water_index,1,1:n_roads)=transfer_water_road
    g_road_init(snow_index,1,1:n_roads)=transfer_snow_road
    g_road_init(ice_index,1,1:n_roads)=transfer_ice_road
    long_rad_in_offset=transfer_long_rad_in_offset
    RH_offset=transfer_RH_offset
    T_a_offset=transfer_T_a_offset

    !Distribute the initial suspendable mass according to road wear over all tracks and convert from g/m^2 to g/km
    do s=1,num_source
    do tr=1,num_track
    !do ro=1,n_roads
        M_road_init(s,pm_all,tr,:)=M_road_init_temp(s,:)*b_road_lanes(:)*1000.
        M_road_init(s,pm_200,tr,:)=M_road_init_temp(s,:)*f_PM(s,pm_200,st)*f_track(tr)*b_road_lanes(:)*1000.
        M_road_init(s,pm_10,tr,:)=M_road_init_temp(s,:)*f_PM(s,pm_10,st)*f_track(tr)*b_road_lanes(:)*1000.
        M_road_init(s,pm_25,tr,:)=M_road_init_temp(s,:)*f_PM(s,pm_25,st)*f_track(tr)*b_road_lanes(:)*1000.
    !enddo
    enddo
    enddo
    write(unit_logfile,'(a,1f12.1)') ' Maximum initial dust layer transferred',maxval(M_road_init(road_index,pm_all,1,:)/b_road_lanes(:)/1000.)
     
    do ro=1,n_roads
    do m=1,num_moisture
        g_road_init(m,1:num_track,ro)=g_road_init(m,1,ro)
    enddo
        !write(*,*) ro,M_road_init(salt_index(1),pm_all,1,ro),M_road_init(salt_index(2),pm_all,1,ro)
    enddo

    if (allocated(M_road_init_temp)) deallocate (M_road_init_temp)    
    
    if (allocated(transfer_M2_dust_road)) deallocate (transfer_M2_dust_road)
    if (allocated(transfer_M2_sand_road)) deallocate (transfer_M2_sand_road)
    if (allocated(transfer_M2_salt_road_1)) deallocate (transfer_M2_salt_road_1)
    if (allocated(transfer_M2_salt_road_2)) deallocate (transfer_M2_salt_road_2)
    if (allocated(transfer_water_road)) deallocate (transfer_water_road)
    if (allocated(transfer_snow_road)) deallocate (transfer_snow_road)
    if (allocated(transfer_ice_road)) deallocate (transfer_ice_road)
    
    !M_road_init=0.
    !g_road_init=0.
    !P_fugitive=0.0;long_rad_in_offset=0.0;RH_offset=0.0;T_a_offset=0.0

end subroutine transfer_combined_to_NORTRIP_initialdata
    