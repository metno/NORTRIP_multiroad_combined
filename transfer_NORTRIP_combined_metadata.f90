    subroutine transfer_preprocessor_to_combined_metadata
    
    use NORTRIP_multiroad_index_definitions
    use NORTRIP_multiroad_combined_definitions
    
    implicit none
    
    integer i,t,j
    integer unit_in
    integer exists
    real, allocatable :: EF_temp(:,:)
    !character(12) char_temp(n_roadlinks)
    character(256) temp_str,search_str
    
    allocate (EF_temp(num_veh,n_roadlinks))
    
 	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Transfering preprocessor metadata to combined (transfer_preprocessor_to_combined_metadata)'
	write(unit_logfile,'(A)') '================================================================' 

   !Set some of the parameters that are not read in yet
    !write(*,*) n_roadlinks,n_save_links,n_save_road
    do i=1,n_roadlinks
        inputdata_int_rl(drivingcycle_rl_index,i)=1
        inputdata_int_rl(pavementtype_rl_index,i)=4
        !inputdata_rl(canyonwidth_rl_index,i)=max(inputdata_rl(width_rl_index,i),inputdata_rl(canyonwidth_rl_index,i))
        inputdata_rl(albedo_rl_index,i)=0.2
        inputdata_rl(heighttemperature_rl_index,i)=2.
        inputdata_rl(heightwind_rl_index,i)=10.
        inputdata_rl(timedifference_rl_index,i)=DIFUTC_H
        inputdata_rl(windspeed_correction_rl_index,i)=wind_speed_correction
    enddo

    !Overwrite the pavement type based on min and max ADT
    if (n_road_pave_ADT_index.gt.0) then
        do i=1,n_roadlinks
        do j=1,n_road_pave_ADT_index
            !write(*,*) i,j
            if (inputdata_rl(adt_rl_index,i).ge.road_type_pave_flag_input(road_pave_min_ADT_index,j).and.inputdata_rl(adt_rl_index,i).lt.road_type_pave_flag_input(road_pave_max_ADT_index,j)) then
                !write(*,*) road_pave_ADT_flag_index,road_type_pave_flag_input(road_pave_ADT_flag_index,j)
                inputdata_int_rl(pavementtype_rl_index,i)=road_type_pave_flag_input(road_pave_ADT_flag_index,j)
            endif
        enddo
        !write(*,*) i,inputdata_rl(adt_rl_index,i),inputdata_int_rl(pavementtype_rl_index,i)
        enddo
    endif

    !Write data that is used by all road links first
    transfer_n_roads=n_save_links
    transfer_nodata=missing_data
    transfer_hours_between_init=hours_between_init
    transfer_calculation_type=calculation_type
    
    transfer_long_rad_in_offset=long_rad_in_offset
    transfer_RH_offset=RH_offset
    transfer_T_a_offset=T_a_offset
        
    !Write the grid definition data if required
    transfer_grid_0(1)=grid_0(1)
    transfer_grid_0(2)=grid_0(2)
    transfer_grid_dim(1)=grid_dim(1)
    transfer_grid_dim(2)=grid_dim(2)
    transfer_grid_delta(1)=grid_delta(1)
    transfer_grid_delta(2)=grid_delta(2)

    !Allocate and initialise arrays for road metadata
    if (.not.allocated(transfer_d_index)) allocate (transfer_d_index(0:transfer_n_roads))
    if (.not.allocated(transfer_p_index)) allocate (transfer_p_index(0:transfer_n_roads))
    if (.not.allocated(transfer_b_road)) allocate (transfer_b_road(0:transfer_n_roads))
    if (.not.allocated(transfer_n_lanes)) allocate (transfer_n_lanes(0:transfer_n_roads))
    if (.not.allocated(transfer_b_road_lanes)) allocate (transfer_b_road_lanes(0:transfer_n_roads))
    if (.not.allocated(transfer_b_lane)) allocate (transfer_b_lane(0:transfer_n_roads))
    if (.not.allocated(transfer_b_canyon)) allocate (transfer_b_canyon(0:transfer_n_roads))
    if (.not.allocated(transfer_h_canyon)) allocate (transfer_h_canyon(2,0:transfer_n_roads)) !Two sides, north and south
    if (.not.allocated(transfer_ang_road)) allocate (transfer_ang_road(0:transfer_n_roads))
    if (.not.allocated(transfer_slope_road)) allocate (transfer_slope_road(0:transfer_n_roads))
    if (.not.allocated(transfer_roadtype_index)) allocate (transfer_roadtype_index(0:transfer_n_roads))

    if (.not.allocated(transfer_LAT)) allocate (transfer_LAT(0:transfer_n_roads))
    if (.not.allocated(transfer_LON)) allocate (transfer_LON(0:transfer_n_roads))
    if (.not.allocated(transfer_Z_SURF)) allocate (transfer_Z_SURF(0:transfer_n_roads))
    if (.not.allocated(transfer_z_FF)) allocate (transfer_z_FF(0:transfer_n_roads))
    if (.not.allocated(transfer_z_T)) allocate (transfer_z_T(0:transfer_n_roads))
    if (.not.allocated(transfer_z2_T)) allocate (transfer_z2_T(0:transfer_n_roads))
    if (.not.allocated(transfer_albedo_road)) allocate (transfer_albedo_road(0:transfer_n_roads))
    if (.not.allocated(transfer_DIFUTC_H)) allocate (transfer_DIFUTC_H(0:transfer_n_roads))
    if (.not.allocated(transfer_Pressure)) allocate (transfer_Pressure(0:transfer_n_roads))

    !Correction factors
    if (.not.allocated(transfer_wind_speed_correction)) allocate (transfer_wind_speed_correction(0:transfer_n_roads))
    if (.not.allocated(transfer_h_sus)) allocate (transfer_h_sus(0:transfer_n_roads))
    if (.not.allocated(transfer_h_texture)) allocate (transfer_h_texture(0:transfer_n_roads))
    
    !OSPM factors
    if (.not.allocated(transfer_choose_receptor_ospm)) allocate (transfer_choose_receptor_ospm(0:transfer_n_roads))
    if (.not.allocated(transfer_SL1_ospm)) allocate (transfer_SL1_ospm(0:transfer_n_roads))
    if (.not.allocated(transfer_SL2_ospm)) allocate (transfer_SL2_ospm(0:transfer_n_roads))
    if (.not.allocated(transfer_f_roof_ospm)) allocate (transfer_f_roof_ospm(0:transfer_n_roads))
    if (.not.allocated(transfer_RecHeight_ospm)) allocate (transfer_RecHeight_ospm(0:transfer_n_roads))
    if (.not.allocated(transfer_f_turb_ospm)) allocate (transfer_f_turb_ospm(0:transfer_n_roads))
   
    !Single factors
    if (.not.allocated(transfer_observed_moisture_cutoff_value)) allocate (transfer_observed_moisture_cutoff_value(0:transfer_n_roads))
    if (.not.allocated(transfer_road_ID)) allocate (transfer_road_ID(0:transfer_n_roads))
    if (.not.allocated(transfer_save_road_data_flag)) allocate (transfer_save_road_data_flag(0:transfer_n_roads))

    !Emisison factors (num_veh,n_road)
    if (.not.allocated(transfer_exhaust_EF)) allocate (transfer_exhaust_EF(transfer_num_veh,0:transfer_n_roads))
    if (.not.allocated(transfer_NOX_EF)) allocate (transfer_NOX_EF(transfer_num_veh,0:transfer_n_roads))
    
    !Grid data
    if (.not.allocated(transfer_x_road)) allocate (transfer_x_road(2,0:transfer_n_roads))
    if (.not.allocated(transfer_y_road)) allocate (transfer_y_road(2,0:transfer_n_roads))
    if (.not.allocated(transfer_length_road)) allocate (transfer_length_road(0:transfer_n_roads))
    if (.not.allocated(transfer_line_or_grid_data_flag)) allocate (transfer_line_or_grid_data_flag(0:transfer_n_roads))
    !if (.not.allocated(adt_road)) allocate (adt_road(0:n_roads))
    
    !Road type actvity factors
    if (.not.allocated(transfer_road_type_activity_flag)) allocate (transfer_road_type_activity_flag(num_road_type_activity,0:transfer_n_roads))

    !Specify data that is not specified by multiroad is read by NORTRIP and defaults used
    transfer_h_sus(1:transfer_n_roads)=1.0
    transfer_h_texture(1:transfer_n_roads)=1.0
    transfer_observed_moisture_cutoff_value(1:transfer_n_roads)=1.5
    transfer_z2_T(1:transfer_n_roads)=25.
    transfer_SL1_ospm(1:transfer_n_roads)=50.
    transfer_SL2_ospm(1:transfer_n_roads)=50.
    transfer_f_roof_ospm(1:transfer_n_roads)=0.8
    transfer_RecHeight_ospm(1:transfer_n_roads)=3.0
    transfer_f_turb_ospm(1:transfer_n_roads)=1.0
    
    !Write data used by individual road links
    !write(unit_in,'(a40,a,<n_save_links>i12)') 'Road index',achar(9),inputdata_int_rl(roadindex_rl_index,save_links(1:n_save_links))
    transfer_Road_ID(1:transfer_n_roads)=inputdata_int_rl(id_rl_index,save_links(1:n_save_links))
    transfer_roadtype_index(1:transfer_n_roads)=inputdata_int_rl(roadstructuretype_rl_index,save_links(1:n_save_links))
    transfer_d_index(1:transfer_n_roads)=inputdata_int_rl(drivingcycle_rl_index,save_links(1:n_save_links))
    transfer_p_index(1:transfer_n_roads)=inputdata_int_rl(pavementtype_rl_index,save_links(1:n_save_links))
    transfer_n_lanes(1:transfer_n_roads)=inputdata_int_rl(nlanes_rl_index,save_links(1:n_save_links))
    transfer_b_road(1:transfer_n_roads)=inputdata_rl(width_rl_index,save_links(1:n_save_links))
    transfer_b_canyon(1:transfer_n_roads)=inputdata_rl(canyonwidth_rl_index,save_links(1:n_save_links))
    transfer_h_canyon(1,1:transfer_n_roads)=inputdata_rl(canyonheight_north_rl_index,save_links(1:n_save_links))
    transfer_h_canyon(2,1:transfer_n_roads)=inputdata_rl(canyonheight_south_rl_index,save_links(1:n_save_links))
    transfer_choose_receptor_ospm(1:transfer_n_roads)=inputdata_int_rl(ospm_pos_rl_index,save_links(1:n_save_links))
    transfer_ang_road(1:transfer_n_roads)=inputdata_rl(angle_rl_index,save_links(1:n_save_links))
    transfer_slope_road(1:transfer_n_roads)=inputdata_rl(slope_rl_index,save_links(1:n_save_links))
    transfer_LAT(1:transfer_n_roads)=inputdata_rl(lat0_rl_index,save_links(1:n_save_links))
    transfer_LON(1:transfer_n_roads)=inputdata_rl(lon0_rl_index,save_links(1:n_save_links))
    transfer_Z_SURF(1:transfer_n_roads)=inputdata_rl(elevation_rl_index,save_links(1:n_save_links))
    transfer_albedo_road(1:transfer_n_roads)=inputdata_rl(albedo_rl_index,save_links(1:n_save_links))
    transfer_wind_speed_correction(1:transfer_n_roads)=inputdata_rl(windspeed_correction_rl_index,save_links(1:n_save_links))
    transfer_z_FF(1:transfer_n_roads)=inputdata_rl(heightwind_rl_index,save_links(1:n_save_links))
    transfer_z_T(1:transfer_n_roads)=inputdata_rl(heighttemperature_rl_index,save_links(1:n_save_links))
    transfer_DIFUTC_H(1:transfer_n_roads)=inputdata_rl(timedifference_rl_index,save_links(1:n_save_links))
    transfer_save_road_data_flag(1:transfer_n_roads)=inputdata_int_rl(savedata_rl_index,save_links(1:n_save_links))

    
    !If no exhaust emission data read in then set the exhaust emission factors for NORTRIP
    if (sum(airquality_data(EP_emis_index,:,1:transfer_n_roads)).eq.0.and.in_exhaust_EF(he).gt.0.and.in_exhaust_EF(li).gt.0) then
        EF_temp(li,1:transfer_n_roads)=in_exhaust_EF(li)
        EF_temp(he,1:transfer_n_roads)=in_exhaust_EF(he)
        transfer_exhaust_EF(he,1:transfer_n_roads)=EF_temp(he,1:n_save_links)
        transfer_exhaust_EF(li,1:transfer_n_roads)=EF_temp(li,1:n_save_links)
    endif       
    if (sum(airquality_data(NOX_emis_index,:,1:transfer_n_roads)).eq.0.and.in_nox_EF(he).gt.0.and.in_nox_EF(li).gt.0) then
        EF_temp(li,1:transfer_n_roads)=in_nox_EF(li)
        EF_temp(he,1:transfer_n_roads)=in_nox_EF(he)
        transfer_NOX_EF(he,1:transfer_n_roads)=EF_temp(he,1:n_save_links)
        transfer_NOX_EF(li,1:transfer_n_roads)=EF_temp(li,1:n_save_links)
    endif       

    if (grid_road_data_flag) then
        transfer_x_road(1,1:transfer_n_roads)=inputdata_rl(x1_rl_index,save_links(1:n_save_links))
        transfer_y_road(1,1:transfer_n_roads)=inputdata_rl(y1_rl_index,save_links(1:n_save_links))
        transfer_x_road(2,1:transfer_n_roads)=inputdata_rl(x2_rl_index,save_links(1:n_save_links))
        transfer_y_road(2,1:transfer_n_roads)=inputdata_rl(y2_rl_index,save_links(1:n_save_links))
        transfer_length_road(1:transfer_n_roads)=inputdata_rl(length_rl_index,save_links(1:n_save_links))
        transfer_line_or_grid_data_flag(1:transfer_n_roads)=inputdata_int_rl(griddata_rl_index,save_links(1:n_save_links))
    endif

    if (n_road_type_flag_index.gt.0) then        
        !Allocate the road type activity data to the roads
        do i=1,n_roadlinks
            if (inputdata_int_rl(roadactivitytype_rl_index,i).gt.0.and.inputdata_int_rl(roadactivitytype_rl_index,i).le.num_max_road_types) then
                road_type_activity_flag_roads(:,i)=road_type_activity_flag(:,inputdata_int_rl(roadactivitytype_rl_index,i))
            else
                road_type_activity_flag_roads(:,i)=1
            endif
        enddo

        transfer_road_type_activity_flag(transfer_road_type_salt_index(1),1:transfer_n_roads)=road_type_activity_flag_roads(road_type_salting_index,save_links(1:n_save_links))
        transfer_road_type_activity_flag(transfer_road_type_salt_index(2),1:transfer_n_roads)=road_type_activity_flag_roads(road_type_binding_index,save_links(1:n_save_links))
        transfer_road_type_activity_flag(transfer_road_type_sanding_index,1:transfer_n_roads)=road_type_activity_flag_roads(road_type_sanding_index,save_links(1:n_save_links))
        transfer_road_type_activity_flag(transfer_road_type_ploughing_index,1:transfer_n_roads)=road_type_activity_flag_roads(road_type_ploughing_index,save_links(1:n_save_links))
        transfer_road_type_activity_flag(transfer_road_type_cleaning_index,1:transfer_n_roads)=road_type_activity_flag_roads(road_type_cleaning_index,save_links(1:n_save_links))
    endif
        
    !Save the skyview data
    if (n_skyview.gt.0) then
        transfer_n_skyview=n_skyview
        allocate (transfer_az_skyview(transfer_n_skyview,0:transfer_n_roads))
        allocate (transfer_zen_skyview(transfer_n_skyview,0:transfer_n_roads))
        do i=1,n_skyview
            transfer_az_skyview(i,1:transfer_n_roads)=360/transfer_n_skyview*(i-1)
        enddo
        transfer_zen_skyview(:,1:transfer_n_roads)=zen_skyview(:,save_links(1:n_save_links))
    endif

    !Set road width based on lane width
    !b_lane does not exist in the static road data. Is normally set to 3.5m by NORTRIP
    !Road width can be larger than b_lane if there is something in between the lanes
    !This is how NORTRIP reads it's data if there is no b_lane information
    transfer_b_lane=3.0
    transfer_b_road_lanes=transfer_n_lanes*transfer_b_lane
    !Set it to the road width anyway
    !transfer_b_road_lanes=transfer_b_road
    
    !do i=1,transfer_n_roads
    !write(*,*) i,transfer_n_lanes(i),transfer_b_lane(i),transfer_b_road_lanes(i)
    !enddo
    
    !Date strings are not written by NORTRIP_multiroad to the metadata file
    
    !Transfer all the activity data, which has now become a form of metadata because it is specified for each road seperately
    allocate (transfer_salting_hour(2,0:transfer_n_roads))
    allocate (transfer_delay_salting_day(0:transfer_n_roads))
    allocate (transfer_check_salting_day(0:transfer_n_roads))
    allocate (transfer_min_temp_salt(0:transfer_n_roads))
    allocate (transfer_max_temp_salt(0:transfer_n_roads))
    allocate (transfer_precip_rule_salt(0:transfer_n_roads))
    allocate (transfer_RH_rule_salt(0:transfer_n_roads))
    allocate (transfer_g_salting_rule(0:transfer_n_roads))
    allocate (transfer_salt_mass(0:transfer_n_roads)) 
    allocate (transfer_salt_dilution(0:transfer_n_roads)) 
    allocate (transfer_salt_type_distribution(0:transfer_n_roads)) 
    
    allocate (transfer_sanding_hour(2,0:transfer_n_roads))
    allocate (transfer_delay_sanding_day(0:transfer_n_roads)) 
    allocate (transfer_check_sanding_day(0:transfer_n_roads))
    allocate (transfer_min_temp_sand(0:transfer_n_roads)) 
    allocate (transfer_max_temp_sand(0:transfer_n_roads))
    allocate (transfer_precip_rule_sand(0:transfer_n_roads))
    allocate (transfer_RH_rule_sand(0:transfer_n_roads)) 
    allocate (transfer_g_sanding_rule(0:transfer_n_roads)) 
    allocate (transfer_sand_mass(0:transfer_n_roads)) 
    allocate (transfer_sand_dilution(0:transfer_n_roads))
    
    allocate (transfer_delay_ploughing_hour(0:transfer_n_roads))
    allocate (transfer_ploughing_thresh_2(0:transfer_n_roads)) 

    allocate (transfer_cleaning_hour(2,0:transfer_n_roads))
    allocate (transfer_delay_cleaning_day(0:transfer_n_roads))
    allocate (transfer_min_temp_cleaning(0:transfer_n_roads))
    allocate (transfer_clean_with_salting(0:transfer_n_roads))
    allocate (transfer_start_month_cleaning(0:transfer_n_roads))
    allocate (transfer_end_month_cleaning(0:transfer_n_roads))
    allocate (transfer_wetting_with_cleaning(0:transfer_n_roads))
    allocate (transfer_efficiency_of_cleaning(0:transfer_n_roads))

    allocate (transfer_binding_hour(2,0:transfer_n_roads))
    allocate (transfer_delay_binding_day(0:transfer_n_roads))
    allocate (transfer_check_binding_day(0:transfer_n_roads))
    allocate (transfer_min_temp_binding(0:transfer_n_roads))
    allocate (transfer_max_temp_binding(0:transfer_n_roads))
    allocate (transfer_precip_rule_binding(0:transfer_n_roads))
    allocate (transfer_RH_rule_binding(0:transfer_n_roads))
    allocate (transfer_g_binding_rule(0:transfer_n_roads))
    allocate (transfer_binding_mass(0:transfer_n_roads))
    allocate (transfer_binding_dilution(0:transfer_n_roads))
    allocate (transfer_start_month_binding(0:transfer_n_roads))
    allocate (transfer_end_month_binding(0:transfer_n_roads))
    
    transfer_read_auto_activity_data=multi_read_auto_activity_data

    transfer_salting_hour=multi_salting_hour
    transfer_delay_salting_day=multi_delay_salting_day
    transfer_check_salting_day=multi_check_salting_day
    transfer_min_temp_salt=multi_min_temp_salt
    transfer_max_temp_salt=multi_max_temp_salt
    transfer_precip_rule_salt=multi_precip_rule_salt
    transfer_RH_rule_salt=multi_RH_rule_salt
    transfer_g_salting_rule=multi_g_salting_rule
    transfer_salt_mass=multi_salt_mass
    transfer_salt_dilution=multi_salt_dilution 
    transfer_salt_type_distribution=multi_salt_type_distribution 
    
    transfer_sanding_hour=multi_sanding_hour
    transfer_delay_sanding_day=multi_delay_sanding_day 
    transfer_check_sanding_day=multi_check_sanding_day
    transfer_min_temp_sand=multi_min_temp_sand 
    transfer_max_temp_sand=multi_max_temp_sand
    transfer_precip_rule_sand=multi_precip_rule_sand
    transfer_RH_rule_sand=multi_RH_rule_sand 
    transfer_g_sanding_rule=multi_g_sanding_rule 
    transfer_sand_mass=multi_sand_mass 
    transfer_sand_dilution=multi_sand_dilution
    
    transfer_delay_ploughing_hour=multi_delay_ploughing_hour
    transfer_ploughing_thresh_2=multi_ploughing_thresh_2 

    transfer_cleaning_hour=multi_cleaning_hour
    transfer_delay_cleaning_day=multi_delay_cleaning_day
    transfer_min_temp_cleaning=multi_min_temp_cleaning
    transfer_clean_with_salting=multi_clean_with_salting
    transfer_start_month_cleaning=multi_start_month_cleaning
    transfer_end_month_cleaning=multi_end_month_cleaning
    transfer_wetting_with_cleaning=multi_wetting_with_cleaning
    transfer_efficiency_of_cleaning=multi_efficiency_of_cleaning

    transfer_binding_hour=multi_binding_hour
    transfer_delay_binding_day=multi_delay_binding_day
    transfer_check_binding_day=multi_check_binding_day
    transfer_min_temp_binding=multi_min_temp_binding
    transfer_max_temp_binding=multi_max_temp_binding
    transfer_precip_rule_binding=multi_precip_rule_binding
    transfer_RH_rule_binding=multi_RH_rule_binding
    transfer_g_binding_rule=multi_g_binding_rule
    transfer_binding_mass=multi_binding_mass
    transfer_binding_dilution=multi_binding_dilution
    transfer_start_month_binding=multi_start_month_binding
    transfer_end_month_binding=multi_end_month_binding

    end subroutine transfer_preprocessor_to_combined_metadata

    subroutine transfer_combined_to_NORTRIP_metadata
    
    use NORTRIP_definitions
    use NORTRIP_multiroad_combined_definitions
    
    implicit none
    
    integer :: i_road=1

 	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Transfering combined metadata to NORTRIP (transfer_combined_to_NORTRIP_metadata)'
	write(unit_logfile,'(A)') '================================================================' 

    !Write data that is used by all road links first
    n_roads=transfer_n_roads
    nodata=transfer_nodata
    hours_between_init=transfer_hours_between_init
    calculation_type=transfer_calculation_type
    
    long_rad_in_offset=transfer_long_rad_in_offset
    RH_offset=transfer_RH_offset
    T_a_offset=transfer_T_a_offset
        
    !Write the grid definition data if required
    grid_0(1)=transfer_grid_0(1)
    grid_0(2)=transfer_grid_0(2)
    grid_dim(1)=transfer_grid_dim(1)
    grid_dim(2)=transfer_grid_dim(2)
    grid_dim(1)=transfer_grid_dim(1)
    grid_dim(2)=transfer_grid_dim(2)

    !Allocate and initialise arrays for road metadata
    if (.not.allocated(d_index)) allocate (d_index(0:n_roads))
    if (.not.allocated(p_index)) allocate (p_index(0:n_roads))
    if (.not.allocated(b_road)) allocate (b_road(0:n_roads))
    if (.not.allocated(n_lanes)) allocate (n_lanes(0:n_roads))
    if (.not.allocated(b_road_lanes)) allocate (b_road_lanes(0:n_roads))
    if (.not.allocated(b_lane)) allocate (b_lane(0:n_roads))
    if (.not.allocated(b_canyon)) allocate (b_canyon(0:n_roads))
    if (.not.allocated(h_canyon)) allocate (h_canyon(2,0:n_roads)) !Two sides, north and south
    if (.not.allocated(ang_road)) allocate (ang_road(0:n_roads))
    if (.not.allocated(slope_road)) allocate (slope_road(0:n_roads))
    if (.not.allocated(roadtype_index)) allocate (roadtype_index(0:n_roads))

    if (.not.allocated(LAT)) allocate (LAT(0:n_roads))
    if (.not.allocated(LON)) allocate (LON(0:n_roads))
    if (.not.allocated(Z_SURF)) allocate (Z_SURF(0:n_roads))
    if (.not.allocated(z_FF)) allocate (z_FF(0:n_roads))
    if (.not.allocated(z_T)) allocate (z_T(0:n_roads))
    if (.not.allocated(z2_T)) allocate (z2_T(0:n_roads))
    if (.not.allocated(albedo_road)) allocate (albedo_road(0:n_roads))
    if (.not.allocated(DIFUTC_H)) allocate (DIFUTC_H(0:n_roads))
    if (.not.allocated(Pressure)) allocate (Pressure(0:n_roads))

    !Correction factors
    if (.not.allocated(wind_speed_correction)) allocate (wind_speed_correction(0:n_roads))
    if (.not.allocated(h_sus)) allocate (h_sus(0:n_roads))
    if (.not.allocated(h_texture)) allocate (h_texture(0:n_roads))
    
    !OSPM factors
    if (.not.allocated(choose_receptor_ospm)) allocate (choose_receptor_ospm(0:n_roads))
    if (.not.allocated(SL1_ospm)) allocate (SL1_ospm(0:n_roads))
    if (.not.allocated(SL2_ospm)) allocate (SL2_ospm(0:n_roads))
    if (.not.allocated(f_roof_ospm)) allocate (f_roof_ospm(0:n_roads))
    if (.not.allocated(RecHeight_ospm)) allocate (RecHeight_ospm(0:n_roads))
    if (.not.allocated(f_turb_ospm)) allocate (f_turb_ospm(0:n_roads))
   
    !Single factors
    if (.not.allocated(observed_moisture_cutoff_value)) allocate (observed_moisture_cutoff_value(0:n_roads))
    if (.not.allocated(road_ID)) allocate (road_ID(0:n_roads))
    if (.not.allocated(save_road_data_flag)) allocate (save_road_data_flag(0:n_roads))

    !Emisison factors (num_veh,n_road)
    if (.not.allocated(exhaust_EF)) allocate (exhaust_EF(num_veh,0:n_roads))
    if (.not.allocated(NOX_EF)) allocate (NOX_EF(num_veh,0:n_roads))
    
    !Grid data
    if (.not.allocated(x_road)) allocate (x_road(2,0:n_roads))
    if (.not.allocated(y_road)) allocate (y_road(2,0:n_roads))
    if (.not.allocated(length_road)) allocate (length_road(0:n_roads))
    if (.not.allocated(line_or_grid_data_flag)) allocate (line_or_grid_data_flag(0:n_roads))
    !if (.not.allocated(adt_road)) allocate (adt_road(0:n_roads))
    
    !Road type actvity factors
    if (.not.allocated(road_type_activity_flag)) allocate (road_type_activity_flag(num_road_type_activity,0:n_roads))
    
    !Write data used by individual road links
    Road_ID=transfer_Road_ID
    b_road=transfer_b_road
    d_index=transfer_d_index
    p_index=transfer_p_index
    roadtype_index=transfer_roadtype_index

    n_lanes=transfer_n_lanes
    b_lane=transfer_b_lane
    b_canyon=transfer_b_canyon
    h_canyon(1,:)=transfer_h_canyon(1,:)
    h_canyon(2,:)=transfer_h_canyon(2,:)
    ang_road=transfer_ang_road
    slope_road=transfer_slope_road
    LAT=transfer_LAT
    LON=transfer_LON
    Z_SURF=transfer_Z_SURF
    z_FF=transfer_z_FF
    z_T=transfer_z_T
    z2_T=transfer_z2_T
    albedo_road=transfer_albedo_road
    DIFUTC_H=transfer_DIFUTC_H
    Pressure=transfer_Pressure
    observed_moisture_cutoff_value=transfer_observed_moisture_cutoff_value
    wind_speed_correction=transfer_wind_speed_correction
    h_sus=transfer_h_sus
    h_texture=transfer_h_texture

    choose_receptor_ospm=transfer_choose_receptor_ospm
    SL1_ospm=transfer_SL1_ospm
    SL2_ospm=transfer_SL2_ospm
    f_roof_ospm=transfer_f_roof_ospm
    RecHeight_ospm=transfer_RecHeight_ospm
    f_turb_ospm=transfer_f_turb_ospm

    exhaust_EF(he,:)=transfer_exhaust_EF(he,:)
    exhaust_EF(li,:)=transfer_exhaust_EF(li,:)
    NOX_EF(he,:)=transfer_NOX_EF(he,:)
    NOX_EF(li,:)=transfer_NOX_EF(li,:)
    
    save_road_data_flag=transfer_save_road_data_flag
    line_or_grid_data_flag=transfer_line_or_grid_data_flag
    
    x_road(1,:)=transfer_x_road(1,:)
    y_road(1,:)=transfer_y_road(1,:)
    x_road(2,:)=transfer_x_road(2,:)
    y_road(2,:)=transfer_y_road(2,:)
    length_road=transfer_length_road

    road_type_activity_flag(road_type_salt_index(1),:)=transfer_road_type_activity_flag(transfer_road_type_salt_index(1),:)
    road_type_activity_flag(road_type_salt_index(2),:)=transfer_road_type_activity_flag(transfer_road_type_salt_index(2),:)
    road_type_activity_flag(road_type_sanding_index,:)=transfer_road_type_activity_flag(transfer_road_type_sanding_index,:)
    road_type_activity_flag(road_type_ploughing_index,:)=transfer_road_type_activity_flag(transfer_road_type_ploughing_index,:)
    road_type_activity_flag(road_type_cleaning_index,:)=transfer_road_type_activity_flag(transfer_road_type_cleaning_index,:)

    
    !Note start and end dates are not read.Runs and saves for the input data period
    
    n_skyview=transfer_n_skyview
    allocate (az_skyview(n_skyview,0:n_roads))
    allocate (zen_skyview(n_skyview,0:n_roads))
    zen_skyview=transfer_zen_skyview
    az_skyview=transfer_az_skyview
    
    !Set road width based on lane width
    b_road_lanes=transfer_b_road_lanes

    !Check
    do i=1,n_roads
        !write(*,'(<n_skyview>f6.1)') zen_skyview(:,i)
        !write(*,'(5f6.1)') h_canyon(:,i),b_canyon(i),b_road(i),b_road_lanes(i)
    enddo
    
    
    !Test to see if any gridding is necessary or not
    !Only if gridding is specified and a grid is defined
    if ((maxval(line_or_grid_data_flag).eq.2.or.maxval(line_or_grid_data_flag).eq.3).and.minval(grid_dim).gt.0) then
        grid_road_data_flag=.true.
    else
        grid_road_data_flag=.false.
    endif

    !Commented out this as it is giving a debug error
    !if (sum(exhaust_EF(:,0:n_roads)).eq.0) then
    !    exhaust_EF_available=0
    !else
    !    exhaust_EF_available=1
    !endif

    !if (sum(NOX_EF(:,0:n_roads)).eq.0) then
    !    NOX_EF_available=0
    !else
    !    NOX_EF_available=1
    !endif
    
    !Set these two to 0 since the emissions are transferred through the time series data
    !Neither of these two emission factors have been defined I think
    !This is not true they transferred i nthe metadata file
    !FIND OUT WHAT IS HAPPENING!
    exhaust_EF_available=0
    NOX_EF_available=0

    if (allocated(transfer_d_index)) deallocate (transfer_d_index)
    if (allocated(transfer_p_index)) deallocate (transfer_p_index)
    if (allocated(transfer_b_road)) deallocate (transfer_b_road)
    if (allocated(transfer_n_lanes)) deallocate (transfer_n_lanes)
    if (allocated(transfer_b_road_lanes)) deallocate (transfer_b_road_lanes)
    if (allocated(transfer_b_lane)) deallocate (transfer_b_lane)
    if (allocated(transfer_b_canyon)) deallocate (transfer_b_canyon)
    if (allocated(transfer_h_canyon)) deallocate (transfer_h_canyon) !Two sides, north and south
    if (allocated(transfer_ang_road)) deallocate (transfer_ang_road)
    if (allocated(transfer_slope_road)) deallocate (transfer_slope_road)
    if (allocated(transfer_roadtype_index)) deallocate (transfer_roadtype_index)

    if (allocated(transfer_LAT)) deallocate (transfer_LAT)
    if (allocated(transfer_LON)) deallocate (transfer_LON)
    if (allocated(transfer_Z_SURF)) deallocate (transfer_Z_SURF)
    if (allocated(transfer_z_FF)) deallocate (transfer_z_FF)
    if (allocated(transfer_z_T)) deallocate (transfer_z_T)
    if (allocated(transfer_z2_T)) deallocate (transfer_z2_T)
    if (allocated(transfer_albedo_road)) deallocate (transfer_albedo_road)
    if (allocated(transfer_DIFUTC_H)) deallocate (transfer_DIFUTC_H)
    if (allocated(transfer_Pressure)) deallocate (transfer_Pressure)

    !Correction factors
    if (allocated(transfer_wind_speed_correction)) deallocate (transfer_wind_speed_correction)
    if (allocated(transfer_h_sus)) deallocate (transfer_h_sus)
    if (allocated(transfer_h_texture)) deallocate (transfer_h_texture)
    
    !OSPM factors
    if (allocated(transfer_choose_receptor_ospm)) deallocate (transfer_choose_receptor_ospm)
    if (allocated(transfer_SL1_ospm)) deallocate (transfer_SL1_ospm)
    if (allocated(transfer_SL2_ospm)) deallocate (transfer_SL2_ospm)
    if (allocated(transfer_f_roof_ospm)) deallocate (transfer_f_roof_ospm)
    if (allocated(transfer_RecHeight_ospm)) deallocate (transfer_RecHeight_ospm)
    if (allocated(transfer_f_turb_ospm)) deallocate (transfer_f_turb_ospm)
   
    !Single factors
    if (allocated(transfer_observed_moisture_cutoff_value)) deallocate (transfer_observed_moisture_cutoff_value)
    if (allocated(transfer_road_ID)) deallocate (transfer_road_ID)
    if (allocated(transfer_save_road_data_flag)) deallocate (transfer_save_road_data_flag)

    !Emisison factors (num_veh,n_road)
    if (allocated(transfer_exhaust_EF)) deallocate (transfer_exhaust_EF)
    if (allocated(transfer_NOX_EF)) deallocate (transfer_NOX_EF)
    
    !Grid data
    if (allocated(transfer_x_road)) deallocate (transfer_x_road)
    if (allocated(transfer_y_road)) deallocate (transfer_y_road)
    if (allocated(transfer_length_road)) deallocate (transfer_length_road)
    if (allocated(transfer_line_or_grid_data_flag)) deallocate (transfer_line_or_grid_data_flag)
    
    !Road type actvity factors
    if (allocated(transfer_road_type_activity_flag)) deallocate (transfer_road_type_activity_flag)
    if (allocated(transfer_az_skyview)) deallocate (transfer_az_skyview)
    if (allocated(transfer_zen_skyview)) deallocate (transfer_zen_skyview)

    
    !Allocate the auto_activity parameters
    allocate (salting_hour(2,0:n_roads))
    allocate (delay_salting_day(0:n_roads))
    allocate (check_salting_day(0:n_roads))
    allocate (min_temp_salt(0:n_roads))
    allocate (max_temp_salt(0:n_roads))
    allocate (precip_rule_salt(0:n_roads))
    allocate (RH_rule_salt(0:n_roads))
    allocate (g_salting_rule(0:n_roads))
    allocate (salt_mass(0:n_roads)) 
    allocate (salt_dilution(0:n_roads)) 
    allocate (salt_type_distribution(0:n_roads)) 
    
    allocate (sanding_hour(2,0:n_roads))
    allocate (delay_sanding_day(0:n_roads)) 
    allocate (check_sanding_day(0:n_roads))
    allocate (min_temp_sand(0:n_roads)) 
    allocate (max_temp_sand(0:n_roads))
    allocate (precip_rule_sand(0:n_roads))
    allocate (RH_rule_sand(0:n_roads)) 
    allocate (g_sanding_rule(0:n_roads)) 
    allocate (sand_mass(0:n_roads)) 
    allocate (sand_dilution(0:n_roads))
    
    allocate (delay_ploughing_hour(0:n_roads))
    allocate (ploughing_thresh_2(0:n_roads)) 
    allocate (cleaning_hour(2,0:n_roads))
    allocate (delay_cleaning_day(0:n_roads))
    allocate (min_temp_cleaning(0:n_roads))
    allocate (clean_with_salting(0:n_roads))
    allocate (start_month_cleaning(0:n_roads))
    allocate (end_month_cleaning(0:n_roads))
    allocate (wetting_with_cleaning(0:n_roads))
    allocate (efficiency_of_cleaning(0:n_roads))

    allocate (binding_hour(2,0:n_roads))
    allocate (delay_binding_day(0:n_roads))
    allocate (check_binding_day(0:n_roads))
    allocate (min_temp_binding(0:n_roads))
    allocate (max_temp_binding(0:n_roads))
    allocate (precip_rule_binding(0:n_roads))
    allocate (RH_rule_binding(0:n_roads))
    allocate (g_binding_rule(0:n_roads))
    allocate (binding_mass(0:n_roads))
    allocate (binding_dilution(0:n_roads))
    allocate (start_month_binding(0:n_roads))
    allocate (end_month_binding(0:n_roads))
    
    read_auto_activity_data=transfer_read_auto_activity_data

    salting_hour=transfer_salting_hour
    delay_salting_day=transfer_delay_salting_day
    check_salting_day=transfer_check_salting_day
    min_temp_salt=transfer_min_temp_salt
    max_temp_salt=transfer_max_temp_salt
    precip_rule_salt=transfer_precip_rule_salt
    RH_rule_salt=transfer_RH_rule_salt
    g_salting_rule=transfer_g_salting_rule
    salt_mass=transfer_salt_mass
    salt_dilution=transfer_salt_dilution 
    salt_type_distribution=transfer_salt_type_distribution 
    
    sanding_hour=transfer_sanding_hour
    delay_sanding_day=transfer_delay_sanding_day 
    check_sanding_day=transfer_check_sanding_day
    min_temp_sand=transfer_min_temp_sand 
    max_temp_sand=transfer_max_temp_sand
    precip_rule_sand=transfer_precip_rule_sand
    RH_rule_sand=transfer_RH_rule_sand 
    g_sanding_rule=transfer_g_sanding_rule 
    sand_mass=transfer_sand_mass 
    sand_dilution=transfer_sand_dilution
    
    delay_ploughing_hour=transfer_delay_ploughing_hour
    ploughing_thresh_2=transfer_ploughing_thresh_2 

    cleaning_hour=transfer_cleaning_hour
    delay_cleaning_day=transfer_delay_cleaning_day
    min_temp_cleaning=transfer_min_temp_cleaning
    clean_with_salting=transfer_clean_with_salting
    start_month_cleaning=transfer_start_month_cleaning
    end_month_cleaning=transfer_end_month_cleaning
    wetting_with_cleaning=transfer_wetting_with_cleaning
    efficiency_of_cleaning=transfer_efficiency_of_cleaning

    binding_hour=transfer_binding_hour
    delay_binding_day=transfer_delay_binding_day
    check_binding_day=transfer_check_binding_day
    min_temp_binding=transfer_min_temp_binding
    max_temp_binding=transfer_max_temp_binding
    precip_rule_binding=transfer_precip_rule_binding
    RH_rule_binding=transfer_RH_rule_binding
    g_binding_rule=transfer_g_binding_rule
    binding_mass=transfer_binding_mass
    binding_dilution=transfer_binding_dilution
    start_month_binding=transfer_start_month_binding
    end_month_binding=transfer_end_month_binding
    
    end subroutine transfer_combined_to_NORTRIP_metadata
