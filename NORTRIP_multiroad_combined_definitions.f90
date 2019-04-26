    module NORTRIP_multiroad_combined_definitions
    
    implicit none
    
    logical :: NORTRIP_multiroad_combined_flag=.true.

    integer transfer_n_time
    integer iiii,jjjj
   
    !Size fraction index. pm_exhaust included here but only used for saving data purposes, not in the model
    integer transfer_pm_all,transfer_pm_200,transfer_pm_10,transfer_pm_25,transfer_num_size,transfer_pm_exhaust,transfer_nox_exhaust
    parameter(transfer_pm_all=1,transfer_pm_200=2,transfer_pm_10=3,transfer_pm_25=4,transfer_num_size=4,transfer_pm_exhaust=5,transfer_nox_exhaust=6)
    integer transfer_pm_sus(3)
    data (transfer_pm_sus(iiii),iiii=1,3) /transfer_pm_200,transfer_pm_10,transfer_pm_25/

    !These must match the NORTRIP num_index
    !Set meteo input file indexes
    integer transfer_T_a_index,transfer_T2_a_index,transfer_FF_index,transfer_DD_index,transfer_RH_index,transfer_Rain_precip_index,transfer_Snow_precip_index
    integer transfer_short_rad_in_index,transfer_long_rad_in_index,transfer_short_rad_in_clearsky_index
    integer transfer_cloud_cover_index,transfer_road_temperature_obs_input_index,transfer_road_wetness_obs_input_index
    integer transfer_pressure_index,transfer_T_dewpoint_index,transfer_T_sub_input_index
    integer transfer_num_meteo_index
    parameter (transfer_T_a_index=1,transfer_T2_a_index=2,transfer_FF_index=3,transfer_DD_index=4,transfer_RH_index=5,transfer_Rain_precip_index=6,transfer_Snow_precip_index=7)
    parameter (transfer_short_rad_in_index=8,transfer_long_rad_in_index=9,transfer_short_rad_in_clearsky_index=10)
    parameter (transfer_cloud_cover_index=11,transfer_road_temperature_obs_input_index=12,transfer_road_wetness_obs_input_index=13)
    parameter (transfer_pressure_index=14,transfer_T_dewpoint_index=15,transfer_T_sub_input_index=16)
    parameter (transfer_num_meteo_index=16)

    !Date input parameter indexes
    integer transfer_year_index,transfer_month_index,transfer_day_index,transfer_hour_index,transfer_minute_index,transfer_datenum_index
    integer transfer_num_date_index
    parameter (transfer_year_index=1,transfer_month_index=2,transfer_day_index=3,transfer_hour_index=4,transfer_minute_index=5,transfer_datenum_index=6)
    parameter (transfer_num_date_index=6)
    
    !Set traffic input file indexes
    !vehicle clases
    integer transfer_he,transfer_li,transfer_num_veh
    parameter(transfer_he=1,transfer_li=2,transfer_num_veh=2)
    
    !tyre type
    integer transfer_st,transfer_wi,transfer_su,transfer_num_tyre
    parameter(transfer_st=1,transfer_wi=2,transfer_su=3,transfer_num_tyre=3)
    
    integer transfer_N_total_index,transfer_N_he_index,transfer_N_li_index
    integer transfer_N_st_he_index,transfer_N_wi_he_index,transfer_N_su_he_index
    integer transfer_N_st_li_index,transfer_N_wi_li_index,transfer_N_su_li_index
    integer transfer_V_he_index,transfer_V_li_index    
    integer transfer_num_traffic_index
    parameter (transfer_N_total_index=1,transfer_N_he_index=2,transfer_N_li_index=3)
    parameter (transfer_N_st_he_index=4,transfer_N_wi_he_index=5,transfer_N_su_he_index=6)
    parameter (transfer_N_st_li_index=7,transfer_N_wi_li_index=8,transfer_N_su_li_index=9)   
    parameter (transfer_V_he_index=10,transfer_V_li_index=11)
    parameter (transfer_num_traffic_index=11)
    integer transfer_N_v_index(transfer_num_veh)
    data (transfer_N_v_index(iiii),iiii=transfer_he,transfer_li) /transfer_N_he_index,transfer_N_li_index/
    integer transfer_N_t_v_index(transfer_num_tyre,transfer_num_veh)
    data ((transfer_N_t_v_index(iiii,jjjj),iiii=1,transfer_num_tyre),jjjj=1,transfer_num_veh) /transfer_N_st_he_index,transfer_N_wi_he_index,transfer_N_su_he_index,transfer_N_st_li_index,transfer_N_wi_li_index,transfer_N_su_li_index/ 
    integer transfer_V_veh_index(transfer_num_veh)
    data (transfer_V_veh_index(iiii),iiii=transfer_he,transfer_li) /transfer_V_he_index,transfer_V_li_index/ 

    !Set air quality input file indexes
    integer transfer_PM10_obs_index,transfer_PM10_bg_index,transfer_PM10_net_index
    integer transfer_PM25_obs_index,transfer_PM25_bg_index,transfer_PM25_net_index
    integer transfer_NOX_obs_index,transfer_NOX_bg_index,transfer_NOX_net_index
    integer transfer_NOX_emis_index,transfer_EP_emis_index,transfer_f_conc_index
    integer transfer_num_airquality_index
    parameter (transfer_PM10_obs_index=1,transfer_PM10_bg_index=2,transfer_PM10_net_index=3)
    parameter (transfer_PM25_obs_index=4,transfer_PM25_bg_index=5,transfer_PM25_net_index=6)
    parameter (transfer_NOX_obs_index=7,transfer_NOX_bg_index=8,transfer_NOX_net_index=9)
    parameter (transfer_NOX_emis_index=10,transfer_EP_emis_index=11,transfer_f_conc_index=12)
    parameter (transfer_num_airquality_index=12)
    integer transfer_PM_obs_index(transfer_num_size),transfer_PM_bg_index(transfer_num_size),transfer_PM_net_index(transfer_num_size)
    data (transfer_PM_obs_index(iiii),iiii=transfer_pm_10,transfer_pm_25) /transfer_PM10_obs_index,transfer_PM25_obs_index/
    data (transfer_PM_bg_index(iiii),iiii=transfer_pm_10,transfer_pm_25) /transfer_PM10_bg_index,transfer_PM25_bg_index/
    data (transfer_PM_net_index(iiii),iiii=transfer_pm_10,transfer_pm_25) /transfer_PM10_net_index,transfer_PM25_net_index/

    !Set extra date indexes for the activity file since this is not necessarilly in chronological order 
    integer transfer_activity_year_index,transfer_activity_month_index,transfer_activity_day_index,transfer_activity_hour_index,transfer_activity_minute_index
    integer transfer_num_activity_input_index
    parameter (transfer_activity_year_index=8,transfer_activity_month_index=9,transfer_activity_day_index=10,transfer_activity_hour_index=11,transfer_activity_minute_index=12)
    parameter (transfer_num_activity_input_index=12)

    logical :: transfer_available_date_data(transfer_num_date_index)=.false.
    logical :: transfer_available_traffic_data(transfer_num_traffic_index)=.false.
    logical :: transfer_available_meteo_data(transfer_num_meteo_index)=.false.
    logical :: transfer_available_activity_data(transfer_num_activity_input_index)=.false.
    logical :: transfer_available_airquality_data(transfer_num_airquality_index)=.false.

    !Order is (time_vector,time)
    integer, allocatable :: transfer_date_data(:,:)
    !Order is (variable_type,time,road)
    real, allocatable :: transfer_traffic_data(:,:,:)
    real, allocatable :: transfer_meteo_data(:,:,:)
    real, allocatable :: transfer_airquality_data(:,:,:)
    real, allocatable :: transfer_activity_data(:,:,:)
    real, allocatable :: transfer_activity_input_data(:,:,:)
    
    !path and file names
    character(256) transfer_filename_log
    character(256) transfer_path_inputparam
    character(256) transfer_filename_inputparam
    character(256) transfer_path_inputdata
    character(256) transfer_filename_inputdata
    character(256) transfer_path_outputdata
    character(256) transfer_filename_outputdata
    character(256) transfer_path_init
    character(256) transfer_filename_init
    character(256) transfer_path_output_emis
    character(256) transfer_filename_output_emis
    character(256) transfer_filename_output_grid_emis
    character(256) transfer_path_output_roadmeteo
    character(256) transfer_filename_output_roadmeteo
    character(256) transfer_path_outputfig
    character(256) transfer_path_ospm
    character(256) transfer_path_fortran
    character(256) transfer_path_fortran_output
    
!Road metadata allocatable to the number of roads
!-----------------------------------------------------------------------
    integer transfer_n_roads
    real transfer_nodata
    integer transfer_hours_between_init
    character(256) transfer_calculation_type
    
    real transfer_long_rad_in_offset
    real transfer_RH_offset
    real transfer_T_a_offset
    
    integer, allocatable :: transfer_d_index(:)
    integer, allocatable :: transfer_p_index(:)
    real, allocatable :: transfer_b_road(:)
    integer, allocatable :: transfer_n_lanes(:)
    real, allocatable :: transfer_b_road_lanes(:)
    real, allocatable :: transfer_b_lane(:)
    real, allocatable :: transfer_b_canyon(:)
    real, allocatable :: transfer_h_canyon(:,:) !Two sides, north and south
    real, allocatable :: transfer_ang_road(:)
    real, allocatable :: transfer_slope_road(:)
    integer, allocatable :: transfer_roadtype_index(:)

    real, allocatable :: transfer_LAT(:)
    real, allocatable :: transfer_LON(:)
    real, allocatable :: transfer_Z_SURF(:)
    real, allocatable :: transfer_z_FF(:)
    real, allocatable :: transfer_z_T(:)
    real, allocatable :: transfer_z2_T(:)
    real, allocatable :: transfer_albedo_road(:)
    real, allocatable :: transfer_DIFUTC_H(:)
    real, allocatable :: transfer_Pressure(:)

    !Correction factors
    real, allocatable :: transfer_wind_speed_correction(:)
    real, allocatable :: transfer_h_sus(:)
    real, allocatable :: transfer_h_texture(:)
    
    !OSPM factors
    integer, allocatable :: transfer_choose_receptor_ospm(:)
    real, allocatable :: transfer_SL1_ospm(:)
    real, allocatable :: transfer_SL2_ospm(:)
    real, allocatable :: transfer_f_roof_ospm(:)
    real, allocatable :: transfer_RecHeight_ospm(:)
    real, allocatable :: transfer_f_turb_ospm(:)
   
    !Single factors
    !real, allocatable :: nodata(:) !Already declared
    real, allocatable :: transfer_observed_moisture_cutoff_value(:)

    !Emission factors (num_veh,n_road)
    real, allocatable :: transfer_exhaust_EF(:,:)
    real, allocatable :: transfer_NOX_EF(:,:)
    
    integer, allocatable :: transfer_road_ID(:)
    integer, allocatable :: transfer_save_road_data_flag(:)
    integer, allocatable :: transfer_line_or_grid_data_flag(:) !1 is line, 2 is grid, 3 is both line and grid
    
    real, allocatable :: transfer_x_road(:,:)
    real, allocatable :: transfer_y_road(:,:)
    real, allocatable :: transfer_length_road(:)
        
    !Activity control flags allocatable to each road (road_type_activity_index,road)
    integer, allocatable :: transfer_road_type_activity_flag(:,:)
    
    !Grid data
    real :: transfer_grid_0(2)=0.
    real :: transfer_grid_delta(2)=0.
    integer :: transfer_grid_dim(2)=0
    !real :: grid_adt_cutoff(2)=0.
    logical :: transfer_grid_road_data_flag=.true.
  
!Skyview
!-----------------------------------------------------------------------
    integer :: transfer_n_skyview=0   !Number of skyview angles
    real, allocatable :: transfer_az_skyview(:,:)
    real, allocatable :: transfer_zen_skyview(:,:)

    integer transfer_road_type_salting_index,transfer_road_type_sanding_index,transfer_road_type_cleaning_index,transfer_road_type_ploughing_index,transfer_road_type_binding_index
    parameter (transfer_road_type_salting_index=1,transfer_road_type_sanding_index=2,transfer_road_type_cleaning_index=3,transfer_road_type_ploughing_index=4,transfer_road_type_binding_index=5)
    integer transfer_num_road_type_activity
    parameter (transfer_num_road_type_activity=5)   
    integer transfer_road_type_salt_index(2)
    data (transfer_road_type_salt_index(iiii),iiii=1,2) /transfer_road_type_salting_index,transfer_road_type_binding_index/

    !Initialdata
    real, allocatable :: transfer_M2_dust_road(:)
    real, allocatable :: transfer_M2_sand_road(:)
    real, allocatable :: transfer_M2_salt_road_1(:)
    real, allocatable :: transfer_M2_salt_road_2(:)
    real, allocatable :: transfer_water_road(:)
    real, allocatable :: transfer_snow_road(:)
    real, allocatable :: transfer_ice_road(:)

!Transfer activity data
!-----------------------------------------------------------------------

    !Auto activity data
    real, allocatable :: transfer_salting_hour(:,:)
    real, allocatable :: transfer_delay_salting_day(:)
    real, allocatable :: transfer_check_salting_day(:)
    real, allocatable :: transfer_min_temp_salt(:) 
    real, allocatable :: transfer_max_temp_salt(:)
    real, allocatable :: transfer_precip_rule_salt(:)
    real, allocatable :: transfer_RH_rule_salt(:) 
    real, allocatable :: transfer_g_salting_rule(:)
    real, allocatable :: transfer_salt_mass(:) 
    real, allocatable :: transfer_salt_dilution(:) 
    real, allocatable :: transfer_salt_type_distribution(:) 
    
    real, allocatable :: transfer_sanding_hour(:,:)
    real, allocatable :: transfer_delay_sanding_day(:) 
    real, allocatable :: transfer_check_sanding_day(:)
    real, allocatable :: transfer_min_temp_sand(:) 
    real, allocatable :: transfer_max_temp_sand(:)
    real, allocatable :: transfer_precip_rule_sand(:)
    real, allocatable :: transfer_RH_rule_sand(:) 
    real, allocatable :: transfer_g_sanding_rule(:) 
    real, allocatable :: transfer_sand_mass(:) 
    real, allocatable :: transfer_sand_dilution(:)
    
    real, allocatable :: transfer_delay_ploughing_hour(:)
    real, allocatable :: transfer_ploughing_thresh_2(:) 

    real, allocatable :: transfer_cleaning_hour(:,:)
    real, allocatable :: transfer_delay_cleaning_day(:)
    real, allocatable :: transfer_min_temp_cleaning(:)
    integer, allocatable :: transfer_clean_with_salting(:)
    real, allocatable :: transfer_start_month_cleaning(:)
    real, allocatable :: transfer_end_month_cleaning(:)
    real, allocatable :: transfer_wetting_with_cleaning(:)
    real, allocatable :: transfer_efficiency_of_cleaning(:)

    real, allocatable :: transfer_binding_hour(:,:)
    real, allocatable :: transfer_delay_binding_day(:)
    real, allocatable :: transfer_check_binding_day(:)
    real, allocatable :: transfer_min_temp_binding(:)
    real, allocatable :: transfer_max_temp_binding(:)
    real, allocatable :: transfer_precip_rule_binding(:)
    real, allocatable :: transfer_RH_rule_binding(:)
    real, allocatable :: transfer_g_binding_rule(:)
    real, allocatable :: transfer_binding_mass(:)
    real, allocatable :: transfer_binding_dilution(:)
    real, allocatable :: transfer_start_month_binding(:)
    real, allocatable :: transfer_end_month_binding(:)

    logical :: transfer_read_auto_activity_data=.false.

    end module