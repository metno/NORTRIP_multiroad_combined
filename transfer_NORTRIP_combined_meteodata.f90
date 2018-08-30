    subroutine transfer_preprocessor_to_combined_meteodata
    
    use NORTRIP_multiroad_index_definitions
    use NORTRIP_multiroad_combined_definitions
    
    implicit none
    
    integer i,t,jj
    integer unit_in
    
    if (.not.allocated(transfer_meteo_data)) allocate(transfer_meteo_data(transfer_num_meteo_index,transfer_n_time,0:transfer_n_roads))

    !Open the file for writing
    unit_in=30
    open(unit_in,file=pathfilename_meteo,access='sequential',status='unknown')  
    
    write(unit_logfile,'(a)') ' Transfering meteodata to NORTRIP '//trim(pathfilename_meteo)

    !Write header
    write(unit_logfile,'(27a)') 'Road_number',achar(9),'Station_number',achar(9),'Time',achar(9),'T2m',achar(9),'FF',achar(9),'DD',achar(9),'RH',achar(9), &
        'Rain',achar(9),'Snow',achar(9),'Global radiation',achar(9),'Longwave radiation',achar(9),'Cloud cover',achar(9),'Pressure',achar(9),'Road surface temperature'
    
        
!Distribute meteo data to roadlinks. Saves all links or specified links.
        do jj=1,n_save_links
        i=save_links(jj)
               
        if ((inputdata_int_rl(savedata_rl_index,i).eq.1.and.use_only_special_links_flag.ge.1) &
            .or.(use_only_special_links_flag.eq.0).or.(use_only_special_links_flag.eq.2)) then
        
            do t=1,transfer_n_time
                
                transfer_meteo_data(transfer_T_a_index,t,i)=meteo_output(temperature_index,t,i)
                transfer_meteo_data(transfer_FF_index,t,i)=meteo_output(speed_wind_index,t,i)
                transfer_meteo_data(transfer_DD_index,t,i)=meteo_output(dir_wind_index,t,i)
                transfer_meteo_data(transfer_RH_index,t,i)=meteo_output(relhumidity_index,t,i)
                transfer_meteo_data(transfer_Rain_precip_index,t,i)=meteo_output(rain_index,t,i)
                transfer_meteo_data(transfer_Snow_precip_index,t,i)=meteo_output(snow_index,t,i)
                transfer_meteo_data(transfer_short_rad_in_index,t,i)=meteo_output(shortwaveradiation_index,t,i)
                transfer_meteo_data(transfer_long_rad_in_index,t,i)=meteo_output(longwaveradiation_index,t,i)
                transfer_meteo_data(transfer_cloud_cover_index,t,i)=meteo_output(cloudfraction_index,t,i)
                transfer_meteo_data(transfer_pressure_index,t,i)=meteo_output(pressure_index,t,i)
                transfer_meteo_data(transfer_road_temperature_obs_input_index,t,i)=meteo_output(road_temperature_index,t,i)

            enddo

        endif
        enddo
    
                transfer_available_meteo_data(transfer_T_a_index)=.true.
                transfer_available_meteo_data(transfer_FF_index)=.true.
                transfer_available_meteo_data(transfer_DD_index)=.true.
                transfer_available_meteo_data(transfer_RH_index)=.true.
                transfer_available_meteo_data(transfer_Rain_precip_index)=.true.
                transfer_available_meteo_data(transfer_Snow_precip_index)=.true.
                transfer_available_meteo_data(transfer_short_rad_in_index)=.true.
                transfer_available_meteo_data(transfer_long_rad_in_index)=.true.
                transfer_available_meteo_data(transfer_cloud_cover_index)=.true.
                transfer_available_meteo_data(transfer_pressure_index)=.true.
                transfer_available_meteo_data(transfer_road_temperature_obs_input_index)=.false.
    
       if (allocated(meteo_output)) deallocate (meteo_output)

    end subroutine transfer_preprocessor_to_combined_meteodata
    
    subroutine transfer_combined_to_NORTRIP_meteodata
    
    use NORTRIP_definitions
    use NORTRIP_multiroad_combined_definitions
    
    implicit none
    
    if (.not.allocated(meteo_data)) allocate(meteo_data(num_meteo_index,n_time,0:n_roads))
    
    meteo_data=transfer_meteo_data
    available_meteo_data=transfer_available_meteo_data
    
    !Set road wetness max and min. Needs to be rethought. Not used
    if (available_meteo_data(road_wetness_obs_input_index)) then
        max_road_wetness_obs=maxval(meteo_data(road_wetness_obs_input_index,:,1:n_roads))
        min_road_wetness_obs=minval(meteo_data(road_wetness_obs_input_index,:,1:n_roads))
    else
        max_road_wetness_obs=nodata
        min_road_wetness_obs=nodata
    endif

    !Set some physical limits on the meteorological data. None of it can be nodata
    do ro=1,n_roads
    do ti=1,n_time
        meteo_data(RH_index,ti,ro)=min(max(meteo_data(RH_index,ti,ro),0.),100.)
        meteo_data(FF_index,ti,ro)=max(meteo_data(FF_index,ti,ro),0.)
        meteo_data(DD_index,ti,ro)=min(max(meteo_data(DD_index,ti,ro),0.),360.)
        meteo_data(Rain_precip_index,ti,ro)=max(meteo_data(Rain_precip_index,ti,ro),0.)
        meteo_data(Snow_precip_index,ti,ro)=max(meteo_data(Snow_precip_index,ti,ro),0.)
        !write(*,'(5f12.2)') meteo_data(T_a_index,ti,ro),meteo_data(RH_index,ti,ro),meteo_data(FF_index,ti,ro),meteo_data(Rain_precip_index,ti,ro),meteo_data(Snow_precip_index,ti,ro)
    enddo
    enddo

    
    if (allocated(transfer_meteo_data)) deallocate (transfer_meteo_data)
    
    end subroutine transfer_combined_to_NORTRIP_meteodata
