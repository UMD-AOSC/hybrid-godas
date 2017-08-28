module hzsmooth
  implicit none

  private

  public :: hzsmooth_gauss

  ! constants
  real, parameter :: pi = 4*atan(1.0)
  real, parameter :: re = 6371d3
  real, parameter :: omega = 7.29e-5



contains


   
  subroutine hzsmooth_gauss(data, grid_lat, grid_lon, grid_btm, grid_hzloc, gauss_iter)
    real,    intent(inout) :: data(:,:,:)
    real,    intent(in)    :: grid_lat(:,:)
    real,    intent(in)    :: grid_lon(:,:)
    integer, intent(in)    :: grid_btm(:,:)
    real,    intent(in)    :: grid_hzloc(:,:)
    integer, intent(in)    :: gauss_iter
    
    real :: ll_a(size(data,1),size(data,2))
    real :: ll_b(size(data,1),size(data,2))
    real :: ll_c(size(data,1),size(data,2))

    real :: data2(size(data,1),size(data,2),size(data,3))
    real :: gb_s(size(data,3))
    real :: gb_v(size(data,3))
    integer :: itr, i, x, y, x2, y2
    real :: d, r
    integer :: btm

    INTEGER ::  OMP_GET_NUM_THREADS,  OMP_GET_THREAD_NUM


    ! calculate lat/lon constants
!$OMP PARALLEL DO PRIVATE(x) SCHEDULE(static, 1)
    do y=1,size(data,2)
       do x=1,size(data,1)
          ll_a(x,y) = sin(grid_lat(x,y)*pi/180.0)
          ll_b(x,y) = cos(grid_lat(x,y)*pi/180.0)*cos(grid_lon(x,y)*pi/180.0)
          ll_c(x,y) = cos(grid_lat(x,y)*pi/180.0)*sin(grid_lon(x,y)*pi/180.0)
       end do
    end do
!$OMP END PARALLEL DO
    
    
    do itr=1,gauss_iter
       
       ! horizontal blur
       !------------------------------
       data2 = data
       data = 0.0
!$OMP PARALLEL DO PRIVATE(x, gb_s, gb_v, i, x2, d, r, btm) SCHEDULE(static,1)
       do y=1,size(data,2)
          do x=1,size(data,1)
             if(grid_btm(x,y) == 0) cycle
             gb_s = 1.0
             gb_v = data2(x,y,:)

             ! do the following loop twice, once for right (i==1) and left (i==-1)
             do i=1,-1,-2
                x2 = x + i
                do while(x2 /= x)
                   ! wrap in the x direction
                   if(i==1) then
                      if (x2 > size(data,1)) x2 = 1
                   else
                      if (x2 < 1) x2 = size(data,1)
                   end if                   

                   d = lldist(x,y,x2,y)
                   r = loc_gc(d, grid_hzloc(x,y))
                   if (r == 0) exit
                   if(grid_btm(x2,y) /= 0) then
                      btm = grid_btm(x2,y)
                      gb_v = gb_v + data2(x2,y,:)*r
                      gb_s(1:btm) = gb_s(1:btm) + r
                   end if
                   x2 = x2 + i
                end do
                ! if we looped back to the original point, dont bother doing the loop
                ! in the other direction
                if(x2 == x) exit 
             end do
             btm = grid_btm(x,y)
             data(x,y,1:btm) = gb_v(1:btm)/gb_s(1:btm)
          end do
       end do
!$OMP END PARALLEL DO
 
      
       ! vertical blur
       !------------------------------
       data2 = data
       data = 0.0
!$OMP PARALLEL DO PRIVATE(x, gb_s, gb_v, i, y2, d, r, btm) SCHEDULE(static,1)
       do y=1,size(data,2)
          do x=1,size(data,1)
             if(grid_btm(x,y) <= 0.0) cycle
             gb_s=1.0
             gb_v=data2(x,y,:)

             ! do the following loop twice, once for up (i==1) and down (i==-1)
             do i=1,-1,-2 
                y2 = y + i
                do while(y2 <= size(data,2) .and. y2 >= 1)
                   d = lldist(x,y,x,y2)
                   r = loc_gc(d, (grid_hzloc(x,y) + grid_hzloc(x,y2))/2.0)
                   if (r == 0) exit
                   if(grid_btm(x,y2) /= 0) then
                      btm = grid_btm(x,y2)
                      gb_v = gb_v + data2(x,y2,:)*r
                      gb_s(1:btm) = gb_s(1:btm) + r
                   end if
                   y2 = y2 + i
                end do
             end do
             btm = grid_btm(x,y)              
             data(x,y,1:btm) = gb_v(1:btm)/gb_s(1:btm)
          end do
       end do
!$OMP END PARALLEL DO        

    end do


  contains


    pure function lldist(x1,y1,x2,y2) result(d)
      integer, intent(in) :: x1,y1,x2,y2
      real :: d, r
      r = ll_a(x1,y1)*ll_a(x2,y2) + ll_b(x1,y1)*ll_b(x2,y2) + ll_c(x1,y1)*ll_c(x2,y2)
      d = re*acos(r)
    end function lldist   
    
  end subroutine hzsmooth_gauss
  


  pure function loc_gc(z, L)
    real, intent(in) :: z
    real, intent(in) :: L
    real :: loc_gc
    
    real :: c
    real :: abs_z, z_c
    
    c = L / sqrt(0.3)
    abs_z = abs(z)
    z_c = abs_z / c
    
    if (abs_z >= 2*c) then
       loc_gc = 0.0
    elseif (abs_z < 2*c .and. abs_z > c) then
       loc_gc = &
            (1.0/12.0)*z_c**5 - 0.5*z_c**4 + &
            (5.0/8.0)*z_c**3 + (5.0/3.0)*z_c**2 &
            - 5.0*z_c + 4 - (2.0/3.0)*c/abs_z
    else
       loc_gc = &
            -0.25*z_c**5 + 0.5*z_c**4 + &
            (5.0/8.0)*z_c**3 - (5.0/3.0)*z_c**2 + 1
    end if
  end function loc_gc



end module hzsmooth
