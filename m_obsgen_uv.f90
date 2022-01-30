module m_obsgen_uv

   use netcdf
   implicit none
   save

   private


   public :: read_uv_nc
   public :: read_obsgen_uv
   public :: get_nobs_obsgen_uv
   public :: cycle_ob_obsgen_uv
   public :: get_datetime_obsgen_uv
   public :: get_lonlatlev_obsgen_uv
   public :: get_uv_obsgen_uv
   real,dimension(:,:),allocatable :: lonlat 
   real,dimension(:),allocatable   :: level, uwnd, vwnd
   integer,dimension(:,:),allocatable :: datetime ! YYYY,MM,DD,HH,MIN,SS


   integer                         :: nobs = -9999
   integer                         :: current_ob = -9999
  

contains

   subroutine read_obsgen_uv(fn)
      character*120,intent(in) :: fn

      integer,parameter ::    lun=789

      integer,dimension(6)        ::    clinetime
      real,dimension(2)           ::    clinell
      real                        ::    clinelevel,clineuwnd,clinevwnd

      integer           ::    iline

      open(unit=789,file=fn,form='formatted')

      nobs = 0

      do 
         read(unit=lun,end=10,fmt='(120x)')
         nobs = nobs + 1
      enddo
   10 rewind(lun)       

      allocate(  lonlat(2,nobs),  &
                 datetime(6,nobs),&
                 level(nobs)     ,&
                 uwnd(nobs)      ,&
                 vwnd(nobs)       )

      do iline=1,nobs
         read(unit=lun,fmt='(6(I4,2X),5F12.3 )')clinetime,clinell,clinelevel,clineuwnd,clinevwnd

         datetime(:,iline) = clinetime
         lonlat(:,iline) = clinell
         level(iline)    = clinelevel
         uwnd(iline)     = clineuwnd
         vwnd(iline)     = clinevwnd
      enddo

      close(lun)

      current_ob = 1

   end subroutine read_obsgen_uv

   function get_nobs_obsgen_uv()
      integer get_nobs_obsgen_uv
      get_nobs_obsgen_uv = nobs
   end function get_nobs_obsgen_uv

   subroutine cycle_ob_obsgen_uv
      if (current_ob .eq. -9999) then
         print *,'ERROR: Sattrack not initialized'
         call abort
      endif

      current_ob = current_ob + 1
   end subroutine cycle_ob_obsgen_uv

   subroutine get_datetime_obsgen_uv(yyyy,mm,dd,hh,mn,ss)
      integer, intent(out) :: yyyy,mm,dd,hh,mn,ss
      yyyy = datetime(1,current_ob)
      mm   = datetime(2,current_ob)
      dd   = datetime(3,current_ob)
      hh   = datetime(4,current_ob)
      mn   = datetime(5,current_ob)
      ss   = datetime(6,current_ob)
   end subroutine get_datetime_obsgen_uv

   subroutine get_lonlatlev_obsgen_uv(lon, lat, lev)
      real,intent(out) :: lon, lat, lev

      lon = lonlat(1,current_ob)
      lat = lonlat(2,current_ob)
      lev = level(current_ob)

   end subroutine get_lonlatlev_obsgen_uv

   subroutine get_uv_obsgen_uv(u,v)
      real,intent(out) :: u, v

      u = uwnd(current_ob)
      v = vwnd(current_ob)

   end subroutine get_uv_obsgen_uv


  subroutine read_dimension(ob_ncid, dim_name, nout)
  !use netcdf, only: nf90_noerr,nf90_inq_dimid,nf90_inquire_dimension
  implicit none
  integer, intent(in) :: ob_ncid
  character(len=*), intent(in) :: dim_name
  integer, intent(out) :: nout
  integer :: ob_err,ob_did

  ob_err = NF90_INQ_DIMID(ob_ncid,dim_name,ob_did)
  if(ob_err /= nf90_noerr) then
    write(*,*),'read_gmi_emissivity: no',dim_name,'dimension.'
    stop 13 
  endif 
  ob_err = nf90_inquire_dimension(ob_ncid, ob_did, len = nout)
  if(ob_err /= nf90_noerr) then
    write(*,*),'read_gmi_emissivity: error read',dim_name,'dimension.'
    stop 13 
  endif 

  end subroutine read_dimension

  subroutine read_variable1d(ob_ncid, var_name, varout)
  !use netcdf, only: nf90_noerr,nf90_inq_varid,nf90_get_var

  implicit none
  integer, intent(in) :: ob_ncid
  character(len=*), intent(in) :: var_name
  real, intent(inout),dimension(:) :: varout
  !local 
  integer :: ob_varid, ob_err

  ob_err = nf90_inq_varid( ob_ncid, var_name, ob_varid )
  if(ob_err /= nf90_noerr) then
    write(*,*),'read_uv: error no',var_name
    stop 13 
  endif 

  ob_err = nf90_get_var( ob_ncid, ob_varid, varout) 
  if(ob_err /= nf90_noerr) then
    write(*,*),'read_uv: error read ',var_name
    stop 13 
  endif  
  end subroutine read_variable1d

  subroutine read_variable2d(ob_ncid, var_name, varout)
  !use netcdf, only: nf90_noerr,nf90_inq_varid,nf90_get_var

  implicit none
  integer, intent(in) :: ob_ncid
  character(len=*), intent(in) :: var_name
  real, intent(inout),dimension(:,:) :: varout
  !local 
  integer :: ob_varid, ob_err

  ob_err = nf90_inq_varid( ob_ncid, var_name, ob_varid )
  if(ob_err /= nf90_noerr) then
    write(*,*),'read_uv: error no',var_name
    stop 13 
  endif 

  ob_err = nf90_get_var( ob_ncid, ob_varid, varout) 
  if(ob_err /= nf90_noerr) then
    write(*,*),'read_uv: error read ',var_name
    stop 13 
  endif  
  end subroutine read_variable2d

  subroutine read_variable1d_asc(ob_ncid, var_name, varout)
  !use netcdf, only: nf90_noerr,nf90_inq_varid,nf90_get_var

  implicit none
  integer, intent(in) :: ob_ncid
  character(len=*), intent(in) :: var_name
  character, intent(inout),dimension(:) :: varout
  !local 
  integer :: ob_varid, ob_err

  ob_err = nf90_inq_varid( ob_ncid, var_name, ob_varid )
  if(ob_err /= nf90_noerr) then
    write(*,*),'read_uv: error no',var_name
    stop 13 
  endif 

  ob_err = nf90_get_var( ob_ncid, ob_varid, varout) 
  if(ob_err /= nf90_noerr) then
    write(*,*),'read_uv: error read ',var_name
    stop 13 
  endif  
  end subroutine read_variable1d_asc


  subroutine read_uv_nc(f)
  !modules (netcdf, etc.)
  !use netcdf, only: nf90_open,nf90_nowrite,nf90_noerr,nf90_close
  implicit none
  !interface variables
  character(len=*), intent(in) :: f
  integer :: ntime, nlev, nls, nchan, ob_err, ob_ncid
  real,allocatable, dimension(:,:) ::ob_u, ob_v, ob_p
  real,allocatable, dimension(:) :: ob_Longitude, ob_Latitude
  
  ! open the file
  ob_err = nf90_open(f, NF90_NOWRITE, ob_ncid) 
  if(ob_err /= nf90_noerr) then
    write(*,*),'read_uv: error reading file',f
    stop 13 
  endif 

    ! get netcdf dimensions
  call read_dimension(ob_ncid,'time',ntime)
  call read_dimension(ob_ncid,'lev',nlev)
  call read_dimension(ob_ncid,'ls',nls)
  !write(*,*)'nlat,nlon,nchan,nmonth',nlat,nlon,nchan,nmonth
  ! allocate variables needed to read netcdf
  allocate( ob_Longitude(ntime),stat = ob_err )
  if(ob_err /= 0) then
    write(*,*),'read_uv: error allocate Longitude'
    stop 13 
  endif 

  allocate( ob_Latitude(ntime),stat = ob_err )
  if(ob_err /= 0) then
    write(*,*),'read_uv: error allocate Latitude'
    stop 13 
  endif 

  allocate( ob_u(nlev,ntime),stat=ob_err )
  if(ob_err /= 0) then
    write(*,*),'read_uv: error allocate u'
    stop 13 
  endif 

  allocate( ob_v(nlev,ntime),stat=ob_err )
  if(ob_err /= 0) then
    write(*,*),'read_uv: error allocate v'
    stop 13 
  endif 

  allocate( ob_p(nlev,ntime),stat=ob_err )
  if(ob_err /= 0) then
    write(*,*),'read_uv: error allocate p'
    stop 13 
  endif 


  !read netcdf variables 
  call read_variable1d(ob_ncid, 'trjLat',ob_Longitude)
  call read_variable1d(ob_ncid, 'trjLon',ob_Latitude)

  call read_variable2d(ob_ncid, 'U',ob_u)
  call read_variable2d(ob_ncid, 'V',ob_v)
  call read_variable2d(ob_ncid, 'DELP',ob_p)


  end subroutine read_uv_nc

end module m_obsgen_uv
