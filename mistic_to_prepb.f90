program rscat_to_prepb

!  use netcdf
  use m_pbmin
  use m_pbutil
  use m_errtbl
  use m_obsgen_uv

  logical, parameter :: debug = .true.

  real lattest, lontest

  external  pxflocaltime
  external  gmtime
 
  integer n_along, n_cross
  integer n_along_id, n_cross_id

  integer ia, ic, i, j
  character*120 :: bfoutfn, uvfnin
  logical lbufropen
  integer strlen

!  real,parameter  :: typ=244.0
  real               :: typ
  character*4        :: ctyp

  character*80,parameter :: bufrtable='/discover/nobackup/projects/gmao/obsdev/bkarpowi/osseObsGen/osse-obgen-bundle/osse-obgen-bufr/qscat_tabl'
  character*80,parameter ::  errtable='/discover/nobackup/projects/gmao/obsdev/bkarpowi/osseObsGen/osse-obgen-bundle/osse-obgen-bufr/prepobs_errtable.global'

  real,parameter :: dtor=0.017453293

  character*80 :: dummy

  integer,parameter      :: lun_bfout = 25
  integer,parameter      :: lun_ncin  = 26

  integer ncid, varid
! time calculation variables
  integer :: ndate, cndate
  character*10 :: str_ndate

  real    :: window_time
!  integer,parameter :: nsec_1970_1999 = 915148800
!  integer           :: nsec
!  integer,dimension(9) :: itime
!  integer           :: ierr

! fields needed for prepbufrization:
  real,allocatable,dimension(:)        :: time
  integer*2,allocatable,dimension(:,:) :: qc_flag
  real*4                               :: clon,  clat,  cwspd,  cwdir,  ctime
  real*4                               :: cuwnd, cvwnd 
  real*4                               :: pres_oe, wind_oe
  integer                              :: lonid, latid, wspdid, wdirid, timeid, qc_flagid
  character*8                          :: stid
  character*8,parameter                :: MsgTyp = 'SATWND'
! superob arrays and variables 
  real*4,parameter :: deltall=0.5
  real*4,parameter :: min_obs_to_superob = 10.0
  integer        :: nobs

  integer :: anl_yyyy,anl_mm,anl_dd,anl_hh,anl_mn,anl_ss
  integer :: yyyy,mm,dd,hh,mn,ss
  integer,dimension(6) :: datetime
  real    :: lon, lat, lev, u,v

  call getarg(1,uvfnin)
  call getarg(2,str_ndate)
  call getarg(3,ctyp)

  read(ctyp,*) typ

  if (debug) print *,'Opening ',uvfnin
  call read_obsgen_uv(uvfnin)
  if (debug) print *,'Opened ',uvfnin

  nobs = get_nobs_obsgen_uv()
  if (debug) print *,'File contains ',nobs,' observations '

!!!  File is read, so let's start making the data file

  cndate = 0
  lbufropen = .false.

  if (debug) print *,'Opening error table ',errtable
  call init_etable(errtable)

  read(str_ndate,fmt='(I10)')ndate
  read(str_ndate,fmt='(I4,I2,I2,I2)')anl_yyyy,anl_mm,anl_dd,anl_hh
  anl_mm=0
  anl_ss=0
  bfoutfn = trim(uvfnin)
  cndate=ndate
!  strlen = len(trim(ncinfn))
  bfoutfn = trim(bfoutfn) // '.bufr'
  write(*,*)'Opening BUFR file: ' // trim(bfoutfn) // ' for writing as type:',typ
  !initbufr
  call init_bufr(bfoutfn, tablefile=bufrtable) !, bfrunit=lun_bfout)
  lbufropen=.true.


!    do ic=1,n_cross
!      if (qc_flag(ic,ia) .eq. 0) then
!        stid = 'DUMM1234'
!!       stid = QSia
!        clat = lat(ic,ia)
!        clon = lon(ic,ia)
!
!        call get_ij_from_ll(clon, clat, deltall, i, j)
!
!!        pres = 1013.0
!!        call getoe(pres, 6., typ, pres_oe, woe=wind_oe)
!        cwspd = wspd(ic, ia)
!        cwdir = wdir(ic, ia)
!        uwnd = cwspd * sin(cwdir * dtor)
!        vwnd = cwspd * cos(cwdir * dtor)
!
!        so_uwnd(i,j) = so_uwnd(i,j) + uwnd
!        so_vwnd(i,j) = so_vwnd(i,j) + vwnd
!        so_time(i,j) = so_time(i,j) + window_time
!        so_lon(i,j)  = so_lon(i,j)  + clon
!        so_lat(i,j)  = so_lat(i,j)  + clat
!        so_nob(i,j)  = so_nob(i,j)  + 1.0
!!        call write_cat6_wind(stid, clon, clat, window_time, &
!!                             typ, pres, uwnd, vwnd, 2., 2., &
!!                             pres_oe, wind_oe, MsgTyp, ndate)
!      endif
!    enddo
!  enddo

!  do i=1, nlon
!     do j=1, nlat
!        if (so_nob(i,j) .ge. min_obs_to_superob) then

  if (debug) print *,'Writing Observations'
  do i=1,nobs
    stid = 'DUMM1234'

!    if (debug) print *,'Getting uv obs', i
    call get_datetime_obsgen_uv(yyyy,mm,dd,hh,mn,ss)
    datetime(1) = yyyy
    datetime(2) =   mm 
    datetime(3) =   dd
    datetime(4) =   hh 
    datetime(5) =   mn
    datetime(6) =   ss
    call get_window_time(datetime, window_time)


    call get_lonlatlev_obsgen_uv(lon, lat, lev)
    call get_uv_obsgen_uv(u,v)

    clon = lon
    clat = lat


    pres = lev

    call getoe(pres, 6., typ, pres_oe, woe=wind_oe)

    uwnd = u
    vwnd = v

    call write_cat6_wind(stid, clon, clat, window_time, &
                         typ, pres, uwnd, vwnd, 2., 2., &
                         pres_oe, wind_oe, MsgTyp, ndate)
    call cycle_ob_obsgen_uv()
  enddo

  call end_bufr()

contains

  subroutine get_ij_from_ll(lon,lat,delta,i,j)
    real*4,    intent(in   )   :: lon, lat, delta
    integer,    intent(  out)   :: i, j
    real*4,    parameter       :: lon0 = 0.0
    real*4,    parameter       :: lat0 = -90.0 

    j = (lat - lat0)/delta + 1
    i = (lon - lon0)/delta + 1

  end subroutine get_ij_from_ll

  subroutine get_nlon_nlat(delta,nlon,nlat)
    real*4,    intent(in   )      :: delta
    integer,    intent(  out)   :: nlon, nlat
    
!    real*4,parameter = lonmax = 
!    call get_ij_from_ll(89.999,359.999,delta,nlon,nlat)
    call get_ij_from_ll(359.999,89.999,delta,nlon,nlat)


  end subroutine get_nlon_nlat

  subroutine get_window_time(datetime, window_time)
    integer,intent(in   ),dimension(6)   :: datetime ! assumed sec since 1-1-1999
    real,    intent(  out)   :: window_time

    integer           :: anlhr
    if (datetime(4) .ge. 3 .and. datetime(4) .lt. 9) then
       anlhr = 6
       window_time = (datetime(4) + datetime(5)/60.0) - anlhr
    else if (datetime(4) .ge. 9 .and. datetime(4) .lt. 15) then
       anlhr = 12
       window_time = (datetime(4) + datetime(5)/60.0) - anlhr
    else if (datetime(4) .ge. 15 .and. datetime(4) .lt. 21) then
       anlhr = 18
       window_time = (datetime(4) + datetime(5)/60.0) - anlhr
    else if (datetime(4) .ge. 21 .and. datetime(4) .lt. 24) then
       anlhr = 24
       window_time = (datetime(4) + datetime(5)/60.0) - anlhr
       anlhr = 0
    else if (datetime(4) .ge. 0 .and. datetime(4) .lt. 3) then
       anlhr = 0
       window_time = (datetime(4) + datetime(5)/60.0) - anlhr
    else
       print *,'Problem Determining anl ndate & time, dying'
       call abort
    endif


  end subroutine get_window_time
  subroutine get_times(nsec_in, ndate, window_time)
    real,    intent(in   )   :: nsec_in ! assumed sec since 1-1-1999
    integer, intent(  out)   :: ndate
    real,    intent(  out)   :: window_time
!   time calculation variables
    integer,parameter :: nsec_1970_1999 = 915148800
    integer           :: nsec
    integer,dimension(9) :: itime
    integer           :: anlhr
    integer           :: tick 
    nsec = nsec_in + nsec_1970_1999
    call gmtime(nsec,itime)

    tick = 0

    if (itime(3) .ge. 3 .and. itime(3) .lt. 9) then
       anlhr = 6
       window_time = (itime(3) + itime(2)/60.0) - anlhr
    else if (itime(3) .ge. 9 .and. itime(3) .lt. 15) then
       anlhr = 12
       window_time = (itime(3) + itime(2)/60.0) - anlhr
    else if (itime(3) .ge. 15 .and. itime(3) .lt. 21) then
       anlhr = 18
       window_time = (itime(3) + itime(2)/60.0) - anlhr
    else if (itime(3) .ge. 21 .and. itime(3) .lt. 24) then
       anlhr = 24
       window_time = (itime(3) + itime(2)/60.0) - anlhr
       anlhr = 0 
       tick = 24*60*60
    else if (itime(3) .ge. 0 .and. itime(3) .lt. 3) then
       anlhr = 0
       window_time = (itime(3) + itime(2)/60.0) - anlhr
    else
       print *,'Problem Determining anl ndate & time, dying'
       call abort
    endif

    if (tick .gt. 0) call gmtime((nsec+tick),itime)
!    print *,itime
!    print *,(1900 + itime(6))*1000000
!    print *,(itime(5)+1)*10000 
!    print *, itime(4)*100
!    print *,anlhr
    ndate = (1900 + itime(6))*1000000 + (itime(5)+1)*10000 + itime(4)*100 + anlhr
  end subroutine get_times
!  subroutine check(status)
!    integer, intent ( in) :: status
!    
!    if(status /= nf90_noerr) then 
!      print *, trim(nf90_strerror(status))
!      stop "Stopped"
!    end if
!  end subroutine check  

end  
