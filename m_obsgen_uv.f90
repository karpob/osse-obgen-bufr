module m_obsgen_uv

   implicit none
   save

   private

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


end module m_obsgen_uv
