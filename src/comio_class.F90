module comio_class

  use ioerr_class
  use io_prec

  implicit none

  type, abstract :: COMIO_T

    private
    integer :: fmt

    type(IOERR_T), public :: err

  contains

    ! -- Public APIs
    generic   :: startup  => io_initialize
    generic   :: shutdown => io_finalize
    generic   :: open     => io_file_open
    generic   :: close    => io_file_close
    generic   :: domain   => io_domain_set
    generic   :: read     => io_dataset_read_1d_int, &
                             io_dataset_read_2d_int, &
                             io_dataset_read_3d_int, &
                             io_dataset_read_1d_sp,  &
                             io_dataset_read_2d_sp,  &
                             io_dataset_read_3d_sp,  &
                             io_dataset_read_1d_dp,  &
                             io_dataset_read_2d_dp,  &
                             io_dataset_read_3d_dp
    generic   :: write    => io_dataset_write_1d_int, &
                             io_dataset_write_2d_int, &
                             io_dataset_write_3d_int, &
                             io_dataset_write_1d_sp,  &
                             io_dataset_write_2d_sp,  &
                             io_dataset_write_3d_sp,  &
                             io_dataset_write_1d_dp,  &
                             io_dataset_write_2d_dp,  &
                             io_dataset_write_3d_dp

    procedure :: pause    => io_pause
    procedure :: version

    procedure, private :: io_initialize
    procedure, private :: io_finalize

    procedure, private :: io_file_open
    procedure, private :: io_file_close
    procedure, private :: io_domain_set
    procedure, private :: io_pause

    procedure, private :: io_dataset_read_1d_int, &
                          io_dataset_read_2d_int, &
                          io_dataset_read_3d_int, &
                          io_dataset_read_1d_sp,  &
                          io_dataset_read_2d_sp,  &
                          io_dataset_read_3d_sp,  &
                          io_dataset_read_1d_dp,  &
                          io_dataset_read_2d_dp,  &
                          io_dataset_read_3d_dp
    procedure, private :: io_dataset_write_1d_int, &
                          io_dataset_write_2d_int, &
                          io_dataset_write_3d_int, &
                          io_dataset_write_1d_sp,  &
                          io_dataset_write_2d_sp,  &
                          io_dataset_write_3d_sp,  &
                          io_dataset_write_1d_dp,  &
                          io_dataset_write_2d_dp,  &
                          io_dataset_write_3d_dp

  end type

  private
  public :: COMIO_T, IOERR_T

contains

  ! -- Versioning

  subroutine version(io)
    class(COMIO_T) :: io
    write(6,'("COMIO Version ",a)') PACKAGE_VERSION
    write(6,'("Supports:")')
#ifdef HAVE_HDF5
    write(6,'(" - HDF5 Version ",a)') HDF5_VERSION
#endif
#ifdef HAVE_PNETCDF
    write(6,'(" - PnetCDF Version ",a)') PNETCDF_VERSION
#endif
    write(6,'("Report bugs to ",a)') PACKAGE_BUGREPORT
  end subroutine version

  subroutine io_initialize(io, comm, info)
    class(COMIO_T)                 :: io
    integer, optional, intent(in)  :: comm
    integer, optional, intent(in)  :: info
    io % err % rc = io % err % success
  end subroutine io_initialize

  subroutine io_finalize(io)
    class(COMIO_T) :: io
    io % err % rc = io % err % success
  end subroutine io_finalize

  ! -- File access APIs

  subroutine io_file_open(io, filename, mode)
    class(COMIO_T)               :: io
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: mode
    io % err % rc = io % err % success
  end subroutine io_file_open

  subroutine io_file_close(io)
    class(COMIO_T) :: io
    io % err % rc = io % err % success
  end subroutine io_file_close

  ! -- Data layout APIs

  subroutine io_domain_set(io, fdims, mstart, mcount)
    class(COMIO_T)                    :: io
    integer, dimension(:), intent(in) :: fdims
    integer, dimension(:), intent(in) :: mstart
    integer, dimension(:), intent(in) :: mcount
    io % err % rc = io % err % success
  end subroutine io_domain_set

  ! -- Basic I/O APIs

  ! -- pause I/O on local task
  subroutine io_pause(io, flag)
    class(COMIO_T)      :: io
    logical, intent(in) :: flag
    io % err % rc = io % err % success
  end subroutine io_pause

  ! -- Dataset I/O APIs

  ! -- reading:
  ! -- integer
  ! -- * 1D
  subroutine io_dataset_read_1d_int(io, dsetname, buffer)
    class(COMIO_T)                :: io
    character(len=*), intent(in)  :: dsetname
    integer,          intent(out) :: buffer(:)
    buffer = 0
    io % err % rc = io % err % success
  end subroutine io_dataset_read_1d_int

  ! -- * 2D
  subroutine io_dataset_read_2d_int(io, dsetname, buffer)
    class(COMIO_T)                :: io
    character(len=*), intent(in)  :: dsetname
    integer,          intent(out) :: buffer(:,:)
    buffer = 0
    io % err % rc = io % err % success
  end subroutine io_dataset_read_2d_int

  ! -- * 3D
  subroutine io_dataset_read_3d_int(io, dsetname, buffer)
    class(COMIO_T)                :: io
    character(len=*), intent(in)  :: dsetname
    integer,          intent(out) :: buffer(:,:,:)
    buffer = 0
    io % err % rc = io % err % success
  end subroutine io_dataset_read_3d_int

  ! -- floating point
  ! -- * 1D
  subroutine io_dataset_read_1d_sp(io, dsetname, buffer)
    class(COMIO_T)                :: io
    character(len=*), intent(in)  :: dsetname
    real(sp),         intent(out) :: buffer(:)
    buffer = 0._sp
    io % err % rc = io % err % success
  end subroutine io_dataset_read_1d_sp

  ! -- * 2D
  subroutine io_dataset_read_2d_sp(io, dsetname, buffer)
    class(COMIO_T)                :: io
    character(len=*), intent(in)  :: dsetname
    real(sp),         intent(out) :: buffer(:,:)
    buffer = 0._sp
    io % err % rc = io % err % success
  end subroutine io_dataset_read_2d_sp

  ! -- * 3D
  subroutine io_dataset_read_3d_sp(io, dsetname, buffer)
    class(COMIO_T)                :: io
    character(len=*), intent(in)  :: dsetname
    real(sp),         intent(out) :: buffer(:,:,:)
    buffer = 0._sp
    io % err % rc = io % err % success
  end subroutine io_dataset_read_3d_sp

  ! -- double
  ! -- * 1D
  subroutine io_dataset_read_1d_dp(io, dsetname, buffer)
    class(COMIO_T)                :: io
    character(len=*), intent(in)  :: dsetname
    real(dp),         intent(out) :: buffer(:)
    buffer = 0._dp
    io % err % rc = io % err % success
  end subroutine io_dataset_read_1d_dp

  ! -- * 2D
  subroutine io_dataset_read_2d_dp(io, dsetname, buffer)
    class(COMIO_T)                :: io
    character(len=*), intent(in)  :: dsetname
    real(dp),         intent(out) :: buffer(:,:)
    buffer = 0._dp
    io % err % rc = io % err % success
  end subroutine io_dataset_read_2d_dp

  ! -- * 3D
  subroutine io_dataset_read_3d_dp(io, dsetname, buffer)
    class(COMIO_T)                :: io
    character(len=*), intent(in)  :: dsetname
    real(dp),         intent(out) :: buffer(:,:,:)
    buffer = 0._dp
    io % err % rc = io % err % success
  end subroutine io_dataset_read_3d_dp

  ! -- writing:
  ! -- integer
  ! -- * 1D
  subroutine io_dataset_write_1d_int(io, dsetname, buffer)
    class(COMIO_T)                  :: io
    character(len=*), intent(in)    :: dsetname
    integer,          intent(inout) :: buffer(:)
    io % err % rc = io % err % success
  end subroutine io_dataset_write_1d_int

  subroutine io_dataset_write_2d_int(io, dsetname, buffer)
    class(COMIO_T)                  :: io
    character(len=*), intent(in)    :: dsetname
    integer,          intent(inout) :: buffer(:,:)
    io % err % rc = io % err % success
  end subroutine io_dataset_write_2d_int

  subroutine io_dataset_write_3d_int(io, dsetname, buffer)
    class(COMIO_T)                  :: io
    character(len=*), intent(in)    :: dsetname
    integer,          intent(inout) :: buffer(:,:,:)
    io % err % rc = io % err % success
  end subroutine io_dataset_write_3d_int

  ! -- * 2D
  subroutine io_dataset_write_1d_sp(io, dsetname, buffer)
    class(COMIO_T)                  :: io
    character(len=*), intent(in)    :: dsetname
    real(sp),         intent(inout) :: buffer(:)
    io % err % rc = io % err % success
  end subroutine io_dataset_write_1d_sp

  subroutine io_dataset_write_2d_sp(io, dsetname, buffer)
    class(COMIO_T)                  :: io
    character(len=*), intent(in)    :: dsetname
    real(sp),         intent(inout) :: buffer(:,:)
    io % err % rc = io % err % success
  end subroutine io_dataset_write_2d_sp

  subroutine io_dataset_write_3d_sp(io, dsetname, buffer)
    class(COMIO_T)                  :: io
    character(len=*), intent(in)    :: dsetname
    real(sp),         intent(inout) :: buffer(:,:,:)
    io % err % rc = io % err % success
  end subroutine io_dataset_write_3d_sp

  ! -- * 3D
  subroutine io_dataset_write_1d_dp(io, dsetname, buffer)
    class(COMIO_T)                  :: io
    character(len=*), intent(in)    :: dsetname
    real(dp),         intent(inout) :: buffer(:)
    io % err % rc = io % err % success
  end subroutine io_dataset_write_1d_dp

  subroutine io_dataset_write_2d_dp(io, dsetname, buffer)
    class(COMIO_T)                  :: io
    character(len=*), intent(in)    :: dsetname
    real(dp),         intent(inout) :: buffer(:,:)
    io % err % rc = io % err % success
  end subroutine io_dataset_write_2d_dp

  subroutine io_dataset_write_3d_dp(io, dsetname, buffer)
    class(COMIO_T)                  :: io
    character(len=*), intent(in)    :: dsetname
    real(dp),         intent(inout) :: buffer(:,:,:)
    io % err % rc = io % err % success
  end subroutine io_dataset_write_3d_dp

end module comio_class
