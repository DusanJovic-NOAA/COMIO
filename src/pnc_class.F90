module pnc_class

#ifdef HAVE_PNETCDF
  
  use mpi, only: MPI_COMM_WORLD, MPI_INFO_NULL, MPI_OFFSET_KIND
  use pnetcdf
  use comio_class
  use io_prec
  use io_utils

  implicit none

  type, extends(COMIO_T) :: PNC_IO_T

    private
    integer :: comm       = MPI_COMM_WORLD
    integer :: info       = MPI_INFO_NULL

    integer :: dset_id    = -1
    integer :: file_id    = -1

    integer :: dtype_id   = -1

    integer :: fspace_dtype        = -1
    integer :: fspace_dtype_int    = NF90_INT
    integer :: fspace_dtype_float  = NF90_FLOAT
    integer :: fspace_dtype_double = NF90_DOUBLE

    integer :: cmode      = -1

    logical :: paused     = .false.
    logical :: readonly   = .false.

    integer,                       dimension(:), pointer :: fdim_id => null()

    integer(kind=MPI_OFFSET_KIND), dimension(:), pointer :: fdims   => null()
    integer(kind=MPI_OFFSET_KIND), dimension(:), pointer :: mstart  => null()
    integer(kind=MPI_OFFSET_KIND), dimension(:), pointer :: mcount  => null()

  contains

    procedure :: io_initialize
    procedure :: io_finalize

    procedure :: io_file_open
    procedure :: io_file_close
    procedure :: io_domain_set
    procedure :: io_pause

    procedure :: io_datatype

    procedure :: io_dataset_get_dims
    procedure :: io_dataset_read_1d_int, &
                 io_dataset_read_2d_int, &
                 io_dataset_read_3d_int, &
                 io_dataset_read_1d_sp,  &
                 io_dataset_read_2d_sp,  &
                 io_dataset_read_3d_sp,  &
                 io_dataset_read_1d_dp,  &
                 io_dataset_read_2d_dp,  &
                 io_dataset_read_3d_dp
    procedure :: io_dataset_write_1d_int, &
                 io_dataset_write_2d_int, &
                 io_dataset_write_3d_int, &
                 io_dataset_write_1d_sp,  &
                 io_dataset_write_2d_sp,  &
                 io_dataset_write_3d_sp,  &
                 io_dataset_write_1d_dp,  &
                 io_dataset_write_2d_dp,  &
                 io_dataset_write_3d_dp
    procedure :: io_describe_string,         &
                 io_describe_int,            &
                 io_describe_1d_int,         &
                 io_describe_sp,             &
                 io_describe_1d_sp,          &
                 io_describe_dp,             &
                 io_describe_1d_dp,          &
                 io_dataset_describe_string, &
                 io_dataset_describe_int,    &
                 io_dataset_describe_1d_int, &
                 io_dataset_describe_sp,     &
                 io_dataset_describe_1d_sp,  &
                 io_dataset_describe_dp,     &
                 io_dataset_describe_1d_dp

    procedure :: fs_itype_get => io_filespace_int_datatype_get
    procedure :: fs_ftype_get => io_filespace_fp_datatype_get
    procedure :: ms_ftype_get => io_memspace_fp_datatype_get
    procedure :: ms_itype_get => io_memspace_int_datatype_get

  end type PNC_IO_T

  private

  public :: PNC_IO_T

contains

  ! -- Initialize/Finalize APIs

  subroutine io_initialize(io, comm, info)
    class(PNC_IO_T)                :: io
    integer, optional, intent(in)  :: comm
    integer, optional, intent(in)  :: info

    ! -- PnetCDF object identifiers
    io % dset_id    = -1
    io % file_id    = -1

    ! -- PnetCDF error handling
    io % err % success = NF90_NOERR
    io % err % failure = -1
    io % err % message = "PnetCDF I/O failure"
    io % err % srcfile = __FILE__

    ! -- store MPI communicator if provided
    if (present(comm)) io % comm = comm

    ! -- store MPI info handle if provided
    if (present(info)) io % info = info

  end subroutine io_initialize

  subroutine io_finalize(io)
    class(PNC_IO_T) :: io

    ! -- shut down
    call io_shutdown(io)
    if (io % err % check(line=__LINE__)) return
    
  end subroutine io_finalize

  subroutine io_shutdown(io)
    class(PNC_IO_T) :: io

    ! -- close PNETCDF file
    call io_file_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_shutdown

  ! -- File access APIs

  subroutine io_file_open(io, filename, mode)
    class(PNC_IO_T)              :: io
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: mode

    io % readonly = .false.

    select case (mode)
      case ("c+", "C+")
        select case (io_link_check(filename, comm=io % comm))
          case (0)
            ! -- default create mode
            io % cmode = NF90_CLOBBER
          case (1)
            ! -- preserve links
            io % cmode = NF90_NOCLOBBER
          case (-1)
            call io % err % set(msg="Unable to check link: "//filename, line=__LINE__)
            return
        end select
        io % cmode = IOR(io % cmode, NF90_64BIT_OFFSET)
        io % err % rc = nf90mpi_create(io % comm, filename, &
          io % cmode, io % info, io % file_id)
        if (io % err % check(msg=nf90mpi_strerror(io % err % rc), &
          line=__LINE__)) return
        ! -- disable define mode
        io % err % rc = nf90mpi_enddef(io % file_id)
        if (io % err % check(msg=nf90mpi_strerror(io % err % rc), &
          line=__LINE__)) return
        ! -- write domain decomposition to file if set
        call io_domain_write(io)
        if (io % err % check(line=__LINE__)) return
      case ("c", "C")
        io % cmode = IOR(NF90_CLOBBER, NF90_64BIT_OFFSET)
        io % err % rc = nf90mpi_create(io % comm, filename, &
          io % cmode, io % info, io % file_id)
        if (io % err % check(msg=nf90mpi_strerror(io % err % rc), &
          line=__LINE__)) return
        ! -- disable define mode
        io % err % rc = nf90mpi_enddef(io % file_id)
        if (io % err % check(msg=nf90mpi_strerror(io % err % rc), &
          line=__LINE__)) return
        ! -- write domain decomposition to file if set
        call io_domain_write(io)
        if (io % err % check(line=__LINE__)) return
      case ("w", "W")
        io % cmode = NF90_WRITE
        io % err % rc = nf90mpi_open(io % comm, filename, &
          io % cmode, io % info, io % file_id)
        if (io % err % check(msg=nf90mpi_strerror(io % err % rc), &
          line=__LINE__)) return
        call io_domain_write(io)
        if (io % err % check(line=__LINE__)) return
      case ("r", "R")
        io % cmode = NF90_NOWRITE
        io % readonly = .true.
        io % err % rc = nf90mpi_open(io % comm, filename, &
          io % cmode, io % info, io % file_id)
        if (io % err % check(msg=nf90mpi_strerror(io % err % rc), &
          line=__LINE__)) return
    end select

  end subroutine io_file_open

  subroutine io_file_close(io)
    class(PNC_IO_T) :: io

    if (io % file_id /= -1) then

      io % err % rc = nf90mpi_close(io % file_id)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), &
        line=__LINE__)) return

      io % file_id = -1
      io % readonly = .false.

    end if

    ! -- free up data decomposition memory
    call io_domain_clear(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_file_close

  ! -- Data layout APIs

  subroutine io_domain_clear(io)
    class(PNC_IO_T) :: io

    ! -- free up data decomposition memory
    if (associated(io % fdims)) then
      deallocate(io % fdims, stat = io % err % rc)
      if (io % err % check(line=__LINE__, msg="Unable to free memory")) return
      nullify(io % fdims)
    end if
    if (associated(io % fdim_id)) then
      deallocate(io % fdim_id, stat = io % err % rc)
      if (io % err % check(line=__LINE__, msg="Unable to free memory")) return
      nullify(io % fdim_id)
    end if
    if (associated(io % mstart)) then
      deallocate(io % mstart, stat = io % err % rc)
      if (io % err % check(line=__LINE__, msg="Unable to free memory")) return
      nullify(io % mstart)
    end if
    if (associated(io % mcount)) then
      deallocate(io % mcount, stat = io % err % rc)
      if (io % err % check(line=__LINE__, msg="Unable to free memory")) return
      nullify(io % mcount)
    end if

  end subroutine io_domain_clear

  subroutine io_domain_set(io, fdims, mstart, mcount)
    class(PNC_IO_T)                   :: io
    integer, dimension(:), intent(in) :: fdims
    integer, dimension(:), intent(in) :: mstart
    integer, dimension(:), intent(in) :: mcount

    ! -- perform sanity check on input
    if (io % err % check(any(fdims < 0), line=__LINE__, &
      msg="Invalid argument: fdims < 0")) return

    if (io % err % check(any(mstart < 0), line=__LINE__, &
      msg="Invalid argument: mstart < 0")) return

    if (io % err % check(any(mcount < 0), line=__LINE__, &
      msg="Invalid argument: mcount < 0")) return

    if (io % err % check(any(mstart + mcount - 1 > fdims), &
      line=__LINE__, &
      msg="Invalid argument: Inconsistent data decomposition")) return

    ! -- clear existing domain decomposition
    call io_domain_clear(io)
    if (io % err % check(line=__LINE__, msg="Unable to clear existing decomposition")) return

    ! -- set dataset global dimensions
    allocate(io % fdims(size(fdims)), stat = io % err % rc)
    if (io % err % check(line=__LINE__, msg="Unable to allocate memory")) return
    io % fdims = fdims

    ! -- set dataset local dimensions
    allocate(io % mcount(size(mcount)), stat = io % err % rc)
    if (io % err % check(line=__LINE__, msg="Unable to allocate memory")) return
    io % mcount = mcount

    ! -- set dataset local offsets
    allocate(io % mstart(size(mstart)), stat = io % err % rc)
    if (io % err % check(line=__LINE__, msg="Unable to allocate memory")) return
    io % mstart = mstart

    ! -- write domain to file if open and if does not exist
    call io_domain_write(io)
    if (io % err % check(line=__LINE__, msg="Unable to write domain to file")) return

  end subroutine io_domain_set

  subroutine io_domain_write(io)
    class(PNC_IO_T)  :: io

    logical                       :: next
    integer                       :: item
    integer                       :: dimid, ndims, nfdims
    integer, allocatable          :: dimids(:)
    integer(kind=MPI_OFFSET_KIND) :: length
    character(len=3)              :: label

    ! -- begin
    io % err % rc = 0

    ! -- check if domain is set
    if (.not.associated(io % fdims)) return

    ! -- check if file is open
    if (io % file_id == -1) return

    ! -- check if file is opened as  only
    if (io % readonly) return

    ! -- check dimensions on file
    io % err % rc = nf90mpi_inquire(io % file_id, nDimensions = ndims)
    if (io % err % check(msg=nf90mpi_strerror(io % err % rc), &
      line=__LINE__)) return

    if (associated(io % fdim_id)) then
      deallocate(io % fdim_id, stat = io % err % rc)
      if (io % err % check(line=__LINE__, msg="Unable to free memory")) return
      nullify(io % fdim_id)
    end if

    nfdims = size(io % fdims)
    allocate(io % fdim_id(nfdims), stat = io % err % rc)
    if (io % err % check(line=__LINE__, msg="Unable to allocate memory")) return

    io % fdim_id = -1

    do dimid = 1, ndims
      io % err % rc = nf90mpi_inquire_dimension(io % file_id, dimid, len=length)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), &
        line=__LINE__)) return
      do item = 1, nfdims
        if (io % fdims(item) == length) io % fdim_id(item) = dimid
      end do
    end do

    io % err % rc = nf90mpi_redef(io % file_id)
    if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

    ! -- add dimensions not found on file
    dimid = ndims
    do item = 1, nfdims
      if (io % fdim_id(item) < 0) then
        dimid = dimid + 1
        write(label, '("x",i2.2)') dimid
        io % err % rc = nf90mpi_def_dim(io % file_id, label, io % fdims(item), &
          io % fdim_id(item))        
        if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return
      end if
    end do

    io % err % rc = nf90mpi_enddef(io % file_id)
    if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

  end subroutine io_domain_write

  ! -- Groups APIs

  subroutine io_group_create(io, grpname)
    class(PNC_IO_T)              :: io
    character(len=*), intent(in) :: grpname

    ! -- groups are not available in PNETCDF

  end subroutine io_group_create

  ! -- Datasets APIs

  logical function io_dataset_inquire(io, dsetname)
    class(PNC_IO_T)              :: io
    character(len=*), intent(in) :: dsetname

    integer :: dset_id
    integer :: rc

    ! -- begin
    io_dataset_inquire = .false.

    ! -- check if file is open
    if (io % file_id /= -1) then
      ! -- test if dataset exists by inquiring for the variable id
      rc = nf90mpi_inq_varid(io % file_id, dsetname, dset_id)
      io_dataset_inquire = (rc == NF90_NOERR)
      ! -- store variable id if dataset exists
      if (io_dataset_inquire) io % dset_id = dset_id
    end if

  end function io_dataset_inquire

  subroutine io_dataset_create(io, dsetname, dsettype, fdims, mstart, mcount)
    class(PNC_IO_T)               :: io
    character(len=*),  intent(in) :: dsetname
    integer,           intent(in) :: dsettype
    integer, optional, intent(in) :: fdims(:)
    integer, optional, intent(in) :: mstart(:)
    integer, optional, intent(in) :: mcount(:)

    ! -- save dataset global dimensions
    if (present(fdims) .and. present(mstart) .and. present(mcount)) then
      call io_domain_set(io, fdims, mstart, mcount)
      if (io % err % check(line=__LINE__)) return
      call io_domain_write(io)
      if (io % err % check(line=__LINE__)) return
    end if

    ! -- check if dataset exists
    if (io_dataset_inquire(io, dsetname)) then
      call io_dataset_open(io, dsetname)
      if (io % err % check(line=__LINE__)) return
    else
      ! -- create dataset
      io % err % rc = nf90mpi_redef(io % file_id)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

      io % err % rc = nf90mpi_def_var(io % file_id, dsetname, dsettype, &
        io % fdim_id, io % dset_id)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

      io % err % rc = nf90mpi_enddef(io % file_id)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return
    end if

  end subroutine io_dataset_create

  subroutine io_dataset_open(io, dsetname)
    class(PNC_IO_T)              :: io
    character(len=*), intent(in) :: dsetname

    integer :: ndims, item

    ! -- check if file is open
    if (io % file_id == -1) return

    ! -- get variable id
    io % err % rc = nf90mpi_inq_varid(io % file_id, dsetname, io % dset_id)
    if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

    ! -- inquire variable id and dimensions
    io % err % rc = nf90mpi_inquire_variable(io % file_id, io % dset_id, ndims=ndims)
    if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

    ! -- check current domain decomposition against dataset on file
    if (associated(io % fdims)) then
      if (size(io % fdims) /= ndims) then
        deallocate(io % fdims, stat = io % err % rc)
        if (io % err % check(line=__LINE__)) return
        allocate(io % fdims(ndims), stat = io % err % rc)
        if (io % err % check(line=__LINE__)) return
      end if
    else
      allocate(io % fdims(ndims), stat = io % err % rc)
      if (io % err % check(line=__LINE__)) return
    end if

    if (associated(io % fdim_id)) then
      if (size(io % fdim_id) /= ndims) then
        deallocate(io % fdim_id, stat = io % err % rc)
        if (io % err % check(line=__LINE__)) return
        allocate(io % fdim_id(ndims), stat = io % err % rc)
        if (io % err % check(line=__LINE__)) return
      end if
    else
      allocate(io % fdim_id(ndims), stat = io % err % rc)
      if (io % err % check(line=__LINE__)) return
    end if

    ! -- get dimensions ids and length and update domain decomposition
    io % err % rc = nf90mpi_inquire_variable(io % file_id, io % dset_id, &
      dimids=io % fdim_id)
    if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

    do item = 1, ndims
      io % err % rc = nf90mpi_inquire_dimension(io % file_id, &
        io % fdim_id(item), len=io % fdims(item))
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return
    end do

    ! -- check if variable dimensions are consistent with domain decomposition
    if (io % err % check(any(io % mstart + io % mcount - 1 > io % fdims), &
      line=__LINE__, &
      msg="Inconsistent data decomposition")) return
    
  end subroutine io_dataset_open

  subroutine io_dataset_close(io)
    class(PNC_IO_T) :: io

    io % dset_id = -1

  end subroutine io_dataset_close

  subroutine io_dataset_get_dims(io, dsetname, dims)
    class(PNC_IO_T)              :: io
    character(len=*), intent(in) :: dsetname
    integer,          pointer    :: dims(:)

    integer :: item, ndims
    integer(kind=MPI_OFFSET_KIND) :: dimlen
    integer, dimension(:), allocatable :: dimids

    ! -- check if file is open
    if (io % file_id == -1) return

    ! -- check if pointer is associated
    if (associated(dims)) then
      call io % err % set(msg="Pointer argument must not be associated", line=__LINE__)
      return
    end if

    ! -- get variable id
    io % err % rc = nf90mpi_inq_varid(io % file_id, dsetname, io % dset_id)
    if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

    ! -- inquire variable id and dimensions
    io % err % rc = nf90mpi_inquire_variable(io % file_id, io % dset_id, ndims=ndims)
    if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

    ! -- allocate arrays to store dataset global dimsnsions
    allocate(dims(ndims), dimids(ndims), stat = io % err % rc)
    if (io % err % check(line=__LINE__, msg="Unable to allocate memory")) return

    ! -- get dimensions ids and length and update domain decomposition
    io % err % rc = nf90mpi_inquire_variable(io % file_id, io % dset_id, dimids=dimids)
    if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

    do item = 1, ndims
      io % err % rc = nf90mpi_inquire_dimension(io % file_id, dimids(item), len=dimlen)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return
      dims(item) = dimlen
    end do

    ! -- deallocate global dimension id array
    deallocate(dimids, stat = io % err % rc)
    if (io % err % check(line=__LINE__, msg="Unable to free memory")) return

    ! -- release dataset id
    call io_dataset_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_get_dims

  ! -- Basic I/O APIs

  ! -- pause I/O on local task
  subroutine io_pause(io, flag)
    class(PNC_IO_T)     :: io
    logical, intent(in) :: flag

    io % paused = flag
  end subroutine io_pause

  ! -- set write data type for automatic conversion
  subroutine io_datatype(io, dtype)
    class(PNC_IO_T)                :: io
    class(*), optional, intent(in) :: dtype

    io % fspace_dtype = -1

    if (present(dtype)) then
      select type (dtype)
        type is (integer)
          io % fspace_dtype = io % fs_itype_get(kind(1))
        type is (real(sp))
          io % fspace_dtype = io % fs_ftype_get(sp)
        type is (real(dp))
          io % fspace_dtype = io % fs_ftype_get(dp)
        class default
          call io % err % set(msg="Datatype unknown", line=__LINE__)
          return
      end select
    end if

  end subroutine io_datatype

  ! -- read: integers
  ! --  * 1D arrays
  subroutine io_read_1d_int(io, buffer)
    class(PNC_IO_T)      :: io
    integer, intent(out) :: buffer(:)

    buffer = 0
    io % err % rc = nf90mpi_get_var_all(io % file_id, io % dset_id, buffer, &
      start = io % mstart, count = io % mcount)
    if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

  end subroutine io_read_1d_int

  ! --  * 2D, integer
  subroutine io_read_2d_int(io, buffer)
    class(PNC_IO_T)      :: io
    integer, intent(out) :: buffer(:,:)

    buffer = 0
    io % err % rc = nf90mpi_get_var_all(io % file_id, io % dset_id, buffer, &
      start = io % mstart, count = io % mcount)
    if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

  end subroutine io_read_2d_int

  ! --  * 3D, integer
  subroutine io_read_3d_int(io, buffer)
    class(PNC_IO_T)      :: io
    integer, intent(out) :: buffer(:,:,:)

    buffer = 0
    io % err % rc = nf90mpi_get_var_all(io % file_id, io % dset_id, buffer, &
      start = io % mstart, count = io % mcount)
    if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

  end subroutine io_read_3d_int

  ! -- read: floating point
  ! --  * 1D arrays
  subroutine io_read_1d_sp(io, buffer)
    class(PNC_IO_T)       :: io
    real(sp), intent(out) :: buffer(:)

    buffer = 0._sp
    io % err % rc = nf90mpi_get_var_all(io % file_id, io % dset_id, buffer, &
      start = io % mstart, count = io % mcount)
    if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

  end subroutine io_read_1d_sp

  ! --  * 2D arrays
  subroutine io_read_2d_sp(io, buffer)
    class(PNC_IO_T)       :: io
    real(sp), intent(out) :: buffer(:,:)

    buffer = 0._sp
    io % err % rc = nf90mpi_get_var_all(io % file_id, io % dset_id, buffer, &
      start = io % mstart, count = io % mcount)
    if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

  end subroutine io_read_2d_sp

  ! --  * 3D arrays
  subroutine io_read_3d_sp(io, buffer)
    class(PNC_IO_T)       :: io
    real(sp), intent(out) :: buffer(:,:,:)

    buffer = 0._sp
    io % err % rc = nf90mpi_get_var_all(io % file_id, io % dset_id, buffer, &
      start = io % mstart, count = io % mcount)
    if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

  end subroutine io_read_3d_sp

  ! -- read: double
  ! --  * 1D arrays
  subroutine io_read_1d_dp(io, buffer)
    class(PNC_IO_T)       :: io
    real(dp), intent(out) :: buffer(:)

    buffer = 0._dp
    io % err % rc = nf90mpi_get_var_all(io % file_id, io % dset_id, buffer, &
      start = io % mstart, count = io % mcount)
    if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

  end subroutine io_read_1d_dp

  ! --  * 2D arrays
  subroutine io_read_2d_dp(io, buffer)
    class(PNC_IO_T)       :: io
    real(dp), intent(out) :: buffer(:,:)

    buffer = 0._dp
    io % err % rc = nf90mpi_get_var_all(io % file_id, io % dset_id, buffer, &
      start = io % mstart, count = io % mcount)
    if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

  end subroutine io_read_2d_dp

  ! --  * 3D arrays
  subroutine io_read_3d_dp(io, buffer)
    class(PNC_IO_T)       :: io
    real(dp), intent(out) :: buffer(:,:,:)

    buffer = 0._dp
    io % err % rc = nf90mpi_get_var_all(io % file_id, io % dset_id, buffer, &
      start = io % mstart, count = io % mcount)
    if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

  end subroutine io_read_3d_dp

  ! -- write: integers
  ! --  * 1D arrays
  subroutine io_write_1d_int(io, buffer)
    class(PNC_IO_T)        :: io
    integer, intent(inout) :: buffer(:)

    io % err % rc = nf90mpi_put_var_all(io % file_id, &
      io % dset_id, buffer, io % mstart, io % mcount)
    if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

  end subroutine io_write_1d_int

  ! --  * 2D arrays
  subroutine io_write_2d_int(io, buffer)
    class(PNC_IO_T)        :: io
    integer, intent(inout) :: buffer(:,:)

    io % err % rc = nf90mpi_put_var_all(io % file_id, &
      io % dset_id, buffer, io % mstart, io % mcount)
    if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

  end subroutine io_write_2d_int

  ! --  * 3D arrays
  subroutine io_write_3d_int(io, buffer)
    class(PNC_IO_T)        :: io
    integer, intent(inout) :: buffer(:,:,:)

    io % err % rc = nf90mpi_put_var_all(io % file_id, &
      io % dset_id, buffer, io % mstart, io % mcount)
    if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

  end subroutine io_write_3d_int

  ! -- floating point
  ! --  * 1D arrays
  subroutine io_write_1d_sp(io, buffer)
    class(PNC_IO_T)         :: io
    real(sp), intent(inout) :: buffer(:)

    io % err % rc = nf90mpi_put_var_all(io % file_id, &
      io % dset_id, buffer, io % mstart, io % mcount)
    if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

  end subroutine io_write_1d_sp

  ! --  * 2D arrays
  subroutine io_write_2d_sp(io, buffer)
    class(PNC_IO_T)         :: io
    real(sp), intent(inout) :: buffer(:,:)

    io % err % rc = nf90mpi_put_var_all(io % file_id, &
      io % dset_id, buffer, io % mstart, io % mcount)
    if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

  end subroutine io_write_2d_sp

  ! --  * 3D arrays
  subroutine io_write_3d_sp(io, buffer)
    class(PNC_IO_T)         :: io
    real(sp), intent(inout) :: buffer(:,:,:)

    io % err % rc = nf90mpi_put_var_all(io % file_id, &
      io % dset_id, buffer, io % mstart, io % mcount)
    if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

  end subroutine io_write_3d_sp

  ! -- double
  ! --  * 1D arrays
  subroutine io_write_1d_dp(io, buffer)
    class(PNC_IO_T)         :: io
    real(dp), intent(inout) :: buffer(:)

    io % err % rc = nf90mpi_put_var_all(io % file_id, &
      io % dset_id, buffer, io % mstart, io % mcount)
    if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

  end subroutine io_write_1d_dp

  ! --  * 2D arrays
  subroutine io_write_2d_dp(io, buffer)
    class(PNC_IO_T)         :: io
    real(dp), intent(inout) :: buffer(:,:)

    io % err % rc = nf90mpi_put_var_all(io % file_id, &
      io % dset_id, buffer, io % mstart, io % mcount)
    if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

  end subroutine io_write_2d_dp

  ! --  * 3D arrays
  subroutine io_write_3d_dp(io, buffer)
    class(PNC_IO_T)        :: io
    real(8), intent(inout) :: buffer(:,:,:)

    io % err % rc = nf90mpi_put_var_all(io % file_id, &
      io % dset_id, buffer, start=io % mstart, count=io % mcount)
    if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

  end subroutine io_write_3d_dp

  ! -- Dataset I/O APIs

  ! -- reading:
  ! -- integer
  ! -- * 1D
  subroutine io_dataset_read_1d_int(io, dsetname, buffer)
    class(PNC_IO_T)               :: io
    character(len=*), intent(in)  :: dsetname
    integer,          intent(out) :: buffer(:)

    call io_dataset_open(io, dsetname)
    if (io % err % check(line=__LINE__)) return

    call io_read_1d_int(io, buffer)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_read_1d_int

  ! -- * 2D
  subroutine io_dataset_read_2d_int(io, dsetname, buffer)
    class(PNC_IO_T)               :: io
    character(len=*), intent(in)  :: dsetname
    integer,          intent(out) :: buffer(:,:)

    call io_dataset_open(io, dsetname)
    if (io % err % check(line=__LINE__)) return

    call io_read_2d_int(io, buffer)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_read_2d_int

  ! -- * 3D
  subroutine io_dataset_read_3d_int(io, dsetname, buffer)
    class(PNC_IO_T)               :: io
    character(len=*), intent(in)  :: dsetname
    integer,          intent(out) :: buffer(:,:,:)

    call io_dataset_open(io, dsetname)
    if (io % err % check(line=__LINE__)) return

    call io_read_3d_int(io, buffer)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_read_3d_int

  ! -- floating point
  ! -- * 1D
  subroutine io_dataset_read_1d_sp(io, dsetname, buffer)
    class(PNC_IO_T)               :: io
    character(len=*), intent(in)  :: dsetname
    real(sp),         intent(out) :: buffer(:)

    call io_dataset_open(io, dsetname)
    if (io % err % check(line=__LINE__)) return

    call io_read_1d_sp(io, buffer)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_read_1d_sp

  ! -- * 2D
  subroutine io_dataset_read_2d_sp(io, dsetname, buffer)
    class(PNC_IO_T)               :: io
    character(len=*), intent(in)  :: dsetname
    real(sp),         intent(out) :: buffer(:,:)

    call io_dataset_open(io, dsetname)
    if (io % err % check(line=__LINE__)) return

    call io_read_2d_sp(io, buffer)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_read_2d_sp

  ! -- * 3D
  subroutine io_dataset_read_3d_sp(io, dsetname, buffer)
    class(PNC_IO_T)               :: io
    character(len=*), intent(in)  :: dsetname
    real(sp),         intent(out) :: buffer(:,:,:)

    call io_dataset_open(io, dsetname)
    if (io % err % check(line=__LINE__)) return

    call io_read_3d_sp(io, buffer)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_read_3d_sp

  ! -- double
  ! -- * 1D
  subroutine io_dataset_read_1d_dp(io, dsetname, buffer)
    class(PNC_IO_T)               :: io
    character(len=*), intent(in)  :: dsetname
    real(dp),         intent(out) :: buffer(:)

    call io_dataset_open(io, dsetname)
    if (io % err % check(line=__LINE__)) return

    call io_read_1d_dp(io, buffer)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_read_1d_dp

  ! -- * 2D
  subroutine io_dataset_read_2d_dp(io, dsetname, buffer)
    class(PNC_IO_T)               :: io
    character(len=*), intent(in)  :: dsetname
    real(dp),         intent(out) :: buffer(:,:)

    call io_dataset_open(io, dsetname)
    if (io % err % check(line=__LINE__)) return

    call io_read_2d_dp(io, buffer)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_read_2d_dp

  ! -- * 3D
  subroutine io_dataset_read_3d_dp(io, dsetname, buffer)
    class(PNC_IO_T)               :: io
    character(len=*), intent(in)  :: dsetname
    real(dp),         intent(out) :: buffer(:,:,:)

    call io_dataset_open(io, dsetname)
    if (io % err % check(line=__LINE__)) return

    call io_read_3d_dp(io, buffer)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_read_3d_dp

  ! -- writing:
  ! -- integer
  ! -- * 1D
  subroutine io_dataset_write_1d_int(io, dsetname, buffer)
    class(PNC_IO_T)                 :: io
    character(len=*), intent(in)    :: dsetname
    integer,          intent(inout) :: buffer(:)

    io % dtype_id = io % fs_itype_get(kind(1))
    if (io % err % check(line=__LINE__)) return

    call io_dataset_create(io, dsetname, io % dtype_id)
    if (io % err % check(line=__LINE__)) return

    call io_write_1d_int(io, buffer)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_write_1d_int

  subroutine io_dataset_write_2d_int(io, dsetname, buffer)
    class(PNC_IO_T)                 :: io
    character(len=*), intent(in)    :: dsetname
    integer,          intent(inout) :: buffer(:,:)

    io % dtype_id = io % fs_itype_get(kind(1))
    if (io % err % check(line=__LINE__)) return

    call io_dataset_create(io, dsetname, io % dtype_id)
    if (io % err % check(line=__LINE__)) return

    call io_write_2d_int(io, buffer)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_write_2d_int

  subroutine io_dataset_write_3d_int(io, dsetname, buffer)
    class(PNC_IO_T)                 :: io
    character(len=*), intent(in)    :: dsetname
    integer,          intent(inout) :: buffer(:,:,:)

    io % dtype_id = io % fs_itype_get(kind(1))
    if (io % err % check(line=__LINE__)) return

    call io_dataset_create(io, dsetname, io % dtype_id)
    if (io % err % check(line=__LINE__)) return

    call io_write_3d_int(io, buffer)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_write_3d_int

  ! -- * 2D
  subroutine io_dataset_write_1d_sp(io, dsetname, buffer)
    class(PNC_IO_T)                 :: io
    character(len=*), intent(in)    :: dsetname
    real(sp),         intent(inout) :: buffer(:)

    io % dtype_id = io % fs_ftype_get(sp)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_create(io, dsetname, io % dtype_id)
    if (io % err % check(line=__LINE__)) return

    call io_write_1d_sp(io, buffer)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_write_1d_sp

  subroutine io_dataset_write_2d_sp(io, dsetname, buffer)
    class(PNC_IO_T)                 :: io
    character(len=*), intent(in)    :: dsetname
    real(sp),         intent(inout) :: buffer(:,:)

    io % dtype_id = io % fs_ftype_get(sp)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_create(io, dsetname, io % dtype_id)
    if (io % err % check(line=__LINE__)) return

    call io_write_2d_sp(io, buffer)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_write_2d_sp

  subroutine io_dataset_write_3d_sp(io, dsetname, buffer)
    class(PNC_IO_T)                 :: io
    character(len=*), intent(in)    :: dsetname
    real(sp),         intent(inout) :: buffer(:,:,:)

    io % dtype_id = io % fs_ftype_get(sp)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_create(io, dsetname, io % dtype_id)
    if (io % err % check(line=__LINE__)) return

    call io_write_3d_sp(io, buffer)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_write_3d_sp

  ! -- * 3D
  subroutine io_dataset_write_1d_dp(io, dsetname, buffer)
    class(PNC_IO_T)                 :: io
    character(len=*), intent(in)    :: dsetname
    real(dp),         intent(inout) :: buffer(:)

    io % dtype_id = io % fs_ftype_get(dp)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_create(io, dsetname, io % dtype_id)
    if (io % err % check(line=__LINE__)) return

    call io_write_1d_dp(io, buffer)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_write_1d_dp

  subroutine io_dataset_write_2d_dp(io, dsetname, buffer)
    class(PNC_IO_T)                 :: io
    character(len=*), intent(in)    :: dsetname
    real(dp),         intent(inout) :: buffer(:,:)

    io % dtype_id = io % fs_ftype_get(dp)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_create(io, dsetname, io % dtype_id)
    if (io % err % check(line=__LINE__)) return

    call io_write_2d_dp(io, buffer)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_write_2d_dp

  subroutine io_dataset_write_3d_dp(io, dsetname, buffer)
    class(PNC_IO_T)                 :: io
    character(len=*), intent(in)    :: dsetname
    real(dp),         intent(inout) :: buffer(:,:,:)

    io % dtype_id = io % fs_ftype_get(dp)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_create(io, dsetname, io % dtype_id)
    if (io % err % check(line=__LINE__)) return

    call io_write_3d_dp(io, buffer)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_write_3d_dp

  ! -- describing (attributes)
  ! -- global

  subroutine io_describe_string(io, key, value)
    class(PNC_IO_T)              :: io
    character(len=*), intent(in) :: key
    character(len=*), intent(in) :: value

    if (io % file_id /= -1) then

      io % err % rc = nf90mpi_redef(io % file_id)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

      ! -- add attribute
      io % err % rc = nf90mpi_put_att(io % file_id, NF90_GLOBAL, trim(key), trim(value))
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

      io % err % rc = nf90mpi_enddef(io % file_id)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

    end if

  end subroutine io_describe_string

  subroutine io_describe_int(io, key, value)
    class(PNC_IO_T)              :: io
    character(len=*), intent(in) :: key
    integer,          intent(in) :: value

    if (io % file_id /= -1) then

      io % err % rc = nf90mpi_redef(io % file_id)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

      ! -- add attribute
      io % err % rc = nf90mpi_put_att(io % file_id, NF90_GLOBAL, trim(key), value)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

      io % err % rc = nf90mpi_enddef(io % file_id)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

    end if

  end subroutine io_describe_int

  subroutine io_describe_1d_int(io, key, values)
    class(PNC_IO_T)              :: io
    character(len=*), intent(in) :: key
    integer,          intent(in) :: values(:)

    if (io % file_id /= -1) then

      io % err % rc = nf90mpi_redef(io % file_id)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

      ! -- add attribute
      io % err % rc = nf90mpi_put_att(io % file_id, NF90_GLOBAL, trim(key), values)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

      io % err % rc = nf90mpi_enddef(io % file_id)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

    end if

  end subroutine io_describe_1d_int

  subroutine io_describe_sp(io, key, value)
    class(PNC_IO_T)              :: io
    character(len=*), intent(in) :: key
    real(sp),         intent(in) :: value

    if (io % file_id /= -1) then

      io % err % rc = nf90mpi_redef(io % file_id)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

      ! -- add attribute
      io % err % rc = nf90mpi_put_att(io % file_id, NF90_GLOBAL, trim(key), value)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

      io % err % rc = nf90mpi_enddef(io % file_id)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

    end if

  end subroutine io_describe_sp

  subroutine io_describe_1d_sp(io, key, values)
    class(PNC_IO_T)              :: io
    character(len=*), intent(in) :: key
    real(sp),         intent(in) :: values(:)

    if (io % file_id /= -1) then

      io % err % rc = nf90mpi_redef(io % file_id)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

      ! -- add attribute
      io % err % rc = nf90mpi_put_att(io % file_id, NF90_GLOBAL, trim(key), values)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

      io % err % rc = nf90mpi_enddef(io % file_id)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

    end if

  end subroutine io_describe_1d_sp

  subroutine io_describe_dp(io, key, value)
    class(PNC_IO_T)              :: io
    character(len=*), intent(in) :: key
    real(dp),         intent(in) :: value

    if (io % file_id /= -1) then

      io % err % rc = nf90mpi_redef(io % file_id)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

      ! -- add attribute
      io % err % rc = nf90mpi_put_att(io % file_id, NF90_GLOBAL, trim(key), value)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

      io % err % rc = nf90mpi_enddef(io % file_id)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

    end if

  end subroutine io_describe_dp

  subroutine io_describe_1d_dp(io, key, values)
    class(PNC_IO_T)              :: io
    character(len=*), intent(in) :: key
    real(dp),         intent(in) :: values(:)

    if (io % file_id /= -1) then

      io % err % rc = nf90mpi_redef(io % file_id)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

      ! -- add attribute
      io % err % rc = nf90mpi_put_att(io % file_id, NF90_GLOBAL, trim(key), values)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

      io % err % rc = nf90mpi_enddef(io % file_id)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

    end if

  end subroutine io_describe_1d_dp

  ! -- variables

  subroutine io_dataset_describe_string(io, dsetname, key, value)
    class(PNC_IO_T)              :: io
    character(len=*), intent(in) :: dsetname
    character(len=*), intent(in) :: key
    character(len=*), intent(in) :: value

    ! -- check if dataset exists
    if (io_dataset_inquire(io, dsetname)) then

      io % err % rc = nf90mpi_redef(io % file_id)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

      ! -- add attribute
      io % err % rc = nf90mpi_put_att(io % file_id, io % dset_id, trim(key), trim(value))
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

      io % err % rc = nf90mpi_enddef(io % file_id)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

    end if

  end subroutine io_dataset_describe_string

  subroutine io_dataset_describe_int(io, dsetname, key, value)
    class(PNC_IO_T)              :: io
    character(len=*), intent(in) :: dsetname
    character(len=*), intent(in) :: key
    integer,          intent(in) :: value

    ! -- check if dataset exists
    if (io_dataset_inquire(io, dsetname)) then

      io % err % rc = nf90mpi_redef(io % file_id)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

      ! -- add attribute
      io % err % rc = nf90mpi_put_att(io % file_id, io % dset_id, trim(key), value)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

      io % err % rc = nf90mpi_enddef(io % file_id)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

    end if

  end subroutine io_dataset_describe_int

  subroutine io_dataset_describe_1d_int(io, dsetname, key, values)
    class(PNC_IO_T)              :: io
    character(len=*), intent(in) :: dsetname
    character(len=*), intent(in) :: key
    integer,          intent(in) :: values(:)

    ! -- check if dataset exists
    if (io_dataset_inquire(io, dsetname)) then

      io % err % rc = nf90mpi_redef(io % file_id)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

      ! -- add attribute
      io % err % rc = nf90mpi_put_att(io % file_id, io % dset_id, trim(key), values)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

      io % err % rc = nf90mpi_enddef(io % file_id)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

    end if

  end subroutine io_dataset_describe_1d_int

  subroutine io_dataset_describe_sp(io, dsetname, key, value)
    class(PNC_IO_T)              :: io
    character(len=*), intent(in) :: dsetname
    character(len=*), intent(in) :: key
    real(sp),         intent(in) :: value

    ! -- check if dataset exists
    if (io_dataset_inquire(io, dsetname)) then

      io % err % rc = nf90mpi_redef(io % file_id)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

      ! -- add attribute
      io % err % rc = nf90mpi_put_att(io % file_id, io % dset_id, trim(key), value)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

      io % err % rc = nf90mpi_enddef(io % file_id)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

    end if

  end subroutine io_dataset_describe_sp

  subroutine io_dataset_describe_1d_sp(io, dsetname, key, values)
    class(PNC_IO_T)              :: io
    character(len=*), intent(in) :: dsetname
    character(len=*), intent(in) :: key
    real(sp),         intent(in) :: values(:)

    ! -- check if dataset exists
    if (io_dataset_inquire(io, dsetname)) then

      io % err % rc = nf90mpi_redef(io % file_id)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

      ! -- add attribute
      io % err % rc = nf90mpi_put_att(io % file_id, io % dset_id, trim(key), values)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

      io % err % rc = nf90mpi_enddef(io % file_id)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

    end if

  end subroutine io_dataset_describe_1d_sp

  subroutine io_dataset_describe_dp(io, dsetname, key, value)
    class(PNC_IO_T)              :: io
    character(len=*), intent(in) :: dsetname
    character(len=*), intent(in) :: key
    real(dp),         intent(in) :: value

    ! -- check if dataset exists
    if (io_dataset_inquire(io, dsetname)) then

      io % err % rc = nf90mpi_redef(io % file_id)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

      ! -- add attribute
      io % err % rc = nf90mpi_put_att(io % file_id, io % dset_id, trim(key), value)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

      io % err % rc = nf90mpi_enddef(io % file_id)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

    end if

  end subroutine io_dataset_describe_dp

  subroutine io_dataset_describe_1d_dp(io, dsetname, key, values)
    class(PNC_IO_T)              :: io
    character(len=*), intent(in) :: dsetname
    character(len=*), intent(in) :: key
    real(dp),         intent(in) :: values(:)

    ! -- check if dataset exists
    if (io_dataset_inquire(io, dsetname)) then

      io % err % rc = nf90mpi_redef(io % file_id)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

      ! -- add attribute
      io % err % rc = nf90mpi_put_att(io % file_id, io % dset_id, trim(key), values)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

      io % err % rc = nf90mpi_enddef(io % file_id)
      if (io % err % check(msg=nf90mpi_strerror(io % err % rc), line=__LINE__)) return

    end if

  end subroutine io_dataset_describe_1d_dp


  ! -- Utilities

  ! -- Get Data Types
  integer function io_memspace_int_datatype_get(io, datakind)
    class(PNC_IO_T)     :: io
    integer, intent(in) :: datakind

    io_memspace_int_datatype_get = 0
    if (datakind == kind(1)) then
      io_memspace_int_datatype_get = NF90_INT
    else
      call io % err % set(msg="Unable to identify memory int data kind", line=__LINE__)
    end if
    
  end function io_memspace_int_datatype_get

  integer function io_memspace_fp_datatype_get(io, datakind)
    class(PNC_IO_T)     :: io
    integer, intent(in) :: datakind

    io_memspace_fp_datatype_get = 0
    if (datakind == kind(1.)) then
      io_memspace_fp_datatype_get = NF90_FLOAT
    else if (datakind == kind(1.d0)) then
      io_memspace_fp_datatype_get = NF90_DOUBLE
    else
      call io % err % set(msg="Unable to identify memory fp data kind", line=__LINE__)
    end if
    
  end function io_memspace_fp_datatype_get

  integer function io_filespace_int_datatype_get(io, datakind)
    class(PNC_IO_T)     :: io
    integer, intent(in) :: datakind

    if (io % fspace_dtype /= -1) then
      io_filespace_int_datatype_get = io % fspace_dtype
      return
    end if

    io_filespace_int_datatype_get = 0
    if (datakind == kind(1)) then
      io_filespace_int_datatype_get = io % fspace_dtype_int
    else
      call io % err % set(msg="Unable to identify file data kind", line=__LINE__)
    end if
    
  end function io_filespace_int_datatype_get

  integer function io_filespace_fp_datatype_get(io, datakind)
    class(PNC_IO_T)     :: io
    integer, intent(in) :: datakind

    if (io % fspace_dtype /= -1) then
      io_filespace_fp_datatype_get = io % fspace_dtype
      return
    end if

    io_filespace_fp_datatype_get = 0
    if (datakind == kind(1.)) then
      io_filespace_fp_datatype_get = io % fspace_dtype_float
    else if (datakind == kind(1.d0)) then
      io_filespace_fp_datatype_get = io % fspace_dtype_double
    else
      call io % err % set(msg="Unable to identify file data kind", line=__LINE__)
    end if
    
  end function io_filespace_fp_datatype_get

#endif
end module pnc_class
