module hdf_class

#ifdef HAVE_HDF5
  
  use comio_class
  use hdf5
  use io_prec

  implicit none

  type, extends(COMIO_T) :: HDF5_IO_T

    private
    integer(HID_T) :: dset_id       = -1
    integer(HID_T) :: file_id       = -1
    integer(HID_T) :: grp_id        = -1
    integer(HID_T) :: dcrt_plist_id = -1
    integer(HID_T) :: facc_plist_id = -1
    integer(HID_T) :: fcrt_plist_id = -1
    integer(HID_T) :: xfer_plist_id = -1
    integer(HID_T) :: filespace     = -1
    integer(HID_T) :: memspace      = -1

    integer(HID_T) :: dtype_id            = -1
    integer(HID_T) :: fspace_dtype_int    = -1
    integer(HID_T) :: fspace_dtype_float  = -1
    integer(HID_T) :: fspace_dtype_double = -1

    integer(HSIZE_T), dimension(:), pointer :: fdims  => null()
    integer(HSIZE_T), dimension(:), pointer :: mstart => null()
    integer(HSIZE_T), dimension(:), pointer :: mcount => null()

    integer :: mpio_xfer_mode = -1
    logical :: paused         = .false.

  contains

    procedure :: io_initialize
    procedure :: io_finalize

    procedure :: io_file_open
    procedure :: io_file_close
    procedure :: io_domain_set
    procedure :: io_pause

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

    procedure :: fs_itype_get => io_filespace_int_datatype_get
    procedure :: fs_ftype_get => io_filespace_fp_datatype_get
    procedure :: ms_ftype_get => io_memspace_fp_datatype_get
    procedure :: ms_itype_get => io_memspace_int_datatype_get

  end type HDF5_IO_T

  private

  public :: HDF5_IO_T

contains

  ! -- Initialize/Finalize APIs

  subroutine io_initialize(io, comm, info)
    class(HDF5_IO_T)               :: io
    integer, optional, intent(in)  :: comm
    integer, optional, intent(in)  :: info

    ! -- HDF5 object identifiers
    io % dset_id       = -1
    io % file_id       = -1
    io % filespace     = H5S_ALL_F
    io % memspace      = H5S_ALL_F
    io % dcrt_plist_id = H5P_DEFAULT_F
    io % facc_plist_id = H5P_DEFAULT_F
    io % fcrt_plist_id = H5P_DEFAULT_F
    io % xfer_plist_id = H5P_DEFAULT_F

    ! -- global and local data decompositions
    io % fdims  => null()
    io % mstart => null()
    io % mcount => null()

    ! -- HDF5 parallel I/O
    ! -- data transfer mode
    io % mpio_xfer_mode = H5FD_MPIO_INDEPENDENT_F
    ! -- individual process control
    io % paused         = .false.

    ! -- HDF5 error handling
    io % err % success  =  0
    io % err % failure  = -1
    io % err % message  = "HDF5 I/O failure"
    io % err % srcfile  = __FILE__

    ! -- initialize HDF5
    call h5open_f(io % err % rc)
    if (io % err % check(line=__LINE__)) return

    ! -- default HDF5 dataset types (types are a
    ! -- NOTE: types are available after initialization
    io % dtype_id            = -1
    io % fspace_dtype_int    = H5T_STD_I32LE
    io % fspace_dtype_float  = H5T_IEEE_F32LE
    io % fspace_dtype_double = H5T_IEEE_F64LE

    if (present(comm) .and. present(info)) then
      ! -- create file property list to enable parallel (MPI) I/O
      call h5pcreate_f(H5P_FILE_ACCESS_F, io % facc_plist_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return

      call h5pset_fapl_mpio_f(io % facc_plist_id, comm, info, io % err % rc)
      if (io % err % check(line=__LINE__)) return

      ! -- create property for parallel (MPI) I/O raw data transfer
      call h5pcreate_f(H5P_DATASET_XFER_F, io % xfer_plist_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return

      call h5pset_dxpl_mpio_f(io % xfer_plist_id, io % mpio_xfer_mode, io % err % rc)
      if (io % err % check(line=__LINE__)) return
    end if

    ! -- do not track time for created datasets to allow for b4b comparison
    call h5pcreate_f(H5P_DATASET_CREATE_F, io % dcrt_plist_id, io % err % rc)
    if (io % err % check(line=__LINE__)) return

    call h5pset_obj_track_times_f(io % dcrt_plist_id, .false., io % err % rc)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_initialize

  subroutine io_finalize(io)
    class(HDF5_IO_T) :: io

    ! -- close property lists
    if (io % dcrt_plist_id /= H5P_DEFAULT_F) then
      call h5pclose_f(io % dcrt_plist_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      io % dcrt_plist_id = H5P_DEFAULT_F
    end if

    if (io % facc_plist_id /= H5P_DEFAULT_F) then
      call h5pclose_f(io % facc_plist_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      io % facc_plist_id = H5P_DEFAULT_F
    end if

    if (io % fcrt_plist_id /= H5P_DEFAULT_F) then
      call h5pclose_f(io % fcrt_plist_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      io % fcrt_plist_id = H5P_DEFAULT_F
    end if

    if (io % xfer_plist_id /= H5P_DEFAULT_F) then
      call h5pclose_f(io % xfer_plist_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      io % xfer_plist_id = H5P_DEFAULT_F
    end if

    ! -- close dataset in file space (global)
    if (io % filespace /= H5S_ALL_F) then
      call h5sclose_f(io % filespace, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      io % filespace = H5S_ALL_F
    end if

    ! -- close dataset in memory space (local)
    if (io % memspace /= H5S_ALL_F) then
      call h5sclose_f(io % memspace, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      io % memspace = H5S_ALL_F
    end if

    ! -- shut down HDF5
    call io_shutdown(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_finalize

  subroutine io_shutdown(io)
    class(HDF5_IO_T) :: io

    ! -- close open file
    call io_file_close(io)
    if (io % err % check(line=__LINE__)) return

    ! -- close HDF5
    call h5close_f(io % err % rc)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_shutdown

  ! -- File access APIs

  subroutine io_file_open(io, filename, mode)
    class(HDF5_IO_T)             :: io
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: mode

    select case (mode)
      case ("c", "C")
        call h5fcreate_f(filename, H5F_ACC_TRUNC_F, io % file_id, &
          io % err % rc, creation_prp = io % fcrt_plist_id,          &
          access_prp = io % facc_plist_id)
        if (io % err % check(line=__LINE__)) return
      case ("w", "W")
        call h5fopen_f(filename, H5F_ACC_RDWR_F, io % file_id, &
          io % err % rc, access_prp = io % facc_plist_id)
        if (io % err % check(line=__LINE__)) return
      case ("r", "R")
        call h5fopen_f(filename, H5F_ACC_RDONLY_F, io % file_id, &
          io % err % rc, access_prp = io % facc_plist_id)
        if (io % err % check(line=__LINE__)) return
    end select

  end subroutine io_file_open

  subroutine io_file_close(io)
    class(HDF5_IO_T) :: io

    integer         :: obj
    integer(SIZE_T) :: obj_count
    integer(HID_T), allocatable :: obj_ids(:)

    if (io % file_id /= -1) then

      ! -- close datasets
      call h5fget_obj_count_f(io % file_id, H5F_OBJ_DATASET_F, obj_count, io % err % rc)
      if (io % err % check(line=__LINE__)) return

      allocate(obj_ids(obj_count), stat = io % err % rc)
      if (io % err % check(line=__LINE__, msg="Unable to allocate memory")) return

      call h5fget_obj_ids_f(io % file_id, H5F_OBJ_DATASET_F, obj_count, obj_ids, io % err % rc)
      if (io % err % check(line=__LINE__)) return

      do obj = 1, obj_count
        call h5dclose_f(obj_ids(obj), io % err % rc)
        if (io % err % check(msg="Unable to close dataset", line=__LINE__)) return
      end do

      deallocate(obj_ids, stat = io % err % rc)
      if (io % err % check(line=__LINE__, msg="Unable to free memory")) return

      ! -- close groups
      call h5fget_obj_count_f(io % file_id, H5F_OBJ_GROUP_F, obj_count, io % err % rc)
      if (io % err % check(line=__LINE__)) return

      allocate(obj_ids(obj_count), stat = io % err % rc)
      if (io % err % check(line=__LINE__, msg="Unable to allocate memory")) return

      call h5fget_obj_ids_f(io % file_id, H5F_OBJ_GROUP_F, obj_count, obj_ids, io % err % rc)
      if (io % err % check(line=__LINE__)) return

      do obj = 1, size(obj_ids)
        call h5gclose_f(obj_ids(obj), io % err % rc)
        if (io % err % check(msg="Unable to close group", line=__LINE__)) return
      end do

      deallocate(obj_ids, stat = io % err % rc)
      if (io % err % check(line=__LINE__, msg="Unable to free memory")) return

      call h5fclose_f(io % file_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return

      io % file_id = -1

    end if

    ! -- free up data decomposition memory
    call io_domain_clear(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_file_close

  ! -- Data layout APIs

  subroutine io_domain_clear(io)
    class(HDF5_IO_T) :: io

    ! -- free up data decomposition memory
    if (associated(io % fdims)) then
      deallocate(io % fdims, stat = io % err % rc)
      if (io % err % check(line=__LINE__, msg="Unable to free memory")) return
      nullify(io % fdims)
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
    class(HDF5_IO_T)                  :: io
    integer, dimension(:), intent(in) :: fdims
    integer, dimension(:), intent(in) :: mstart
    integer, dimension(:), intent(in) :: mcount

    ! -- perform sanity check on input
    if (io % err % check(any(fdims < 0), line=__LINE__, &
      msg="Invalid argument: fdims < 0")) return

    if (io % err % check(any(mstart < 1), line=__LINE__, &
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
    io % mstart = mstart - 1

    ! -- replace dataset space on file (global)
    if (io % filespace /= H5S_ALL_F) then
      call h5sclose_f(io % filespace, io % err % rc)
      if (io % err % check(line=__LINE__)) return
    end if

    call h5screate_simple_f(size(io % fdims), io % fdims, &
      io % filespace, io % err % rc)
    if (io % err % check(line=__LINE__)) return

    ! -- replace dataset in memory space (local)
    if (io % memspace /= H5S_ALL_F) then
      call h5sclose_f(io % memspace, io % err % rc)
      if (io % err % check(line=__LINE__)) return
    end if

    call h5screate_simple_f(size(io % mcount), io % mcount, &
      io % memspace, io % err % rc)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_domain_set

  ! -- Groups APIs

  subroutine io_group_create(io, grpname)
    class(HDF5_IO_T)             :: io
    character(len=*), intent(in) :: grpname

    call h5gcreate_f(io % file_id, grpname, io % grp_id, io % err % rc)
    if (io % err % check(line=__LINE__)) return
  end subroutine io_group_create

  ! -- Datasets APIs

  subroutine io_dataset_create(io, dsetname, dsettype, fdims, mstart, mcount)
    class(HDF5_IO_T)              :: io
    character(len=*),  intent(in) :: dsetname
    integer(HID_T),    intent(in) :: dsettype
    integer, optional, intent(in) :: fdims(:)
    integer, optional, intent(in) :: mstart(:)
    integer, optional, intent(in) :: mcount(:)

    integer(HID_T) :: plist_id

    ! -- save dataset global dimensions
    if (present(fdims) .and. present(mstart) .and. present(mcount)) then
      call io_domain_set(io, fdims, mstart, mcount)
      if (io % err % check(line=__LINE__)) return
    end if

    ! -- select hyperslab for data I/O
    call h5sselect_hyperslab_f(io % filespace, H5S_SELECT_SET_F, &
      io % mstart, io % mcount, io % err % rc)
    if (io % err % check(line=__LINE__)) return

    ! -- create dataset
    call h5dcreate_f(io % file_id, dsetname, dsettype, io % filespace, &
      io % dset_id, io % err % rc, dcpl_id = io % dcrt_plist_id)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_create

  subroutine io_dataset_open(io, dsetname)
    class(HDF5_IO_T)             :: io
    character(len=*), intent(in) :: dsetname

    ! -- open dataset on file
    call h5dopen_f(io % file_id, dsetname, io % dset_id, io % err % rc)
    if (io % err % check(line=__LINE__)) return

    ! -- get filespace from dataset
    call h5dget_space_f(io % dset_id, io % filespace, io % err % rc)
    if (io % err % check(line=__LINE__)) return

    ! -- set local portion of dataset as hyperslab
    call h5sselect_hyperslab_f(io % filespace, H5S_SELECT_SET_F, &
      io % mstart, io % mcount, io % err % rc)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_open

  subroutine io_dataset_close(io)
    class(HDF5_IO_T) :: io

    if (io % dset_id /= -1) then
      call h5dclose_f(io % dset_id, io % err % rc)
      if (io % err % check(msg="Unable to close dataset", line=__LINE__)) return
      io % dset_id = -1
    end if

  end subroutine io_dataset_close

  ! -- Basic I/O APIs

  ! -- pause I/O on local task
  subroutine io_pause(io, flag)
    class(HDF5_IO_T)    :: io
    logical, intent(in) :: flag

    io % paused = flag
  end subroutine io_pause

  ! -- read: integers
  ! --  * 1D arrays
  subroutine io_read_1d_int(io, buffer)
    class(HDF5_IO_T)     :: io
    integer, intent(out) :: buffer(:)

    io % dtype_id = io % ms_itype_get(kind(1))
    if (io % err % check(line=__LINE__)) return

    buffer = 0
    call h5dread_f(io % dset_id, io % dtype_id, buffer, io % mcount, io % err % rc, &
      file_space_id = io % filespace, mem_space_id = io % memspace, &
      xfer_prp = io % xfer_plist_id)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_read_1d_int

  ! --  * 2D, integer
  subroutine io_read_2d_int(io, buffer)
    class(HDF5_IO_T)     :: io
    integer, intent(out) :: buffer(:,:)

    io % dtype_id = io % ms_itype_get(kind(1))
    if (io % err % check(line=__LINE__)) return

    buffer = 0
    call h5dread_f(io % dset_id, io % dtype_id, buffer, io % mcount, io % err % rc, &
      file_space_id = io % filespace, mem_space_id = io % memspace, &
      xfer_prp = io % xfer_plist_id)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_read_2d_int

  ! --  * 3D, integer
  subroutine io_read_3d_int(io, buffer)
    class(HDF5_IO_T)     :: io
    integer, intent(out) :: buffer(:,:,:)

    io % dtype_id = io % ms_itype_get(kind(1))
    if (io % err % check(line=__LINE__)) return

    buffer = 0
    call h5dread_f(io % dset_id, io % dtype_id, buffer, io % mcount, io % err % rc, &
      file_space_id = io % filespace, mem_space_id = io % memspace, &
      xfer_prp = io % xfer_plist_id)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_read_3d_int

  ! -- read: floating point
  ! --  * 1D arrays
  subroutine io_read_1d_sp(io, buffer)
    class(HDF5_IO_T)      :: io
    real(sp), intent(out) :: buffer(:)

    io % dtype_id = io % ms_ftype_get(sp)
    if (io % err % check(line=__LINE__)) return

    buffer = 0._sp
    call h5dread_f(io % dset_id, io % dtype_id, buffer, io % mcount, io % err % rc, &
      file_space_id = io % filespace, mem_space_id = io % memspace, &
      xfer_prp = io % xfer_plist_id)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_read_1d_sp

  ! --  * 2D arrays
  subroutine io_read_2d_sp(io, buffer)
    class(HDF5_IO_T)      :: io
    real(sp), intent(out) :: buffer(:,:)

    io % dtype_id = io % ms_ftype_get(sp)
    if (io % err % check(line=__LINE__)) return

    buffer = 0._sp
    call h5dread_f(io % dset_id, io % dtype_id, buffer, io % mcount, io % err % rc, &
      file_space_id = io % filespace, mem_space_id = io % memspace, &
      xfer_prp = io % xfer_plist_id)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_read_2d_sp

  ! --  * 3D arrays
  subroutine io_read_3d_sp(io, buffer)
    class(HDF5_IO_T)      :: io
    real(sp), intent(out) :: buffer(:,:,:)

    io % dtype_id = io % ms_ftype_get(sp)
    if (io % err % check(line=__LINE__)) return

    buffer = 0._sp
    call h5dread_f(io % dset_id, io % dtype_id, buffer, io % mcount, io % err % rc, &
      file_space_id = io % filespace, mem_space_id = io % memspace, &
      xfer_prp = io % xfer_plist_id)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_read_3d_sp

  ! -- read: double
  ! --  * 1D arrays
  subroutine io_read_1d_dp(io, buffer)
    class(HDF5_IO_T)      :: io
    real(dp), intent(out) :: buffer(:)

    io % dtype_id = io % ms_ftype_get(dp)
    if (io % err % check(line=__LINE__)) return

    buffer = 0._dp
    call h5dread_f(io % dset_id, io % dtype_id, buffer, io % mcount, io % err % rc, &
      file_space_id = io % filespace, mem_space_id = io % memspace, &
      xfer_prp = io % xfer_plist_id)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_read_1d_dp

  ! --  * 2D arrays
  subroutine io_read_2d_dp(io, buffer)
    class(HDF5_IO_T)      :: io
    real(dp), intent(out) :: buffer(:,:)

    io % dtype_id = io % ms_ftype_get(dp)
    if (io % err % check(line=__LINE__)) return

    buffer = 0._dp
    call h5dread_f(io % dset_id, io % dtype_id, buffer, io % mcount, io % err % rc, &
      file_space_id = io % filespace, mem_space_id = io % memspace, &
      xfer_prp = io % xfer_plist_id)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_read_2d_dp

  ! --  * 3D arrays
  subroutine io_read_3d_dp(io, buffer)
    class(HDF5_IO_T)      :: io
    real(dp), intent(out) :: buffer(:,:,:)

    io % dtype_id = io % ms_ftype_get(dp)
    if (io % err % check(line=__LINE__)) return

    buffer = 0._dp
    call h5dread_f(io % dset_id, io % dtype_id, buffer, io % mcount, io % err % rc, &
      file_space_id = io % filespace, mem_space_id = io % memspace, &
      xfer_prp = io % xfer_plist_id)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_read_3d_dp

  ! -- write: integers
  ! --  * 1D arrays
  subroutine io_write_1d_int(io, buffer)
    class(HDF5_IO_T)       :: io
    integer, intent(inout) :: buffer(:)

    if (io % paused) return

    io % dtype_id = io % ms_itype_get(kind(1))
    if (io % err % check(line=__LINE__)) return

    call h5dwrite_f(io % dset_id, io % dtype_id, buffer, io % mcount, io % err % rc, &
      file_space_id = io % filespace, mem_space_id = io % memspace, &
      xfer_prp = io % xfer_plist_id)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_write_1d_int

  ! --  * 2D arrays
  subroutine io_write_2d_int(io, buffer)
    class(HDF5_IO_T)       :: io
    integer, intent(inout) :: buffer(:,:)

    if (io % paused) return

    io % dtype_id = io % ms_itype_get(kind(1))
    if (io % err % check(line=__LINE__)) return

    call h5dwrite_f(io % dset_id, io % dtype_id, buffer, io % mcount, io % err % rc, &
      file_space_id = io % filespace, mem_space_id = io % memspace, &
      xfer_prp = io % xfer_plist_id)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_write_2d_int

  ! --  * 3D arrays
  subroutine io_write_3d_int(io, buffer)
    class(HDF5_IO_T)       :: io
    integer, intent(inout) :: buffer(:,:,:)

    if (io % paused) return

    io % dtype_id = io % ms_itype_get(kind(1))
    if (io % err % check(line=__LINE__)) return

    call h5dwrite_f(io % dset_id, io % dtype_id, buffer, io % mcount, io % err % rc, &
      file_space_id = io % filespace, mem_space_id = io % memspace, &
      xfer_prp = io % xfer_plist_id)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_write_3d_int

  ! -- floating point
  ! --  * 1D arrays
  subroutine io_write_1d_sp(io, buffer)
    class(HDF5_IO_T)        :: io
    real(sp), intent(inout) :: buffer(:)

    if (io % paused) return

    io % dtype_id = io % ms_ftype_get(sp)
    if (io % err % check(line=__LINE__)) return

    call h5dwrite_f(io % dset_id, io % dtype_id, buffer, io % mcount, io % err % rc, &
      file_space_id = io % filespace, mem_space_id = io % memspace, &
      xfer_prp = io % xfer_plist_id)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_write_1d_sp

  ! --  * 2D arrays
  subroutine io_write_2d_sp(io, buffer)
    class(HDF5_IO_T)        :: io
    real(sp), intent(inout) :: buffer(:,:)

    if (io % paused) return

    io % dtype_id = io % ms_ftype_get(sp)
    if (io % err % check(line=__LINE__)) return

    call h5dwrite_f(io % dset_id, io % dtype_id, buffer, io % mcount, io % err % rc, &
      file_space_id = io % filespace, mem_space_id = io % memspace, &
      xfer_prp = io % xfer_plist_id)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_write_2d_sp

  ! --  * 3D arrays
  subroutine io_write_3d_sp(io, buffer)
    class(HDF5_IO_T)        :: io
    real(sp), intent(inout) :: buffer(:,:,:)

    if (io % paused) return

    io % dtype_id = io % ms_ftype_get(sp)
    if (io % err % check(line=__LINE__)) return

    call h5dwrite_f(io % dset_id, io % dtype_id, buffer, io % mcount, io % err % rc, &
      file_space_id = io % filespace, mem_space_id = io % memspace, &
      xfer_prp = io % xfer_plist_id)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_write_3d_sp

  ! -- double
  ! --  * 1D arrays
  subroutine io_write_1d_dp(io, buffer)
    class(HDF5_IO_T)        :: io
    real(dp), intent(inout) :: buffer(:)

    if (io % paused) return

    io % dtype_id = io % ms_ftype_get(dp)
    if (io % err % check(line=__LINE__)) return

    call h5dwrite_f(io % dset_id, io % dtype_id, buffer, io % mcount, io % err % rc, &
      file_space_id = io % filespace, mem_space_id = io % memspace, &
      xfer_prp = io % xfer_plist_id)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_write_1d_dp

  ! --  * 2D arrays
  subroutine io_write_2d_dp(io, buffer)
    class(HDF5_IO_T)        :: io
    real(dp), intent(inout) :: buffer(:,:)

    if (io % paused) return

    io % dtype_id = io % ms_ftype_get(dp)
    if (io % err % check(line=__LINE__)) return

    call h5dwrite_f(io % dset_id, io % dtype_id, buffer, io % mcount, io % err % rc, &
      file_space_id = io % filespace, mem_space_id = io % memspace, &
      xfer_prp = io % xfer_plist_id)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_write_2d_dp

  ! --  * 3D arrays
  subroutine io_write_3d_dp(io, buffer)
    class(HDF5_IO_T)        :: io
    real(dp), intent(inout) :: buffer(:,:,:)

    if (io % paused) return

    io % dtype_id = io % ms_ftype_get(dp)
    if (io % err % check(line=__LINE__)) return

    call h5dwrite_f(io % dset_id, io % dtype_id, buffer, io % mcount, io % err % rc, &
      file_space_id = io % filespace, mem_space_id = io % memspace, &
      xfer_prp = io % xfer_plist_id)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_write_3d_dp

  ! -- Dataset I/O APIs

  ! -- reading:
  ! -- integer
  ! -- * 1D
  subroutine io_dataset_read_1d_int(io, dsetname, buffer)
    class(HDF5_IO_T)              :: io
    character(len=*), intent(in)  :: dsetname
    integer,          intent(out) :: buffer(:)

    call io_dataset_open(io, dsetname)
    if (io % err % check(line=__LINE__)) return

    call io_read_1d_int(io, buffer)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_read_1d_int

  ! -- * 2D
  subroutine io_dataset_read_2d_int(io, dsetname, buffer)
    class(HDF5_IO_T)              :: io
    character(len=*), intent(in)  :: dsetname
    integer,          intent(out) :: buffer(:,:)

    call io_dataset_open(io, dsetname)
    if (io % err % check(line=__LINE__)) return

    call io_read_2d_int(io, buffer)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_read_2d_int

  ! -- * 3D
  subroutine io_dataset_read_3d_int(io, dsetname, buffer)
    class(HDF5_IO_T)              :: io
    character(len=*), intent(in)  :: dsetname
    integer,          intent(out) :: buffer(:,:,:)

    call io_dataset_open(io, dsetname)
    if (io % err % check(line=__LINE__)) return

    call io_read_3d_int(io, buffer)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_read_3d_int

  ! -- floating point
  ! -- * 1D
  subroutine io_dataset_read_1d_sp(io, dsetname, buffer)
    class(HDF5_IO_T)              :: io
    character(len=*), intent(in)  :: dsetname
    real(sp),         intent(out) :: buffer(:)

    call io_dataset_open(io, dsetname)
    if (io % err % check(line=__LINE__)) return

    call io_read_1d_sp(io, buffer)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_read_1d_sp

  ! -- * 2D
  subroutine io_dataset_read_2d_sp(io, dsetname, buffer)
    class(HDF5_IO_T)              :: io
    character(len=*), intent(in)  :: dsetname
    real(sp),         intent(out) :: buffer(:,:)

    call io_dataset_open(io, dsetname)
    if (io % err % check(line=__LINE__)) return

    call io_read_2d_sp(io, buffer)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_read_2d_sp

  ! -- * 3D
  subroutine io_dataset_read_3d_sp(io, dsetname, buffer)
    class(HDF5_IO_T)              :: io
    character(len=*), intent(in)  :: dsetname
    real(sp),         intent(out) :: buffer(:,:,:)

    call io_dataset_open(io, dsetname)
    if (io % err % check(line=__LINE__)) return

    call io_read_3d_sp(io, buffer)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_read_3d_sp

  ! -- double
  ! -- * 1D
  subroutine io_dataset_read_1d_dp(io, dsetname, buffer)
    class(HDF5_IO_T)              :: io
    character(len=*), intent(in)  :: dsetname
    real(dp),         intent(out) :: buffer(:)

    call io_dataset_open(io, dsetname)
    if (io % err % check(line=__LINE__)) return

    call io_read_1d_dp(io, buffer)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_read_1d_dp

  ! -- * 2D
  subroutine io_dataset_read_2d_dp(io, dsetname, buffer)
    class(HDF5_IO_T)              :: io
    character(len=*), intent(in)  :: dsetname
    real(dp),         intent(out) :: buffer(:,:)

    call io_dataset_open(io, dsetname)
    if (io % err % check(line=__LINE__)) return

    call io_read_2d_dp(io, buffer)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_read_2d_dp

  ! -- * 3D
  subroutine io_dataset_read_3d_dp(io, dsetname, buffer)
    class(HDF5_IO_T)              :: io
    character(len=*), intent(in)  :: dsetname
    real(dp),         intent(out) :: buffer(:,:,:)

    call io_dataset_open(io, dsetname)
    if (io % err % check(line=__LINE__)) return

    call io_read_3d_dp(io, buffer)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_read_3d_dp

  ! -- writing:
  ! -- integer
  ! -- * 1D
  subroutine io_dataset_write_1d_int(io, dsetname, buffer)
    class(HDF5_IO_T)                :: io
    character(len=*), intent(in)    :: dsetname
    integer,          intent(inout) :: buffer(:)

    io % dtype_id = io % fs_itype_get(kind(1))
    if (io % err % check(line=__LINE__)) return

    call io_dataset_create(io, dsetname, io % dtype_id)
    if (io % err % check(line=__LINE__)) return

    call io_write_1d_int(io, buffer)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_write_1d_int

  subroutine io_dataset_write_2d_int(io, dsetname, buffer)
    class(HDF5_IO_T)                :: io
    character(len=*), intent(in)    :: dsetname
    integer,          intent(inout) :: buffer(:,:)

    io % dtype_id = io % fs_itype_get(kind(1))
    if (io % err % check(line=__LINE__)) return

    call io_dataset_create(io, dsetname, io % dtype_id)
    if (io % err % check(line=__LINE__)) return

    call io_write_2d_int(io, buffer)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_write_2d_int

  subroutine io_dataset_write_3d_int(io, dsetname, buffer)
    class(HDF5_IO_T)                :: io
    character(len=*), intent(in)    :: dsetname
    integer,          intent(inout) :: buffer(:,:,:)

    io % dtype_id = io % fs_itype_get(kind(1))
    if (io % err % check(line=__LINE__)) return

    call io_dataset_create(io, dsetname, io % dtype_id)
    if (io % err % check(line=__LINE__)) return

    call io_write_3d_int(io, buffer)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_write_3d_int

  ! -- * 2D
  subroutine io_dataset_write_1d_sp(io, dsetname, buffer)
    class(HDF5_IO_T)                :: io
    character(len=*), intent(in)    :: dsetname
    real(sp),         intent(inout) :: buffer(:)

    io % dtype_id = io % fs_ftype_get(sp)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_create(io, dsetname, io % dtype_id)
    if (io % err % check(line=__LINE__)) return

    call io_write_1d_sp(io, buffer)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_write_1d_sp

  subroutine io_dataset_write_2d_sp(io, dsetname, buffer)
    class(HDF5_IO_T)                :: io
    character(len=*), intent(in)    :: dsetname
    real(sp),         intent(inout) :: buffer(:,:)

    io % dtype_id = io % fs_ftype_get(sp)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_create(io, dsetname, io % dtype_id)
    if (io % err % check(line=__LINE__)) return

    call io_write_2d_sp(io, buffer)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_write_2d_sp

  subroutine io_dataset_write_3d_sp(io, dsetname, buffer)
    class(HDF5_IO_T)                :: io
    character(len=*), intent(in)    :: dsetname
    real(sp),         intent(inout) :: buffer(:,:,:)

    io % dtype_id = io % fs_ftype_get(sp)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_create(io, dsetname, io % dtype_id)
    if (io % err % check(line=__LINE__)) return

    call io_write_3d_sp(io, buffer)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_write_3d_sp

  ! -- * 3D
  subroutine io_dataset_write_1d_dp(io, dsetname, buffer)
    class(HDF5_IO_T)                :: io
    character(len=*), intent(in)    :: dsetname
    real(dp),         intent(inout) :: buffer(:)

    io % dtype_id = io % fs_ftype_get(dp)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_create(io, dsetname, io % dtype_id)
    if (io % err % check(line=__LINE__)) return

    call io_write_1d_dp(io, buffer)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_write_1d_dp

  subroutine io_dataset_write_2d_dp(io, dsetname, buffer)
    class(HDF5_IO_T)                :: io
    character(len=*), intent(in)    :: dsetname
    real(dp),         intent(inout) :: buffer(:,:)

    io % dtype_id = io % fs_ftype_get(dp)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_create(io, dsetname, io % dtype_id)
    if (io % err % check(line=__LINE__)) return

    call io_write_2d_dp(io, buffer)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_write_2d_dp

  subroutine io_dataset_write_3d_dp(io, dsetname, buffer)
    class(HDF5_IO_T)                :: io
    character(len=*), intent(in)    :: dsetname
    real(dp),         intent(inout) :: buffer(:,:,:)

    io % dtype_id = io % fs_ftype_get(dp)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_create(io, dsetname, io % dtype_id)
    if (io % err % check(line=__LINE__)) return

    call io_write_3d_dp(io, buffer)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_write_3d_dp

  ! -- Utilities

  ! -- Get Data Types
  integer(HID_T) function io_memspace_int_datatype_get(io, datakind)
    class(HDF5_IO_T)    :: io
    integer, intent(in) :: datakind

    io_memspace_int_datatype_get = 0
    if (datakind == kind(1)) then
      io_memspace_int_datatype_get = H5T_NATIVE_INTEGER
    else
      call io % err % set(msg="Unable to identify memory int data kind", line=__LINE__)
    end if
    
  end function io_memspace_int_datatype_get

  integer(HID_T) function io_memspace_fp_datatype_get(io, datakind)
    class(HDF5_IO_T)    :: io
    integer, intent(in) :: datakind

    io_memspace_fp_datatype_get = 0
    if (datakind == kind(1.)) then
      io_memspace_fp_datatype_get = H5T_NATIVE_REAL 
    else if (datakind == kind(1.d0)) then
      io_memspace_fp_datatype_get = H5T_NATIVE_DOUBLE
    else
      call io % err % set(msg="Unable to identify memory fp data kind", line=__LINE__)
    end if
    
  end function io_memspace_fp_datatype_get

  integer(HID_T) function io_filespace_int_datatype_get(io, datakind)
    class(HDF5_IO_T)    :: io
    integer, intent(in) :: datakind

    io_filespace_int_datatype_get = 0
    if (datakind == kind(1)) then
      io_filespace_int_datatype_get = io % fspace_dtype_int
    else
      call io % err % set(msg="Unable to identify file data kind", line=__LINE__)
    end if
    
  end function io_filespace_int_datatype_get

  integer(HID_T) function io_filespace_fp_datatype_get(io, datakind)
    class(HDF5_IO_T)    :: io
    integer, intent(in) :: datakind

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
end module hdf_class
