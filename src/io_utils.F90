module io_utils

  use iso_c_binding
  use mpi

  implicit none

  interface
    function comio_link_check(path, pathlen) result (rc) bind(C)
      import
      character(kind=c_char),   intent(in) :: path(*)
      integer(c_size_t), value, intent(in) :: pathlen
      integer(c_int) :: rc
    end function comio_link_check
  end interface

  private

  public :: io_link_check

contains

  integer function io_link_check(path, comm) 
    character(len=*),  intent(in) :: path
    integer, optional, intent(in) :: comm

    integer :: lcomm, rank, ierr

    lcomm = MPI_COMM_WORLD
    if (present(comm)) lcomm = comm

    call mpi_comm_rank(lcomm, rank, ierr)

    io_link_check = 0
    if (rank == 0) then
      io_link_check = comio_link_check(trim(path) // C_NULL_CHAR, &
        len_trim(path, kind=c_size_t) + 1_c_size_t)
    end if

    call mpi_bcast(io_link_check, 1, MPI_INTEGER, 0, lcomm, ierr)

  end function io_link_check

end module io_utils
