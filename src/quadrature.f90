module quadrature
  use kinds, only: dp
  use quadpack_interface
  implicit none

contains

  subroutine update_workspace_integer(w, n)
    integer, intent(inout), dimension(:) :: w
    integer, intent(in) :: n

    if (.not.allocated(w)) then
       allocate(w(n))
    else if (size(w) < n) then
       deallocate(w)
       allocate(w(n))
    else
       continue
    end if
  end subroutine update_workspace_integer

  subroutine update_workspace_real(w, n)
    real(dp), intent(inout), dimension(:) :: w
    integer, intent(in) :: n

    if (.not.allocated(w)) then
       allocate(w(n))
    else if (size(w) < n) then
       deallocate(w)
       allocate(w(n))
    else
       continue
    end if
  end subroutine update_workspace_real
  
  function qags_ez(f, a, b, epsabs, epsrel, limit, abserr, neval, ier) result(answer)
    procedure(integrand_dp) :: f
    real(dp), intent(in) :: a, b
    real(dp), intent(in) :: epsabs, epsrel
    integer, intent(in) :: limit
    real(dp), intent(out) :: abserr
    integer, intent(out) :: neval
    integer, intent(out) :: ier
    real(dp) :: answer

    integer :: lenw
    integer :: last ! ignored
    integer, dimension(:), allocatable, save :: iwork
    real(dp), dimension(:), allocatable, save :: work

    call update_workspace
    call dqags(f, a, b, epsabs, epsrel, answer, abserr, neval, ier, &
         limit, lenw, last, iwork, work)

  contains
    
    subroutine update_workspace
      if (.not.allocated(iwork)) then
         allocate(iwork(limit))
      else if (size(iwork) < limit) then
         deallocate(iwork)
         allocate(iwork(limit))
      else
         continue       
      end if

      lenw = limit*4
      if (.not.allocated(work)) then
         allocate(work(lenw))
      else if (size(work) < lenw) then
         deallocate(iwork)
         allocate(iwork(limit))
      else
         continue
      end if

    end subroutine update_workspace

  end function qags_ez

  ! subroutine qagp_ez(f, a, b, npts2, points, epsabs, epsrel, result, abserr, &
  !      neval, ier, leniw, lenw, last, iwork, work)
  ! end subroutine qagp

  ! subroutine qagi_ez(f, bound, inf, epsabs, epsrel, result, abserr, neval, &
  !      ier, limit, lenw, last, iwork, work)
  ! end subroutine qagi

  ! subroutine qawo_ez(f, a, b, omega, integr, epsabs, epsrel, result, abserr, &
  !      neval, ier, leniw, maxp1, lenw, last, iwork, work)
  ! end subroutine qawo

  ! subroutine qawf_ez(f, a, omega, integr, epsabs, result, abserr, neval, ier, &
  !      limlst, lst, leniw, maxp1, lenw, iwork, work)
  ! end subroutine qawf

  ! subroutine qaws_ez(f, a, b, alfa, beta, integr, epsabs, epsrel, result, &
  !      abserr, neval, ier, limit, lenw, last, iwork, work)
  ! end subroutine qaws

  ! subroutine qawc_ez(f, a, b, c, epsabs, epsrel, result, abserr, neval, ier, &
  !      limit, lenw, last, iwork, work)
  ! end subroutine qawc

end module quadrature
