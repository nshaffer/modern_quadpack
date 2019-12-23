module quadpack_test_cases
  use quadpack_interface
  implicit none

contains

  subroutine test_qng
    real(sp), parameter :: a = 0.0_sp
    real(sp), parameter :: b = 1.0_sp
    real(sp), parameter :: epsabs = 0.0_sp
    real(sp), parameter :: epsrel = 1.0e-3_sp

    real(sp) :: result, abserr
    integer :: neval, ier

    real(sp), parameter :: ans = 1.270724139833620_sp

    call qng(f, a, b, epsabs, epsrel, result, abserr, neval, ier)

    print *, 'QNG integration of exp(x)/(x^2+1) from 0 to 1'
    print *, 'Obtained result: ', result
    print *, 'Expected result: ', ans
    print *, 'Absolute error estimate: ', abserr
    print *, 'Actual error: ', abs(result-ans)
    print *, 'Required number of integrand evaluations: ', neval
    print *, 'Exit status: ', ier
    print *,

  contains

    function f(x) 
      real(sp) :: f
      real(sp), intent(in) :: x
      f = exp(x)/(x*x + 1.0_sp)
    end function f

  end subroutine test_qng

  subroutine test_qag
    real(sp), parameter :: a = 0.0_sp
    real(sp), parameter :: b = 1.0_sp
    real(sp), parameter :: epsabs = 0.0_sp
    real(sp), parameter :: epsrel = 1.0e-3_sp
    integer, parameter :: key = QAG_KEY_30_61

    real(sp) :: result, abserr
    integer :: neval, ier

    integer, parameter :: limit = 100
    integer, parameter :: lenw = limit*4

    integer :: last
    integer, dimension(limit) :: iwork
    real(sp), dimension(lenw) :: work

    real(sp), parameter :: ans = 2.0_sp/sqrt(3.0_sp)

    call qag(f, a, b, epsabs, epsrel, key, result, abserr, neval, ier, & 
         limit, lenw, last, iwork, work)

    print *, 'QAG integration of 2/(2+sin(10pi.x) from 0 to 1'
    print *, 'Obtained result: ', result
    print *, 'Expected result: ', ans
    print *, 'Absolute error estimate: ', abserr
    print *, 'Actual error: ', abs(result-ans)
    print *, 'Required number of integrand evaluations: ', neval
    print *, 'Exit status: ', ier
    print *,

  contains

    function f(x) 
      real(sp) :: f
      real(sp), intent(in) :: x
      real(sp), parameter :: pi = 4.0_sp*atan(1.0_sp)
      f = 2.0_sp/(2.0_sp + sin(10.0_sp*pi*x))
    end function f

  end subroutine test_qag

  subroutine test_qags
    real(sp), parameter :: a = 0.0_sp
    real(sp), parameter :: b = 1.0_sp
    real(sp), parameter :: epsabs = 0.0_sp
    real(sp), parameter :: epsrel = 1.0e-3_sp

    real(sp) :: result, abserr
    integer :: neval, ier

    integer, parameter :: limit = 100
    integer, parameter :: lenw = limit*4

    integer :: last
    integer, dimension(limit) :: iwork
    real(sp), dimension(lenw) :: work

    real(sp), parameter :: ans = 2.0_sp

    call qags(f, a, b, epsabs, epsrel, result, abserr, neval, ier, & 
         limit, lenw, last, iwork, work)

    print *, 'QAGS integration of 1/sqrt(x) from 0 to 1'
    print *, 'Obtained result: ', result
    print *, 'Expected result: ', ans
    print *, 'Absolute error estimate: ', abserr
    print *, 'Actual error: ', abs(result-ans)
    print *, 'Required number of integrand evaluations: ', neval
    print *, 'Exit status: ', ier
    print *,

  contains

    function f(x) 
      real(sp) :: f
      real(sp), intent(in) :: x
      f = 1.0_sp/sqrt(x)
    end function f

  end subroutine test_qags

  subroutine test_qagp
    real(sp), parameter :: a = 0.0_sp
    real(sp), parameter :: b = 1.0_sp
    real(sp), parameter :: epsabs = 0.0_sp
    real(sp), parameter :: epsrel = 1.0e-3_sp
    integer, parameter :: npts2 = 4
    real(sp), dimension(npts2-2), parameter :: points = [1.0_sp/7.0_sp, 2.0_sp/3.0_sp]

    real(sp) :: result, abserr
    integer :: neval, ier

    integer, parameter :: limit = 100
    integer, parameter :: leniw = limit*2 + npts2
    integer, parameter :: lenw = limit*4 + npts2

    integer :: last
    integer, dimension(leniw) :: iwork
    real(sp), dimension(lenw) :: work

    real(sp), parameter :: ans = 4.25368769_sp

    call qagp(f, a, b, npts2, points, epsabs, epsrel, result, abserr, neval, ier, & 
         leniw, lenw, last, iwork, work)

    print *, 'QAGP integration of |x-1/7|^(-1/4) |x-2/3|^(-11/20) from 0 to 1'
    print *, 'Obtained result: ', result
    print *, 'Expected result: ', ans
    print *, 'Absolute error estimate: ', abserr
    print *, 'Actual error: ', abs(result-ans)
    print *, 'Required number of integrand evaluations: ', neval
    print *, 'Exit status: ', ier
    print *,

  contains

    function f(x) 
      real(sp) :: f
      real(sp), intent(in) :: x
      f = abs(x-points(1))**(-0.25_sp) * abs(x-points(2))**(-0.55_sp)
    end function f

  end subroutine test_qagp

  subroutine test_qagi
    use kinds, only: sp
    use qagi_interface
    implicit none

    real(sp), parameter :: boun = 0.0_sp
    integer, parameter :: inf = QAGI_INF_UPPER
    real(sp), parameter :: epsabs = 0.0_sp
    real(sp), parameter :: epsrel = 1.0e-3_sp

    real(sp) :: result, abserr
    integer :: neval, ier

    integer, parameter :: limit = 100
    integer, parameter :: lenw = limit*4

    integer :: last
    integer, dimension(limit) :: iwork
    real(sp), dimension(lenw) :: work

    real(sp), parameter :: ans = 3.07957178_sp

    call qagi(f, boun, inf, epsabs, epsrel, result, abserr, neval, ier, & 
         limit, lenw, last, iwork, work)

    print *, 'QAGI integration of sqrt(x)log(x)/((x+1)(x+2)) from 0 to +inf'
    print *, 'Obtained result: ', result
    print *, 'Expected result: ', ans
    print *, 'Absolute error estimate: ', abserr
    print *, 'Actual error: ', abs(result-ans)
    print *, 'Required number of integrand evaluations: ', neval
    print *, 'Exit status: ', ier
    print *,

  contains

    function f(x) 
      real(sp) :: f
      real(sp), intent(in) :: x
      f = sqrt(x)*log(x) / ((x + 1.0_sp)*(x + 2.0_sp))
    end function f

  end subroutine test_qagi

  subroutine test_qawo
    use kinds, only: sp
    use qawo_interface
    implicit none

    real(sp), parameter :: a = 0.0_sp
    real(sp), parameter :: b = 1.0_sp
    real(sp), parameter :: omega = 10.0_sp
    integer, parameter :: integr = QAWO_COS
    real(sp), parameter :: epsabs = 0.0_sp
    real(sp), parameter :: epsrel = 1.0e-3_sp

    real(sp) :: result, abserr
    integer :: neval, ier

    integer, parameter :: limit = 100
    integer, parameter :: leniw = limit*2
    integer, parameter :: maxp1 = 21
    integer, parameter :: lenw = limit*4 + maxp1*25

    integer :: last
    integer, dimension(leniw) :: iwork
    real(sp), dimension(lenw) :: work

    real(sp), parameter :: ans = -0.17763920651138898591_sp

    call qawo(f, a, b, omega, integr, epsabs, epsrel, result, abserr, neval, ier, & 
         leniw, maxp1, lenw, last, iwork, work)

    print *, 'QAWO integration of exp(-x)log(x)cos(10.x) from 0 to 1'
    print *, 'Obtained result: ', result
    print *, 'Expected result: ', ans
    print *, 'Absolute error estimate: ', abserr
    print *, 'Actual error: ', abs(result-ans)
    print *, 'Required number of integrand evaluations: ', neval
    print *, 'Exit status: ', ier
    print *,

  contains

    function f(x) 
      real(sp) :: f
      real(sp), intent(in) :: x
      f = 0.0_sp
      if (x > 0.0_sp) f = exp(-x)*log(x)
    end function f

  end subroutine test_qawo

  subroutine test_qawf
    use kinds, only: sp
    use qawf_interface
    implicit none

    real(sp), parameter :: a = 0.0_sp
    real(sp), parameter :: omega = 8.0_sp
    integer, parameter :: integr = QAWF_SIN
    real(sp), parameter :: epsabs = 1.0e-3_sp

    real(sp) :: result, abserr
    integer :: neval, ier

    integer, parameter :: limlst = 50
    integer, parameter :: limit = 100
    integer, parameter :: leniw = limit*2 + limlst
    integer, parameter :: maxp1 = 21
    integer, parameter :: lenw = leniw*2 + maxp1*25

    integer :: lst
    integer, dimension(leniw) :: iwork
    real(sp), dimension(lenw) :: work

    real(sp), parameter :: ans = 1.422552162575912242782619650_sp

    call qawf(f, a, omega, integr, epsabs, result, abserr, neval, ier, & 
         limlst, lst, leniw, maxp1, lenw, iwork, work)

    print *, 'QAWF integration of sin(50.x)sin(8.x)/(x.sqrt(x)) from 0 to oo'
    print *, 'Obtained result: ', result
    print *, 'Expected result: ', ans
    print *, 'Absolute error estimate: ', abserr
    print *, 'Actual error: ', abs(result-ans)
    print *, 'Required number of integrand evaluations: ', neval
    print *, 'Exit status: ', ier
    print *,

  contains

    function f(x) 
      real(sp) :: f
      real(sp), intent(in) :: x
      f = 0.0_sp
      if (x > 0.0_sp) f = sin(50.0_sp*x)/(x*sqrt(x))
    end function f

  end subroutine test_qawf

  subroutine test_qaws
    use kinds, only: sp
    use qaws_interface
    implicit none

    real(sp), parameter :: a = 0.0_sp
    real(sp), parameter :: b = 1.0_sp
    real(sp), parameter :: alfa = -0.5_sp
    real(sp), parameter :: beta = -0.5_sp
    integer, parameter :: integr = QAWS_ALG_ALG
    real(sp), parameter :: epsabs = 0.0_sp
    real(sp), parameter :: epsrel = 1.0e-3_sp

    real(sp) :: result, abserr
    integer :: neval, ier

    integer, parameter :: limit = 100
    integer, parameter :: lenw = limit*4

    integer :: last
    integer, dimension(limit) :: iwork
    real(sp), dimension(lenw) :: work

    real(sp), parameter :: ans = 0.535019056922365344126359_sp

    call qaws(f, a, b, alfa, beta, integr, epsabs, epsrel, result, &
         abserr, neval, ier, limit, lenw, last, iwork, work)

    print *, 'QAWS integration of sin(10.x)/sqrt(x.(1-x)) from 0 to 1'
    print *, 'Obtained result: ', result
    print *, 'Expected result: ', ans
    print *, 'Absolute error estimate: ', abserr
    print *, 'Actual error: ', abs(result-ans)
    print *, 'Required number of integrand evaluations: ', neval
    print *, 'Exit status: ', ier
    print *,

  contains

    function f(x) 
      real(sp) :: f
      real(sp), intent(in) :: x
      f = sin(10.0_sp*x)
    end function f

  end subroutine test_qaws

  subroutine test_qawc
    use kinds, only: sp
    use qawc_interface
    implicit none

    real(sp), parameter :: a = -1.0_sp
    real(sp), parameter :: b = 1.0_sp
    real(sp), parameter :: c = 0.5_sp
    real(sp), parameter :: epsabs = 0.0_sp
    real(sp), parameter :: epsrel = 1.0e-3_sp

    real(sp) :: result, abserr
    integer :: neval, ier

    integer, parameter :: limit = 100
    integer, parameter :: lenw = limit*4

    integer :: last
    integer, dimension(limit) :: iwork
    real(sp), dimension(lenw) :: work

    real(sp), parameter :: ans = -628.461728506562366229_sp

    call qawc(f, a, b, c, epsabs, epsrel, result, abserr, neval, ier, & 
         limit, lenw, last, iwork, work)

    print *, 'QAWC integration of 1/(x^2 + 10^-4) 1/(x-0.5) from -1 to 1'
    print *, 'Obtained result: ', result
    print *, 'Expected result: ', ans
    print *, 'Absolute error estimate: ', abserr
    print *, 'Actual error: ', abs(result-ans)
    print *, 'Required number of integrand evaluations: ', neval
    print *, 'Exit status: ', ier
    print *,

  contains

    function f(x) 
      real(sp) :: f
      real(sp), intent(in) :: x
      f = 1.0_sp/(x*x + 1.0e-4_sp)
    end function f

  end subroutine test_qawc

end module quadpack_test_cases
