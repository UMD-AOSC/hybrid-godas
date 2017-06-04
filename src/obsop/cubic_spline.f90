module cubic_spline
  !! cubic spline class
  implicit none
  private

  public :: cspline

  type :: cspline
     real, allocatable :: x(:)
     real, allocatable :: y(:)
     real, allocatable :: y2(:)
   contains
     procedure :: interp => cspline_interp
     procedure :: deriv  => cspline_deriv
  end type cspline

  interface cspline
     module procedure cspline_init
  end interface cspline

  real, parameter, public :: cspline_error = -9.99e30
  
contains



  !============================================================
  !============================================================
  function cspline_init(x,y)
    ! TODO, allow specification of the derivative at the boundaries.
    ! for now only natural boundaries are possible, but this
    ! should take care of the majority of the situations
    real, intent(in) :: x(:), y(:)
    type(cspline) :: cspline_init
    
    real :: u(size(x)-1)
    real :: sig, p, qn, un
    integer :: n, i

    n = size(x)
    if(n <= 1) then
       print *, "ERROR: cspline_init() given array of size < 2"
       stop 1
    end if
    if(size(x) /= size(y)) then
       print *, "ERROR: cspline_init() given x,y of different sizes"
       stop 1
    end if


    allocate(cspline_init%y2(n))
    allocate(cspline_init%x(n))
    allocate(cspline_init%y(n))
    cspline_init%x = x
    cspline_init%y = y

    ! natural lower boundary condition
    cspline_init%y2(1) = 0.0
    u(1) = 0.0

    ! decomposition loop of the tridiagonal
    do i=2, n-1
       sig = (x(i)-x(i-1))/(x(i+1)-x(i-1))
       p = sig*cspline_init%y2(i-1)+2.0
       cspline_init%y2(i) = (sig-1.0) / p
       u(i) = (y(i+1)-y(i))/(x(i+1)-x(i)) - &
            (y(i)-y(i-1))/(x(i)-x(i-1))
       u(i) = (6.0*u(i)/(x(i+1)-x(i-1))-sig*u(i-1))/p
    end do

    ! natural upper boundary conditions
    un = 0.0
    qn = 0.0

    !backsubsitution
    cspline_init%y2(n) = (un-qn*u(n-1)) / (qn*cspline_init%y2(n-1)+1.0)
    do i=n-1,1,-1
       cspline_init%y2(i) = cspline_init%y2(i)*cspline_init%y2(i+1)+u(i)
    end do
  end function cspline_init
  !============================================================



  !============================================================
  !============================================================  
  pure function locate(this, x) result(idx)
    class(cspline), intent(in) :: this
    real, intent(in) :: x
    integer :: idx

    integer :: il, iu, im

    ! find the 2 x points the given x is between
    ! TODO, use the smarter algorithm in numerical recipes
    ! (hunt / locate)
    il = 1
    iu = size(this%x)
    do while(iu-il > 1)
       im = (il+iu) / 2
       if(x >= this%x(im)) then
          il=im
       else
          iu = im
       end if
    end do
    idx = max(1,min(size(this%x)-1,il))

    
  end function locate
  !============================================================



  !============================================================
  !============================================================  
  elemental real function cspline_interp(this, x) result(y)
    class(cspline), intent(in) :: this
    real, intent(in) :: x

    real :: a,b,h
    integer :: i1, i2


    ! ensure x is in the correct range
    if(x < this%x(1) .or. x > this%x(size(this%x))) then
       y = cspline_error
       return
    end if

    ! find index
    i1 = locate(this, x)
    i2 = i1+1

    ! calculate value at given x
    h=this%x(i2)-this%x(i1)
    a=(this%x(i2)-x)/h
    b=(x-this%x(i1))/h
    y=a*this%y(i1)+b*this%y(i2)+&
         ((a*a*a-a)*this%y2(i1)+&
         (b*b*b-b)*this%y2(i2))*(h*h)/6.0
  end function cspline_interp
  !============================================================



  !============================================================
  !============================================================
  elemental real function cspline_deriv(this, x) result(y)
    class(cspline), intent(in) :: this
    real, intent(in) :: x

    real :: a,b,h
    integer :: i1, i2


    ! ensure x is in the correct range
    if(x < this%x(1) .or. x > this%x(size(this%x))) then
       y = cspline_error
       return
    end if

    ! find index
    i1 = locate(this, x)
    i2 = i1+1

    ! calculate value at given x
    h=this%x(i2)-this%x(i1)
    a=(this%x(i2)-x)/h
    b=(x-this%x(i1))/h
    y=(this%y(i2)-this%y(i1))/h + &
         ((3*(b*b)-1)*this%y2(i2) - &
           (3*(a*a)-1)*this%y2(i1))*h/6.0

  end function cspline_deriv
  !============================================================


end module cubic_spline
