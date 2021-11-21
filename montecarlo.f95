program montecarlo_method
    implicit none
    ! P_(in circle) = Area circle / Area square = pi*2^2 / 4^2 = pi / 4
    
    ! assuming center of circle is p(0,0)
    ! (Xc,Yc) (Xp, Yp) 
    ! point (Xp, Yp) is inside circle if d < r & on the circle if d = r
    ! this is equal to d^2 < r^2 for inside circle & d^2 = r^2 for on circle
    ! d^2= (Xp - Xc)^2 + (Yp - Yc) (and compare with r^2)

    integer :: k, i, inside_circle, outside_circle
    real, parameter :: pi = 4.*atan(1.)
    real :: r, area_circle, area_sqr
    real :: x_c, y_c, x_p, y_p
    ! random number stuff
    real :: low, high, random
    integer, dimension(8) :: values 
    integer, dimension(:), allocatable :: seed
    r = 2.0
    area_circle = pi*r**2
    area_sqr = r**2
    x_c = 0
    y_c = 0
    low = -2.0
    high = 2.0
    inside_circle = 0
    outside_circle = 0

    ! random number generation weirdness
    call date_and_time(VALUES=values)
    call random_seed(size=k)
    allocate(seed(k))
    seed(:) = values(:)
    call random_seed(put=seed)

    do  i = 0,10000
        call random_number(random)
        x_p = low + (high - low)* random
        call random_number(random)
        y_p = low + (high - low) * random

        print*, '(x_p, y_p) = ', x_p, y_p
        if ((x_p**2 + y_p**2) .le. r**2) then
            inside_circle = inside_circle + 1
            print*, "point inside of circle"
        else 
            print*, "point is outside of circle"     
            outside_circle = outside_circle + 1
        endif
    end do

    print*, ' pi: ', pi
    print*, ' area square = ', area_sqr
    print*, ' area circle = ', area_circle
    print*, "Number of points inside circle: ", inside_circle
    print*, "Number of points outside circle: ", outside_circle

end program montecarlo_method
