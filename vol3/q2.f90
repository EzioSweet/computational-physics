program homework
    implicit none
    integer :: i, j, k, r
    real*8, dimension(9, 9) :: A, u, l
    real*8, dimension(9) :: x, b, y
    real*8 :: sum
    open(unit = 60, file = "./matrix.txt", action = "read")
    open(unit = 50, file = "./vector.txt", action = "read")
    do i = 1, 9
        read (60, *) A(i, :)
        read (50, *) b(i)
    end do
    u = 0.0d0
    l = 0.0d0
    u(1, :) = A(1, :)
    l(:, 1) = A(:, 1) / u(1, 1)
    do k = 2, 9
        do j = k, 9
            sum = 0
            do r = 1, k - 1
                sum = sum + l(k, r) * u(r, j)
            end do
            u(k, j) = A(k, j) - sum
        end do
        do i = k, 9
            if (i == k) then
                l(i, i) = 1
            else
                sum = 0
                do r = 1, k - 1
                    sum = sum + l(i, r) * u(r, k)
                end do
                l(i, k) = (A(i, k) - sum) / u(k, k)
            end if
        end do
    end do
    y(1) = b(1) / l(1, 1)
    do i = 2, 9
        sum = 0
        do j = 1, i - 1
            sum = sum + l(i, j) * y(j)
        end do
        y(i) = (b(i) - sum) / l(i, i)
    end do
    x(9) = y(9) / u(9, 9)
    do i = 8, 1, -1
        sum = 0
        do j = 9, i + 1, -1
            sum = sum + u(i, j) * x(j)
        end do
        x(i) = (y(i) - sum) / u(i, i)
    end do
    print "(1f12.8)", x
end program homework