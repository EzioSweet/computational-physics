real function P(n,m) result(result)
    implicit none
    integer :: i,n,m
    real :: result
    result = 1
    do i=n-m+1,n
        result = result*i
    end do
    return
end function P

real function C(n,m) result(result)
    implicit none
    integer :: i,n,m
    real :: result
    result = 1
    if (n-m>m) then
        do i=n-m+1,n
            result = result*i
        end do
        do i=1,m
            result = result/i
        end do
    else
        do i=m+1,n
            result = result*i
        end do
        do i=1,n-m
            result = result/i
        end do
    end if
    return
end function C

program homework

    implicit none
    integer:: n,m
    real, external :: C,P
    print *,"(v2)请输入n和m"
    read (*,*) n,m
    print *,"排列数为："
    print "(f9.0)", P(n,m)
    print *,"组合数为："
    print "(f9.0)" ,C(n,m)
end program