real function fict(n) result(result)
    implicit none
    integer :: i,n
    real :: result
    result = 1
    do i=1,n
        result = result*i
    end do
    return
end function fict

program homework

    implicit none
    integer:: n,m
    real::n_fict,nm_fict,m_fict
    real, external :: fict
    print *,"请输入n和m"
    read (*,*) n,m
    m_fict =  fict(m)
    n_fict = fict(n)
    nm_fict = fict(n-m)
    print *,"排列数为："
    print "(f9.0)", n_fict/nm_fict
    print *,"组合数为："
    print "(f9.0)" ,n_fict/(nm_fict*m_fict)
end program