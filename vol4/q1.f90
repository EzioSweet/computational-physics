real*8 function phi(x,i,xarray) result(y)
    implicit none
    real*8::x
    integer::i,j
    real*8,dimension(16)::xarray
    y=1d0
    do j=1,16
        if (j==i) then
            cycle
        end if
        y=y*(x-xarray(j))/(xarray(i)-xarray(j))
    end do
    return
end function

program homework
    implicit none
    integer:: i,j
    real*8,dimension(16)::x,y
    real*8,external::phi
    real*8::result
    do i=1,16
        x(i)=-5d0+10d0/15.0*(i-1)
        y(i)=1d0/(1d0+x(i)**2)
    end do
    open(50,file="result.txt")
    do i=-500,500
        result = 0
        do j=1,16
            result= result + y(j) * phi(i/100d0,j,x)
        end do
        write(50,"(f10.4,a2,f10.4)") i/100d0,",",result
    end do
end program homework