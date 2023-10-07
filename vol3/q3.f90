program homework
    implicit none
    integer::i,j,k
    real*8,dimension(9, 9) :: A
    real*8,dimension(9)::x,b,error
    real*8::sum
    open(unit=60,file="./matrix.txt",action="read")
    open(unit=50,file="./vector.txt",action="read")
    do i=1,9
        read (60,*) A(i,:)
        read (50,*) b(i)
    end do
    x=0.0d0
    k=0
    do while(1)
        k=k+1
        do i=1,9
            sum = 0
            do j=1,9
                sum=sum+A(i,j)*x(j)
            end do
            error(i) = (-(sum-A(i,i)*x(i)-b(i))/A(i,i))**2-x(i)**2
            x(i) =-(sum-A(i,i)*x(i)-b(i))/A(i,i)
        end do
        sum = 0
        do i = 1,9
            sum = sum +error(i)
        end do
        sum = sum**(1/2d0)
        if(sum<0.001)then
            exit
        end if
    end do
    print "(a12,i3)", "迭代次数：",k
    print "(1f12.8)", x
end program homework