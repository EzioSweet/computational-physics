program homework
    implicit none
    integer::i,j,k
    real*8,dimension(9, 9) :: A,mat
    real*8,dimension(9)::x,b,error,vec
    real*8::sum,omega
    open(unit=60,file="./matrix.txt",action="read")
    open(unit=50,file="./vector.txt",action="read")
    do i=1,9
        read (60,*) mat(i,:)
        read (50,*) vec(i)
    end do

    do omega=1.0,1.8,0.01
        x=0.0d0
        k=0
        A=mat
        b=vec
        do while(1)
            k=k+1
            do i=1,9
                sum = 0
                do j=1,9
                    sum=sum+A(i,j)*x(j)
                end do
                error(i) = ((1-omega)*x(i) - omega * (sum - A(i,i)*x(i) - b(i)) / A(i,i))**2-x(i)**2
                x(i) = (1-omega)*x(i) - omega * (sum - A(i,i)*x(i) - b(i)) / A(i,i)
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
        print "(a12,f4.2,a12,i3)", "松弛系数：",omega,"迭代次数：",k
    end do
end program homework