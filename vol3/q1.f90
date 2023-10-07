program homework
    implicit none
    integer::i,j
    real*8,dimension(9, 9) :: A
    real*8,dimension(9)::v,x
    real*8::sum
    open(unit=60,file="./matrix.txt",action="read")
    open(unit=50,file="./vector.txt",action="read")
    do i=1,9
        read (60,*) A(i,:)
        read (50,*) v(i)
    end do
    do i=1,8
        do j=i+1,9
            v(j)=v(j)-v(i)*A(j,i)/A(i,i)
            A (j,:) = A (j,:) - A(i,:)*A(j,i)/A(i,i)
        end do
    end do
    x(9)=v(9)/A(9,9)
    do i =8 ,1 , -1
        sum = 0
        do j = 9, +1 ,-1
            sum = sum+A(i,j)*x(j)
        end do
        x(i)=(v(i)-sum)/A(i,i)
    end do
    print "(1f12.8)", x
end program homework