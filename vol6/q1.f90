program homework
    implicit none
    real*8::x,y,step(4)
    integer::i,n ,j
    step=[0.1d0,0.1d0/2d0,0.1d0/4d0,0.1d0/8d0]
    n=int(1.5/step(1))+1
    x=0d0
    y=3
    open(50,file= "euler-1.csv")
    do i=1,n
        y=y+step(1)*(-x*x*y*y)
        x=x+step(1)
        write(50,"(f15.10,a4,f15.10)"),x,",",y
    end do
    close(50)

    n=int(1.5/step(2))+1
    x=0d0
    y=3
    open(60,file= "euler-2.csv")
    do i=1,n
        y=y+step(2)*(-x*x*y*y)
        x=x+step(2)
        write(60,"(f15.10,a4,f15.10)"),x,",",y
    end do
    close(60)

    n=int(1.5/step(3))+1
    x=0d0
    y=3
    open(70,file= "euler-3.csv")
    do i=1,n
        y=y+step(3)*(-x*x*y*y)
        x=x+step(3)
        write(70,"(f15.10,a4,f15.10)"),x,",",y
    end do
    close(70)

    n=int(1.5/step(4))+1
    x=0d0
    y=3
    open(80,file= "euler-4.csv")
    do i=1,n
        y=y+step(4)*(-x*x*y*y)
        x=x+step(4)
        write(80,"(f15.10,a4,f15.10)"),x,",",y
    end do
    close(80)
end program homework

