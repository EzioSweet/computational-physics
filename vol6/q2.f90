program homework
    implicit none
    real*8::x,y,y2,step(4)
    integer::i,n ,j
    step=[0.1d0,0.1d0/2d0,0.1d0/4d0,0.1d0/8d0]
    n=int(1.5/step(1))+1
    x=0d0
    y=3
    open(50,file= "euler-improved-1.csv")
    do i=1,n
        y2=y+step(1)*(-x*x*y*y)
        y=y+step(1)/2d0*((-x*x*y*y)+(-(x+step(1))*(x+step(1))*y2*y2))
        x=x+step(1)
        write(50,"(f15.10,a4,f15.10)"),x,",",y
    end do
    close(50)

    n=int(1.5/step(2))+1
    x=0d0
    y=3
    open(60,file= "euler-improved-2.csv")
    do i=1,n
        y2=y+step(2)*(-x*x*y*y)
        y=y+step(2)/2d0*((-x*x*y*y)+(-(x+step(2))*(x+step(2))*y2*y2))
        x=x+step(2)
        write(60,"(f15.10,a4,f15.10)"),x,",",y
    end do
    close(60)

    n=int(1.5/step(3))+1
    x=0d0
    y=3
    open(70,file= "euler-improved-3.csv")
    do i=1,n
        y2=y+step(3)*(-x*x*y*y)
        y=y+step(3)/2d0*((-x*x*y*y)+(-(x+step(3))*(x+step(3))*y2*y2))
        x=x+step(3)
        write(70,"(f15.10,a4,f15.10)"),x,",",y
    end do
    close(70)

    n=int(1.5/step(4))+1
    x=0d0
    y=3
    open(80,file= "euler-improved-4.csv")
    do i=1,n
        y2=y+step(4)*(-x*x*y*y)
        y=y+step(4)/2d0*((-x*x*y*y)+(-(x+step(4))*(x+step(4))*y2*y2))
        x=x+step(4)
        write(80,"(f15.10,a4,f15.10)"),x,",",y
    end do
    close(80)
end program homework

