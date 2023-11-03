function f(x,y)
    implicit none
    real*8::x,y,  f
    f = - x*x*y*y
end function
program homework
    implicit none
    real*8::x,y,k1,k2,k3,k4,step(4)
    real*8,external::f
    integer::i,n ,j
    step=[0.1d0,0.1d0/2d0,0.1d0/4d0,0.1d0/8d0]
    n=int(1.5/step(1))+1
    x=0d0
    y=3
    open(50,file= "ode4-1.csv")
    do i=1,n
        k1=f(x,y)
        k2=f(x+step(1)/2,y+(step(1)/2)*k1)
        k3=f(x+step(1)/2,y+(step(1)/2)*k2)
        k4=f(x+step(1),y+step(1)*k3)
        y=y+step(1)/6d0*(k1+2*k2+2*k3+k4)
        x=x+step(1)
        write(50,"(f15.10,a4,f15.10)"),x,",",y
    end do
    close(50)

    n=int(1.5/step(2))+1
    x=0d0
    y=3
    open(60,file= "ode4-2.csv")
    do i=1,n
        k1=f(x,y)
        k2=f(x+step(2)/2,y+(step(2)/2)*k1)
        k3=f(x+step(2)/2,y+(step(2)/2)*k2)
        k4=f(x+step(2),y+step(2)*k3)
        y=y+step(2)/6d0*(k1+2*k2+2*k3+k4)
        x=x+step(2)
        write(60,"(f15.10,a4,f15.10)"),x,",",y
    end do
    close(60)

    n=int(1.5/step(3))+1
    x=0d0
    y=3
    open(70,file= "ode4-3.csv")
    do i=1,n
        k1=f(x,y)
        k2=f(x+step(3)/2,y+(step(3)/2)*k1)
        k3=f(x+step(3)/2,y+(step(3)/2)*k2)
        k4=f(x+step(3),y+step(3)*k3)
        y=y+step(3)/6d0*(k1+2*k2+2*k3+k4)
        x=x+step(3)
        write(70,"(f15.10,a4,f15.10)"),x,",",y
    end do
    close(70)

    n=int(1.5/step(4))+1
    x=0d0
    y=3
    open(80,file= "ode4-4.csv")
    do i=1,n
        k1=f(x,y)
        k2=f(x+step(4)/2,y+(step(4)/2)*k1)
        k3=f(x+step(4)/2,y+(step(4)/2)*k2)
        k4=f(x+step(4),y+step(4)*k3)
        y=y+step(4)/6d0*(k1+2*k2+2*k3+k4)
        x=x+step(4)
        write(80,"(f15.10,a4,f15.10)"),x,",",y
    end do
    close(80)
end program homework

