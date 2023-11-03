program homework
    real*8::a,b,x,h,S
    a=1d0
    b=5d0
    h=0.1d0
    S=0d0
    x=a
    do while(x<b)
        S=S+h/6d0*(sin(x)+4*sin(x+h/2)+sin(x+h))
        x=x+h
    end do
    print *,"采用Simpson积分："
    print "(a10,f6.4,a14,f10.8,a10,f12.9)","步长为",h,"积分值为",S,"误差为",S-cos(1d0)+cos(5d0)
    h=0.01d0
    x=a
    S=0d0
    do while(x<b)
        S=S+h/6d0*(sin(x)+4*sin(x+h/2)+sin(x+h))
        x=x+h
    end do
    print "(a10,f6.4,a14,f10.8,a10,f12.9)","步长为",h,"积分值为",S,"误差为",S-cos(1d0)+cos(5d0)
    h=0.001d0
    x=a
    S=0d0
    do while(x<b)
        S=S+h/6d0*(sin(x)+4*sin(x+h/2)+sin(x+h))
        x=x+h
    end do
    print "(a10,f6.4,a14,f10.8,a10,f12.9)","步长为",h,"积分值为",S,"误差为",S-cos(1d0)+cos(5d0)
end program homework