program homework
    real*8::a,b,x,h,S,n
    a=1d0
    b=5d0
    n=40
    open(50,file="Simpson.csv")
    do while(n<1000)
        h=(b-a)/n
        S=0d0
        x=a
        do while(x<b)
            S=S+h/6d0*(sin(x)+4*sin(x+h/2)+sin(x+h))
            x=x+h
        end do
        write (50,"(f6.4,a3,f12.9)"),h,",",S-cos(1d0)+cos(5d0)
        n=n+5
    end do
end program homework