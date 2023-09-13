real*8 function g(x_last,x) result(x_next)
    implicit none
    real*8 :: x,x_last
    x_next = x - ((x**3)/3-x)*(x-x_last)/(((x**3)/3-x)-((x_last**3)/3-x_last))
    return
end function g

program homework
    implicit none
    real*8 :: x,epsilon,x_next,x_last
    real*8,external ::g
    integer:: i = 1
    print *, "请输入初值x和阈值epsilon"
    read (*,*), x,epsilon
    x_last = x-0.5
    x_next = g(x_last,x)
    print "(a15,i3,a3,f12.8,a7,f12.8)", "迭代次数:",i,",x:",x,",g(x):",x_next
    do while (abs(x_next - x)>epsilon)
        i=i+1
        x_last = x
        x=x_next
        x_next = g(x_last,x)
        print "(a15,i3,a3,f12.8,a7,f12.8)", "迭代次数:",i,",x:",x,",g(x):",x_next
    end do
    print "(a15,f12.8)", " 最终结果:",x_next
end program