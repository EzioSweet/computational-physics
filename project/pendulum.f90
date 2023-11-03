real function f1(theta1,theta2,v1,v2,m1,m2,L1,L2)
    real::theta1,theta2,v1,v2,m1,m2,L1,L2
    f1=v1
    return
end function f1
real function f2(theta1,theta2,v1,v2,m1,m2,L1,L2)
    real::theta1,theta2,v1,v2,m1,m2,L1,L2
    f2=v2
    return
end function f2
real function f3(theta1,theta2,v1,v2,m1,m2,L1,L2)
    real::theta1,theta2,v1,v2,m1,m2,L1,L2,g
    g=9.8
    f3=1/(L1*((m2*(cos(theta1-theta2)**2))-m1-m2))&
            *(L1*cos(theta1-theta2)*sin(theta1-theta2)*m2*(v1**2)&
            +m2*L2*sin(theta1-theta2)*(v2**2)&
            -cos(theta1-theta2)*sin(theta2)*g*m2+g*sin(theta1)*(m1+m2))
    return
end function f3
real function f4(theta1,theta2,v1,v2,m1,m2,L1,L2)
    real::theta1,theta2,v1,v2,m1,m2,L1,L2,g
    g=9.8
    f4=-1/(L2*(m2*(cos(theta1-theta2)**2)-m1-m2))&
            *(L2*cos(theta1-theta2)*sin(theta1-theta2)*m2*(v2**2)&
            +L1*sin(theta1-theta2)*(m1+m2)*(v1**2)&
            +cos(theta1-theta2)*sin(theta1)*g*m1+cos(theta1-theta2)*sin(theta1)*g*m2-sin(theta2)*g*m1-sin(theta2)*g*m2)
    return
end function f4

real function euler_theta1(theta1,theta2,v1,v2,step,m1,m2,L1,L2)
    real::theta1,theta2,v1,v2,m1,m2,L1,L2,step,delta
    delta=theta1+step*f1(theta1,theta2,v1,v2,m1,m2,L1,L2)
    euler_theta1=theta1+step/2*(f1(theta1,theta2,v1,v2,m1,m2,L1,L2)+f1(delta,theta2,v1,v2,m1,m2,L1,L2))
    return
end function euler_theta1

real function euler_theta2(theta1,theta2,v1,v2,step,m1,m2,L1,L2)
    real::theta1,theta2,v1,v2,m1,m2,L1,L2,step,delta

    delta=theta2+step*f2(theta1,theta2,v1,v2,m1,m2,L1,L2)
    euler_theta2=theta2+step/2*(f2(theta1,theta2,v1,v2,m1,m2,L1,L2)+f2(theta1,delta,v1,v2,m1,m2,L1,L2))
    return
end function euler_theta2

real function euler_v1(theta1,theta2,v1,v2,step,m1,m2,L1,L2)
    real::theta1,theta2,v1,v2,m1,m2,L1,L2,step,g,delta
    g=9.8
    delta=v1+step*f3(theta1,theta2,v1,v2,m1,m2,L1,L2)
    euler_v1=v1+step/2*(f3(theta1,theta2,v1,v2,m1,m2,L1,L2)+f3(theta1,theta2,delta,v2,m1,m2,L1,L2))
    return
end function euler_v1

real function euler_v2(theta1,theta2,v1,v2,step,m1,m2,L1,L2)
    real::theta1,theta2,v1,v2,m1,m2,L1,L2,step,g
    g=9.8
    delta=v2+step*f4(theta1,theta2,v1,v2,m1,m2,L1,L2)
    euler_v2=v2+step/2*(f4(theta1,theta2,v1,v2,m1,m2,L1,L2)+f4(theta1,theta2,v1,delta,m1,m2,L1,L2))
    return
end function euler_v2