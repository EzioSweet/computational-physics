
program homework
    implicit none
    integer:: i
    real, dimension(5, 5):: input_matrix
    real,dimension(5,1):: input_vector
    read(*,*), input_matrix
    read(*,*), input_vector
    print *,"A*v的计算结果为:"
    print "(1f8.3)",matmul(input_matrix,input_vector)
    print *,"vT*A*v的计算结果为："
    print "(1f8.3)",matmul(matmul(transpose(input_vector),input_matrix),input_vector)
end program
