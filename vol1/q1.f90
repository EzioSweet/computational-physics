program homework
    implicit none
    integer:: i
    real, dimension(5, 5):: input_matrix,output_matrix
    read(*,*), input_matrix
    open(50,file="data.txt")
    write(50,"(5f8.3)"), input_matrix
    close(50)
    open(60,file="data.txt")
    read(60,*), output_matrix
    print "(5f8.3)", output_matrix
    close(60)
end program