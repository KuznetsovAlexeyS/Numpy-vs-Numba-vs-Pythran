program TestExpTime
    implicit None
    include 'mpif.h'
    integer :: i, j, k
    real :: startTime, endTime, resultTime, r, jr
    complex, allocatable :: customResult(:, :, :), source(:, :, :)
    allocate(customResult(50, 300, 710))
    allocate(source(50, 300, 710))
    resultTime = 0
    do i = 1, 100
        call FillArrayByRandomComplexNumbers(source)
        startTime = MPI_Wtime()
        call MagExp(source, customResult)
        endTime = MPI_Wtime()
        resultTime = resultTime + (endTime - startTime)
    end do
   
    print *, (resultTime)

    contains

    subroutine FillArrayByRandomComplexNumbers(result)
        implicit None
        complex, dimension(:, :, :) :: result
        integer :: i, j, k
        real :: r, jr
    
        do i=1, size(result, 1)
            do j=1, size(result, 2)
                do k=1, size(result, 3)
                    call random_number(r)
                    call random_number(jr)
                    result(i, j, k) = complex(r, jr)
                end do
            end do
        end do
    
    end subroutine FillArrayByRandomComplexNumbers

    subroutine MagExp(source, result)
        use OMP_LIB
        implicit None
        integer :: i, j, k
        complex, dimension(:, :, :) :: source, result

        !$OMP PARALLEL PRIVATE(i, j, k)
        !$OMP DO
        do k = 1, size(result, 3)
            do j = 1, size(result, 2)
                do i = 1, size(result, 1)
                    result(i, j, k) = complex(cos(real(source(i, j, k))), sin(real(source(i, j, k)))) * exp(-aimag(source(i, j, k)))
                 end do
            end do
        end do
        !$OMP END DO
        !$OMP END PARALLEL
    end subroutine MagExp

end program TestExpTime