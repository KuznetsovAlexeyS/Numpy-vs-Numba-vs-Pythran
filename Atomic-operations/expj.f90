    subroutine expj_external_module(source, result)
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
    end subroutine expj_external_module
