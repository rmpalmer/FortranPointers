program main
    use binary_struct, only: binary_tree => tree, binary_lookup => lookup, &
                             binary_smallest => smallest, binary_print => print_tree
!    use avl_struct, only: avl_tree => tree, avl_lookup => lookup, &
!                            avl_height => max_height, &
!                            avl_smallest => smallest, avl_print => print_tree
    use avl, only: avl_tree => tree, avl_lookup => lookup, &
                            avl_height => max_height, &
                            avl_smallest => smallest, avl_print => print_tree
    use timer_class
    implicit none
    !
    integer :: count
    integer :: i
    real :: r
    integer :: n
    integer :: un
    integer :: istat
    integer :: added_b
    integer :: added_a
    integer, allocatable :: seed(:)
    type(binary_tree) :: t
    type(avl_tree) :: at
    type(timer) :: stopwatch
    !
    write(*,'(''Begin'')')
    count = 4
    call random_seed(size=n)
    allocate(seed(n))
    do i=1,n
       seed(i) = i
    end do
    open(newunit=un, file="/dev/urandom", access="stream", &
        form="unformatted", action="read", status="old", iostat=istat)
    if (istat == 0) then
        write (*,*) 'open ok'
        read(un) seed
        close(un)
    else
        write(*,*) 'no open'
    end if
    call random_seed(put=seed)
    deallocate(seed)
    added_b = 0
    added_a = 0
    call stopwatch%start_timer()
    do i=1,count
        call random_number(r)
        write (*,'(''adding '',i6,'' '',f10.9)') i, r
        !added_b = added_b + binary_lookup(t, r)
        call avl_lookup(at, r)
        write (*,'(''max h is '',i6)') avl_height(at)
        call avl_print(at)
    end do
    write (*,'(I12,'' random numbers: '',F12.2)') added_a,stopwatch%elapsed_time()
    !call avl_print(at)
    !call binary_print(t)
    !added_b = 0
    !call stopwatch%start_timer()
    !do i=1,count
    !    call random_number(r)
    !    added_b = added_b + binary_lookup(t,real(i))
    !end do
    !write (*,'(I12,'' sequential numbers: '',F12.2)') added_b,stopwatch%elapsed_time()
    !write (*,'(''smallest: '',F10.9)') binary_smallest(t)
    !!call print_tree(t)
    !write(*,'(''added_b '',i0,'' elements'')') added_b
    !
    write(*,'(''End'')')
end program main
