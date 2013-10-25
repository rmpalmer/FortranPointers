program main
    use binary_struct, only: binary_tree => tree, binary_lookup => lookup, &
                             binary_smallest => smallest, binary_print => print_tree
    use timer_class
    implicit none
    !
    integer :: count
    integer :: i
    real :: r
    integer :: n
    integer :: un
    integer :: istat
    integer :: added
    integer, allocatable :: seed(:)
    type(binary_tree) :: t
    type(timer) :: stopwatch
    !
    write(*,'(''Begin'')')
    count = 10000
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
    added = 0
    call stopwatch%start_timer()
    do i=1,count
        call random_number(r)
        added = added + binary_lookup(t, r)
    end do
    write (*,'(I12,'' random numbers: '',F12.2)') added,stopwatch%elapsed_time()
    added = 0
    call stopwatch%start_timer()
    do i=1,count
        call random_number(r)
        added = added + binary_lookup(t,real(i))
    end do
    write (*,'(I12,'' sequential numbers: '',F12.2)') added,stopwatch%elapsed_time()
    write (*,'(''smallest: '',F10.9)') binary_smallest(t)
    !call print_tree(t)
    write(*,'(''added '',i0,'' elements'')') added
    !
    write(*,'(''End'')')
end program main
