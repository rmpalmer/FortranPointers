module avl_struct
    implicit none
    private

    type info_type
        real :: value
    end type info_type

    type node_type
        type(info_type) :: info
        type(node_type), pointer :: left_np
        type(node_type), pointer :: right_np
        integer :: node_height = 0
    end type node_type

    type tree
        type(node_type), pointer :: next_np => null()
        integer :: count = 0
    end type tree

    public lookup, tree, print_tree, smallest

contains

    function lookup(t, value) result (added)
        type(tree), intent(inout) :: t
        real, intent(in) :: value
        integer :: added
        !
        added = r_lookup(t%next_np)
      contains
        !
        recursive function r_lookup(arg_np) result(added)
            type(node_type), pointer, intent(inout) :: arg_np
            integer :: added
            !
            added = 0
            if (associated(arg_np)) then
                if (value < arg_np%info%value) then
                    added = r_lookup(arg_np%left_np)
                    if (height(arg_np%left_np) == height(arg_np%right_np) + 2) then
                        if (value > arg_np%left_np%info%key) call rotate_right(arg_np%left_np)
                        call rotate_left(arg_np)
                    else
                        arg_np%node_height = 1 + max(height(arg_np%left_np), height(arg_np%right_np))
                    end if
                else if (value > arg_np%info%value) then
                    added = r_lookup(arg_np%right_np)
                    if (height(arg_np%right_np) == height(arg_np%left_np) + 2) then
                        if (value > arg_np%right_np%info%key) call rotate_left(arg_np%right_np)
                        call rotate_right(arg_np)
                    else
                        arg_np%node_height = 1 + max(height(arg_np%right_np), height(arg_np%left_np))
                    end if
                else
                    call modify_target(arg_np)
                    added = 0
                end if
            else
                call insert_target(arg_np)
                added = 1
            end if
        contains
            !
            subroutine rotate_left
            end subroutine rotate_left
            !
            subroutine rotate_right
            end subroutine rotate_right
            !
            pure function height(arg_np) result(height_r)
            end function height
            !
        end function r_lookup
        !
        subroutine insert_target(arg_np)
            type(node_type), pointer, intent(inout) :: arg_np
            allocate(arg_np)
            nullify(arg_np%left_np)
            nullify(arg_np%right_np)
            arg_np%info%value = value
        end subroutine insert_target
        !
        subroutine modify_target(arg_np)
            type(node_type), pointer, intent(inout) :: arg_np
            arg_np%info%value = value
        end subroutine modify_target
        !
    end function lookup

    subroutine print_tree(t)
        type(tree), intent(in) :: t
        !
        call r_print_tree(t%next_np)
    end subroutine print_tree

    recursive subroutine r_print_tree(arg_np)
        type(node_type), pointer, intent(in) :: arg_np
        !
        if (associated(arg_np)) then
            call r_print_tree(arg_np%left_np)
            write(*,'(F10.9)') arg_np%info%value
            call r_print_tree(arg_np%right_np)
        end if
    end subroutine r_print_tree

    function smallest(t) result(small)
        type(tree), intent(in) :: t
        real :: small
        small = r_smallest(t%next_np)
    end function smallest

    recursive function r_smallest(arg_np) result(current)
        type(node_type), pointer, intent(in) :: arg_np
        real :: current
        if (associated(arg_np%left_np)) then
            current = r_smallest(arg_np%left_np)
        else
            current = arg_np%info%value
        end if
    end function r_smallest

end module avl_struct
