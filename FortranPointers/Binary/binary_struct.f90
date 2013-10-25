module binary_struct

    implicit none
    private

    type info_type
        real :: value
    end type info_type

    type node_type
        type(info_type) :: info
        type(node_type), pointer :: left_np
        type(node_type), pointer :: right_np
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
                else if (value > arg_np%info%value) then
                    added = r_lookup(arg_np%right_np)
                else
                    call modify_target(arg_np)
                    added = 0
                end if
            else
                call insert_target(arg_np)
                added = 1
            end if
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

end module binary_struct
