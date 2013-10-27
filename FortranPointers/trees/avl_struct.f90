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
        integer :: id = 0
    end type node_type
    
    type tree
        type(node_type), pointer :: next_np => null()
        integer :: count = 0
    end type tree

    public lookup, tree, print_tree, smallest, max_height

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
                write(*,'(''will insert new value on left'')')
                added = r_lookup(arg_np%left_np)
                if (height(arg_np%left_np) == (height(arg_np%right_np) + 2)) then
                   if (value > arg_np%left_np%info%value) call rotate_right(arg_np%left_np)
                   call rotate_left(arg_np)
                else
                   arg_np%node_height = 1 + max(height(arg_np%left_np), height(arg_np%right_np))
                end if
             else if (value > arg_np%info%value) then
                write(*,'(''will insert new value on right'')')
                added = r_lookup(arg_np%right_np)
                if (height(arg_np%right_np) == (height(arg_np%left_np) + 2)) then
                   if (value > arg_np%right_np%info%value) call rotate_left(arg_np%right_np)
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
          !
        end function r_lookup
        !
        subroutine rotate_left(arg_np)
            type(node_type), pointer, intent(inout) :: arg_np
            !
            type(node_type), pointer :: temp_np
            !
            ! only crashes when I uncomment this...
            write (*,'(''rotate_left at id: '',i6)') arg_np%id
            !
            temp_np => arg_np%left_np
            write(*,'(''{'')')
            if (associated(temp_np%right_np)) then
               arg_np%left_np => temp_np%right_np ! bombs here
            else
               nullify(arg_np%left_np)
            end if
            write(*,'(''}'')')
            temp_np%right_np => arg_np
            arg_np%node_height = 1 + max(height(arg_np%left_np), height(arg_np%right_np))
            temp_np%node_height = 1 + max(height(temp_np%left_np), arg_np%node_height)
            arg_np => temp_np
            !
        end subroutine rotate_left
        !
        subroutine rotate_right(arg_np)
            type(node_type), pointer, intent(inout) :: arg_np
            !
            type(node_type), pointer :: temp_np
            !
            write (*,'(''rotate_right at id: '',i6)') arg_np%id
            !
            temp_np => arg_np%right_np
            write(*,'(''<'')')
            arg_np%right_np => temp_np%left_np
            write(*,'(''>'')')
            temp_np%left_np => arg_np
            arg_np%node_height = 1 + max(height(arg_np%right_np), height(arg_np%left_np))
            temp_np%node_height = 1 + max(height(temp_np%right_np), arg_np%node_height)
            arg_np => temp_np
            !
        end subroutine rotate_right
        !
        pure function height(arg_np) result(height_r)
            type(node_type), pointer :: arg_np
            integer :: height_r
            !
            if (associated(arg_np)) then
                height_r = arg_np%node_height
            else
                height_r = -1
            endif
        end function height
        !
        subroutine insert_target(arg_np)
          type(node_type), pointer, intent(inout) :: arg_np
          !
          write (*,'(''insert_target'')')
          !
          allocate(arg_np)
          nullify(arg_np%left_np)
          nullify(arg_np%right_np)
          t%count = t%count + 1
          arg_np%id = t%count
          arg_np%info%value = value
        end subroutine insert_target
        !
        subroutine modify_target(arg_np)
          type(node_type), pointer, intent(inout) :: arg_np
          !
          write (*,'(''modify_target'')')
          !
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
        integer :: l, r
        !
        l = -1
        r = -1
        if (associated(arg_np)) then
           call r_print_tree(arg_np%left_np)
           !write(*,'(F10.9)') arg_np%info%value
           if (associated(arg_np%left_np)) l = arg_np%left_np%id
           if (associated(arg_np%right_np)) r = arg_np%right_np%id
           write(*,'(''id: '',i4,'' value: '',F10.9,'' left: '',I4,'' right: '',I4)') arg_np%id, arg_np%info%value, l, r
           call r_print_tree(arg_np%right_np)
        end if
      end subroutine r_print_tree
      
      function smallest(t) result(small)
        type(tree), intent(in) :: t
        real :: small
        small = r_smallest(t%next_np)
      end function smallest
      
      function max_height(t) result(h)
        type(tree), intent(in) :: t
        integer :: h
            if (associated(t%next_np)) then
                h = t%next_np%node_height
            else
                h = -1
            endif
      end function max_height

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
