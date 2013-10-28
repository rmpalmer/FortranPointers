module avl
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

    subroutine lookup(t, value)
        type(tree), intent(inout) :: t
        real, intent(in) :: value
        type(node_type), pointer :: added
        !
        added = r_lookup(t%next_np)
      contains
        !
        recursive function r_lookup(arg_np) result(added)
          type(node_type), pointer, intent(inout) :: arg_np
          type(node_type), pointer :: added
          !
          if (associated(arg_np)) then
             if (value < arg_np%info%value) then
                added%left_np => r_lookup(arg_np%left_np)
                if (height(arg_np%left_np) == (height(arg_np%right_np) + 2)) then
                   if (value < arg_np%left_np%info%value) then
                      added => rotate_with_left_child(arg_np)
                   else
                      added => double_with_left_child(arg_np)
                   end if
                end if
             else if (value > arg_np%info%value) then
                added%right_np = r_lookup(arg_np%right_np)
                if (height(arg_np%right_np) == (height(arg_np%left_np) + 2)) then
                   if (value > arg_np%right_np%info%value) then
                      added => rotate_with_right_child(arg_np)
                   else
                      added => double_with_right_child(arg_np)
                   end if
                end if
             else
                call modify_target(arg_np)
             end if
          else
             call insert_target(arg_np)
          end if
          arg_np%node_height = 1 + max(height(arg_np%right_np), height(arg_np%left_np))
          !
        end function r_lookup
        !
        function rotate_with_left_child(arg_np) result(rotated)
            type(node_type), pointer, intent(inout) :: arg_np
            !
            type(node_type), pointer :: rotated
            !
            write (*,'(''rotate_with_left_child at id: '',i6)') arg_np%id
            !
            rotated => arg_np%left_np
            arg_np%left_np => rotated%right_np
            rotated%right_np => arg_np
            arg_np%node_height = 1 + max(height(arg_np%left_np), height(arg_np%right_np))
            rotated%node_height = 1 + max(height(rotated%left_np), arg_np%node_height)
            !
        end function rotate_with_left_child
        !
        function rotate_with_right_child(arg_np) result(rotated)
            type(node_type), pointer, intent(inout) :: arg_np
            !
            type(node_type), pointer :: rotated
            !
            write (*,'(''rotate_with_right_child at id: '',i6)') arg_np%id
            !
            rotated => arg_np%right_np
            arg_np%right_np => rotated%left_np
            rotated%left_np => arg_np
            arg_np%node_height = 1 + max(height(arg_np%right_np), height(arg_np%left_np))
            rotated%node_height = 1 + max(height(rotated%right_np), arg_np%node_height)
            !
        end function rotate_with_right_child
        !
        function double_with_left_child(arg_np) result(rotated)
            type(node_type), pointer, intent(inout) :: arg_np
            !
            type(node_type), pointer :: rotated
            arg_np%left_np => rotate_with_right_child(arg_np%left_np)
            rotated => rotate_with_left_child(arg_np)
        end function double_with_left_child
        !
        function double_with_right_child(arg_np) result(rotated)
            type(node_type), pointer, intent(inout) :: arg_np
            !
            type(node_type), pointer :: rotated
            arg_np%right_np => rotate_with_left_child(arg_np%right_np)
            rotated => rotate_with_right_child(arg_np)
        end function double_with_right_child
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
      end subroutine lookup

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

end module avl
