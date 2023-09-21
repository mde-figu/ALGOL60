program MyTreesearch
  implicit none

  integer, parameter :: max_nodes = 100
  integer, dimension(max_nodes) :: Data
  integer, dimension(max_nodes) :: Left
  integer, dimension(max_nodes) :: Right
  integer result_index
  logical found

  ! Initialize the binary tree structure (example)
  Data(1) = 10
  Left(1) = 2
  Right(1) = 3

  Data(2) = 5
  Left(2) = 4
  Right(2) = 0

  Data(3) = 15
  Left(3) = 0
  Right(3) = 6

  ! Call the MyTreesearch procedure
  found = MyTreesearch(1, 15, result_index)

  ! Interpret the result
  if (found) then
    print *, "Value 15 found at index:", result_index
  else
    print *, "Value 15 not found in the tree."
  end if

contains

  recursive logical function MyTreesearch(root, arg, m) 
    integer, intent(in) :: root, arg
    integer, intent(out) :: m

    integer d
    d = Data(root)

    if (root == 0) then
      MyTreesearch = .false.
    else if (arg == d) then
      m = root
      MyTreesearch = .true.
    else if (arg > d) then
      MyTreesearch = MyTreesearch(Left(root), arg, m)
    else
      MyTreesearch = MyTreesearch(Right(root), arg, m)
    end if
  end function MyTreesearch

end program MyTreesearch
