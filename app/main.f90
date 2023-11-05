!***************************************************************************************************
!*  ReArrayFX: Reallocatable array like Fortran-container
!*  Copyright (C) 2023 Bálint Aradi, University of Bremen
!*  Licensed under the BSD-2-Clause-Patent license.
!***************************************************************************************************
program main
  use iso_fortran_env, only : real64
  use rearrayfx, only: rearray_real64_r2
  implicit none

  type(rearray_real64_r2) :: rearray
  real(real64) :: dummy(2, 2)

  ! Reallocation should have an overprovision factor of 2.0 (twice as much as necessarily needed)
  rearray = rearray_real64_r2(overprov=2.0)

  call rearray%allocate(newshape=[2, 2])
  rearray%view(:,:) = reshape([1, 2, 3, 4], [2, 2])
  print *, "Shape:", shape(rearray%view)
  print *, "Content:", rearray%view

  call rearray%allocate(newshape=[2, 3])
  print *, "Shape:", shape(rearray%view)
  print *, "Content:", rearray%view

  call rearray%allocate(newshape=[2, 6])
  print *, "Shape:", shape(rearray%view)
  print *, "Content:", rearray%view

  dummy(:,:) = reshape([9, 8, 7, 6], shape=[2, 2])
  rearray = dummy
  print *, "Shape:", shape(rearray%view)
  print *, "Content:", rearray%view

end program main
