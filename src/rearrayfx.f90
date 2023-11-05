!***************************************************************************************************
!*  ReArrayFX: Reallocatable array like Fortran-container
!*  Copyright (C) 2023 Bálint Aradi, University of Bremen
!*  Licensed under the BSD-2-Clause-Patent license.
!***************************************************************************************************
module rearrayfx
  use iso_fortran_env, only : real64
  implicit none
  private

  public :: rearray_real64_r2


  type :: rearray_real64_r2
    private
    real, public :: overprov = 1.0
    real(real64), pointer, public :: view(:,:) => null()
    real(real64), pointer :: storage_(:) => null()
  contains
    procedure :: allocate => rearray_real64_r2_allocate
    procedure :: assign_array => rearray_real64_r2_assign_array
    generic :: assignment(=) => assign_array
    final :: rearray_real64_r2_final
  end type rearray_real64_r2

contains


  subroutine rearray_real64_r2_allocate(this, newshape)
    class(rearray_real64_r2), intent(inout) :: this
    integer, intent(in) :: newshape(:)

    integer :: oldsize, newsize
    real(real64), pointer :: buffer(:)

    if (associated(this%storage_)) then
      oldsize = size(this%storage_)
    else
      oldsize = -1
    end if
    newsize = product(newshape)
    if (newsize > oldsize) then
      newsize = int(real(newsize) * this%overprov)
      print *, "Debug: reallocating to size", newsize
      allocate(buffer(1:newsize), source=0.0_real64)
      if (oldsize > 0) then
        buffer(1:size(this%storage_)) = this%storage_
        deallocate(this%storage_)
      end if
      this%storage_ => buffer
    end if
    this%view(1:newshape(1), 1:newshape(2)) => this%storage_(1:product(newshape))

  end subroutine rearray_real64_r2_allocate


  elemental subroutine rearray_real64_r2_final(this)
    type(rearray_real64_r2), intent(inout) :: this

    if (associated(this%storage_)) deallocate(this%storage_)

  end subroutine rearray_real64_r2_final


  subroutine rearray_real64_r2_assign_array(this, array)
    class(rearray_real64_r2), intent(inout) :: this
    real(real64), intent(in) :: array(:,:)

    call this%allocate(shape(array))
    this%view = array

  end subroutine rearray_real64_r2_assign_array

end module rearrayfx
