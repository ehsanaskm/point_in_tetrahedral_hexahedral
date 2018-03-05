module my_subs

implicit none

contains

FUNCTION cross(a, b)
  REAL, DIMENSION(3) :: cross
  REAL :: size
  REAL, DIMENSION(3), INTENT(IN) :: a, b

  cross(1) = a(2) * b(3) - a(3) * b(2)
  cross(2) = a(3) * b(1) - a(1) * b(3)
  cross(3) = a(1) * b(2) - a(2) * b(1)

  size=sqrt(cross(1)**2+cross(2)**2+cross(3)**2)

  cross(1)=cross(1)/size
  cross(2)=cross(2)/size
  cross(3)=cross(3)/size


END FUNCTION cross

FUNCTION vector(c,f,ii)
  INTEGER :: ii
  REAL, DIMENSION(2,3) :: vector
  REAL, DIMENSION(:,:), allocatable :: c
  INTEGER, DIMENSION(:,:), allocatable :: f


  vector(1,:)=c(f(ii,2),1:3)-c(f(ii,1),1:3)
  Vector(2,:)=c(f(ii,3),1:3)-c(f(ii,1),1:3)

END FUNCTION vector

end module my_subs


PROGRAM crosstest
  use my_subs
  IMPLICIT NONE

  INTEGER ::i,sump,sumn,k,kk,nv,nf,nvf,z
  INTEGER, DIMENSION(3) :: nvnf
  REAL, DIMENSION(:,:), allocatable :: C
  INTEGER, DIMENSION(:,:), allocatable :: F
  REAL, DIMENSION(:,:), allocatable :: Pv
  REAL, DIMENSION(:,:), allocatable :: Vb
  REAL, DIMENSION(:), allocatable :: ds
  REAL, DIMENSION(2,3) :: Vt
  REAL, DIMENSION(3) :: Pd

  open(unit=10, file='nvnf_tetra')


    read(10,*) nvnf


  close(10)

  nv=nvnf(1)  !number of vertex
  nf=nvnf(2)  !number of faces
  nvf=nvnf(3)  !number of vetices on each face

  allocate(C(nv,3))  !the coordinates of each point
  allocate(F(nf,nvf)) !the verices located in certain face
  allocate(Pv(nf,3))
  allocate(Vb(nf,3))
  allocate(ds(nf))

  open(unit=11, file='vertex_tetra')



  do k = 1, nv

    read(11,*) C(k,:)


  end do

  close(11)

  open(unit=12, file='face_tetra')


  do k = 1, nf

    read(12,*) F(k,:)


  end do

  close(12)




  print *, "Please input the point coordinates: "
  read *,Pd  !reading the given point which should be determined if it is Inside or Outside


  do i = 1, nf

  Vt=vector(C,F,i)                  !the vectors to construct the face
  Vb(i,:)=cross(Vt(1,:),Vt(2,:))    !unit nomral of each face (using cross)
  Pv(i,:)=C(F(i,1),:)-pd             !taking vector from the given point to a point existing in face
  ds(i)=dot_product(Vb(i,:),Pv(i,:))   !to determine the direction of the vectors (unit normal and point vector)



  end do

!checking the sign if each ds(i)

  sump=0
  sumn=0
  do i = 1, nf

    if ( ds(i) >= 0 ) then
      sump=sump+1

    else
      sumn=sumn+1
    end if

  end do

if ( sump == nf .OR. sumn == nf ) then

print *, "The Point is is INSIDE"

else
  print *,"The Point is OUTSIDE"

end if


END PROGRAM crosstest
