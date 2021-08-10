 ! =================================================================
 ! Language: Fortran90
 ! Description: calculate bessel J(n,x) with miller-method
 ! Last change: 2016, Mar. 28
 ! Note:
 !   for integer n
 !   double accuracy ( abs(error)~1.0d-16 )
 !   noarray input
 ! =================================================================

 SUBROUTINE besjno( ninput, xinput, bes )
 ! variables %{{{
 IMPLICIT NONE
 INTRINSIC :: IABS, DABS, DLOG10, DBLE, FLOOR, MAX, INT, MOD
 INTEGER ( KIND= 4 ), INTENT( IN ) :: ninput
 DOUBLE PRECISION, INTENT( IN ) :: xinput
 DOUBLE PRECISION, INTENT( OUT ) :: bes
 DOUBLE PRECISION :: xi
 DOUBLE PRECISION, ALLOCATABLE :: jns(:)
 INTEGER ( KIND= 4 ) :: intn, intl, intm
 INTEGER ( KIND= 4 ):: inta
 ! temp
 DOUBLE PRECISION :: rtmpa, rtmpb
 ! parameters
 DOUBLE PRECISION, PARAMETER :: alpha = 1.d-75
 INTEGER ( KIND= 4 ), PARAMETER :: log_underf = -323 ! detect underflow
 !%}}}

 intn = IABS( ninput )
 IF( intn > 1000 ) STOP "Error too big n. Input n<=1000."
 IF( DABS( xinput ) <= 2.0d-5 )THEN
   ! pattern (i) %{{{
   IF( intn == 0 )THEN
     ! pattern A
     IF( DABS( xinput ) < 1.0d-8 )THEN
       bes = 1.d0
     ELSE
       bes = 1.d0 - ( xinput / 2.d0 ) * ( xinput / 2.d0 )
     END IF
   ELSE
     IF( DABS( xinput ) <= 1.0d-77 )THEN
       ! pattern B
       bes = 0.d0
     ELSE
       ! pattern C
       rtmpa = DLOG10( DABS( xinput ) / 2.d0 ) * DBLE( intn )
       DO inta = 1, intn
         rtmpa = rtmpa - DLOG10( DBLE( inta ) )
       END DO
       IF( rtmpa < log_underf )THEN
         bes = 0.d0
       ELSE
         rtmpa = ( xinput / 2.d0 ) ** intn
         rtmpb = 1.d0
         DO inta = 1, intn
           rtmpb = rtmpb / DBLE( inta )
         END DO
         bes = rtmpa * rtmpb * &
         & ( 1.d0 - ( ( xinput / 2.d0 ) ** 2 ) / DBLE( intn + 1 ) )
       END IF
     END IF
   END IF
   !%}}}
 ELSE
   ! pattern (ii) %{{{
   ! step 1 (decide intm)
   rtmpa = DABS( xinput )
   IF( rtmpa >= 100.d0 )THEN
     intl = FLOOR( 0.073d0 * rtmpa + 47.d0 )
   ELSE IF( rtmpa > 10.d0 )THEN
     intl = FLOOR( 0.27d0 * rtmpa + 27.d0 )
   ELSE IF( rtmpa > 1.d0 )THEN
     intl = FLOOR( 1.4d0 * rtmpa + 14.d0 )
   ELSE
     intl = 14
   END IF
   intm = MAX( ABS( intn ), INT( rtmpa ) ) + intl
   ! step 2 (make array jns)
   ALLOCATE( jns( 0 : intm + 1 ) )
   jns( intm + 1 ) = 0.d0
   jns( intm ) = alpha
   DO inta = intm - 1, 0, -1
     jns( inta ) = jns( inta+1) * 2.d0 * DBLE( inta + 1 ) / xinput
     jns( inta ) = jns( inta ) - jns( inta + 2 )
   END DO
   ! step 3 (make array xi)
   xi = jns( 0 )
   DO inta = 1, intm
     IF( MOD( inta, 2 ) == 0 ) xi = xi + 2.d0 * jns( inta )
   END DO
   !step 4
   bes = jns( intn ) / xi
   !%}}}
 END IF

 IF( ( ninput < 0 ) .AND. ( MOD( intn, 2 ) == 1 ) ) bes = - bes

 END SUBROUTINE

