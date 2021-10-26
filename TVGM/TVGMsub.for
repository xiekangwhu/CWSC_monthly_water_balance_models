
      SUBROUTINE TWBM(n1,n2,X,rain,panev,sq,se,hw)
      use numerical_libraries
      integer n1,n2,i,j
      real,parameter :: dt=1.0
      real X(5),g1,g2,kr,sc,gama
      real hw(n2+1),rain(n2),panev(n2),se(n2),sq(n2),sqs(n2),sqg(n2)
      common /area/area

      g1=X(1)
      g2=X(2)
      kr=X(3)
      sc=X(4)
      gama=X(5)
      hw(1)=50.0

      do i=n1,n2
      ! simulated evaporation
      hw0=hw(i)+rain(i)*dt
      se(i)=panev(i)*((hw0/sc)**gama)*dt
      ! simulated discharge
      sqs(i)=g1*((hw(i)/sc)**g2)*rain(i)*dt
      sqg(i)=kr*(hw(i)+hw0)/2*dt
      sq(i)=sqs(i)+sqg(i)
c      sq(i)=sq(i)*area/86.4
      ! simulated water depth
      IF(i<n2)  hw(i+1)=hw0-(sq(i)+se(i))*dt
      IF(hw(i+1)<EPSILON(1.)) hw(i+1)=EPSILON(1.)
      j=(i-1)/12 
      enddo

      END