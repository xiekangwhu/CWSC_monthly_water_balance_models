
      SUBROUTINE TWBM(n1,n2,X,rain,panev,sq,se,S)
	use numerical_libraries
      integer n1,n2,i,j
	real,parameter :: dt=1.0
	real X(4),SC,Alpha1,Alpha2,KG
	real rain(n2),panev(n2),se(n2),sq(n2)
      real W_sim(n2),S(n2+1),Qd_sim(n2),Y_sim(n2)
      real Qb_sim(n2),G(n2+1),R_sim(n2),X_sim(n2)
      real part1(n2),part2(n2),part3(n2),part4(n2),part5(n2),part6(n2)
      common /area/area

	SC=X(1)
	Alpha1=X(2)
	Alpha2=X(3)
	KG=X(4)      

	do i=n1,n2
          S(1)=0.1*SC
          G(1)=0.001
          if(rain(i)<0.01)then
              W_sim(i)=S(i)
              Qd_sim(i)=0
              if (ISNAN(W_sim(i)))then
                  se(i) = 0
                  Qb_sim(i) = 0
                  S(i+1) = 0
                  G(i+1) = 0
              else
                  if(panev(i)/W_sim(i)>50)then
                      panev(i)=50*W_sim(i)
                  else
                      panev(i)=panev(i)
                  endif
                  part3(i)=(panev(i)+SC)/W_sim(i)
              part4(i)=(1+((SC+panev(i))/W_sim(i))**Alpha2)**(1/Alpha2)
                  Y_sim(i)=W_sim(i)*(1+part3(i)-part4(i))
                  R_sim(i)=W_sim(i)-Y_sim(i)
                  part5(i)=panev(i)/W_sim(i)  
                  part6(i)=(1+(panev(i)/W_sim(i))**Alpha2)**(1/Alpha2)  
                  se(i)= W_sim(i)*(1+part5(i)-part6(i))
                  S(i+1)=Y_sim(i)-se(i)
                  Qb_sim(i)=KG*G(i)
                  G(i+1)=(1-KG)*G(i)+R_sim(i)
              endif
          else
              if(panev(i)/rain(i)>50)then
                  panev(i)=50*rain(i)
              else
                  panev(i)=panev(i)
              endif
              part1(i)=(panev(i)+SC-S(i))/rain(i)
         part2(i)=(1+((SC-S(i)+panev(i))/rain(i))**Alpha1)**(1/Alpha1)
              X_sim(i)= rain(i)*(1+part1(i)-part2(i))
      
              Qd_sim(i)=rain(i)-X_sim(i)
              W_sim(i)=X_sim(i)+S(i)
              if (ISNAN(W_sim(i)))then
                  se(i) = 0
                  Qb_sim(i) = 0
                  Qd_sim(i) = 0
                  S(i+1) = 0
                  G(i+1) = 0
              else
                  if(panev(i)/W_sim(i)>50)then
                      panev(i)=50*W_sim(i)
                  else
                      panev(i)=panev(i)
                  endif
                  part3(i)=(panev(i)+SC)/W_sim(i)
              part4(i)=(1+((SC+panev(i))/W_sim(i))**Alpha2)**(1/Alpha2)
                  Y_sim(i)=W_sim(i)*(1+part3(i)-part4(i))
        
                  R_sim(i)=W_sim(i)-Y_sim(i)
        
                  part5(i)=panev(i)/W_sim(i)
                  part6(i)=(1+(panev(i)/W_sim(i))**Alpha2)**(1/Alpha2)  
                  se(i)= W_sim(i)*(1+part5(i)-part6(i))
        
                  S(i+1)=Y_sim(i)-se(i)
        
                  Qb_sim(i)=KG*G(i)
                  G(i+1)=(1-KG)*G(i)+R_sim(i)
              endif
          endif 
          sq(i)=Qd_sim(i) + Qb_sim(i)
           
	enddo

      END
     