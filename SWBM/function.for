!       ///////////目标函数计算///////////////	
	
	FUNCTION FUNCTN(npar,par,parmin,parmax)
      parameter(nmax=60000)
      dimension r(nmax),e(nmax),qobs(nmax),qhat(nmax),
     &	hhw(nmax),sse(nmax)
      dimension par(npar),parmin(npar),parmax(npar),par0(npar)
      common /rain/r,/evap/e,/qisc/qobs,/qhat/qhat,/hhw/hhw,sse
      common /number/n1,nc2,nv1,n2,nwarm, /switch/isw,/area/area
      common /a/sumof(80000)
	real sumobs,sumhat
      real:: functn

      do i=1,npar
          par0(i)=par(i)
          if(par0(i).lt.parmin(i)) par0(i)=parmin(i)
          if(par0(i).gt.parmax(i)) par0(i)=parmax(i)
      end do

!     Discharge estimation
      n=nwarm+1

!     Estimated discharges qhat(i)
      call TWBM(n1,nc2,par0,r,e,qhat,sse,hhw)
     
!function1:总体水量误差

c         f2=0.0
c         do 40 i=n,n2	
c            f2=f2+(qobs(i)-qhat(i))
c 40      continue
c         f1=abs((f1)/real(n2-n+1))								   
        
c 		functn=f1

!function2:均方误
 
      f1=0.0
      f2=0.0
      f3=0.0
      f4=0.0
      sumobs=0.0
      sumhat=0.0
      aveobs=0.0
      m=0
      datanull=-999 
c      datanull=-999*86.4*30.5/area 
      do 41 i=n,n2
            if(qobs(i).NE.datanull)then
              m=m+1
              sumobs=sumobs+qobs(i)
	        sumhat=sumhat+qhat(i)
            end if
 41   continue
      aveobs=sumobs/m
      m=0

      do 40 i=n,n2
          if(qobs(i).NE.datanull)then
              m=m+1
 		    f1=f1+(qobs(i)-qhat(i))**2
		    f3=f3+(qobs(i)-aveobs)**2
          end if
 40   continue


c     f1=sqrt(f1/(n2-n+1))
      f2=abs(sumhat-sumobs)/m
      f4=f1/f3

 !    functn=f1+f2
      functn=f4
      return
c      ll=ll+1

c      sumof(ll) = functn

	END
