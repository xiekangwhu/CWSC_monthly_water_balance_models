      PROGRAM main
      use numerical_libraries
	parameter(npar=4)
      common /number/nc1,nc2,nv1,nv2,nwarm, /switch/isw,/area/area	      
      dimension a(npar),bl(npar),bu(npar),jseed(10)
      data jseed/2,3,5,7,11,13,17,19,23,29/
      parameter (idnx=80000,indxy=500)
      dimension rain(idnx),evap(idnx),qobs(idnx),qhat(idnx),
     &	sse(idnx),hhw(idnx),rain1(idnx),evap1(idnx),qobs1(idnx)
      dimension par(npar),parmin(npar),parmax(npar),bestpar0(npar),
     &           ititle(npar),alogg(npar)
      dimension rsqr11(idnx),rsqr21(idnx)
      character*20 catchm,fyle,fyler,fylee,fyleq,filename,
     &            fyout,fyresult,fyle_dc,fyle_wbi
      common /syspar/iuniti,iunito,idebug
      common /rain/rain,/evap/evap,/qisc/qobs,/hhw/hhw,sse
     &        /qhat/qhat
      common /a/sumof(idnx)
	dimension bestpar(10,npar),bestfunction(10),bestparameter(npar),
     &	bestx(npar)
      real bestf
      data iuniti/5/,iunito/6/,idebug/0/ 
      external functn,ran1,gasdev
	integer be,en
	REAL :: yrain(indxy),ypanev(indxy),ydis(indxy),yse(indxy),
     &	ysq(indxy),yhw(indxy) 
      integer i_file
      character(len=80):: basin1
      character(len=80):: basin2
      
c      write (*,*) ' ENTER THE MAIN PROGRAM TWBM  '
      
      basin2="GRUN_grids\"
      
c      do  i_file=35253,35273
      do  i_file=60001,61345
	    call scein(a,bl,bu,npar,maxn,kstop,pcento,iseed,ngs,npg,
     &	    nps,nspl,mings,iniflg,iprint,i_file)     
      
          write (filename,'((i5.5),"data.dat")') i_file
          open(unit=10,file=trim(basin2)//trim(filename),status='old')
          read(10,*)nv2,area,catchm
          do i=1,nv2
              read(10,*)  rain(i),evap(i),qobs(i)
          end do
          nwarm=nv2*1/8
          if (nwarm>24)   nwarm=24
          nc1=nwarm+1
          nc2=nv2*3/4
          nv1=nc2+1
       
      
c          do i=1,npar
c              write(*,*) a(i),bl(i),bu(i)
c          end do

          write(iunito,190)   i_file
190       format(/4x,'File for general model output : ',i5)
      
c          write (fyout,'((i5.5),"output.txt")') i_file
c          open(unit=14,file=trim(basin2)//trim(fyout),
c     &     status='unknown')

!          write(14,5) catchm
!5         format(4x,' TWBM model for catchment ',a7)
!          write(14,6) nc1,nc2
!6         format(4x,'Calibration period starts at: ',
!     &        i6,' and finishes at: ',i6)
!          write(14,7) nv1,nv2
!7         format(4x,'Verification period starts at: ',
!     &        i6,' and finishes at: ',i6)
!          write(14,8) nwarm
!8         format(4x,'Number of days of warm up period: ',i4)
!
!          write(14,40)
!40        format(//4x,'The starting parameters for Optimisation')
!          write(14,100)(a(i),i=1,npar)
!100       format(/4x,7X,'X1',8X,'X2',
!     &            /4X,4F9.3)
!

          if (iseed .gt. 0) then
              nrun = min(iseed,10)
          else
              nrun = 1
          end if
          do i=1, nrun
              if (nrun .ne. 1) iseed = jseed(i)
c              write (*,*) '@ SCE-UA Run Number',i,' Random Seed Value'
c     &         ,iseed      
     
              call sceua(a,bl,bu,npar,maxn,kstop,pcento,iseed,ngs,npg,
     &	    nps,nspl,mings,iniflg,iprint,bestx,bestf)

		    do j=1,npar
			    bestpar(i,j)=bestx(j)
		    end do
		    bestfunction(i)=bestf
		
          end do

          best=bestfunction(1)
	    j=1
	    do i=2,nrun
		    if(bestfunction(i)<best)then
			    best=bestfunction(i)
			    j=i
		    end if
	    end do
	    do i=1,npar
		    bestparameter(i)=bestpar(j,i)
          end do
c          n=1
c          nc2=nv2
c	    best=functn(npar,bestparameter,bl,bu)
          do i=1,npar
              bestpar0(i)=bestparameter(i)
              if(bestpar0(i).lt.bl(i)) bestpar0(i)=bl(i)
              if(bestpar0(i).gt.bu(i)) bestpar0(i)=bu(i)
          end do

          call TWBM(1,nv2,bestpar0,rain,evap,qhat,sse,hhw)
c	    write(*,*)1-best

!         PRINT THE FINAL PARAMETER ESTIMATE AND ITS FUNCTION VALUE
!	    write(14,34)
!34        format(//4x,'The final parameters for Optimisation')
!          write(14,100)(bestparameter(i),i=1,npar)
      
          qmc=0
          mqmc=0
          datanull=-999 
          nc2=nv2*3/4
c          datanull=-999*86.4*30.5/area 
          do 30 i=nc1,nc2
              if(qobs(i).NE.datanull)then
                  mqmc=mqmc+1              
                  qmc=qmc+qobs(i)
              end if
30        continue
          qmc=qmc/mqmc

          qmv=0
          mqmv=0
          do 31 i=nv1,nv2
              if(qobs(i).NE.datanull)then
                  mqmv=mqmv+1
                  qmv=qmv+qobs(i)
              end if
31        continue
          qmv=qmv/mqmv

!          write(14,110) 
!110       format(///4x,'Results in the calibration period'/)

          itemp=iunito
          iunito=14
          
          call objfunc(qobs,nc1,nc2,qhat,qmc,f0,f,rsqr,1,0)
          rsqr11(i_file)=rsqr
          
!          write(14,120)
!120       format(///4x,'Results in the verification period'/)        
          n1=nv1
          if(n1.le.(nc1)) n1=nc1
        
          call objfunc(qobs,nv1,nv2,qhat,qmv,f0,f,rsqr,1,0)
          rsqr21(i_file)=rsqr
          iunito=itemp

!         OUTPUT the discharge simulations     

c          write (fyresult,'((i5.5), "outflow.dat")') i_file
c          open(unit=8,file=trim(basin2)//trim(fyresult)
c     &      ,status='unknown')
	    !write(8,"('No.',4X,'Qobs',6X,'Qsim',6x,'AEP',6x,'Stora')")
!	    do i=1,nv2
!              qobs(i) = qobs(i)
!              qhat(i) = qhat(i)
!c              qobs(i) = qobs(i)*area/86.4/30.5
!c              qhat(i) = qhat(i)*area/86.4/30.5
!	        write(8,"(I5,4F12.2)")i,qobs(i),qhat(i),sse(i),hhw(i)
!          enddo    
          
          fyle_dc="scheme.txt"
          open(unit=66,file=fyle_dc,status='unknown')
     	    write(66,*)i_file,rsqr11(i_file),rsqr21(i_file),
     &                bestparameter(1)
		write(*,*)i_file,rsqr11(i_file),rsqr21(i_file),
     &                bestparameter(1)
          
      enddo
	stop
     
      END PROGRAM MAIN

