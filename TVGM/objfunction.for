!-------------------------------------------------------------------

	SUBROUTINE objfunc(x,n1,n2,xhat,ybar,f0,f,rsqr,nprint,ier)
	real::x(n2),xhat(n2),y11(10),y22(10),fmse(10)
	common /syspar/iuniti,iunito,idebug
	real::ratio,rsqr,sx,sxhat,f,f0
!
!       Print debugging information if required.
!
	!if(idebug.ne.0)then
!	Write(iunito,1000)
!1000    Format(/,15x,'Debug information',/,15x,30('-'))
!	write(iunito,1001)x(n1),x(n2),xhat(n1),xhat(n2),n1,n2,ybar
!1001    Format(/,15x,'Subroutine ffw17 called',/,15x,'x(n1) = ',t30,
!     1  e13.6, /,15x,'x(n2) = ',t30,e13.6,
!     2         /,15x,'xhat(n1) = ',t30,e13.6,
!     3         /,15x,'xhat(n2) = ',t30,e13.6,
!     4         /,15x,'n1 = ',t30,i4,/,15x,'n2 = ',t30,i4,/,
!     5               15x, 'ybar = ',t30,e13.6,/,15x,30('-'))
	!endif

!
!       Check if the information is legal.
!
	if(n1.le.0)then
	if(idebug.ne.0)then
!	write(iunito,998)
!998     Format(/,20x,'Illegal parameter n1 in ffw17')
	endif
	n1 = 1
	ier = 1
	endif
!
	if(n1.gt.n2)then
!	write(iunito,995)n2
!995     Format(/,20x,'Illegal parameter in ffw17 - n2 = ',i3)
	ier = 999
	go to 999
	endif
!
!       calculate the mean of x series and xhat series.
!
	n = n2-n1+1
	sxhat = 0.0
	sx = 0.0
      nm=0
      datanull=-999 
c      datanull=-999*86.4*30.5/area 
	do 15 j = n1,n2
          if (x(j).NE.datanull)then
              nm=nm+1
	        sx = sx +x(j)
	        sxhat = sxhat + xhat(j)
          end if
15    continue
	sx = sx/nm    !Qobs mean value
	sxhat = sxhat/nm   !Qsim mean value
!
!       calculate the initial variance and the residual variance after
!       fitting the model expressed /day.
!
	f = 0.0
	f0 = 0.0
      nm=0
	do 20 j = n1,n2
          if (x(j).NE.datanull)then
              nm=nm+1
	        f0 = f0+(x(j)-ybar)**2
	        f = f+(x(j)-xhat(j))**2
          end if
20    continue
	f = f/nm
	f0 = f0/nm
!
!       Calculate the index NASH index.
!
	rsqr = (1-(f/f0))*100.0
!
!       check if printing of the results are required.
!
	if(nprint.eq.1)then
!	Write(iunito,500)
!500     Format(///,10x,'TABLE',/,10x,15('-'),//,10x,'Catchment',/,
!     &   10x,'Model',/,10x,'Calibration/verification period',/,
!     &   10x,'Design/updating mode',//)
	ratio = sxhat/sx
!	write(iunito,505)n,n1,n2,ybar,sx,sxhat,ratio,f0,f,rsqr
!505     Format(/,10x,'Short summary of the results',/,10x,29('-'),
!     &   //,10x,'(for',i5,' values, from',i5,' to',i5,' )',/,
!     &   10x,60('-'),/,10x,'1. Mean of the outflow in calibration',
!     &   2x,'=',e13.6,//,10x,'2. Mean of the observed series',
!     &   2x,'=',e13.6,/,10x,'3. Mean of the estimated series',2x,'=',
!     &   e13.6,/,10x,'4. Ratio of the estimated to the ',
!     &   /,10x,'   observed mean of the outflow',
!     &   2x,'=',f10.4,//,10x,'5. The initial S.O.S per unit time',
!     &   2x,'=',e13.6,/,10x,'6. The final S.O.S per unit time',2x,'=',
!     &   e13.6,/,10x,'7. The performance index (R sqr. %)',2x,'=',f10.2
!     &   ,/,10x,60('-'))
	endif
!
	If(nprint .eq. 2)then

	nband = 3
!	write(iunito,510)
!510     format(//2x,'Enter the band limits (2 values):',t60,'=>')
	read(iuniti,*)y22(1),y22(2)
	y22(3) = 1.0e10
	y11(1) = 0.0    
	do 600 i=2,nband
	y11(i) = y22(i-1)                       
600     continue

	do 720 j=1,nband
	fmse(j) = 0.0
	num = 0 
	do 710 i=n1,n2
	if(x(i) .ge. y11(j) .and. x(i).lt.y22(j))then   
	fmse(j) = fmse(j) + (x(i)-xhat(i))**2
	num = num + 1
	endif
710     continue
	if(num .gt. 0)then
	fmse(j) = fmse(j)/num
	else
	fmse(j) = -9.9
	endif
!	write(6,640)j,num,j,fmse(j)
!640     format(/2x,'Number of flows in zone-',i1,' = ',i5,
!     &         /2x,'MSE of zone-',i1,' flows = ',e13.6)
720     continue

	Endif
999     return
	END SUBROUTINE objfunc