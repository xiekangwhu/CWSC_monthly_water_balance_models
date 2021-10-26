	subroutine DATAIN(ifile)
      parameter(idnx=60000)
      dimension rain(idnx),evap(idnx),qobs(idnx),qhat(idnx)
      common /rain/rain,/evap/evap,/qisc/qobs,/qhat/qhat
      common /number/nc1,nc2,nv1,nv2,nwarm, /switch/isw,/area/area	
      
      write (filename,'(".\monthly\",(i3.3), "data.dat")') i_file
      open(10,file=filename)
      read(10,*)nv2,area,basinnum
      do i=1,nv2
          read(10,*)  rain(i),evap(i),qobs(i)
c          qobs(i)=qobs(i)*86.4/area   
      end do
      nwarm=nv2*1/8
      if (nwarm>24)   nwarm=24
      nc1=11
      nc2=nv2*3/4
      nv1=nc2+1
       
      return
      END SUBROUTINE DATAIN 