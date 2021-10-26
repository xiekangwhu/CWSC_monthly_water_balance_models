      subroutine scein(a,bl,bu,npar,maxn,kstop,pcento,iseed,
     &                 ngs,npg,nps,nspl,mings,iniflg,iprint,i_file)
$debug
c
c   THIS SUBROUTINE READS AND PRINTS THE INPUT VARIABLES FOR
c   SHUFFLED COMPLEX EVOLUTION METHOD FOR GLOBAL OPTIMIZATION
c     -- Version 2.1
c
c   WRITTEN BY QINGYUN DUAN - UNIVERSITY OF ARIZONA, APRIL 1992
c
c
c      implicit real*8 (a-h,o-z)
      dimension a(npar),bl(npar),bu(npar)
      common /iopar/ inp,ipr
      character*10 pcntrl,deflt,usrsp
      character*4 reduc,initl,ysflg,noflg,xname(2)
c
      data deflt/' DEFAULT  '/
      data usrsp/'USER SPEC.'/
      data ysflg/'YES '/
      data noflg/'NO  '/
      data xname /'  X1','  X2'/
c
c      write (*,*) ' ENTER THE SCEIN SUBROUTINE --- '
c
c
c  INITIALIZE I/O VARIABLES
      inp = 2
c      ipr = 5
      open(inp,file='scein.txt',status='old')
c      open(ipr,file='sceout.txt',status='unknown')
ct
      ierror = 0
      iwarn = 0
  !    write(ipr,700)
  !700 format(10x,'SHUFFLED COMPLEX EVOLUTION GLOBAL OPTIMIZATION',
  !   &       /,10x,46(1h=))
c
c
c  READ THE SCE CONTROL PARAMETERS
      ideflt = 0
      read(inp,800) maxn,kstop,pcento,ngs,iseed,ideflt
  800 format(2i5,f5.2,3i5)
      if (iseed .eq. 0) iseed = 1969
c
c
c  IF ideflt IS EQUAL TO 1, READ THE SCE CONTROL PARAMETERS
      if (ideflt .eq. 1) Then
        read(inp,810) npg,nps,nspl,mings,iniflg,iprint
  810   format(6i5)
        pcntrl = usrsp
      else
        read(inp,*)
        pcntrl = deflt
      end if
      
c      write(*,*)maxn,kstop,pcento,ngs,iseed,ideflt
c      write(*,*)npg,nps,nspl,mings,iniflg,iprint
c
c
c  READ THE INITIAL PARAMETER VALUES AND THE PARAMETER BOUNDS

      
      do i=1,npar
          read(inp,*) a(i),bl(i),bu(i)
      end do
      

c
c
c  IF ideflt IS EQUAL TO 0, SET THE SCE CONTROL PARAMETERS TO
c  THE DEFAULT VALUES
      if (ideflt .eq. 0) then
        npg = 2*npar + 1
        nps = npar + 1
        nspl = npg
        mings = ngs
        iniflg = 0
        iprint = 0
      end if
c
c
c  CHECK IF THE SCE CONTROL PARAMETERS ARE VALID
      if (ngs .lt. 1 .or. ngs .ge. 1320) then
  !      write(ipr,900) ngs
  !900   format(//,1x,'**ERROR** NUMBER OF COMPLEXES IN INITIAL ',
  !   *         ' POPULATION ',i5,' IS NOT A VALID CHOICE')
        ierror = ierror + 1
      end if
c
      if (kstop .lt. 0 .or. kstop .ge. 20) then
  !      write(ipr,901) kstop
  !901   format(//,1x,'**WARNING** THE NUMBER OF SHUFFLING LOOPS IN',
  !   *  ' WHICH THE CRITERION VALUE MUST CHANGE ',/,13x,'SHOULD BE',
  !   *  ' GREATER THAN 0 AND LESS THAN 10.  ','kstop = ',i2,
  !   *  ' WAS SPECIFIED.'/,13x,'BUT kstop = 5 WILL BE USED INSTEAD.')
        iwarn = iwarn + 1
        kstop=5
      end if
c
      if (mings .lt. 1 .or. mings .gt. ngs) then
  !      write(ipr,902) mings
  !902   format(//,1x,'**WARNING** THE MINIMUM NUMBER OF COMPLEXES ',
  !   *         i2,' IS NOT A VALID CHOICE. SET IT TO DEFAULT')
        iwarn = iwarn + 1
        mings = ngs
      end if
c
      if (npg .lt. 2 .or. npg .gt. 1320/max(ngs,1)) then
  !      write(ipr,903) npg
  !903   format(//,1x,'**WARNING** THE NUMBER OF POINTS IN A COMPLEX ',
  !   *         I4,' IS NOT A VALID CHOICE, SET IT TO DEFAULT')
        iwarn = iwarn + 1
        npg = 2*npar+1
      end if
c
      if (nps.lt.2 .or. nps.gt.npg .or. nps.gt.50) then
  !      write(ipr,904) nps
  !904   format(//,1x,'**WARNING** THE NUMBER OF POINTS IN A SUB-',
  !   *  'COMPLEX ',i4,' IS NOT A VALID CHOICE, SET IT TO DEFAULT')
        iwarn = iwarn + 1
        nps = npar + 1
      end if
c
      if (nspl .lt. 1) then
  !      write(ipr,905) nspl
  !905   format(//,1x,'**WARNING** THE NUMBER OF EVOLUTION STEPS ',
  !   *         'TAKEN IN EACH COMPLEX BEFORE SHUFFLING ',I4,/,13x,
  !   *         'IS NOT A VALID CHOICE, SET IT TO DEFAULT')
        iwarn = iwarn + 1
        nspl = npg
      end if
c
c  COMPUTE THE TOTAL NUMBER OF POINTS IN INITIAL POPULATION
      npt = ngs * npg
c
      if (npt .gt. 1320) then
  !      write(ipr,906) npt
  !906   format(//,1x,'**WARNING** THE NUMBER OF POINTS IN INITIAL ',
  !   *         'POPULATION ',i5,' EXCEED THE POPULATION LIMIT,',/,13x,
  !   *         'SET NGS TO 2, AND NPG, NPS AND NSPL TO DEFAULTS')
        iwarn = iwarn + 1
        ngs = 2
        npg = 2*npar + 1
        nps = npar + 1
        nspl = npg
      end if
c
c  PRINT OUT THE TOTAL NUMBER OF ERROR AND WARNING MESSAGES
!      if (ierror .ge. 1) write(ipr,907) ierror
!  907 format(//,1x,'*** TOTAL NUMBER OF ERROR MESSAGES IS ',i2)
!c
!      if (iwarn .ge. 1) write(ipr,908) iwarn
!  908 format(//,1x,'*** TOTAL NUMBER OF WARNING MESSAGES IS ',i2)
c
      if (mings .lt. ngs) then
        reduc = ysflg
      else
        reduc = noflg
      end if
c
      if (iniflg .ne. 0) then
        initl = ysflg
      else
        initl = noflg
      end if
c
c
c  PRINT SHUFFLED COMPLEX EVOLUTION OPTIMIZATION OPTIONS
  !104 write(ipr,910)
  !910 format(//,2x,'SCE CONTROL',5x,'MAX TRIALS',5x,
  !   &'REQUIRED IMPROVEMENT',5x,'RANDOM',/,3x,'PARAMETER',8x,
  !   &'ALLOWED',6x,'PERCENT',4x,'NO. LOOPS',6x,'SEED',/,
  !   &2x,11(1h-),5x,10(1H-),5x,7(1h-),4x,9(1h-),5x,6(1h-))
c
      pcenta=pcento*100.
  !    write(ipr,912) pcntrl,maxn,pcenta,kstop,iseed
  !912 format(3x,a10,7x,i5,10x,f3.1,9x,i2,9x,i5)
  !    write(ipr,914) ngs,npg,npt,nps,nspl
  !914 format(//,18x,'SCE ALGORITHM CONTROL PARAMETERS',/,18x,32(1H=),
  !   &//,2x,'NUMBER OF',5x,'POINTS PER',5x,'POINTS IN',6x,'POINTS PER',
  !   &4x,'EVOL. STEPS',/,2x,'COMPLEXES',6X,'COMPLEX',6x,'INI. POPUL.',
  !   &5x,'SUB-COMPLX',4x,'PER COMPLEX',/,2x,9(1h-),5x,10(1h-),4x,
  !   &11(1h-),5x,10(1h-),4x,11(1h-),5x,/,2x,5(i5,10x))
  !    write(ipr,915) reduc,mings,initl
  !915 format(//,15x,'COMPLX NO.',5x,'MIN COMPLEX',5x,'INI. POINT',/,
  !   &15x,'REDUCTION',6x,'NO. ALLOWED',6x,'INCLUDED',/,
  !   &15x,10(1h-),5x,11(1h-),5x,10(1h-),/,18x,a4,6x,i8,13x,a4)
  !    write(ipr,916)
  !916 format(//,8x,'INITIAL PARAMETER VALUES AND PARAMETER BOUNDS',/,
  !   &       8x,45(1h=),//,2x,'PARAMETER',5x,'INITIAL VALUE',5x,
  !   &       'LOWER BOUND',5x,'UPPER BOUND',/,2x,9(1h-),5x,13(1h-),5x,
  !   &       11(1h-),5x,11(1h-))
  !    do 920 i = 1, npar
  !      write(ipr,918) xname(i),a(i),bl(i),bu(i)
  !918   format(4x,a4,4x,3(6x,f10.3))
  !920 continue
      if (ierror .ge. 1) then
  !    write(ipr,922)
  !922 format(//,'*** THE OPTIMIZATION SEARCH IS NOT CONDUCTED BECAUSE',
  !   &       ' OF INPUT DATA ERROR ***')
      stop
      end if
      close(inp)
C  END OF SUBROUTINE SCEIN
      return
      end
