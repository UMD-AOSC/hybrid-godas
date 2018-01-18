module coare40vn
implicit none
private

public coare40vn_ocean_fluxes

contains

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
! Over-ocean fluxes following COARE3 code (Fairall et al, 2003) with 
! modification based on the CLIMODE, MBL and CBLAST experiments 
! (Edson et al., 2011). The cool skin option is retained but warm layer 
! and surface wave options removed.           !
! Code : Senya Grodsky  (converted from MATLAB version)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!
   subroutine coare40vn_ocean_fluxes (u, t, ts, Q, Qs, ztq, zu, avail, &
!     &                               Cd, Ch, Ce, ustar, bstar,n      )  ! Senya's test interface
                                    Cd, Ch, Ce, ustar, bstar       )     
!*********************************************************************

!function A=coare40vn(u,zu,t,zt,rh,zq,P,ts,Rs,Rl,lat,zi)
!
! Vectorized version of COARE3 code (Fairall et al, 2003) with 
! modification based on the CLIMODE, MBL and CBLAST experiments 
! (Edson et al., 2011). The cool skin option is retained but warm layer 
! and surface wave options removed. 
!
!********************************************************************
! An important component of this code is whether the inputed ts 
! represents the skin temperature of a near surface temperature.  
! How this variable is treated is determined by the jcool parameter:
! set jcool=1 if Ts is bulk ocean temperature (default),
!     jcool=0 if Ts is true ocean skin temperature. 
!********************************************************************

!jcool=0; !ts is treated as the true SST

! The code assumes u,t,rh,ts are vectors; 
! sensor heights zu,zt,zl, latitude lat, and PBL height zi are constants;
! air pressure P and radiation Rs,Rl may be vectors or constants. 
! Default values are assigned for P,Rs,Rl,lat,and zi if these data are not 
! available.  Input NaNs to indicate no data. Defaults should be set to 
! representative regional values if possible.
!
! Input:  
!
!     u = relative wind speed (m/s) at height zu(m)
!     t = bulk air temperature (degC) at height zt(m)
!    rh = relative humidity (!) at height zq(m)
!     P = surface air pressure (mb) (default = 1013)
!    ts = water temperature (degC) see jcool below
!    Rs = downward shortwave radiation (W/m^2) (default = 150) 
!    Rl = downward longwave radiation (W/m^2) (default = 370)
!   lat = latitude (default = +45 N)
!    zi = PBL height (m) (default = 600m)
!
! The user controls the output.  This is currently set as:
!
! Output:  A=[usr tau hsb hlb hbb hsbb tsr qsr zot zoq Cd Ch Ce  L zet dter
! dqer tkt Urf Trf Qrf RHrf UrfN Rnl Le rhoa UN U10 U10N Cdn_10 Chn_10 Cen_10];
!
!  where
!
!   usr = friction velocity that includes gustiness (m/s)
!   tau = wind stress (N/m^2)
!   hsb = sensible heat flux into ocean (W/m^2)
!   hlb = latent heat flux into ocean (W/m^2)
!   hbb = buoyany flux into ocean (W/m^2)
!   hsbb = "sonic" buoyancy flux measured directly by sonic anemometer 
!   tsr = temperature scaling parameter (K)
!   qsr = specific humidity scaling parameter (g/Kg)
!   zot = thermal roughness length (m)
!   zoq = moisture roughness length (m)
!   Cd = wind stress transfer (drag) coefficient at height zu   
!   Ch = sensible heat transfer coefficient (Stanton number) at height zu   
!   Ce = latent heat transfer coefficient (Dalton number) at height zu
!    L = Obukhov length scale (m) 
!  zet = Monin-Obukhov stability parameter zu/L 
! dter = cool-skin temperature depression (degC)
! dqer = cool-skin humidity depression (degC)
!  tkt = cool-skin thickness (m)
!  Urf = wind speed at reference height (user can select height below)
!  Tfr = temperature at reference height
!  Qfr = specific humidity at reference height
! RHfr = relative humidity at reference height
! UrfN = neutral value of wind speed at reference height
!  Rnl = Upwelling IR radiation computed by COARE
!   Le = latent heat of vaporization
! rhoa = density of air
!   UN = neutral value of wind speed at zu
!  U10 = wind speed adjusted to 10 m
! UN10 = neutral value of wind speed at 10m
!Cdn_10 = neutral value of drag coefficient at 10m    
!Chn_10 = neutral value of Stanton number at 10m    
!Cen_10 = neutral value of Dalton number at 10m    
!

! Notes: 1) u is the relative wind speed, i.e., the magnitude of the
!           difference between the wind (at zu) and ocean surface current 
!           vectors.
!        2) Set jcool=0 in code if ts is true surface skin temperature,
!           otherwise ts is assumed the bulk temperature and jcool=1.
!        3) Set P=NaN to assign default value if no air pressure data 
!           available. 
!        4) Set Rs=NaN, Rl=NaN if no radiation data available.  This assigns 
!           default values to Rs, Rl so that cool skin option can be applied. 
!        5) Set lat=NaN and/or zi=NaN to assign default values if latitude
!           and/or PBL height not given. 
!        6) The code to compute the heat flux caused by precipitation is 
!           included if rain data is available (default is no rain).
!        7) Code updates the cool-skin temperature depression dter and thickness
!           tkt during iteration loop for consistency.
!        8) Number of iterations set to nits = 6.

! Reference:
!
!  Fairall, C.W., E.F. Bradley, J.E. Hare, A.A. Grachev, and J.B. Edson (2003),
!  Bulk parameterization of air sea fluxes: updates and verification for the 
!  COARE algorithm, J. Climate, 16, 571-590.

! Code history:
! 
! 1. 12/14/05 - created based on scalar version coare26sn.m with input
!    on vectorization from C. Moffat.  
! 2. 12/21/05 - sign error in psiu_26 corrected, and code added to use variable
!    values from the first pass through the iteration loop for the stable case
!    with very thin M-O length relative to zu (zetu>50) (as is done in the 
!    scalar coare26sn and COARE3 codes).
! 3. 7/26/11 - S = dt was corrected to read S = ut.
! 4. 7/28/11 - modification to roughness length parameterizations based 
!    on the CLIMODE, MBL, Gasex and CBLAST experiments are incorporated
! 5. 02/14/2017 matlab-->fortran senya@umd.edu mimics ncar_ocean_fluxes I/O parameters
!
!-----------------------------------------------------------------------

!	real   , intent(in)   , dimension(1) :: u, t, ts, Q, Qs, zu          ! Senya's test interface
!	logical, intent(in)   , dimension(1) :: avail                        ! Senya's test interface
!	real   , intent(inout), dimension(n) :: cd, ch, ce, ustar, bstar     ! Senya's test interface

	real   , intent(in)   , dimension(:) :: u, t, ts, Q, Qs, zu, ztq
        logical, intent(in)   , dimension(:) :: avail
        real   , intent(inout), dimension(:) :: cd, ch, ce, ustar, bstar
!ta, ts [K]
!Q, Qs [kg/kg]

!  	real    :: cd_n10, ce_n10, ch_n10, cd_n10_rt    ! neutral 10m drag coefficients
!  	real    :: cd_rt                                ! full drag coefficients @ z
!  	real    :: zeta, x2, x, psi_m, psi_h            ! stability parameters
!  	real    :: u10, tv, tstar, qstar, z0, xx, stab
	real    :: Le, cpv, rhoa, visa, Al, bigc, wetc
	real    :: P, Rs, Rl, lat, zi, Rns , L, L50, L10
  	integer :: i, j, n
	
	real    :: gs, Rnl, us, rain, du, dt, dq, ta, ug, dter, ut
	real    :: Ch10, Ct10, Cd10, Cd1, Ct1, CC, Ribcu, Ribu, zetu
	real    :: usr, zo10, k50, gf, tsr, qsr, tkt, u10, zot10,charn
	real    :: umax, a1, a2, zet, zo, rr, zot, zoq, cdhf, cqhf, cthf
	real    :: tvsr, tssr, Bf, hsb, hlb, qout, dels, qcol, alq, xlamx
	real    :: dqer, usr50, tsr50, zet50, dter50, dqer50, tkt50
	real    :: qsr50, tau, hbb, hsbb
	
!  	real,    parameter :: zt   = 2, zq = 2;         ! T and Q measurement level (m)
        real, dimension(:) :: zt(size(u)), zq(size(u))
	integer, parameter :: nits = 6;
	

!***********  set constants **********************************************
	real, parameter :: Beta = 1.2;
	real, parameter :: von  = 0.4;
	real, parameter :: fdg  = 1.00; ! Turbulent Prandtl number
	real, parameter :: tdk  = 0; !273.16; !ta, ts [K]
	real, parameter :: tdc  = 273.16; !ta, ts [K]->[C]
	real, parameter :: grav = 9.8062; !grv(45deg);

!***********  air constants **********************************************
	real, parameter :: Rgas = 287.1;
	real, parameter :: cpa  = 1004.67;

!***********  cool skin constants  ***************************************
	integer, parameter :: jcool= 0; !ts is treated as the true SST
	real,    parameter :: be   = 0.026;
	real,    parameter :: cpw  = 4000;
	real,    parameter :: rhow = 1022;
	real,    parameter :: visw = 1e-6;
	real,    parameter :: tcw  = 0.6;


!************************************************************

!The following parameter determines which version of the moisture 
!roughness length.
!  0: Step this to 0 to use the form of the moisture rough derived by 
!  Fairall et al. (2003).
!
!  1: Step this to 1 to use the form of the moisture rough determined by the
!  CLIMODE, GASEX and CBLAST data.
!
!  Note that the thermal roughness length gives a Stanton number that is
!  very similar to COARE 3.0.
!
	integer, parameter :: climodeversion=0;
!******************************************************************

  zt(:) = ztq
  zq(:) = ztq
! set  variables that are not inputed to fortran codes to default values
!***********************************************************************
	P=1013;     ! pressure (mb)
	Rs=150;     ! incident shortwave radiation (W/m^2)
	Rl=370;     ! incident longwave radiation (W/m^2)
	lat=45;     ! latitude
	zi=600;     ! PBL height (m)
	
!***********  net radiation fluxes ***************************************
	Rns = 0.945*Rs; ! albedo correction
! IRup = eps*sigma*T^4 + (1-eps)*IR
! Rnl = IRup - IR
! Rll = eps*sigma*T^4 - eps*IR  as below


!****	do i=1,n
	do i=1,size(u(:))
	   if (avail(i)) then
	Rnl = 0.97*(5.67e-8*(ts(i)-0.3*jcool+tdk)**4-Rl); ! initial value

! IRup = Rnl + IR
!


! input variable u is assumed relative wind speed (magnitude of difference
! between wind and surface current vectors). to follow orginal Fairall code, set
! surface current speed us=0. if us data are available, construct u prior to
! using this code.
	us = 0;

! convert rh to specific humidity !! not needed Qs and Q are inputed in (kg/kg)
!Qs = qsat26sea(ts,P)./1000;    ! surface water specific humidity (kg/kg)
!Q  = qsat26air(t,P,rh)./1000;  ! specific humidity of air (kg/kg)

! set rain to zero
	rain = 0; ! rain rate (mm/hr) - keep as option

!***********  air variables **********************************************
	Le   = (2.501-.00237*(ts(i)-tdc) )*1e6;
	cpv  = cpa*(1+0.84*Q(i));
	rhoa = P*100./(Rgas*(t(i)+tdk)*(1+0.61*Q(i)));
	visa = 1.326e-5*(1+6.542e-3*(t(i)-tdc)+8.301e-6*(t(i)-tdc)**2 &
               -4.84e-9*(t(i)-tdc)**3);
	
!***********  cool skin variables  ***************************************
	Al   = 2.1e-5*(ts(i)-tdc+3.2)**0.79;
	bigc = 16*grav*cpw*(rhow*visw)**3/(tcw**2*rhoa**2);
	wetc = 0.622*Le*Qs(i)/(Rgas*(ts(i)+tdk)**2);

	Rnl = 0.97*(5.67e-8*(ts(i)-0.3*jcool+tdk)**4-Rl); ! net LW initial value

! IRup = Rnl + IR

!****************  begin bulk loop ********************************************

!***********  first guess ************************************************
	du = u(i)-us;
	dt = ts(i)-t(i)-.0098*zt(i);
	dq = Qs(i)-Q(i);
	ta = t(i)+tdk;
	ug=0.5;
	dter=0.3;
	ut    = sqrt(du**2+ug**2); 
	u10   = ut*log(10./1e-4)/log(zu(i)/1e-4);
	usr   = 0.035*u10;
	zo10  = 0.011*usr**2/grav + 0.11*visa/usr;
	Cd10  = (von/log(10./zo10))**2;
	Ch10  = 0.00115;
	Ct10  = Ch10/sqrt(Cd10);
	zot10 = 10/exp(von/Ct10);
	Cd1    = (von/log(zu(i)/zo10))**2;
	Ct1    = von/log(zt(i)/zot10);
	CC    = von*Ct1/Cd1;
	Ribcu = -zu(i)/zi/0.004/Beta**3;
	Ribu  = -grav*zu(i)/ta*((dt-dter*jcool)+0.61*ta*dq)/ut**2;
	zetu = CC*Ribu*(1.+27./9.*Ribu/CC);
!!!!!!!!k50=find(zetu>50); ! stable with very thin M-O length relative to zu
	k50 = 0; if (zetu.gt.50.) k50 = 1;
!k=find(Ribu<0); zetu(k)=CC(k).*Ribu(k)./(1+Ribu(k)./Ribcu(k)); clear k;
        if (Ribu.lt.0.) zetu=CC*Ribu/(1+Ribu/Ribcu);
	L10 = zu(i)/zetu;
	gf=ut/du; 
	usr = ut*von/(log(zu(i)/zo10)-psiu_26(zu(i)/L10));
	tsr = -(dt-dter*jcool)*von*fdg/(log(zt(i)/zot10)-psit_26(zt(i)/L10));
	qsr = -(dq-wetc*dter*jcool)*von*fdg/(log(zq(i)/zot10) &
              -psit_26(zq(i)/L10));
	tkt = 0.001;

!**********************************************************
!  The following gives the new formulation for the Charnock parameter
!**********************************************************

	charn = 0.011;
	umax=22;
	a1=0.0016;
	a2=-0.0035;
	charn=a1*u10+a2;
!k=find(u10>umax);
!charn(k)=a1*umax+a2;
	if (u10.gt.umax) charn=a1*umax+a2;


!**************  bulk loop **************************************************

	do j=1,nits
    	zet=von*grav*zu(i)/ta*(tsr +0.61*ta*qsr)/(usr**2); !in the original coare40vn.m
!	zet=von*grav*zu(i)/ta*(tsr +0.61*ta*qsr/(1+0.61*Q(i)))/(usr**2);
    	zo=charn*usr**2/grav+0.11*visa/usr; ! surface roughness
    	rr=zo*usr/visa;
    	L=zu(i)/zet;
    	zot=min(1.0e-4/rr**0.55,2.4e-4/rr**1.2); ! temp roughness
    	if (climodeversion.eq.1) then
        	zoq=min(2.0e-5/rr**0.22,1.1e-4/rr**0.9);  ! moisture roughness
    	else
        	zoq=min(1.15e-4,5.5e-5/rr**0.60);         ! moisture roughness
    	end if

    	cdhf=von/(log(zu(i)/zo)-psiu_26(zu(i)/L));
    	cqhf=von/(log(zq(i)/zoq)-psit_26(zq(i)/L));
    	cthf=von/(log(zt(i)/zot)-psit_26(zt(i)/L));
    	usr=ut*cdhf;
    	qsr=-(dq-wetc*dter*jcool)*cqhf;
    	tsr=-(dt-dter*jcool)*cthf;
    	tvsr=tsr+0.61*ta*qsr;
    	tssr=tsr+0.51*ta*qsr;
    	Bf=-grav/ta*usr*tvsr;
    	ug=0.2;
!    k=find(Bf>0); ug(k)=max(.2,Beta*(Bf(k).*zi).^.333); clear k;
	if (Bf.gt.0.) ug=max(.2,Beta*(Bf*zi)**.333); 
    	ut=sqrt(du**2+ug**2);
    	gf=ut/du;
    	hsb=-rhoa*cpa*usr*tsr;
    	hlb=-rhoa*Le*usr*qsr;
    	qout=Rnl+hsb+hlb;
	dels=Rns*(0.065+11*tkt-6.6e-5/tkt*(1-exp(-tkt/8.0e-4)));
    	qcol=qout-dels;
    	alq=Al*qcol+be*hlb*cpw/Le;
    	xlamx=6.0;
    	tkt=min(0.01, xlamx*visw/(sqrt(rhoa/rhow)*usr));
!    k=find(alq>0); xlamx(k)=6./(1+(bigc(k).*alq(k)./usr(k).^4).^0.75).^0.333;
!    tkt(k)=xlamx(k).*visw./(sqrt(rhoa(k)./rhow).*usr(k)); clear k;
	if (alq.gt.0.) then
	xlamx=6./(1+(bigc*alq/usr**4)**0.75)**0.333;
	tkt=xlamx*visw/(sqrt(rhoa/rhow)*usr);
	end if
    	dter=qcol*tkt/tcw;
    	dqer=wetc*dter;
    	Rnl=0.97*(5.67e-8*(ts(i)-dter*jcool+tdk)**4-Rl); ! update dter
!    if j==1; ! save first iteration solution for case of zetu>50;
!        usr50=usr(k50);tsr50=tsr(k50);qsr50=qsr(k50);L50=L(k50);
!        zet50=zet(k50);dter50=dter(k50);dqer50=dqer(k50);tkt50=tkt(k50);
!    end
	if (j.eq.1.and.k50.eq.1) then
	usr50=usr;tsr50=tsr;qsr50=qsr;L50=L;
        zet50=zet;dter50=dter;dqer50=dqer;tkt50=tkt;
	end if

    	u10 = ut + usr/von*(log(10./zu(i))-psiu_26(10./L)  &
             +psiu_26(zu(i)/L));
    	charn=a1*u10+a2;
    	if (u10.gt.umax) charn=a1*umax+a2;
	end do ! iteration

! insert first iteration solution for case with zetu>50
!usr(k50)=usr50;tsr(k50)=tsr50;qsr(k50)=qsr50;L(k50)=L50;
!zet(k50)=zet50;dter(k50)=dter50;dqer(k50)=dqer50;tkt(k50)=tkt50;
	if (k50.eq.1) then
	usr=usr50;tsr=tsr50;qsr=qsr50;L=L50;
	zet=zet50;dter=dter50;dqer=dqer50;tkt=tkt50;
	end if

!****************  compute fluxes  ********************************************
	tau=rhoa*usr*usr/gf;      ! wind stress
	hsb=rhoa*cpa*usr*tsr;     ! sensible heat flux
	hlb=rhoa*Le*usr*qsr;      ! latent heat flux
	hbb=rhoa*cpa*usr*tvsr;    ! buoyancy flux
	hsbb=rhoa*cpa*usr*tssr;   ! sonic heat flux

!*****  compute transfer coeffs relative to ut @ meas. ht  ********************
	Cd(i)=tau/rhoa/ut/max(.1,du);
	Ch(i)=-usr*tsr/ut/(dt-dter*jcool);
	Ce(i)=-usr*qsr/(dq-dqer*jcool)/ut;
	ustar(i)=usr;
!	bstar(i)=zet*usr**2/(von*zu(i));
!	bstar(i) = grav*(tsr/ta+0.608*qsr)/(0.608*q(i)+1);
	bstar(i) = -Bf/usr;
!	print *, 'tstar=',tsr,' qstar=',qsr
!***  compute 10-m neutral coeff relative to ut (output if needed x1000) ************
!	Cdn_10=1000*von**2/log(10./zo)**2;
!	Chn_10=1000*von**2*fdg/log(10./zo)/log(10./zot);
!	Cen_10=1000*von**2*fdg/log(10./zo)/log(10./zoq);

!***  compute 10-m neutral coeff relative to ut (output if needed) ************
!  Find the stability functions
!*********************************
!	zrf_u=10;             !User defined reference heights
!	zrf_t=10;
!	zrf_q=10;
!	psi=psiu_26(zu(i)/L);
!	psi10=psiu_26(10./L);
!	psirf=psiu_26(zrf_u/L);
!	psiT=psit_26(zt/L);
!	psi10T=psit_26(10./L);
!	psirfT=psit_26(zrf_t/L);
!	psirfQ=psit_26(zrf_q/L);
!	gf=ut/du;

!*********************************************************
!  Determine the wind speeds relative to ocean surface
!  Note that usr is the friction velocity that includes 
!  gustiness usr = sqrt(Cd) S, which is equation (18) in
!  Fairall et al. (1996)
!*********************************************************
!********************still in Matlab**********************
!using UPPER CASE may interfere with FORTRAN
!*********************************************************
!*
!*

!S = ut;
!U = du;
!S10 = S + usr./von.*(log(10./zu)-psi10+psi);
!U10 = S10./gf;
! or U10 = U + usr./von./gf.*(log(10/zu)-psi10+psi);
!Urf = U + usr./von./gf.*(log(zrf_u./zu)-psirf+psi);
!UN = U + psi.*usr/von./gf;
!U10N = U10 + psi10.*usr/von./gf;
!UrfN = Urf + psirf.*usr/von./gf;

!UN2 = usr/von./gf.*log(zu./zo);
!U10N2 = usr./von./gf.*log(10./zo);
!UrfN2  = usr./von./gf.*log(zrf_u./zo);

!lapse=grav/cpa;
!SST=ts-dter*jcool;

!T = t;
!T10 = T + tsr./von.*(log(10./zt)-psi10T+psiT) + lapse*(zt-10);
!Trf = T + tsr./von.*(log(zrf_t./zt)-psirfT+psiT) + lapse*(zt-zrf_t);
!TN = T + psiT.*tsr/von;
!T10N = T10 + psi10T.*tsr/von;
!TrfN = Trf + psirfT.*tsr/von;

!TN2 = SST + tsr/von.*log(zt./zot)-lapse*zt;
!T10N2 = SST + tsr/von.*log(10./zot)-lapse*10;
!TrfN2 = SST + tsr/von.*log(zrf_t./zot)-lapse*zrf_t;

!dqer=wetc.*dter*jcool;
!SSQ=Qs-dqer;
!SSQ=SSQ*1000;
!Q=Q*1000;
!qsr=qsr*1000;
!Q10 = Q + qsr./von.*(log(10./zq)-psi10T+psiT);
!Qrf = Q + qsr./von.*(log(zrf_q./zq)-psirfQ+psiT);
!QN = Q + psiT.*qsr/von./sqrt(gf);
!Q10N = Q10 + psi10T.*qsr/von;
!QrfN = Qrf + psirfQ.*qsr/von;

!QN2 = SSQ + qsr/von.*log(zq./zoq);
!Q10N2 = SSQ + qsr/von.*log(10./zoq);
!QrfN2 = SSQ + qsr/von.*log(zrf_q./zoq);
!RHrf=RHcalc(Trf,P,Qrf/1000);

!****************  output  ****************************************************

!A=[usr tau hsb hlb hbb hsbb tsr qsr zot zoq Cd Ch Ce  L zet dter dqer tkt Urf Trf Qrf RHrf UrfN Rnl Le rhoa UN U10 U10N Cdn_10 Chn_10 Cen_10];
!   1   2   3   4   5   6    7   8   9  10  11 12 13 14  15  16   17   18  19  20  21  22   23  24  25  26  27  28  29     30     31    32
	end if !! avail
	end do !! U
	end subroutine coare40vn_ocean_fluxes
!
!------------------------------------------------------------------------------
	function psit_26(zet) result(psi)
	real, intent(in)  :: zet
	real            :: psi, dzet, x, psik, f, psic  
	
! computes temperature structure function
	dzet=min(50.,0.35*zet); ! stable
	psi=-((1+0.6667*zet)**1.5+0.6667*(zet-14.28)*exp(-dzet)+8.525);
	if (zet.lt.0.) then ! unstable
	x=(1-15*zet)**0.5;
	psik=2*log((1+x)/2);
	x=(1-34.15*zet)**0.3333;
	psic=1.5*log((1+x+x**2)/3)-sqrt(3.)*atan((1+2*x)/sqrt(3.))  &
            +4*atan(1.)/sqrt(3.);
	f=zet**2/(1+zet**2);
	psi=(1-f)*psik+f*psic;
	end if
	end function psit_26
!------------------------------------------------------------------------------
	function psiu_26(zet) result(psi)
	real, intent(in)  :: zet
	real            :: psi, x, psik, f, dzet, psic
	
! computes velocity structure function
	dzet=min(50.,0.35*zet); ! stable
	psi=-((1+zet)+0.6667*(zet-14.28)*exp(-dzet)+8.525);
	if (zet.lt.0.) then ! unstable
	x=(1-15*zet)**0.25;
	psik=2*log((1+x)/2)+log((1+x*x)/2)-2*atan(x)+2*atan(1.);
	x=(1-10.15*zet)**0.3333;

	psic=1.5*log((1+x+x**2)/3)-sqrt(3.)*atan((1+2*x)/sqrt(3.))  &
            +4*atan(1.)/sqrt(3.);
	f=zet**2./(1+zet**2);
	psi=(1-f)*psik+f*psic;
	end if
	end function psiu_26
!------------------------------------------------------------------------------
	function bucksat(T,P) result(exx)
	real, intent(in)  :: T,P
	real            :: exx, ex, es, em
	
! computes saturation vapor pressure [mb]
! given T [degC] and P [mb]
	exx=6.1121*exp(17.502*T/(T+240.97))*(1.0007+3.46e-6*P);
	end function bucksat
!------------------------------------------------------------------------------
	function qsat26sea(T,P) result(qs)
	real, intent(in)  :: T,P
	real            :: qs, ex, es, em
	
! computes surface saturation specific humidity [g/kg]
! given T [degC] and P [mb]
	ex=bucksat(T,P);
	es=0.98*ex; ! reduction at sea surface
	qs=622*es/(P-0.378*es);
	end function qsat26sea
!------------------------------------------------------------------------------
	function qsat26air(T,P,rh) result(qs)
	real, intent(in)  :: T,P,rh
	real            :: qs, es, em
	
! computes saturation specific humidity [g/kg]
! given T [degC],rh [%], and P [mb]
	es=bucksat(T,P);
	em=0.01*rh*es;
	qs=622*em/(P-0.378*em);
	end function qsat26air
!------------------------------------------------------------------------------
!function g=grv(lat)
! computes g [m/sec^2] given lat in deg
!gamma=9.7803267715;
!c1=0.0052790414;
!c2=0.0000232718;
!c3=0.0000001262;
!c4=0.0000000007;
!phi=lat*pi/180;
!x=sin(phi);
!g=gamma*(1+c1*x.^2+c2*x.^4+c3*x.^6+c4*x.^8);
!end

!------------------------------------------------------------------------------
!function RHrf=RHcalc(T,P,Q)
! computes relative humidity given T,P, & Q

!es=6.1121.*exp(17.502.*T./(T+240.97)).*(1.0007+3.46e-6.*P);
!em=Q.*P./(0.378.*Q+0.622);
!RHrf=100*em./es;
!end

end module coare40vn

