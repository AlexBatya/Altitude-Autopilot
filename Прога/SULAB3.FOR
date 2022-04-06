              external fnc
        real*8 hhi,pi
        character*12 idat,ires
       dimension y0(5),yk(5),yp(5),aktet(3),hhi(3),pi(3),pp(9,4),trg(3),
     *si(3)
       common/a/a11,a21,a22,b2,a46,a51,akwz,aaktet,hii,pii
       common/b/f,hzad,anu
      write(*,*)'Вам предлагается выполнить лабораторную работу "Система
     * стабилизации высоты     полета"'
    1  write(*,*)'Введите имя файла начальных данных в зависимости от  н
     *омера варианта, например:V1.dat'
       read(*,2)idat
       write(*,*)'Введите имя файла результатов, например, V1.res'
       read(*,2)ires
    2  FORMAT(A12)
       write(*,*)'Какую часть работы будете выполнять, 1-ю или 2-ю? Набе
     *рите 1 или 2 соответственно.'
       read(*,*) M
       open(5,file=idat)
       open(6,file=ires)
       write(6,*)'     Лабораторная работа "Система стабилизации высоты
     *полета"'
       write(6,*)idat
       read(5,*)
       read(5,*)a11,a21,a22,b2,a46,a51,akwz,aktet,hhi,pi
c       write(*,3)a11,a21,a22,b2,a46,a51,akwz,aktet,hhi,pi
    3  FORMAT(4F16.8)
       if(M.eq.2)go to 100
       write(6,*)'          Таблица 1.'
       f=0.
       hzad=100.
       pii=0.
       do 15 j=1,3
       aaktet=aktet(j)
       do 15 k=1,3
       hii=hhi(k)
       ip=1
       anm=0.
       aky=0.
       kk=k
       do 5 i=1,5
    5  y0(i)=0.
       do 6 i=1,5
    6  yk(i)=y0(i)
       x0=0.
       xpe=1.
   16 do 17 i=1,5
   17 y0(i)=yk(i)
      hi=0.01
      n=5
         call ork4s1(x0,y0,hi,n,fnc,xk,yk)
       do 10 i=1,5
   10  yp(i)=yk(i)
       if(xk.ge.41.)go to 13
       if(xpe-xk+0.01)7,7,12
    7  do 8 i=1,3
    8  yp(i)=yp(i)*57.3
c       write(6,14)xpe,yp(1),yp(2),yp(3),yp(4),anu
   14  format(1x,F5.2,5f16.6)
c       write(*,14)xpe,yp
       xpe=xpe+1.
   12  x0=xk
       if(ip)22,22,24
   24  dh=hzad-yk(4)
        dd=dh-0.01
       if(dd)21,21,22
   21  tsr=xk
       ip=-1
   22  continue
       anm=max(anm,anu)
       hma=max(aky,yk(4))
       aky=hma
        go to 16
   13  hmax=hma-hzad
       if(j.eq.2)kk=k+3
       if(j.eq.3)kk=k+6
       pp(kk,1)=tsr
       pp(kk,2)=hmax
       pp(kk,3)=anm
   48 format(1x,3f16.6)
   15  continue
c   15 write(6,48)tsr,hmax,anm
       f=-0.035
       hzad=0.
       pii=0.
       do 33 j=1,3
       aaktet=aktet(j)
       jj=1
       do 33 k=1,3
       hii=hhi(k)
       ip=1
       kk=k
       do 25 i=1,5
   25  y0(i)=0.
       do 26 i=1,5
   26  yk(i)=y0(i)
       x0=0.
       xpe=1.
   36 do 37 i=1,5
   37 y0(i)=yk(i)
      hi=0.01
      n=5
         call ork4s1(x0,y0,hi,n,fnc,xk,yk)
       do 20 i=1,5
   20  yp(i)=yk(i)
       if(xk.ge.101.)go to 38
       if(xpe-xk+0.01)27,27,32
   27  do 28 i=1,3
   28  yp(i)=yp(i)*57.3
c       write(6,34)xpe,yp(1),yp(2),yp(3),yp(4),anu
c   34  format(1x,'2-я:',F5.0,5f10.2)
c        write(*,34)xpe,yp
              xpe=xpe+1.
   32  x0=xk
         go to 36
   38  continue
       if(j.eq.2)kk=k+3
       if(j.eq.3)kk=k+6
        pp(kk,4)=yk(4)
   33  continue
c         write(6,40)((pp(l,m),m=1,4),l=1,9)
c   40  format(1x,4f16.6)
         write(6,140)
         write(6,120)
         write(6,130)hhi
         write(6,140)
  110   format(2h+ ,5(1h-),1h+ ,14(1h-),1h+ ,38(1h-),1h+)
  120   format(2hI ,5hKTET ,1hI ,2x,10hПОКАЗАТЕЛЬ,2x,1hI ,20x,2hIH ,
     *16x,1hI)
  130   format(2hI ,5x,1hI ,14x,1hI ,3(f12.6),2x,1hI)
  140   format(2h+ ,5(1h-),1h+ ,14(1h-),1h+ ,3(12(1h-),1h+))
        do 131 i=1,3
        if(i.eq.1)j=i
        if(i.eq.2)j=i+2
        if(i.eq.3) j=i+4
        write(6,150)aktet(i),pp(j,1),pp(j+1,1),pp(j+2,1)
        write(6,165)pp(j,2),pp(j+1,2),pp(j+2,2)
        write(6,170)pp(j,3),pp(j+1,3),pp(j+2,3)
        write(6,180)pp(j,4),pp(j+1,4),pp(j+2,4)
         write(6,140)
  150   format(2hI ,f3.1,2x,1hI ,2x,3hTSR,9x,1hI ,3(f10.2,2x,1hI))
  165   format(2hI ,5x,1hI,2x,12hdHmax       ,1hI,3(f10.2,2x,1hI))
  170   format(2hI ,5x,1hI,2x,12hdnymax      ,1hI,3(f10.2,2x,1hI))
  180   format(2hI ,5x,1hI,2x,12hdHст        ,1hI,3(f10.2,2x,1hI))
  131   continue
         close(6)
         close(5)
       go to 160
  100        f=0.
       hzad=100.
       write(*,*)'Введите выбранное значение коэффициента KTET'
       read(*,*)aaktet
       write(*,*)'Введите выбранное значение коэффициента IH'
       read(*,*)hii
       write(6,*)'            Таблица 2.'
       do 55 j=1,3
       pii=pi(j)
       sid=0.
       do 65 i=1,5
   65  y0(i)=0.
       do 66 i=1,5
   66  yk(i)=y0(i)
       x0=0.
       xpe=1.
   56 do 57 i=1,5
   57 y0(i)=yk(i)
      hi=0.01
      n=5
         call ork4s1(x0,y0,hi,n,fnc,xk,yk)
       do 60 i=1,5
   60  yp(i)=yk(i)
       if(xk.ge.16.)go to 53
       if(xpe-xk+0.01)67,67,62
   67  do 68 i=1,3
   68  yp(i)=yp(i)*57.3
c       write(6,34)xpe,yp(1),yp(2),yp(3),yp(4),anu
c   34  format(1x,'2-я:',F5.0,5f10.2)
       xpe=xpe+1.
   62  x0=xk
       sih=max(sid,yk(4))
       sid=sih
        go to 56
   53  sidh=sid-hzad
c       write(6,34)xpe,yp(1),yp(2),yp(3),yp(4),anu
c   34  format(1x,'2-я:',F5.0,5f10.2)
c       write(6,*)'sidh=',sidh
       si(j)=sidh
   55  continue
       f=0.035
       hzad=0.
       do 85 j=1,3
       pii=pi(j)
       amax=0.
       treg=0.
       l=1
       do 75 i=1,5
   75  y0(i)=0.
       do 76 i=1,5
   76  yk(i)=y0(i)
       x0=0.
       xpe=1.
   86 do 87 i=1,5
   87 y0(i)=yk(i)
      hi=0.01
      n=5
         call ork4s1(x0,y0,hi,n,fnc,xk,yk)
       do 80 i=1,5
   80  yp(i)=yk(i)
       if(xk.ge.151.)go to 83
       if(xpe-xk+0.01)77,77,82
   77  do 88 i=1,3
   88  yp(i)=yp(i)*57.3
c       write(6,84)xpe,yp(1),yp(2),yp(3),yp(4),anu
c   84  format(1x,F5.0,5f10.2)
c        write(*,34)xpe,yp
              xpe=xpe+1.
   82  x0=xk
       xx=abs(yk(4))
       hma=max(amax,xx)
       amax=hma
       tr=0.05*amax
c       write(6,*)'tr=',tr
       if(l.ge.2)go to 101
       if(tr-xx)101,102,102
  102  treg=xk
       l=l+1
  101  if(tr-xx)103,104,104
  103  l=1
  104       continue
         go to 86
   83  continue
       trg(j)=treg
c       write(6,*)'TREG=',treg
   85  continue
  171   format(2h+ ,14(1h-),1h+ ,45(1h-) ,1h+ )
  181   format(2hI ,2x,10hПОКАЗАТЕЛЬ,2x,1hI ,10x,5hKTET= ,f3.1,3x,
     *3hIH= ,f8.6,13x,1hI)
  190   format(2hI ,14x,1hI ,3(2x,3hIP= ,f8.7,2x),1hI)
  191   format(2hI ,2x,4hTREG ,8x,1hI ,1x,3(4x,f6.2,4x,1hI))
  192   format(2hI ,2x,7hSIGMAdH ,5x,1hI ,1x,3(4x,f6.2,4x,1hI))
       write(6,171)
       write(6,181)aaktet,hii
       write(6,190)pi
       write(6,171)
       write(6,191)trg
       write(6,192)si
       write(6,171)
         close(6)
         close(5)
  160   continue
        stop
        end
           subroutine fnc(t,y,n,z)
           dimension y(5),z(5)
           common/a/a11,a21,a22,b2,a46,a51,akwz,aaktet,hii,pii
           common/b/f,hzad,anu
           anu=a51*y(1)
           tez=hii*(hzad-y(4))+pii*y(5)
           da=akwz*y(2)+aaktet*(y(3)-tez+f)
           z(1)=-a11*y(1)+y(2)
           z(2)=-a21*y(1)-a22*y(2)-b2*da
           z(3)=y(2)
           tet=y(3)-y(1)
           z(4)=a46*tet
           z(5)=hzad-y(4)
           return
           end