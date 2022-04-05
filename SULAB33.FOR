              external fnc
        real*8 fi,pi
        character*12 idat,ires
       dimension y0(5),yk(5),yp(5),aktet(3),fi(3),pi(3),tsrr(50),
     *pp(9,4)
       common/a/a11,a21,a22,b2,a46,a51,akwz,aaktet,hii,pii
       common/b/f,hzad,anu
      write(*,*)'Вам предлагается выполнить лабораторную работу "Система
     * стабилизации высоты полета"'
    1  write(*,*)'Введите имя файла начальных данных в зависимости от
     *номера варианта, например:V1.dat'
       read(*,2)idat
       write(*,*)'Введите имя файла результатов, напрмер, V1.res'
       read(*,2)ires
    2  FORMAT(A12)
       open(5,file=idat)
       open(6,file=ires)
       write(*,*)' '
       read(5,*)
       read(5,*)a11,a21,a22,b2,a46,a51,akwz,aktet,fi,pi
       write(*,3)a11,a21,a22,b2,a46,a51,akwz,aktet,fi,pi
    3  FORMAT(4F16.7)
       hzad=100.
       f=0.
       aaktet=1.
       hii=0.000875
       pii=0.
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
       if(xk.ge.201.)go to 13
       if(xpe-xk+0.01)7,7,12
    7  do 8 i=1,3
    8  yp(i)=yp(i)*57.3
       write(6,*)xpe,yp
   14  format(1x,F5.2,5f16.6)
c       write(*,14)xpe,yp
       xpe=xpe+1.
   12  x0=xk
          go to 16
   13   write(*,*)'Пока'
         close(6)
         close(5)
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