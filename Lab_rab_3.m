clc
fprintf(' ������� �������� � ���������� ��\n')
fprintf('   ���������� ������� ���������\n')
fprintf('     ������������ ������ � 3\n')
fprintf('������� ������������ ������ ������\n\n')
global a11 a21 a22 b2 a46 a51 Kwz Ktet Ih Ip f DHz N_of_var
N_of_var=0;
while N_of_var < 1 | N_of_var > 3
N_of_var=input('Задай вариант, пёс: ');
switch N_of_var
   case 1
      a11=1.18;
      a21=3.56;
      a22=0.212;
      b2 =1.38;
      a46=168;
      a51=20.17;
      Kwz=2.5;
   case 2
      a11=0.857;
      a21=4.62;
      a22=0.094;
      b2 =1.32;
      a46=265;
      a51=23.2;
      Kwz=3;
   case 3
      a11=0.642;
      a21=5.65;
      a22=0.468;
      b2 =4.9;
      a46=168;
      a51=11;
      Kwz=0.4;
   otherwise
      fprintf('����� �������� ����� ��������\n')
   end
end   
fprintf('�������� �������������:\n')
fprintf(sprintf('a11=%7.3f\n',a11))
fprintf(sprintf('a21=%7.3f,   a22=%7.3f,   b2 =%7.3f\n',a21,a22,b2))
fprintf(sprintf('a46=%7.3f,   a51=%7.3f,   Kwz=%7.3f\n\n',a46,a51,Kwz))
Ktet=input('������� �������� ������������ Ktet: ');
f=0;
DHz=100;
Ih=0.000875;
Ip=0;
fprintf('������������ �������� Ih = 0.000875; Ip = 0. ���������� f = 0\n')
fprintf('����� �������� ����������� ���������� \n')
fprintf('��������� � ��������� ���� � ���� "�������� = ��������"\n')
Dummy=input('������� <<Enter>> ��� �������� ������');
open_system('gouskov.mdl')