{ test1 : OK }
program test4(a,b);
var
  a, a1, a2 : Char;
const
  b : Integer = 1;
var
  c, c1, c2 : Real;
const
  d : Real = 1.0e1;
  e : Real = 100;
  f : Real = -4;
const
  g : Integer = -4;
  h : Real = 2;
  i : Real = 2.3e35;
  j : Char = 'h';
begin
  read(a,c);
  write('wartosc a=',a,' wartosc c=',c:1:2);
end.
