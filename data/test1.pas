var
  a, b, c : integer;
  e, f, g : real;
Begin
  a := 1;
  b := 2;
  c := 4;
  a := a * ((b shl c) mod b);
  writeln(a);
  g := 1;
  f := 2;
  e := f / g;
  writeln(e:2:2);
END.
