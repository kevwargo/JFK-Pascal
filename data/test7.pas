var
  t1 : array [-1..3,4..5.) of integer;
  t2 : array (.'a' .. 'd'] of array ['k' .. 'm'] of char;
begin
  t1[0,4]:=2;
  t2['c']:='f';
end.
