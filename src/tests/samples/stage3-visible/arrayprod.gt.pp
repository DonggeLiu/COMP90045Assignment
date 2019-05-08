proc main ()
    int i;
    int result;
    int a[3];
    int b[3];
begin
    a[0] := 5;
    a[1] := 7;
    a[2] := 9;
    b[0] := 6;
    b[1] := 8;
    b[2] := 10;
    while i < 3 do
        result := result + (a[i] * b[i]);
        i := i + 1;
    od
    write result;
    write "\n";
end
