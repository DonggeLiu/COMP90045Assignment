proc main ()
    int a[2, 3];
begin
    a[1, 1] := 42;
    a[0, 2] := a[1, 1];
    write a[0, 2];
    write "\n";
end
