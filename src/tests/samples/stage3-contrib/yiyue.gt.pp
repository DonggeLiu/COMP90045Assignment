proc main ()
    int a[1, 2];
    int c[1, 2];
    int b;
begin
    a[1, 0] := 1;
    b := 3;
    while b > 0 do
        write b;
        write " ";
        b := b - 1;
    od
    write "\n";
end
