proc p (val int x, val int y)
    int a[2];
begin
    a[x] := 42;
    a[y] := a[x];
    write a[y];
    write "\n";
end

proc main ()
begin
    call p(1, 0);
end
