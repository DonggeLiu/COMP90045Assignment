proc f (val int w, val int x, val int y, val int z)
    int a[2, 3];
begin
    a[w, x] := 42;
    a[y, z] := a[w, x];
    write a[y, z];
    write "\n";
end

proc main ()
begin
    call f(1, 1, 0, 2);
end
