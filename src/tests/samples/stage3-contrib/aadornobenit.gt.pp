proc main ()
    int a;
begin
    call a(1, 3, 4 + 3);
end

proc a (val int x, val int y, val int z)
begin
    write (x + y) + x;
    write "\n";
end
