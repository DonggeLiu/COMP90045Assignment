proc f (val int x)
begin
    x := x * (3 + x);
    write x;
end

proc main ()
    int y;
begin
    y := 5;
    call f(y);
    write y;
    write "\n";
end
