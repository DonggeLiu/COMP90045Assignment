proc f (val float x)
begin
    x := x + 1;
end

proc main ()
    int n;
begin
    n := 41;
    call f(n);
end
