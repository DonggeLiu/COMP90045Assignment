proc main ()
begin
    call f(8, false);
    write "\n";
end

proc f (val int x, val bool y)
begin
    write (x > 4) || y;
end
