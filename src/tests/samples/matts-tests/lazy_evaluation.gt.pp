proc main ()
begin
    write true && (false || ((1.0 / 0) > 0));
    write false && ((1 / 0) < 0);
end
