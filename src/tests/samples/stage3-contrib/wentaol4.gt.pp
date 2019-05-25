proc main ()
    int number;
begin
    write "Give an integer:\n";
    read number;
    write "The factors are:\n";
    call factors(number);
end

proc factors (val int n)
    int i;
    int r;
begin
    i := 1;
    while i <= n do
        call mod(n, i, r);
        if r = 0 then
            write i;
            write "\n";
        fi
        i := i + 1;
    od
end

proc mod (val int a, val int b, ref int r)
begin
    r := a - ((a / b) * b);
end
