proc main ()
    int n;
    int r;
    int combos;
begin
    write "Combinations calculator.\n";
    write "Enter collection size (n): ";
    read n;
    write "Enter selection size (r): ";
    read r;
    if (n < 0) || (r > n) then
        write "Bad input. n must be non-negative and r must not exceed n.\n";
    else
        call nCr(n, r, combos);
        write "There are ";
        write combos;
        write " combinations of size ";
        write r;
        write " from a collection of ";
        write n;
        write " items.\n";
    fi
end

proc nCr (val int n, val int r, ref int result)
    int i;
    int numerator;
    int rfact;
begin
    call fact(r, rfact);
    i := (n - r) + 1;
    numerator := 1;
    while i <= n do
        numerator := numerator * i;
        i := i + 1;
    od
    result := numerator / rfact;
end

proc fact (val int n, ref int result)
    int i;
begin
    if n < 0 then
        result := -1;
    else
        i := 1;
        result := 1;
        while i <= n do
            result := result * i;
            i := i + 1;
        od
    fi
end
