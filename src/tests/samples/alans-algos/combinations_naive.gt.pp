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
    int nfact;
    int rfact;
    int nminusrfact;
begin
    call fact(n, nfact);
    call fact(r, rfact);
    call fact(n - r, nminusrfact);
    result := nfact / (rfact * nminusrfact);
end

proc fact (val int n, ref int result)
    int subresult;
begin
    if n < 0 then
        result := -1;
    else
        if n = 0 then
            result := 1;
        else
            call fact(n - 1, subresult);
            result := n * subresult;
        fi
    fi
end
