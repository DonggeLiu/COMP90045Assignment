proc main ()
    int n;
    int count;
begin
    write "Enter number <= 10000: \n";
    read n;
    if n <= 10000 then
        call primes(n, count);
        write "π(";
        write n;
        write ") = ";
        write count;
        write "\n";
    else
        write "Error: number greater than 10000\n";
    fi
end

proc primes (val int n, ref int count)
    int i;
    int multiplier;
    bool is_prime[10001];
    int largest_found_prime;
begin
    i := 1;
    while i <= n do
        is_prime[i] := true;
        i := i + 1;
    od
    is_prime[1] := false;
    i := 2;
    while (i * i) <= n do
        if is_prime[i] = true then
            multiplier := 2;
            while (i * multiplier) <= n do
                is_prime[multiplier * i] := false;
                multiplier := multiplier + 1;
            od
        fi
        i := i + 1;
    od
    i := 1;
    while i <= n do
        if is_prime[i] = true then
            count := count + 1;
            largest_found_prime := i;
        fi
        i := i + 1;
    od
    i := 1;
    while i <= n do
        if is_prime[i] = true then
            write i;
            if largest_found_prime = i then
                write "\n";
            else
                write ", ";
            fi
        fi
        i := i + 1;
    od
end
