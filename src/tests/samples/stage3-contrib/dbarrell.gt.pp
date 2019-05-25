proc main ()
    int result;
    int n;
begin
    write "Calculates the n-th number in the Fibonacci sequence\n";
    write "Enter n (<= 45): \n";
    read n;
    if n <= 45 then
        call fib(n, result);
        write result;
        write "\n";
    else
        write "Number must be less than or equal to 45\n";
    fi
end

proc fib (val int n, ref int result)
    int i;
    int f[45];
begin
    f[0] := 0;
    f[1] := 1;
    i := 2;
    while i <= n do
        f[i] := f[i - 1] + f[i - 2];
        i := i + 1;
    od
    result := f[n];
end
