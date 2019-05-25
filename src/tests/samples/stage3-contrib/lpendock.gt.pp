proc modulo (val int x, val int n, ref int out)
    int i;
begin
    i := 0;
    while i < x do
        i := i + n;
    od
    out := (n - i) + x;
end

proc mod_pow (val int x, val int e, val int n, ref int r)
    int i;
begin
    r := 1;
    i := 0;
    while i < e do
        r := r * x;
        call modulo(r, n, r);
        i := i + 1;
    od
end

proc modular_inverse (val int a, val int b, ref int out)
    int b0;
    int t;
    int q;
    int x0;
    int x1;
begin
    x0 := 0;
    x1 := 1;
    if b = 0 then
        out := 1;
    else
        while a > 1 do
            q := a / b;
            t := b;
            call modulo(a, b, b);
            a := t;
            t := x0;
            x0 := x1 - (q * x0);
            x1 := t;
        od
        if x1 < 0 then
            x1 := x1 + b0;
        fi
        out := x1;
    fi
end

proc sum_digits (val int n, ref int out)
    int sum;
    int t;
begin
    sum := 0;
    while n > 0 do
        call modulo(n, 10, t);
        sum := sum + t;
        n := (n - t) / 10;
    od
    out := sum;
end

proc main ()
    int m;
begin
    call modulo(34, 7, m);
    write m;
    write "\n";
    call mod_pow(43, 13, 3, m);
    write m;
    write "\n";
    call modular_inverse(476, 1453, m);
    write m;
    write "\n";
    call sum_digits(1794675, m);
    write m;
    write "\n";
end
