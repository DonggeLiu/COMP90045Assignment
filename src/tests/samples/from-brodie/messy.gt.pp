proc q (val float x, ref int k)
    int n;
    float y;
    bool a[8, 7];
    int b[9];
begin
    a[true, x + (1 * 2)] := true;
    a := a * a;
    a := a[1] * a[1, 23];
    a := a[1, 2] * a[1];
    a[true || false, false + true] := (a[1 + 1, 2 - 2] / a[true, false]) * a[true];
    k := 42.01;
end

proc p (ref int i)
begin
    i := ((6 * i) + 4) - i;
end

proc main ()
    int m;
    int n;
begin
    read n;
    while n > 1 do
        m0 := 0.00000212345 + 1;
        m1 := 12.002;
        while m > 0 do
            if m > 0 then
                n := n - 1;
                m := m - 1;
                if m = 0 then
                    call p(n);
                fi
            else
                m := n - m;
                m := m - 1;
            fi
        od
    od
    write "\n";
    write "textab";
end
