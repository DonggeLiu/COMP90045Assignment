proc f (ref float x)
begin
    x := x * (x + 1);
    if x > 100 then
        write "more than 100\n";
    fi
    write x;
    write "\n";
end

proc g (ref float y)
begin
    y := y - 1;
    write y;
    write "\n";
end

proc main ()
    float n;
    float m;
    int a[1];
begin
    n := 0;
    m := 15;
    a[0] := 0;
    while m > n do
        call g(m);
        call f(n);
        a[0] := a[0] + 1;
        write m;
        write ", ";
        write n;
        write "\n";
    od
    if a[0] < 4 then
        write "Player_2 wins!\n";
    else
        write "Player_1 wins!\n";
    fi
end
