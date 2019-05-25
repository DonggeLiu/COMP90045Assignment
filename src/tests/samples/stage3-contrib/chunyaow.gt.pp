proc main ()
    int x;
    int iter;
    float y;
    float a;
    float b;
    float delta;
begin
    x := 7;
    a := 0;
    b := x;
    y := (a + b) / 2.0;
    iter := 0;
    delta := (y * y) - x;
    while ((delta > 0.1) || (delta < -0.1)) && (iter < 100) do
        iter := iter + 1;
        if delta > 0 then
            b := y;
        else
            a := y;
        fi
        y := (a + b) / 2.0;
        delta := (y * y) - x;
    od
    write "\n";
    write "The square root of ";
    write x;
    write "is ";
    write y;
    write "\n";
end
