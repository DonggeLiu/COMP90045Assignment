proc main ()
    int a1;
    int b1;
    int c1;
    int input_var;
    float a2;
    float b2;
    bool result;
    int arra[3];
    int mart[2, 3];
begin
    read input_var;
    a1 := 1;
    b1 := 2;
    if a1 < b1 then
        result := true;
    else
        result := false;
    fi
    a2 := 1.1;
    b2 := 2.2;
    c1 := 0;
    while c1 < 3 do
        if a1 != b1 then
            c1 := c1 + 1;
        fi
    od
    call func1(a1, b1);
    write c1;
    write "\n";
end

proc func1 (val int a1, val int a2)
    int c;
begin
    c := a1 + a2;
end
