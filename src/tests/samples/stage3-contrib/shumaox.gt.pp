proc main ()
    int a;
    int b;
    int c;
    int d;
    int e;
    int temp;
begin
    a := 2;
    write a;
    write "\n";
    b := 4;
    write "Integer: ";
    read b;
    write b;
    write "\n";
    c := ((a + b) + (2 * 2)) + ((20 + 20) / 2);
    write c;
    write "\n";
    d := 1;
    e := 2;
    temp := e;
    if e < d then
        temp := d;
    fi
    write "The bigger number is ";
    write temp;
    write "\n";
end
