proc modInplace (ref int a, val int b)
    int m;
begin
    m := a / b;
    a := a - (m * b);
end

proc mod (val int a, val int b, ref int m)
begin
    m := a / b;
    m := a - (m * b);
end

proc powMod (val int base, val int pow, val int modBy, ref int m)
    int tmp;
begin
    call modInplace(base, modBy);
    m := 1;
    while pow > 0 do
        call mod(pow, 2, tmp);
        if tmp > 0 then
            m := m * base;
            call modInplace(m, modBy);
        fi
        base := base * base;
        call modInplace(base, modBy);
        pow := pow / 2;
    od
    call modInplace(m, modBy);
end

proc main ()
    int i;
    float f;
    bool b;
    int base;
    int pow;
    int lastDigit;
begin
    i := 90045;
    f := 21.45;
    b := true;
    write "int value i = ";
    write i;
    write "\n";
    write "float value f = ";
    write f;
    write "\n";
    write "boolean value b = ";
    write b;
    write "\n";
    write "input base (non-negative int): ";
    read base;
    write "input pow (positive int): ";
    read pow;
    call powMod(base, pow, 10, lastDigit);
    write "\n----------------------------------\n";
    write "The last digit of (base ^ pow) is ";
    write lastDigit;
    write ".\n";
end
