proc main ()
    int a;
    int b;
    int c;
    int d;
    int result;
begin
    write "This program is going to calculate the value of ((a+b)*c)/d.";
    write "\nPlease type in the value of a: ";
    read a;
    write "\nPlease type in the value of b: ";
    read b;
    write "\nPlease type in the value of c: ";
    read c;
    write "\nPlease type in the value of d: ";
    read d;
    call calculate(a, b, c, d, result);
    write "\nThe result is: ";
    write result;
    write "\n";
end

proc calculate (val int a, val int b, val int c, val int d, ref int out)
begin
    if d = 0 then
        out := 0;
    else
        out := ((a + b) * c) / d;
    fi
end
