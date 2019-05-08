proc main ()
    float x;
    float result;
    int n;
begin
    write "Float x: ";
    read x;
    write "Positive integer n: ";
    read n;
    call power(x, n, result);
    write "x^n is: ";
    write result;
    write "\n";
end

proc power (val float x, val int n, ref float out)
    float res;
begin
    if n = 1 then
        out := x;
    else
        call power(x * x, n / 2, res);
        if (2 * (n / 2)) = n then
            out := res;
        else
            out := x * res;
        fi
    fi
end
