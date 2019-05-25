proc main ()
    int m;
    int n;
    int result;
begin
    write "I will compute Ackermann(m, n) for you!\n";
    write "Type m (>= 0): ";
    read m;
    write "Type n (>= 0): ";
    read n;
    if (m < 0) || (n < 0) then
        write "Bad input. m and n must be non-negative.\n";
    else
        call ackermann(m, n, result);
        write result;
        write "\n";
    fi
end

proc ackermann (val int m, val int n, ref int result)
    int temp;
begin
    if m = 0 then
        result := n + 1;
    else
        if (m > 0) && (n = 0) then
            call ackermann(m - 1, 1, result);
        else
            call ackermann(m, n - 1, temp);
            call ackermann(m - 1, temp, result);
        fi
    fi
end
