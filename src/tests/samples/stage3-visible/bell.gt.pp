proc main ()
    int n;
    int result;
begin
    write "Type integer n: ";
    read n;
    call bell(n, 0, result);
    write "Bell(n) is: ";
    write result;
    write "\n";
end

proc bell (val int n, val int m, ref int out)
    int res1;
    int res2;
begin
    if m = 0 then
        if n = 0 then
            out := 1;
        else
            call bell(n - 1, n - 1, out);
        fi
    else
        call bell(n, m - 1, res1);
        call bell(n - 1, m - 1, res2);
        out := res1 + res2;
    fi
end
