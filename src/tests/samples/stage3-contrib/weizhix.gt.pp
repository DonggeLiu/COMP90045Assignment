proc main ()
    int a[6, 6];
    int n;
    int i;
    int j;
begin
    read n;
    if (n > 5) || (n <= 10) then
        write "Result:\n";
        i := 0;
        while i < n do
            if i = 0 then
                a[0, 0] := 1;
                write "1\n";
            else
                if i = 1 then
                    a[1, 0] := 1;
                    a[1, 1] := 1;
                    write "1 1\n";
                else
                    a[i, 0] := 1;
                    a[i, i] := 1;
                    j := 1;
                    while j <= (i - 1) do
                        a[i, j] := a[i - 1, j - 1] + a[i - 1, j];
                        j := j + 1;
                    od
                    j := 0;
                    while j <= i do
                        write a[i, j];
                        write " ";
                        j := j + 1;
                    od
                    write "\n";
                fi
            fi
            i := i + 1;
        od
    else
        write "Invalid input\n";
    fi
end
