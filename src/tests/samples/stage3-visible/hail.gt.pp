proc main ()
    int n;
    int result;
begin
    n := 1;
    while n < 21 do
        call hail(n, result);
        write result;
        write " ";
        n := n + 1;
    od
    write "\n";
end

proc hail (val int in, ref int out)
    int count;
begin
    count := 1;
    if in = 1 then
        out := count;
    else
        if ((in / 2) * 2) = in then
            call hail(in / 2, count);
        else
            call hail((3 * in) + 1, count);
        fi
        out := count + 1;
    fi
end
