proc main ()
    int n;
    int i;
begin
    write "Enter number of line: ";
    read n;
    write "\n";
    while n > 0 do
        i := 0;
        while i < n do
            write "*";
            i := i + 1;
        od
        write "\n";
        n := n - 1;
    od
end
