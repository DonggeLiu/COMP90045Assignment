proc main ()
    int numbers[10];
    int i;
    int j;
    int i_min;
begin
    write "Selection sort of 10 numbers!\n";
    i := 0;
    while i < 10 do
        write "Please type number ";
        write i + 1;
        write ": ";
        read numbers[i];
    od
    j := 0;
    while j < 10 do
        i_min := j;
        i := j + 1;
        while i < 10 do
            if numbers[i] < numbers[i_min] then
                i_min := i;
            fi
            i := i + 1;
        od
        call int_swap(numbers[j], numbers[i_min]);
        j := j + 1;
    od
end

proc int_swap (ref int a, ref int b)
    int temp;
begin
    temp := a;
    a := b;
    b := temp;
end
