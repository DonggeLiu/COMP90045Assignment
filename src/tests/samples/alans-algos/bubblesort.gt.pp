proc main ()
    int numbers[10];
    int i;
    int n;
    bool swapped;
begin
    write "Bubble sort of 10 numbers!\n";
    i := 0;
    while i < 10 do
        write "Please type number ";
        write i + 1;
        write ": ";
        read numbers[i];
    od
    n := 10;
    swapped := true;
    while swapped = true do
        swapped := false;
        i := 0;
        while i < (n - 1) do
            if numbers[i] > numbers[i + 1] then
                call int_swap(numbers[i], numbers[i + 1]);
                swapped := true;
            fi
            i := i + 1;
        od
        n := n - 1;
    od
end

proc int_swap (ref int a, ref int b)
    int temp;
begin
    temp := a;
    a := b;
    b := temp;
end
