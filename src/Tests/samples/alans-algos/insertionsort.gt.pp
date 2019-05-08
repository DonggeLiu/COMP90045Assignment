proc main ()
    int numbers[10];
    int i;
    int j;
begin
    write "Insertion sort of 10 numbers!\n";
    i := 0;
    while i < 10 do
        write "Please type number ";
        write i + 1;
        write ": ";
        read numbers[i];
    od
    i := 1;
    while i < 10 do
        j := i;
        while (j > 0) && (numbers[j - 1] > numbers[j]) do
            call int_swap(numbers[j - i], numbers[j]);
            j := j - 1;
        od
        i := i + 1;
    od
end

proc int_swap (ref int a, ref int b)
    int temp;
begin
    temp := a;
    a := b;
    b := temp;
end
