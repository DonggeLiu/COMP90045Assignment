proc main ()
    float x;
    float sum;
    float sumsq;
    float mean;
    int count;
    bool done;
begin
    write "Type a list of real numbers, ending with a negative number\n";
    read x;
    done := x < 0;
    while !done do
        count := count + 1;
        sum := sum + x;
        sumsq := sumsq + (x * x);
        read x;
        done := x < 0;
    od
    mean := sum / count;
    write "The mean is ";
    write mean;
    write "\n";
    write "The variance is ";
    write (sumsq / count) - (mean * mean);
    write "\n";
end
