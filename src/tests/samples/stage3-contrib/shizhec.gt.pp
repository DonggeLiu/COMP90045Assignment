proc main ()
    int x;
    int y;
    int z;
begin
    write "legs: x ";
    read x;
    write "legs: y ";
    read y;
    write "hypotenuse: z ";
    read z;
    call test(x, y, z);
end

proc test (val int x, val int y, val int z)
begin
    if ((x * x) + (y * y)) = (z * z) then
        write "True\n";
    else
        write "False\n";
    fi
end
