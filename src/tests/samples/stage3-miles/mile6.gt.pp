proc main ()
    int n;
    float y;
    int v[3];
    bool b[2, 2];
begin
    v[1] := 2;
    write "Provide an integer: ";
    read v[0];
    write v[1];
    write "\n";
    write v[0];
    write "\n";
    if !b[0, 0] then
        write b[0, 1] || b[1, 0];
        write "\n";
    fi
end
