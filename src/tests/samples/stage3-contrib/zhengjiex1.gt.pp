proc main ()
    int a;
    int b[3];
begin
    if true then
        call p(a, b[2]);
    fi
end

proc p (val float x, ref int y)
    bool b1;
    bool b2;
    int a;
begin
    if b1 then
        write "i\n";
        if b2 then
            while b1 do
                write "iw";
                if b2 then
                    write "iwi";
                fi
            od
        fi
    else
        write "e";
        if b2 then
            write "ei";
        else
            while b1 do
                write "eew";
            od
        fi
        write "e";
    fi
    write "end";
    write "\n";
end
