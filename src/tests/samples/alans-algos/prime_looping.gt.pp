proc main ()
    int n;
    bool isprime;
begin
    write "Primality checking by looping.\n";
    write "Type a non-negative integer: ";
    read n;
    write n;
    if n < 0 then
        write " is a negative number.\n";
    else
        if n > 1000 then
            write " is too large to check.\n";
        else
            call check_prime(n, isprime);
            if isprime then
                write " is a prime number!\n";
            else
                write " is not a prime number.\n";
            fi
        fi
    fi
end

proc check_prime (val int n, ref bool result)
    int i;
    int j;
begin
    if n < 2 then
        result := false;
    else
        if n = 2 then
            result := true;
        else
            i := 2;
            j := 2;
            result := true;
            while ((i * i) <= n) && (result = true) do
                if (i * j) = n then
                    result := false;
                else
                    if (i * j) > n then
                        i := i + 1;
                        j := 2;
                    else
                        j := j + 1;
                    fi
                fi
            od
        fi
    fi
end
