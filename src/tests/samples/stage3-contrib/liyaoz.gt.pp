proc main ()
    int q1;
    int q2;
    int q3;
    int q4;
    int q5;
    int i;
    bool flag;
begin
    write "Quiz: Would you survive Thanos? [SPOILER ALERT] \n\n";
    flag := false;
    while !flag do
        write "Q1: Who is your favorite avenger? (Type the number)\n";
        write "1-Iron Man, 2-Captain America, 3-Thor, 4-Black Widow, 5-Hulk, 6-Hawk Eye\n";
        read q1;
        call check(q1, 1, 6, flag);
        if !flag then
            write "Invalid input! Please type one number.\n";
        fi
    od
    flag := false;
    while !flag do
        write "Q2: If you can wield the power of one Infinity Stone, which one do you prefer? \n";
        write "1-Space, 2-Soul, 3-Time, 4-Reality, 5-Power, 6-Mind\n";
        read q2;
        call check(q2, 1, 6, flag);
        if !flag then
            write "Invalid input! Please type one number.\n";
        fi
    od
    flag := false;
    while !flag do
        write "Q3: Team Cap or Team Iron Man?\n";
        write "1-I'm with Cap, 2-I'm with Tony\n";
        read q3;
        call check(q3, 1, 2, flag);
        if !flag then
            write "Invalid input! Please type one number.\n";
        fi
    od
    flag := false;
    while !flag do
        write "Q4: Who should lead the Avengers after Endgame?\n";
        write "1-Captain Marvel, 2-Falcon, 3-Scarlet Witch, 4-Nick Fury, 5-Doctor Strange, 6-Black Panther\n";
        read q4;
        call check(q4, 1, 6, flag);
        if !flag then
            write "Invalid input! Please type one number.\n";
        fi
    od
    flag := false;
    while !flag do
        write "Q5: Who leads the guardians of the galaxy after Endgame?\n";
        write "1-Quill, 2-Thor, 3-Rocket, 4-Drax, 5-Gamora, 6-I am Groot, 7-Nebula";
        read q5;
        call check(q5, 1, 7, flag);
        if !flag then
            write "Invalid input! Please type one number.\n";
        fi
    od
    i := 0;
    call mod2((((q1 + q2) + q3) + q4) + q5, i);
    if i = 0 then
        write "Congrats! You survived (but were left in sad and sorrow)\n";
    else
        write "Oh No! You are dust now...\n";
    fi
end

proc check (val int q, val int min, val int max, ref bool flag)
begin
    if (q >= min) && (q <= max) then
        flag := true;
    else
        flag := false;
    fi
end

proc mod2 (val int q, ref int i)
begin
    if q <= 1 then
        i := q;
    else
        call mod2(q - 2, i);
    fi
end
