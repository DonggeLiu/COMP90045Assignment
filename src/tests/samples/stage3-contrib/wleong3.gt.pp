proc checkIsWin (val int opleft, val int opright, ref bool isWin)
begin
    if (opleft = 0) && (opright = 0) then
        isWin := true;
    else
        isWin := false;
    fi
end

proc printHand (val int left, val int right, val int player)
begin
    write "Player ";
    write player;
    write " hands: ";
    write left;
    write " ";
    write right;
    write "\n";
end

proc tap (ref int p1Left, ref int p2Left, ref int p1Right, ref int p2Right, val int player)
    bool fromLeft;
    bool toLeft;
begin
    read fromLeft;
    read toLeft;
    if player = 1 then
        if fromLeft && toLeft then
            p2Left := p2Left + p1Left;
        fi
        if fromLeft && !toLeft then
            p2Right := p2Right + p1Left;
        fi
        if !fromLeft && toLeft then
            p2Left := p2Left + p1Right;
        fi
        if !fromLeft && !toLeft then
            p2Right := p2Right + p1Right;
        fi
    else
        if fromLeft && toLeft then
            p1Left := p1Left + p2Left;
        fi
        if fromLeft && !toLeft then
            p1Right := p1Right + p2Left;
        fi
        if !fromLeft && toLeft then
            p1Left := p1Left + p2Right;
        fi
        if !fromLeft && !toLeft then
            p1Right := p1Right + p2Right;
        fi
    fi
    if p1Left >= 5 then
        p1Left := p1Left - 5;
    fi
    if p2Left >= 5 then
        p2Left := p2Left - 5;
    fi
    if p1Right >= 5 then
        p1Right := p1Right - 5;
    fi
    if p2Right >= 5 then
        p2Right := p2Right - 5;
    fi
end

proc swap (ref int left, ref int right)
    bool toLeft;
    int nPoints;
begin
    read toLeft;
    read nPoints;
    if toLeft then
        left := left + nPoints;
        right := right - nPoints;
    else
        left := left - nPoints;
        right := right + nPoints;
    fi
end

proc main ()
    bool isWin;
    int player;
    int winner;
    int p1Left;
    int p1Right;
    int p2Left;
    int p2Right;
    int move;
begin
    player := 1;
    isWin := false;
    p1Left := 1;
    p2Left := 1;
    p1Right := 1;
    p2Right := 1;
    while !isWin do
        write "Enter move: ";
        read move;
        if move = 0 then
            call tap(p1Left, p2Left, p1Right, p2Right, player);
        else
            if player = 1 then
                call swap(p1Left, p1Right);
            else
                call swap(p2Left, p2Right);
            fi
        fi
        call printHand(p1Left, p1Right, player);
        call printHand(p2Left, p2Right, player);
        if player = 1 then
            call checkIsWin(p2Left, p2Right, isWin);
            if isWin then
                winner := player;
            fi
        else
            call checkIsWin(p1Left, p1Right, isWin);
            if isWin then
                winner := player;
            fi
        fi
        if player = 1 then
            player := 2;
        else
            player := 1;
        fi
    od
    write "Winner: ";
    write winner;
    write "\n";
end
