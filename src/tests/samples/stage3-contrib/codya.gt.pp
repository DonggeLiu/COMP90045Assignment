proc main ()
begin
    call explore();
end

proc explore ()
    int node_count;
    int from;
    int to;
    bool echo;
    bool isDFS;
    int i;
    int cur;
    int ret;
    bool visited[100];
    bool edges[100, 100];
    int queue[1000];
    int qend;
    int start;
begin
    call get_info(isDFS, node_count, echo);
    if echo then
        write "\nAdd some edges (-1 to stop)\n";
    fi
    call get_edge(from, to, echo);
    while (from != -1) && (to != -1) do
        edges[from, to] := true;
        call get_edge(from, to, echo);
    od
    visited[0] := true;
    while i < node_count do
        if edges[0, i] && !visited[i] then
            queue[qend] := i;
            qend := qend + 1;
        fi
        i := i + 1;
    od
    write 0;
    call choose(isDFS, start, qend, ret);
    cur := queue[ret];
    while start != qend do
        write " -> ";
        visited[cur] := true;
        write cur;
        call advance(isDFS, start, qend);
        i := 0;
        while i < node_count do
            if !visited[i] && edges[cur, i] then
                queue[qend] := i;
                qend := qend + 1;
            fi
            i := i + 1;
        od
        call choose(isDFS, start, qend, ret);
        cur := queue[ret];
        while visited[cur] && (start != qend) do
            call advance(isDFS, start, qend);
            call choose(isDFS, start, qend, ret);
            cur := queue[ret];
        od
    od
    write "\n";
end

proc get_info (ref bool isDFS, ref int node_count, ref bool echo)
begin
    write "Print? (true or false)\n";
    read echo;
    if echo then
        write "DFS (true) or BFS (false)?:\n";
    fi
    read isDFS;
    if echo then
        write "How many Nodes (< 101):\n";
        write ":> ";
    fi
    read node_count;
    if echo then
        write "Your nodes are labeled 0 to ";
        write node_count - 1;
        write " (0 is the start node)\n";
    fi
end

proc get_edge (ref int from, ref int to, val bool echo)
begin
    if echo then
        write "From: ";
    fi
    read from;
    if from != -1 then
        if echo then
            write "To: ";
        fi
        read to;
    fi
end

proc print_type (val bool isDFS)
begin
    if isDFS then
        write "\nDFS: ";
    else
        write "\nBFS: ";
    fi
end

proc advance (val bool isDFS, ref int start, ref int qend)
begin
    if isDFS then
        qend := qend - 1;
    else
        start := start + 1;
    fi
end

proc choose (val bool isDFS, ref int start, ref int qend, ref int ret)
begin
    if isDFS then
        ret := qend - 1;
    else
        ret := start;
    fi
end
