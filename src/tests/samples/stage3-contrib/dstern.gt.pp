proc main ()
    int n;
    int result;
begin
    write "Fibonacci calculator using DP.\n";
    write "This calculator finds the 12th Fibonacci number.\n";
    n := 12;
    call fib(n, result);
    write "The 12th Fibonacci number = ";
    write result;
    write "\n";
end

proc fib (val int n, ref int result)
    int dp[14];
    int i;
begin
    dp[0] := 0;
    dp[1] := 1;
    i := 2;
    while i <= n do
        dp[i] := dp[i - 1] + dp[i - 2];
        i := i + 1;
    od
    result := dp[n];
end
