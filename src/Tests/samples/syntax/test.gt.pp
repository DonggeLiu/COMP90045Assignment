proc zeroParams ()
    int var;
    int Var;
    float var1;
    bool var_2;
    bool v'ar;
begin
    var_2 := false;
    var_2 := true;
    var := 0;
    var := 12345678910;
    var1 := 0.1;
    var1 := 0.12345679;
    var1 := 123456.79;
    write "";
    write "hello world! 1234 $%^ \n () [] -+";
    write "begin end write call if while fi do";
    write "var := 0";
end

proc oneParam (val int var1)
    int array1[10];
    int array2[10, 10];
begin
    while i < 10 do
        array1[i] := i;
    od
    while i < 10 do
        while j < 10 do
            array2[i, j] := var1;
        od
    od
    read var1;
    read array[0];
    read array[0, 0];
end

proc twoParams (ref int var1, val bool var2, ref float var3)
    float var4;
    int something;
begin
    if var4 then
        write "hello";
    fi
    if !var4 then
        write "hello";
    else
        write "world";
    fi
end

proc callFunctions ()
    int val1;
    float val2;
begin
    val1 := 1;
    val2 := 1.1;
    call zeroParams();
    call oneParam(2);
    call twoParams(val1, true, val2);
end

proc booleanExpressions ()
    bool val1;
    bool val2;
    bool val3;
begin
    val1 := true;
    val2 := true;
    val1 := !val2;
    val3 := val1 && val2;
    val3 := val1 || val2;
    val3 := val1 = val2;
    val3 := val1 > val2;
    val3 := val1 < val2;
    val3 := val1 <= val2;
    val3 := val1 >= val2;
    val3 := val1 != val2;
    val3 := (val1 && val2) || !val1;
end

proc binaryExpressions ()
    int val1;
begin
    val1 := 10 + 20;
    val1 := 10 * 20;
    val1 := 10 / 20;
    val1 := 10 - 20;
    val1 := (10 - 20) + ((30 / 10) * 2);
    val1 := (((10 - 20) + 30) / 10) * 2;
end
