proc main ()
    int num_1;
    int num_2;
    int sum;
    int difference;
    int product;
    int division_result;
begin
    write "Hello, give us two numbers, we will do some simple arithmetic operations for you: ";
    write "\n";
    write "Please enter the first number: ";
    read num_1;
    write "Please enter the second number: ";
    read num_2;
    write "\nNice, the two numbers you entered were ";
    write num_1;
    write " and ";
    write num_2;
    write "\nThe sum of the numbers are: ";
    sum := num_1 + num_2;
    write sum;
    write "\nThe difference of the numbers are: ";
    difference := num_1 - num_2;
    write difference;
    write "\nThe product of the numbers are: ";
    product := num_1 * num_2;
    write product;
    write "\nThe division result of the numbers are: ";
    product := num_1 / num_2;
    write division_result;
    write "\n";
end
