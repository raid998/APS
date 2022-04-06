for file in test3/*.aps
do
    echo $file ": " 
    type=$(./prologTerm $file | swipl -s typeCheck.pl -g main_stdin 2>&1)
    if [[ $type = *"void"* ]]; then
        echo " Type Checking OK."
    else
        echo " Type Checking error! Result: $type"
    fi
    res=$(./eval $file)
    if [[ $res = "42" ]]; then
        echo " Eval Checking OK."
    else
        echo " Eval Checking error! Result: $res" 
    fi
done